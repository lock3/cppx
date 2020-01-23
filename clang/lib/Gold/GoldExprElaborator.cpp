//=== GoldExprElaborator.cpp - Elaboration for Gold Expressions -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ExprElaborator interface, which creates
//  clang::Expr nodes out of gold expressions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringMap.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldTokens.h"

#include <cstring>

namespace gold {

using TypeInfo = ExprElaborator::TypeInfo;
using Expression = ExprElaborator::Expression;

ExprElaborator::ExprElaborator(SyntaxContext &Context, Sema &SemaRef)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef)
{}

Expression ExprElaborator::elaborateExpr(const Syntax *S) {
  clang::Expr *Ret = nullptr;
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S), clang::QualType());
  if (isa<CallSyntax>(S))
    return Ret;
    return elaborateCall(cast<CallSyntax>(S));

  assert(false && "Unsupported expression.");
}

static clang::IntegerLiteral *
createIntegerLiteral(clang::ASTContext &CxxAST, Token T, clang::QualType IntType,
                     clang::SourceLocation Loc) {
  llvm::APInt Value;
  unsigned Width = 0;

  // If we don't have a specified type, just create a default int.
  if (IntType.isNull() || IntType == CxxAST.AutoDeductTy)
    IntType = CxxAST.IntTy;

  // TODO: support all kinds of integer types.
  if (IntType == CxxAST.IntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    long long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.ShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.UnsignedShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    unsigned short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedIntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    unsigned int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else {
    assert(false && "Unsupported integer type.");
  }

  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);

  return clang::IntegerLiteral::Create(CxxAST, Value, IntType, Loc);
}

static clang::DeclRefExpr *
createDeclRefExpr(clang::ASTContext &CxxAST, Sema &SemaRef, Token T,
                  clang::QualType Ty, clang::SourceLocation Loc) {
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(T.getSpelling())}, Loc);
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());
  if (!R.empty()) {
    if (!R.isSingleResult()) {
      SemaRef.Diags.Report(T.getLocation(), clang::diag::err_multiple_declarations);
      return nullptr;
    }

    clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>();
    clang::QualType FoundTy = VD->getType();

    // If the user annotated the DeclRefExpr with an incorrect type.
    if (!Ty.isNull() && Ty != FoundTy) {
      SemaRef.Diags.Report(T.getLocation(), clang::diag::err_type_annotation_mismatch)
        << FoundTy << Ty;
      return nullptr;
    }

    // FIXME: discern whether this is an lvalue or rvalue properly
    clang::DeclRefExpr *DRE =
      clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                 clang::SourceLocation(), VD, /*Capture=*/false,
                                 Loc, FoundTy, clang::VK_LValue);
    return DRE;
  }

  return nullptr;
}

Expression ExprElaborator::elaborateAtom(const AtomSyntax *S,
                                         clang::QualType ExplicitType) {
  Token T = S->Tok;

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(CxxAST, T, ExplicitType, S->getTokenLoc());
  case tok::DecimalFloat:
    break;
  case tok::BinaryInteger:
    break;
  case tok::HexadecimalInteger:
    break;
  case tok::HexadecimalFloat:
    break;
  case tok::Identifier:
    return createDeclRefExpr(CxxAST, SemaRef, T, ExplicitType, S->getTokenLoc());
    break;
  case tok::Character:
    break;
  case tok::String:
    break;
  default: break;
  }

  return nullptr;
}

// Mapping of Gold's fused operator strings to clang Opcodes.
static const llvm::StringMap<clang::BinaryOperatorKind> BinaryOperators = {
  {"operator'+'" , clang::BO_Add},
  {"operator'-'" , clang::BO_Sub},
  {"operator'*'" , clang::BO_Mul},
  {"operator'/'" , clang::BO_Div},
  {"operator'%'" , clang::BO_Rem},
  {"operator'&'" , clang::BO_And},
  {"operator'|'" , clang::BO_Or},
  {"operator'^'" , clang::BO_Xor},
  {"operator'&&'" , clang::BO_LAnd},
  {"operator'||'" , clang::BO_LOr},
  {"operator'=='" , clang::BO_EQ},
  {"operator'<>'", clang::BO_NE},
  {"operator'<'", clang::BO_LT},
  {"operator'>'", clang::BO_GT},
  {"operator'<='", clang::BO_LE},
  {"operator'>='", clang::BO_GE},
  {"operator'+='" , clang::BO_AddAssign},
  {"operator'-='" , clang::BO_SubAssign},
  {"operator'*='" , clang::BO_MulAssign},
  {"operator'/='" , clang::BO_DivAssign},
  {"operator'%='" , clang::BO_RemAssign},
  {"operator'&='" , clang::BO_AndAssign},
  {"operator'|='" , clang::BO_OrAssign},
  {"operator'^='" , clang::BO_XorAssign},
};

Expression ExprElaborator::elaborateCall(const CallSyntax *S) {
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  std::string Spelling = Callee->Tok.getSpelling();

  // a fused operator':' call
  if (&CxxAST.Idents.get(Spelling) == SemaRef.OperatorColonII) {
    Elaborator Elab(SemaRef.getContext(), SemaRef);

    // If the LHS of the operator':' call is just a name, we can try to
    // reference or create it.
    if (isa<AtomSyntax>(S->getArgument(0))) {
      // FIXME: replace this with a normal type elaboration
      clang::QualType T = Elab.getOperatorColonType(S);
      return elaborateAtom(cast<AtomSyntax>(S->getArgument(0)), T);
    }

    // Otherwise, we need to continue elaborating the LHS until it is an atom.
    elaborateExpr(S->getArgument(0));
    return nullptr;
  }

  // Check if this is a binary operator.
  auto BinOpMapIter = BinaryOperators.find(Spelling);
  if (BinOpMapIter != BinaryOperators.end()) {
    return elaborateBinOp(S, BinOpMapIter->second);
  }

  // Try to construct a normal function-call expression.
  // First do unqualified lookup.
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Spelling)}, S->getLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  // If we found something, see if it is viable.
  if (!R.empty()) {
    clang::Expr *Fn = nullptr;

    R.resolveKind();
    if (R.isOverloadedResult()) {
      Fn =
        clang::UnresolvedLookupExpr::Create(CxxAST, R.getNamingClass(),
                                            clang::NestedNameSpecifierLoc(),
                                            R.getLookupNameInfo(), /*ADL=*/true,
                                            /*Overloaded=*/true, R.begin(),
                                            R.end());
    } else if (R.isSingleResult()) {
      clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>();

      // This had better be a reference to a function.
      clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(VD);
      if (!FD) return nullptr;

      Fn =
        clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                   clang::SourceLocation(), VD, /*Capture=*/false,
                                   S->getLoc(), VD->getType(), clang::VK_RValue);
    }

    if (!Fn)
      return nullptr;

    // Get the passed arguments.
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
    assert(ArgList && "Unexpected argument format.");
    for (const Syntax *A : ArgList->children()) {
      ExprElaborator Elab(Context, SemaRef);
      Expression Argument = Elab.elaborateExpr(A);

      // FIXME: What kind of expression is the unary ':typename' expression?
      if (Argument.is<clang::TypeSourceInfo *>()) {
        SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
        return nullptr;
      }

      Args.push_back(Argument.get<clang::Expr *>());
    }

    // Create the call.
    clang::MultiExprArg MultiArgs(Args);
    clang::ExprResult Call =
      SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                         Fn, S->getCalleeLoc(),
                                         MultiArgs, S->getCalleeLoc());
    if (Call.isInvalid()) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_failed_to_translate_expr);
      return nullptr;
    }

    return Call.get();
  }

  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

Expression ExprElaborator::elaborateBinOp(const CallSyntax *S,
                                          clang::BinaryOperatorKind Op) {
  const Syntax *LHSSyntax = S->getArgument(0);
  const Syntax *RHSSyntax = S->getArgument(1);

  Expression LHS = elaborateExpr(LHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>() || LHS.isNull()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression RHS = elaborateExpr(RHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>() || RHS.isNull()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  // FIXME: Replace with ActOnBinOp so precedence issues get warnings.
  clang::ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr,
                                               S->getLoc(), Op,
                                               LHS.get<clang::Expr *>(),
                                               RHS.get<clang::Expr *>());
  if (Res.isInvalid()) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  return Res.get();
}

/// Create an expression for a block condition. Ex:
///
/// \code
/// if:
///   expr_1
///   expr_2
///   ...
///   expr_n
/// \endcode
/// We just create a logical and expression with n terms: one for each
/// sub expression.
Expression
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  Expression LHS, RHS;

  {
    ExprElaborator ExEl(Context, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));

    if (LHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(1));

    if (RHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(1)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }

  clang::ExprResult BinOp =
    SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                    clang::tok::ampamp,
                                    LHS.get<clang::Expr *>(),
                                    RHS.get<clang::Expr *>());
  if (BinOp.isInvalid()) {
    SemaRef.Diags.Report(Conditions->getLoc(),
                         clang::diag::err_invalid_block_condition);
    return nullptr;
  }

  // For all remaining terms, append them to the back of the && expression.
  // Ex., if we had `1 && 2`, we would append `3` to get `1 && 2 && 3`.
  for (unsigned I = 2; I < Conditions->getNumChildren(); ++I) {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(I));

    BinOp =
      SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                      clang::tok::ampamp, BinOp.get(),
                                      RHS.get<clang::Expr *>());
    if (BinOp.isInvalid()) {
      SemaRef.Diags.Report(Conditions->getLoc(),
                           clang::diag::err_invalid_block_condition);
      return nullptr;
    }
  }

  return BinOp.get();
}

//===----------------------------------------------------------------------===//
//                        Type Expression Elaboration                         //
//===----------------------------------------------------------------------===//

// Get a vector of declarators.
static void getDeclarators(Declarator *D,
                           llvm::SmallVectorImpl<Declarator *> &Decls) {
  while (D) {
    Decls.push_back(D);
    D = D->Next;
  }
}

Expression ExprElaborator::elaborateTypeExpr(Declarator *D) {
  // The type of a declarator is constructed back-to-front.
  llvm::SmallVector<Declarator *, 4> Decls;
  getDeclarators(D, Decls);

  // The type is computed from back to front. Start by assuming the type
  // is auto. This will be replaced if an explicit type specifier is given.
  clang::QualType AutoType = CxxAST.getAutoDeductType();
  TypeInfo *TInfo = BuildAnyTypeLoc(CxxAST, AutoType, D->getLoc());

  for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
    D = *Iter;
    switch (D->Kind) {
    case DK_Identifier:
      // The identifier is not part of the type.
      break;

    case DK_Pointer: {
      Expression TypeExpr = elaboratePointerType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Array: {
      Expression TypeExpr = elaborateArrayType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Function: {
      Expression TypeExpr = elaborateFunctionType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Type: {
      Expression TypeExpr = elaborateExplicitType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    default:
      llvm_unreachable("Invalid declarator");
    }
  }

  return TInfo;
}

Expression ExprElaborator::elaboratePointerType(Declarator *D, TypeInfo *Ty) {
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (BaseTypeExpr.is<clang::Expr *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType = BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::QualType PtrType = CxxAST.getPointerType(BaseType);

  return BuildAnyTypeLoc(CxxAST, PtrType, D->getType()->getLoc());
}

Expression ExprElaborator::elaborateArrayType(Declarator *D, TypeInfo *Ty) {
  llvm_unreachable("Arrays not supported");
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
Expression ExprElaborator::elaborateFunctionType(Declarator *D, TypeInfo *Ty) {
  const auto *Call = cast<CallSyntax>(D->Call);

  // FIXME: Handle array-based arguments.
  assert(isa<ListSyntax>(D->Data.ParamInfo.Params)
         && "Array parameters not supported");
  const Syntax *Args = D->Data.ParamInfo.Params;

  // Elaborate the parameter declarations in order to get their types, and save
  // the resulting scope with the declarator.
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  SemaRef.enterScope(SK_Parameter, Call);
  for (const Syntax *P : Args->children()) {
    Elaborator Elab(Context, SemaRef);
    clang::ValueDecl *VD = cast<clang::ValueDecl>(Elab.elaborateDeclSyntax(P));

    assert(isa<clang::ParmVarDecl>(VD) && "Parameter is not a ParmVarDecl");

    Types.push_back(VD->getType());
    Params.push_back(cast<clang::ParmVarDecl>(VD));
  }
  D->Data.ParamInfo.Scope = SemaRef.saveScope(Call);

  // FIXME: We probably need to configure parts of the prototype (e.g.,
  // make this noexcept by default).
  clang::FunctionProtoType::ExtProtoInfo EPI;

  using clang::SourceLocation;
  using clang::SourceRange;

  clang::QualType FnTy = CxxAST.getFunctionType(Ty->getType(), Types, EPI);
  return BuildFunctionTypeLoc(CxxAST, FnTy,
    SourceLocation(), SourceLocation(), SourceLocation(),
    SourceRange(), SourceLocation(), Params);
}

Expression ExprElaborator::elaborateExplicitType(Declarator *D, TypeInfo *Ty) {
  assert(isa<clang::AutoType>(Ty->getType()));
  assert(D->Kind == DK_Type);

  // FIXME: We should really elaborate the entire type expression. We're
  // just cheating for now.
  if (const auto *Atom = dyn_cast<AtomSyntax>(D->Data.Type)) {
    auto BuiltinMapIter = BuiltinTypes.find(Atom->getSpelling());
    if (BuiltinMapIter == BuiltinTypes.end()) {
      // FIXME: This requires a type lookup.
      assert(false && "User-defined types not supported.");
    }

    return BuildAnyTypeLoc(CxxAST, BuiltinMapIter->second,
                           D->getType()->getLoc());
  }

  llvm_unreachable("Unknown type specification");
}

} // namespace gold
