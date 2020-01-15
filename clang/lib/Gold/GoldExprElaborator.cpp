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
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringMap.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldTokens.h"

#include <cstring>

namespace gold {

ExprElaborator::ExprElaborator(clang::ASTContext &CxxAST, Sema &SemaRef)
  : CxxAST(CxxAST), SemaRef(SemaRef)
{}

ExprElaborator::Expression
ExprElaborator::elaborateExpr(const Syntax *S) {
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

ExprElaborator::Expression
ExprElaborator::elaborateAtom(const AtomSyntax *S, clang::QualType ExplicitType) {
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
// TODO: Assignment will probably be handled differently than other bin ops?
const llvm::StringMap<clang::BinaryOperatorKind> BinaryOperators = {
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
};

const llvm::StringMap<clang::BinaryOperatorKind> CompoundAssignOperators = {
  {"operator'+='" , clang::BO_AddAssign},
  {"operator'-='" , clang::BO_SubAssign},
  {"operator'*='" , clang::BO_MulAssign},
  {"operator'/='" , clang::BO_DivAssign},
  {"operator'%='" , clang::BO_RemAssign},
  {"operator'&='" , clang::BO_AndAssign},
  {"operator'|='" , clang::BO_OrAssign},
  {"operator'^='" , clang::BO_XorAssign},
};

ExprElaborator::Expression
ExprElaborator::elaborateCall(const CallSyntax *S) {
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  std::string Spelling = Callee->Tok.getSpelling();

  if (&CxxAST.Idents.get(Spelling) == SemaRef.OperatorColonII) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

    Elaborator Elab(SemaRef.getContext(), SemaRef);
    clang::QualType T = Elab.getOperatorColonType(S);

    return elaborateAtom(cast<AtomSyntax>(ArgList->Elems[0]), T);
  }

  // Check if this is a standard binary operator (one that doesn't assign).
  auto BinOpMapIter = BinaryOperators.find(Spelling);
  if (BinOpMapIter != BinaryOperators.end()) {
    return elaborateBinOp(S, BinOpMapIter->second);
  }

  // Check if this is a compound assignment operator like operator'+='.
  auto CmpAssnMapIter = CompoundAssignOperators.find(Spelling);
  if (CmpAssnMapIter != CompoundAssignOperators.end()) {
    return elaborateBinOp(S, CmpAssnMapIter->second);
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
      ExprElaborator Elab(CxxAST, SemaRef);
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
    if (Call.isInvalid())
      return nullptr;
    return Call.get();
  }

  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

ExprElaborator::Expression
ExprElaborator::elaborateBinOp(const CallSyntax *S,
                               clang::BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  // FIXME: what?
  // The LHS as written becomes the RHS in our implicit statement,
  // due to recursion. 
  Expression RHS = elaborateExpr(LHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression LHS = elaborateExpr(RHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  // FIXME: replace with Sema::ActOnBinOp
  clang::ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr,
                                               clang::SourceLocation(), Op,
                                               LHS.get<clang::Expr *>(),
                                               RHS.get<clang::Expr *>());
  if (Res.isInvalid())
    return nullptr;

  return Res.get();
}

// FIXME: how is this different from elaborateBinOp?
ExprElaborator::Expression
ExprElaborator::elaborateCmpAssignOp(const CallSyntax *S,
                                     clang::BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  // FIXME: error carry-forward? see: ElaborateBinOp FIXME
  Expression RHS = elaborateExpr(LHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression LHS = elaborateExpr(RHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  clang::ExprResult Res =
    ClangSema.CreateBuiltinBinOp(clang::SourceLocation(), Op,
                                 LHS.get<clang::Expr *>(),
                                 RHS.get<clang::Expr *>());
  if (Res.isInvalid())
    return nullptr;

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
ExprElaborator::Expression
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  Expression LHS, RHS;

  {
    ExprElaborator ExEl(CxxAST, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));

    if (LHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(CxxAST, SemaRef);
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
  if (BinOp.isInvalid())
    return nullptr;

  // For all remaining terms, append them to the back of the && expression.
  // Ex., if we had `1 && 2`, we would append `3` to get `1 && 2 && 3`.
  for (unsigned I = 2; I < Conditions->getNumChildren(); ++I) {
    ExprElaborator ExEl(CxxAST, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(I));

    BinOp =
      SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                      clang::tok::ampamp, BinOp.get(),
                                      RHS.get<clang::Expr *>());
    if (BinOp.isInvalid())
      return nullptr;
  }

  return BinOp.get();
}

} // namespace gold
