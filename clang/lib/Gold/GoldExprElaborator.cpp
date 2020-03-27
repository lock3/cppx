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
#include "clang/AST/ExprCppx.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringMap.h"
#include "clang/AST/OperationKinds.h"


#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldExprMarker.h"
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
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S), clang::QualType());
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if(isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));
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

static clang::Expr *
CreateIdentiferAccess(clang::ASTContext &CxxAST, Sema &SemaRef, Token T,
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
    VD->setIsUsed();

    // If the user annotated the DeclRefExpr with an incorrect type.
    if (!Ty.isNull() && Ty != FoundTy) {
      SemaRef.Diags.Report(T.getLocation(), clang::diag::err_type_annotation_mismatch)
        << FoundTy << Ty;
      return nullptr;
    }

    if (isa<clang::FieldDecl>(VD)) {
      // Building this access.
      clang::FieldDecl* Field = cast<clang::FieldDecl>(VD);
      clang::RecordDecl* RD = Field->getParent();
      // FIXME: Add CV qualifiers here if needed
      clang::QualType ThisTy(RD->getTypeForDecl(), 0);
      clang::QualType ThisPtrTy = SemaRef.getContext().CxxAST.getPointerType(ThisTy);
      clang::Expr* This = SemaRef.getCxxSema().BuildCXXThisExpr(T.getLocation(),
          ThisPtrTy, true);
      clang::DeclAccessPair FoundDecl = clang::DeclAccessPair::make(Field,
            clang::AccessSpecifier::AS_public);
      clang::CXXScopeSpec SS;
      clang::ExprResult MemberExpr
          = SemaRef.getCxxSema().BuildFieldReferenceExpr(
            This, true, clang::SourceLocation(), SS, Field, FoundDecl, DNI
          );
      clang::Expr *Ret = MemberExpr.get();
      if (!Ret) {
        SemaRef.Diags.Report(Loc, clang::diag::err_no_member)
            << Field << ThisTy;
      }
      return Ret;
    } else {
      // FIXME: discern whether this is an lvalue or rvalue properly
      clang::DeclRefExpr *DRE =
        clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                  clang::SourceLocation(), VD, /*Capture=*/false,
                                  Loc, FoundTy, clang::VK_LValue);
      return DRE;
    }
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
    // TODO: Make this figure out if the declref is a member variable or not.
    return CreateIdentiferAccess(CxxAST, SemaRef, T, ExplicitType, S->getTokenLoc());
  case tok::Character:
    break;
  case tok::String:
    break;

  /// Keyword Literals

  case tok::IntKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.IntTy, S->getLoc());
  case tok::VoidKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.VoidTy, S->getLoc());
  case tok::BoolKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.BoolTy, S->getLoc());
  case tok::CharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CharTy, S->getLoc());
  case tok::Wchar_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.WCharTy, S->getLoc());
  case tok::Wint_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.WIntTy, S->getLoc());
  case tok::Char8_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char8Ty, S->getLoc());
  case tok::Char16_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char16Ty, S->getLoc());
  case tok::Char32_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char32Ty, S->getLoc());
  case tok::SignedCharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.SignedCharTy, S->getLoc());
  case tok::ShortKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.ShortTy, S->getLoc());
  case tok::LongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongTy, S->getLoc());
  case tok::LongLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongLongTy, S->getLoc());
  case tok::Int128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Int128Ty, S->getLoc());
  case tok::UnsignedCharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedCharTy, S->getLoc());
  case tok::UnsignedShortKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedShortTy, S->getLoc());
  case tok::UnsignedKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedIntTy, S->getLoc());
  case tok::UnsignedLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedLongTy, S->getLoc());
  case tok::UnsignedLongLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedLongLongTy, S->getLoc());
  case tok::Uint128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedInt128Ty, S->getLoc());
  case tok::FloatKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.FloatTy, S->getLoc());
  case tok::DoubleKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.DoubleTy, S->getLoc());
  case tok::LongDoubleKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongDoubleTy, S->getLoc());
  case tok::Float128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Float128Ty, S->getLoc());
  case tok::TypeKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CppxKindTy, S->getLoc());

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
  {"operator'^='" , clang::BO_XorAssign}
};

Expression ExprElaborator::elaborateCall(const CallSyntax *S) {
  if (isa<ElemSyntax>(S->getCallee()))
    return elaborateElemCall(S);
  // I may need to work on this a bit in order to make sure that everything
  // still works as expected when it comes to constructing functions.
  if(isa<CallSyntax>(S->getCallee())) {
    // This will need to be done recursively, because we will create the member
    // access or scope lookup and on the way out we need to actually create the
    // call here with the returned decl, this could be a function decl, or a
    // member function call, or a qualifying name expression.

    const CallSyntax *InnerCall = cast<CallSyntax>(S->getCallee());
    Expression Expr = elaborateCall(InnerCall);

    if (Expr.isNull()) {
      // FIXME: Need to add diagnostics here.
      return nullptr;
    }
    clang::Expr *E = Expr.get<clang::Expr*>();
    // Getting arguments for current function call.
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
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
    clang::ExprResult Call =
      SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                        E, S->getCalleeLoc(),
                                        Args, S->getCalleeLoc());
    return Call.get();
  }
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  FusedOpKind Op = getFusedOpKind(SemaRef, Callee->getSpelling());

  // a fused operator':' call
  if (Op == FOK_Colon) {
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

  if (Op == FOK_MemberAccess) {
    // Need to locate do variable/type lookup.
    const ListSyntax *Args =  cast<ListSyntax>(S->getArguments());
    return elaborateMemberAccess(Args->getChild(0), S, Args->getChild(1));
  }

  llvm::StringRef Spelling = Callee->getSpelling();

  // Check if this is a binary operator.
  auto BinOpMapIter = BinaryOperators.find(Spelling);
  if (BinOpMapIter != BinaryOperators.end()) {
    return elaborateBinOp(S, BinOpMapIter->second);
  }

  // Try to construct a normal function-call expression.
  // First do unqualified lookup.
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Spelling)}, S->getLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
    // FIXME: Figure out how to correctly output the diagnostic here.
    llvm::errs() << "Failed to locate given name: \n";
    return nullptr;
  }

  // Parsing all arguments because in most cases this needs to be done first.
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
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
      
      clang::Decl *Decl = R.getAsSingle<clang::Decl>();

      if (isa<clang::ValueDecl>(Decl)) {
        clang::ValueDecl *VD = dyn_cast<clang::ValueDecl>(Decl);
        // This had better be a reference to a function.
        clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(VD);
        if (!FD)
          return nullptr;

        Fn =
          clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                    clang::SourceLocation(), VD, /*Capture=*/false,
                                    S->getLoc(), VD->getType(), clang::VK_RValue);
      } else if (isa<clang::CXXRecordDecl>(Decl)) {
        clang::CXXRecordDecl *Record = dyn_cast<clang::CXXRecordDecl>(Decl);
        clang::QualType Ty = Context.CxxAST.getTypeDeclType(Record);
        clang::ParsedType PT = clang::ParsedType::make(Ty);
        clang::ExprResult ConstructorExpr =
          SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(PT, S->getLoc(), Args,
                                                        S->getLoc(), false);
        if (!ConstructorExpr.get()) {
          SemaRef.Diags.Report(S->getLoc(),
                               clang::diag::err_coroutine_invalid_func_context)
                               << Ty << "a constructor";
          return nullptr;
        }
        return ConstructorExpr.get();
      }

      if (!Fn)
        return nullptr;

      // Create the call.
      clang::ExprResult Call =
        SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                          Fn, S->getCalleeLoc(),
                                          Args, S->getCalleeLoc());
      if (Call.isInvalid()) {
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_failed_to_translate_expr);
        return nullptr;
      }
      return Call.get();
    }

  } else {
    // This handles the special case of a built in type constructor
    // call/implicit cast.
    auto BuiltInIter = SemaRef.BuiltinTypes.find(Spelling);
    if (BuiltInIter == SemaRef.BuiltinTypes.end()) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_unknown_typename) << Spelling;
      return nullptr;
    }
    clang::ParsedType PT = clang::ParsedType::make(BuiltInIter->second);
    clang::ExprResult ConstructorExpr =
      SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(PT, S->getLoc(), Args,
                                                     S->getLoc(), false);
    return ConstructorExpr.get();
  }
  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

Expression ExprElaborator::elaborateMemberAccess(const Syntax *LHS,
    const CallSyntax *Op, const Syntax *RHS) {
  Expression ElaboratedLHS = elaborateExpr(LHS);
  if (isa<AtomSyntax>(RHS)) {
    const AtomSyntax *RHSAtom = cast<AtomSyntax>(RHS);
    // TODO: figure out how to make the pointer work correctly?

    clang::UnqualifiedId Id;
    clang::IdentifierInfo *IdInfo = &Context.CxxAST.Idents.get(
      RHSAtom->getSpelling());

    // TODO: Figure out how to get the desired scope.
    Id.setIdentifier(IdInfo, RHSAtom->getLoc());
    clang::CXXScopeSpec SS;
    clang::SourceLocation Loc;
    clang::ExprResult HandledLHS = SemaRef.getCxxSema().ActOnMemberAccessExpr(
      SemaRef.getCurClangScope(), ElaboratedLHS.get<clang::Expr*>(), Op->getLoc(),
      clang::tok::TokenKind::period, SS, Loc, Id, nullptr);
    clang::MemberExpr *MemberExpression
      = cast<clang::MemberExpr>(HandledLHS.get());
    MemberExpression->getMemberDecl()->setIsUsed();
    return HandledLHS.get();
  }

  llvm_unreachable("Member access to anything other then a member variable "
      "not implemented yet.");
}

Expression ExprElaborator::elaborateElemCall(const CallSyntax *S) {
  const ElemSyntax *Callee = cast<ElemSyntax>(S->getCallee());

  // FIXME: this can be anything
  const AtomSyntax *Id = cast<AtomSyntax>(Callee->getObject());

  // Try to construct a normal function-call expression.
  // First do unqualified lookup.
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Id->getSpelling())}, S->getLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  R.setTemplateNameLookup(true);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  if (R.empty())
    return nullptr;

  // Build the template argument list.
  clang::TemplateArgumentListInfo TemplateArgs(Callee->getLoc(), Callee->getLoc());
  for (const Syntax *SS : Callee->getArguments()->children()) {
    ExprElaborator ParamElaborator(Context, SemaRef);
    Expression ParamExpression = ParamElaborator.elaborateExpr(SS);
    if (ParamExpression.isNull())
      return nullptr;

    if (ParamExpression.is<clang::TypeSourceInfo *>()) {
      auto *TypeParam = ParamExpression.get<clang::TypeSourceInfo *>();
      clang::TemplateArgument Arg(TypeParam->getType());
      TemplateArgs.addArgument({Arg, TypeParam});
    } else {
      clang::TemplateArgument Arg(ParamExpression.get<clang::Expr *>(),
                                  clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ParamExpression.get<clang::Expr *>()});
    }
  }

  // Build the ULE if we found something.
  clang::Expr *Fn = nullptr;
  R.resolveKind();
  if (R.isOverloadedResult()) {
    Fn =
      clang::UnresolvedLookupExpr::Create(CxxAST, R.getNamingClass(),
                                          clang::NestedNameSpecifierLoc(),
                                        Callee->getLoc(), R.getLookupNameInfo(),
                               /*ADL=*/true, &TemplateArgs, R.begin(), R.end());
  } else {
    llvm_unreachable("Non-overloaded template call?");
  }

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

  ExprMarker(Context.CxxAST, SemaRef).Visit(LHS.get<clang::Expr *>());
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHS.get<clang::Expr *>());

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

static clang::Expr *handleArrayMacro(SyntaxContext &Context, Sema &SemaRef,
                                     const MacroSyntax *S) {
  const ArraySyntax *ArrayInit = cast<ArraySyntax>(S->getBlock());
  const ListSyntax *Init = cast<ListSyntax>(ArrayInit->getChild(0));

  llvm::SmallVector<clang::Expr *, 8> Elements;
  for (const Syntax *SI :  Init->children()) {
    Expression Element = ExprElaborator(Context, SemaRef).elaborateExpr(SI);

    if (Element.is<clang::TypeSourceInfo *>() || Element.isNull())
      return nullptr;

    Elements.push_back(Element.get<clang::Expr *>());
  }

  clang::ExprResult InitList =
    SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Elements,
                                       S->getLoc());
  if (InitList.isInvalid())
    return nullptr;

  return InitList.get();
}

Expression ExprElaborator::elaborateMacro(const MacroSyntax *S) {
  assert (isa<AtomSyntax>(S->getCall()) && "Unexpected macro call");

  const AtomSyntax *Call = cast<AtomSyntax>(S->getCall());

  if (Call->getSpelling() == "if")
    assert(false && "If expression processing not implemented yet.");
  else if (Call->getSpelling() == "while")
    assert(false && "while loop processing not implemented yet.");
  else if(Call->getSpelling() == "for")
    assert(false && "For loop processing not implemented yet.");
  else if (Call->getSpelling() == "array")
    return handleArrayMacro(Context, SemaRef, S);
  else
    // FIXME: Need to handle any other conditions here.
    assert(false && "Unsupported macro");
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

    case DK_NameQualifier:{
      Expression TypeExpr = elaborateQName(D, TInfo);
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
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (BaseTypeExpr.is<clang::Expr *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  Expression IndexExpr =
    ExprElaborator(Context, SemaRef).elaborateExpr(D->Data.Index);

  // FIXME: what do we do for an empty array index, such as []int = {...}
  if (IndexExpr.is<clang::TypeSourceInfo *>() || IndexExpr.isNull()) {
    SemaRef.Diags.Report(D->Data.Index->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType =
    BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::Expr *Index = IndexExpr.get<clang::Expr *>();

  clang::Expr::EvalResult IdxResult;
  clang::Expr::EvalContext
    EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

  if (!Index->EvaluateAsConstantExpr(IdxResult, clang::Expr::EvaluateForCodeGen,
                                     EvalCtx))
    return nullptr;

  clang::QualType ArrayType =
    Context.CxxAST.getConstantArrayType(BaseType, IdxResult.Val.getInt(), Index,
                                        clang::ArrayType::Normal, 0);
  return BuildAnyTypeLoc(CxxAST, ArrayType, D->getType()->getLoc());
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
    clang::ValueDecl *VD =
      cast_or_null<clang::ValueDecl>(Elab.elaborateDeclSyntax(P));
    if (!VD)
      return nullptr;

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");

    assert(isa<clang::ParmVarDecl>(VD) && "Parameter is not a ParmVarDecl");

    Types.push_back(VD->getType());
    Params.push_back(cast<clang::ParmVarDecl>(VD));
  }
  D->Data.ParamInfo.ConstructedScope = SemaRef.saveScope(Call);

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
    clang::SourceLocation Loc = Atom->getLoc();

    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())}, Loc);
    clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupTagName);
    if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())){
      return nullptr;
    }

    if (R.empty()) {
      auto BuiltinMapIter = SemaRef.BuiltinTypes.find(Atom->getSpelling());
      if (BuiltinMapIter == SemaRef.BuiltinTypes.end())
        return nullptr;

      return BuildAnyTypeLoc(CxxAST, BuiltinMapIter->second, Loc);
    }

    clang::TypeDecl *TD = R.getAsSingle<clang::TypeDecl>();
    clang::QualType TDType(TD->getTypeForDecl(), 0);
    return BuildAnyTypeLoc(CxxAST, TDType, Loc);
  }
  llvm_unreachable("Unknown type specification");
}

Expression ExprElaborator::elaborateQName(Declarator *D, TypeInfo *Ty) {
  if (const AtomSyntax *QualifyingName
    = dyn_cast<AtomSyntax>(D->Data.QName.Qualifier)) {

    // Attempting to do qualified name lookup
    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(
      QualifyingName->getSpelling())}, QualifyingName->getLoc());
    clang::LookupResult Results(SemaRef.getCxxSema(), DNI,
      clang::Sema::LookupNestedNameSpecifierName);
      
    if(!SemaRef.lookupUnqualifiedName(Results, SemaRef.getCurrentScope())) {
      llvm::errs() << "Failed to find qualifier "
        << QualifyingName->getSpelling() << "\n";
      return nullptr;
    }
    clang::NamedDecl *Decl = Results.getFoundDecl();
    if (!Decl) {
      llvm::errs() << "No declaration located.\n";
      return nullptr;
    }
    if (!isa<clang::DeclContext>(Decl)) {
      llvm::errs() << "Returned result is not a decl context\n";
      return nullptr;
    }
    clang::DeclContext *DC = cast<clang::DeclContext>(Decl);


    
    // Need a qualified version of each of the type function so that we can properly
    // lookup nested types.
    // TODO: Do scope look up.
    Declarator *NestedDecl = D->Data.QName.NestedName;

    // The type of a declarator is constructed back-to-front.
    llvm::SmallVector<Declarator *, 4> Decls;
    getDeclarators(NestedDecl, Decls);

    // The type is computed from back to front. Start by assuming the type
    // is auto. This will be replaced if an explicit type specifier is given.
    // clang::QualType AutoType = CxxAST.getAutoDeductType();
    // TypeInfo *TInfo = BuildAnyTypeLoc(CxxAST, AutoType, D->getLoc());
    for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
      D = *Iter;
      switch (D->Kind) {
      case DK_Identifier:{
        if(!isa<AtomSyntax>(D->Data.Id)) {
          llvm::errs() << "Some how the identifier I Received wasn't valid\n";
          return nullptr;
        }
        const AtomSyntax *NameInSyntax = dyn_cast<AtomSyntax>(D->Data.Id);

        // Handling nested lookup.
        clang::DeclarationNameInfo DNI2({
            &CxxAST.Idents.get(NameInSyntax->getSpelling())
          }, NameInSyntax->getLoc());
        
        auto LookUpResults = DC->lookup(DNI2.getName());
        if (LookUpResults.empty()) {
          // TODO: Figure out appropriate error here.
          llvm::errs() << "Failed to locate valid nested memebr\n";
          return nullptr;
        }

        if (LookUpResults.size() != 1) {
          // TODO: Figure out appropriate error here.
          llvm::errs() << "We have lookup ambiguity unable to figure out what "
              "is meant by " << DNI2.getName().getAsString() << "\n";
          return nullptr;
        }

        if (!isa<clang::TypeDecl>(LookUpResults.front())) {
          // TODO: Figure out appropriate error here.
          llvm::errs() << "Returned result is not a type.\n";
          return nullptr;
        }

        clang::TypeDecl *TD = cast<clang::TypeDecl>(LookUpResults.front());
        clang::QualType QT = Context.CxxAST.getTypeDeclType(TD);
        return BuildAnyTypeLoc(CxxAST, QT, D->getLoc()); 
        break;
      }
      case DK_Pointer:
      case DK_Array:
      case DK_Function:
      case DK_Type:
        llvm_unreachable("Qualified versions of type look up are not available yet "
          "are not supported yet.");
      // case DK_Pointer: {
      //   Expression TypeExpr = elaboratePointerType(D, TInfo);
      //   if (TypeExpr.isNull())
      //     return nullptr;

      //   TInfo = TypeExpr.get<TypeInfo *>();
      //   break;
      // }

      // case DK_Array: {
      //   Expression TypeExpr = elaborateArrayType(D, TInfo);
      //   if (TypeExpr.isNull())
      //     return nullptr;

      //   TInfo = TypeExpr.get<TypeInfo *>();
      //   break;
      // }

      // case DK_Function: {
      //   Expression TypeExpr = elaborateFunctionType(D, TInfo);
      //   if (TypeExpr.isNull())
      //     return nullptr;

      //   TInfo = TypeExpr.get<TypeInfo *>();
      //   break;
      // }

      // case DK_Type: {
      //   Expression TypeExpr = elaborateExplicitType(D, TInfo);
      //   if (TypeExpr.isNull())
      //     return nullptr;

      //   TInfo = TypeExpr.get<TypeInfo *>();
      //   break;
      // }

      case DK_NameQualifier:{
        llvm_unreachable("Arbitrary nesting not implemented yet. Currently we "
            "only support nesting of a single element.");
        // Expression TypeExpr = elaborateQName(D, TInfo);
        // if (TypeExpr.isNull())
        //   return nullptr;

        // TInfo = TypeExpr.get<TypeInfo *>();
        break;
      }
      
      default:
        llvm_unreachable("Invalid declarator");
      }
    }
  }else {
    llvm_unreachable("Qualifiers that are not atoms are not implemented yet.");
  }
  // return TInfo;
  llvm_unreachable("Incomplete function Still working on it.");
}

} // namespace gold
