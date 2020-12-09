//===- GoldPartialExpr.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  The implementation for partial expressions.
//
//===----------------------------------------------------------------------===//
#include "clang/Gold/GoldPartialExpr.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Sema/TypeLocUtil.h"

namespace gold {

PartialInPlaceNewExpr::PartialInPlaceNewExpr(Sema &SemaRef,
                                             const Syntax *ConstructKW,
                                             clang::Expr *PtrExprArg)
  :CppxPartialExprBase(PartialExprKind::PEK_InPlaceNew),
  SemaRef(SemaRef),
  Keyword(ConstructKW),
  PlacementArg(PtrExprArg),
  TemplateArgs(),
  CTorArgs(),
  ExprState(HasPlacementPtr)
{
  BeginLocation = PtrExprArg->getBeginLoc();
  EndLocation = ConstructKW->getLoc();
}

bool PartialInPlaceNewExpr::canAcceptElementArgs(const ExprList &Args) const {
  return ExprState == HasPlacementPtr;
}

void PartialInPlaceNewExpr::applyElementArgs(const ExprList &Args) {
  TemplateArgs.assign(Args.begin(), Args.end());
  ExprState = HasTemplateTypeArgs;
}

bool PartialInPlaceNewExpr::canAcceptFunctionArgs(const ExprList &Args) const {
  return ExprState == HasPlacementPtr || ExprState == HasTemplateTypeArgs;
}

void PartialInPlaceNewExpr::applyFunctionArgs(const ExprList &Args) {
  CTorArgs.assign(Args.begin(), Args.end());
  ExprState = CanCreateExpr;
}

bool PartialInPlaceNewExpr::isCompletable() const {
  return ExprState == CanCreateExpr;
}

clang::Expr *PartialInPlaceNewExpr::completeExpr() {
  assert(PlacementArg && "Invalid placement expression.");
  llvm::SmallVector<clang::Expr *, 1> InPlaceArgs({PlacementArg});
  clang::Expr *TyExpr = nullptr;
  // Attempting to figure out if we were given type information or not.
  if (TemplateArgs.empty()) {
    TyExpr = SemaRef.buildTypeExpr(PlacementArg->getType(),
                                   PlacementArg->getExprLoc());
  } else if(TemplateArgs.size() != 1) {
    SemaRef.Diags.Report(PlacementArg->getExprLoc(),
                         clang::diag::err_invalid_inplace_template_params)
                         << /*construct*/0;
    return nullptr;
  } else {
    TyExpr = TemplateArgs[0];
  }

  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TyExpr,
                                                          TyExpr->getExprLoc());
  if (!TInfo)
    return nullptr;
  clang::SourceLocation Loc = PlacementArg->getExprLoc();
  auto MemberTy = TInfo->getType();
  clang::QualType InnerTy;
  if (!MemberTy->isPointerType()) {
    if (MemberTy->isDependentType()) {
      InnerTy = SemaRef.buildQualTypeExprTypeFromExpr(PlacementArg,
                                                      PlacementArg->getExprLoc(),
                                                      true);
    } else {
      SemaRef.Diags.Report(Keyword->getLoc(),
                           clang::diag::err_construct_on_non_pointer_ty);
      return nullptr;
    }
    // We need to make this happen instead of creathing the unresolved
    // constructor expression. The unresolved constructor expression
    // is used to create a temporary object and not invoke the constructor.
    // ParenListExpr
    // CXXNewExpr 0x7fffbb559af8 <line:11:3, col:14> 'T *'
    //      |-ParenListExpr 0x7fffbb559ac8 <col:12, col:14> 'NULL TYPE'
    //      | `-IntegerLiteral 0x7fffbb559aa8 <col:13> 'int' 3
    //      `-DeclRefExpr 0x7fffbb559a88 <col:8> 'T *' lvalue Var 0x7fffbb559a08 'x' 'T *'

    clang::Expr *Args = clang::ParenListExpr::Create(SemaRef.getContext().CxxAST,
                                                     Loc,
                                                     CTorArgs,
                                                     Loc);
    clang::TypeSourceInfo *TInfoAdjusted = BuildAnyTypeLoc(
                    SemaRef.getContext().CxxAST, InnerTy, TyExpr->getExprLoc());
    auto NewExpr = clang::CXXNewExpr::Create(
        SemaRef.getContext().CxxAST,
        /*IsGlobalNew*/false,
        /*OperatorNew*/SemaRef.getInPlaceNew(),
        // No delete operator needed because technically there isn't one for
        // placement new because it doesn't allocate memory.
        /*OperatorDelete*/nullptr,
        /*ShouldPassAlignment*/false,
        /*UsualArrayDeleteWantsSize*/false,
        /*PlacementArgs*/InPlaceArgs,
        /*TypeIdParens*/clang::SourceRange(Loc, Loc),
        /*ArraySize*/llvm::Optional<clang::Expr *>(),
        /*InitializationStyle*/clang::CXXNewExpr::CallInit,
        /*Initializer*/Args,
        /*Ty*/TInfoAdjusted->getType(),
        /*AllocatedTypeInfo*/TInfoAdjusted,
        /*Range*/clang::SourceRange(Loc, Loc),
        /*DirectInitRange*/clang::SourceRange(Loc, Loc)
      );
    return NewExpr;
  } else {
    InnerTy = TInfo->getType()->getPointeeType();
  }

  clang::TypeSourceInfo *TInfoAdjusted = BuildAnyTypeLoc(
                    SemaRef.getContext().CxxAST, InnerTy, TyExpr->getExprLoc());
  llvm::SmallVector<clang::Expr *, 16> Temp(CTorArgs.begin(), CTorArgs.end());

  auto CtorExpr = SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(
             SemaRef.getCxxSema().CreateParsedType(TInfoAdjusted->getType(),
                                                   TInfoAdjusted),
             Loc, Temp, Loc, false);
  if (CtorExpr.isInvalid())
    return nullptr;
  auto NewExpr = clang::CXXNewExpr::Create(
      SemaRef.getContext().CxxAST,
      /*IsGlobalNew*/false,
      /*OperatorNew*/SemaRef.getInPlaceNew(),
      // No delete operator needed because technically there isn't one for
      // placement new because it doesn't allocate memory.
      /*OperatorDelete*/nullptr,
      /*ShouldPassAlignment*/false,
      /*UsualArrayDeleteWantsSize*/false,
      /*PlacementArgs*/InPlaceArgs,
      /*TypeIdParens*/clang::SourceRange(Loc, Loc),
      /*ArraySize*/llvm::Optional<clang::Expr *>(),
      /*InitializationStyle*/clang::CXXNewExpr::CallInit,
      /*Initializer*/CtorExpr.get(),
      /*Ty*/TInfoAdjusted->getType(),
      /*AllocatedTypeInfo*/TInfoAdjusted,
      /*Range*/clang::SourceRange(Loc, Loc),
      /*DirectInitRange*/clang::SourceRange(Loc, Loc)
    );
  return NewExpr;
}

void PartialInPlaceNewExpr::diagnoseIncompleteReason() {
  SemaRef.Diags.Report(Keyword->getLoc(),
                       clang::diag::err_incomplete_placement_expression)
                       << /*construct*/0;
}

} // end namespace gold