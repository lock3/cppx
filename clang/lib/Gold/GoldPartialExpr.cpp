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
namespace gold{

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

clang::Expr *PartialInPlaceNewExpr::completeExpr() const {
  assert(PlacementArg && "Invalid placement expression.");
  llvm::SmallVector<clang::Expr *, 1> InPlaceArgs({PlacementArg});
  clang::Expr *TyExpr = nullptr;
  // Attempting to figure out if we were given type information or not.
  if (TemplateArgs.empty()) {
    TyExpr = SemaRef.buildTypeExpr(PlacementArg->getType(),
                                   PlacementArg->getExprLoc());
  } else if(TemplateArgs.size() != 1) {
    SemaRef.Diags.Report(PlacementArg->getExprLoc(),
                         clang::diag::err_invalid_inplace_new_template_params);
    return nullptr;
  } else {
    TyExpr = TemplateArgs[0];
  }

  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TyExpr,
                                                          TyExpr->getExprLoc());
  if (!TInfo)
    return nullptr;
  clang::SourceLocation Loc = PlacementArg->getExprLoc();
  llvm::SmallVector<clang::Expr *, 16> Temp(CTorArgs.begin(), CTorArgs.end());
  llvm::outs() << "Number of arguments given to constructor? = " << Temp.size() << "\n";

  auto CtorExpr = SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(
             SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo),
             Loc, Temp, Loc, false);
  // TODO: Figure out if this needs to emit an error message or not.
  if (CtorExpr.isInvalid())
    return nullptr;
  llvm::outs() << "Number of aguments given to operator new?? = " << InPlaceArgs.size() << "\n";
  auto NewExpr = SemaRef.getCxxSema().BuildCXXNew(
    /*Range*/clang::SourceRange(Loc, Loc),
    /*UseGlobal*/false,
    /*PlacementLParen*/Loc,
    /*PlacementArgs*/InPlaceArgs,
    /*PlacementRParen*/Loc,
    /*TypeIdParens*/clang::SourceRange(Loc, Loc),
    /*AllocType*/TInfo->getType(),
    /*AllocTypeInfo*/TInfo,
    /*ArraySize*/llvm::Optional<clang::Expr *>(),
    /*DirectInitRange*/Loc,
    /*Initializer*/CtorExpr.get());
  return NewExpr.get();
}

void PartialInPlaceNewExpr::diagnoseIncompleteReason() {
  llvm_unreachable("Create error messages for incomplete expression.");
}
}