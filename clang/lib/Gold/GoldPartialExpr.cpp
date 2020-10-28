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
  clang::QualType InnerTy = TInfo->getType()->getPointeeType();
  clang::TypeSourceInfo *TInfoAdjusted = BuildAnyTypeLoc(
                    SemaRef.getContext().CxxAST, InnerTy, TyExpr->getExprLoc());
  clang::SourceLocation Loc = PlacementArg->getExprLoc();
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




// // ====-------------------------------------------------------------------====//
// //                        PartialInPlaceDestructExpr
// // ====-------------------------------------------------------------------====//
// PartialInPlaceDestructExpr::PartialInPlaceDestructExpr(Sema &SemaRef,
//                                                       const Syntax *DestructKW,
//                                                       clang::Expr *PtrExprArg,
//                                                       clang::SourceLocation OpLoc)
//   :CppxPartialExprBase(PartialExprKind::PEK_InPlaceNew),
//   SemaRef(SemaRef),
//   Keyword(DestructKW),
//   PlacementArg(PtrExprArg),
//   TemplateArgs(),
//   DTorArgs(),
//   ExprState(HasPlacementPtr),
//   OperatorLocation(OpLoc)
// {
//   BeginLocation = PtrExprArg->getBeginLoc();
//   EndLocation = DestructKW->getLoc();
// }

// bool PartialInPlaceDestructExpr::canAcceptElementArgs(const ExprList &Args) const {
//   return ExprState == HasPlacementPtr;
// }

// void PartialInPlaceDestructExpr::applyElementArgs(const ExprList &Args) {
//   // TemplateArgs.assign(Args.begin(), Args.end());
//   // ExprState = HasTemplateTypeArgs;
//   // ReceivedTemplateArgs = true;
//   return true;
// }

// bool PartialInPlaceDestructExpr::canAcceptFunctionArgs(const ExprList &Args) const {
//   return ExprState == HasPlacementPtr || ExprState == HasTemplateTypeArgs;
// }

// void PartialInPlaceDestructExpr::applyFunctionArgs(const ExprList &Args) {
//   DTorArgs.assign(Args.begin(), Args.end());
//   ExprState = CanCreateExpr;
//   ReceivedDTorArgs = true;
// }

// bool PartialInPlaceDestructExpr::isCompletable() const {
//   return ExprState == CanCreateExpr;
// }

// clang::Expr *PartialInPlaceDestructExpr::completeExpr() {
//   // attempting to complete everything correctly here by using the template
//   // type argument to determine the correct kind of expression that we would need
//   // in order complete the name of the destructor and attempt
//   // to construct the call.
//   // ReceivedDTorArgs
//   clang::QualType ExprTy = PlacementArg->getType();
//   if (!ExprTy->isPointerType()) {
//     // FIXME: I need to figure out how to ensure that we have the correct
//     // error message for our destructor.
//     SemaRef.Diags.Report(PlacementArg->getExprLoc(),
//                          clang::diag::err_pseudo_dtor_base_not_scalar)
//                          << ExprTy;
//     return nullptr;
//   } else {
//     ExprTy = ExprTy->getPointeeType();
//   }
//   bool ShouldDoConversionCast = false;
//   clang::TypeSourceInfo *TInfo = nullptr;
//   if (ReceivedTemplateArgs) {
//     if (TemplateArgs.size() != 1) {
//       // This make for incorrect number of template arguments given to the
//       // delete expr.
//       SemaRef.Diags.Report(PlacementArg->getExprLoc(),
//                           clang::diag::err_invalid_inplace_template_params)
//                           << /*destruct*/1;
//       return nullptr;
//     }

//     TInfo = SemaRef.getTypeSourceInfoFromExpr(TemplateArgs[0],
//                                               TemplateArgs[0]->getExprLoc());
//     if (!TInfo)
//       return nullptr;
//     ShouldDoConversionCast = true;
//   } else {
//     // Building type location
//     TInfo = BuildAnyTypeLoc(SemaRef.getContext().CxxAST,
//                             ExprTy, Keyword->getLoc());
//   }

//   if (!ReceivedDTorArgs) {
//     SemaRef.Diags.Report(Keyword->getLoc(),
//                          clang::diag::err_incomplete_placement_expression)
//                          << /*destruct*/1;
//     return nullptr;
//   }
//   // Building the remainder of the call expression over the given type.
//   return makeDtorCallForType(TInfo, ShouldDoConversionCast);
// }

// void PartialInPlaceDestructExpr::diagnoseIncompleteReason() {
//   SemaRef.Diags.Report(Keyword->getLoc(),
//                        clang::diag::err_incomplete_placement_expression)
//                        << /*destruct*/1;
// }

// clang::Expr *
// PartialInPlaceDestructExpr::makeDtorCallForType(clang::TypeSourceInfo *TInfo,
//                                                 bool ShouldDoConversionCast) {
//   TInfo->getType()->dump();
//   clang::CXXScopeSpec SS;
//   clang::SourceLocation Loc;
//   clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::arrow;

//   clang::UnqualifiedId Id;

//   auto PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
//   Id.setDestructorName(Keyword->getLoc(), PT, Keyword->getLoc());
//   auto Ret =
//     SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
//                                                PlacementArg, OperatorLocation,
//                                                AccessTokenKind, SS, Loc,
//                                                Id, nullptr);
//   if (Ret.isInvalid())
//     return nullptr;
//   // Creating the actuall call to the destructor.
//   clang::ExprResult Call = SemaRef.getCxxSema().ActOnCallExpr(
//                SemaRef.getCxxSema().getCurScope(), Ret.get(), Keyword->getLoc(),
//                DTorArgs, Keyword->getLoc());

//   if (Call.isInvalid())
//     return nullptr;

//   return Call.get();
// }

} // end namespace gold