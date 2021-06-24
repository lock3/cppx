//===- BluePartialImplicitMember.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Partial expression for transforming member access expressions into
//  free function calls implicitly.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BluePartialImplicitMember.h"

namespace blue {

clang::Expr *CppxPartialImplicitMemberTransform::setLhsExpr(clang::Expr *E) {
  BaseExpr = E;
  return getIncompleteExpr();
}

clang::Expr *CppxPartialImplicitMemberTransform::getLhsExpr() {
  return BaseExpr;
}

void CppxPartialImplicitMemberTransform::setName(clang::DeclarationNameInfo DNI) {
  NameInfo = DNI;
}

const clang::DeclarationNameInfo &CppxPartialImplicitMemberTransform::getName() {
  return NameInfo;
}

clang::Expr *
CppxPartialImplicitMemberTransform::appendCall(clang::SourceLocation B,
                                               clang::SourceLocation E,
                                               const ExprList &Args) {
  // llvm::SmallVector<clang::Expr *, 32> transformedArgs;
  // clang::Sema &CxxSema = SemaRef.getCxxSema();
  // clang::ExprResult Call = CxxSema.ActOnCallExpr(CxxSema.getCurScope(), BaseExpr,
  //                                                B, transformedArgs, E);
  // return Call.get():
  llvm_unreachable("Working on it.");
}

clang::Expr *CppxPartialImplicitMemberTransform::appendElementExpr(
  clang::SourceLocation Beginning, clang::SourceLocation EndingLoc,
  clang::TemplateArgumentListInfo &TemplateArgs,
  llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ActualArgs,
  llvm::SmallVectorImpl<clang::Expr *> &OnlyExprArgs)
{
  llvm_unreachable("Working on it.");
}

} // end blue namespace