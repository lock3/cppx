//===- BluePartialExpr.cpp ------------------------------------------------===//
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
#include "clang/Blue/BluePartialExpr.h"
#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueElaborator.h"
#include "clang/Sema/TypeLocUtil.h"

namespace blue {
clang::Expr *PartialNameAccessExprImpl::setIsWithinClass(bool IsInClassScope){
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::allowUseOfImplicitThis(bool AllowImplicitThis){
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::setBaseExpr(clang::Expr *B) {
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::appendName(clang::SourceLocation L,
                                                   clang::IdentifierInfo *Id)
{
  return nullptr;
}

clang::Expr *
PartialNameAccessExprImpl::appendElementExpr(clang::SourceLocation B,
                                             clang::SourceLocation E,
                                  clang::TemplateArgumentListInfo &TemplateArgs,
                     llvm::SmallVectorImpl<clang::TemplateArgument> &ActualArgs)
{
  return nullptr;
}

clang::Expr *
PartialNameAccessExprImpl::appendFunctionCall(clang::SourceLocation B,
                                              clang::SourceLocation E,
                                              const ExprList &Args)
{
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::completeExpr() {
  return nullptr;
}

}