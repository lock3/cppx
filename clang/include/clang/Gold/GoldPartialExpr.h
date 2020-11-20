//===- GoldPartialExpr.h - Derived for partial expression -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the derived classes that we will use to store and
//  manage information related to how we partially evaluate expressions.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_PARTIAL_EPXR_H
#define CLANG_GOLD_PARTIAL_EPXR_H
#include "clang/AST/ExprCppx.h"
#include "clang/Gold/GoldSyntax.h"

namespace gold {
class Sema;

enum class PartialExprKind : std::size_t {
  PEK_InPlaceNew,
  PEK_InPlaceDestruct
};

class PartialInPlaceNewExpr : public CppxPartialExprBase {
public:
  Sema &SemaRef;
  const Syntax *Keyword = nullptr;
  using ArgList = llvm::SmallVector<clang::Expr *, 4>;
  clang::Expr *PlacementArg = nullptr;
  ArgList TemplateArgs;
  ArgList CTorArgs;
  enum State {
    HasPlacementPtr,
    HasTemplateTypeArgs,
    CanCreateExpr
  };
  State ExprState;
public:
  PartialInPlaceNewExpr(Sema &SemaRef, const Syntax *DestructKW,
                        clang::Expr *PtrExprArg);

  virtual bool canAcceptElementArgs(const ExprList &Args) const;
  virtual void applyElementArgs(const ExprList &Args);

  virtual bool canAcceptFunctionArgs(const ExprList &Args) const;
  virtual void applyFunctionArgs(const ExprList &Args);

  virtual bool isCompletable() const;
  virtual clang::Expr *completeExpr();
  virtual void diagnoseIncompleteReason();

  static bool classof(const CppxPartialExprBase *E) {
    return E->getKind() == PartialExprKind::PEK_InPlaceNew;
  }
};

}

#endif