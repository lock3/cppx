//===- BluePartialExpr.h - Blue Language Parser ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the partial expression implementation member access
//  evaluation.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEPARTIALEXPR_H
#define CLANG_BLUE_BLUEPARTIALEXPR_H
#include "clang/AST/ExprCppx.h"
#include "clang/Blue/BlueSyntax.h"

namespace blue {
class Sema;

enum class PartialExprKind : std::size_t {
  PEK_PartialNameAccessExprImpl
};

class PartialNameAccessExprImpl : public CppxPartialNameAccessBase {
  Sema &SemaRef;
public:
  PartialNameAccessExprImpl(Sema &S)
    :CppxPartialNameAccessBase(PartialExprKind::PEK_PartialNameAccessExprImpl),
    SemaRef(S)
  { }

  virtual ~PartialNameAccessExprImpl() = default;

  virtual clang::Expr *setIsWithinClass(bool IsInClassScope) override;
  virtual clang::Expr *allowUseOfImplicitThis(bool AllowImplicitThis) override;
  virtual clang::Expr *setBaseExpr(clang::Expr *) override;

  /// Return true if the given arguments can be handled applied to the
  /// partial expression, this could be template parameters or array access.
  virtual clang::Expr *appendName(clang::SourceLocation L,
                                  clang::IdentifierInfo *Id) override;
  virtual clang::Expr *appendElementExpr(clang::SourceLocation B,
                                         clang::SourceLocation E,
                                 clang::TemplateArgumentListInfo &TemplateArgs,
           llvm::SmallVectorImpl<clang::TemplateArgument> &ActualArgs) override;
  virtual clang::Expr *appendFunctionCall(clang::SourceLocation B,
                                          clang::SourceLocation E,
                                          const ExprList &Args) override;

  /// This is used to generate the complete expression.
  virtual clang::Expr *completeExpr() override;

  static bool classof(const CppxPartialNameAccessBase *S) {
    return S->getKind() == PartialExprKind::PEK_PartialNameAccessExprImpl;
  }
private:
};


}
#endif