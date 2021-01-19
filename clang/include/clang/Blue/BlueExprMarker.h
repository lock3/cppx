//=== BluexprMarker.h - Mark declarations in exprs referenced ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines a helper class that marks all declarations used by an
//  expression referenced.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_EXPRMARKER_H
#define CLANG_BLUE_EXPRMARKER_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Sema/Sema.h"

#include "clang/Blue/BlueSema.h"

namespace blue {

/// Helper class that marks all of the declarations referenced by
/// potentially-evaluated subexpressions as "referenced".
/// Effectively a less complicated version of the EvaluatedExprMarker
/// found in Sema/SemaExpr.cpp
struct ExprMarker : public clang::EvaluatedExprVisitor<ExprMarker> {
  clang::ASTContext &CxxAST;
  Sema &SemaRef;
  typedef EvaluatedExprVisitor<ExprMarker> Inherited;

  ExprMarker(clang::ASTContext &CxxAST, Sema &SemaRef)
    : Inherited(CxxAST), CxxAST(CxxAST), SemaRef(SemaRef)
    {}

  void VisitDeclRefExpr(clang::DeclRefExpr *E) {
    // FIXME: references to virtual methods may cause problems here.
    SemaRef.getCxxSema().MarkAnyDeclReferenced(E->getBeginLoc(),
                                               E->getDecl(),
                                               /*OdrUsed=*/false);
    E->getDecl()->markUsed(CxxAST);
  }
};

}

#endif
