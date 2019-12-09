//=== ExprElaborator.h - Elaboration for Green Expressions ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the ExprElaborator interface, which creates
//  clang::Expr nodes out of Green "expressions".
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_EXPRELABORATOR_H
#define CLANG_GREEN_EXPRELABORATOR_H

#include "clang/Green/Syntax.h"

namespace clang {

class ASTContext;
class Expr;
class Sema;

} // namespace clang

namespace green {

class GreenSema;

class ExprElaborator {
  clang::ASTContext &ClangContext;

  GreenSema &SemaRef;
public:
  ExprElaborator(clang::ASTContext &ClangContext, GreenSema &SemaRef);

  clang::Expr *elaborateExpr(const AtomSyntax *S, clang::QualType ExplicitType);
};

} // namespace green

#endif
