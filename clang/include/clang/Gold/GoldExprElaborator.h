//=== GoldExprElaborator.h - Elaboration for Gold Expressions -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the ExprElaborator interface, which creates
//  clang::Expr nodes out of gold expressions.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_EXPRELABORATOR_H
#define CLANG_GOLD_EXPRELABORATOR_H

#include "clang/AST/OperationKinds.h"

#include "clang/Gold/GoldSyntax.h"

namespace clang {

class ASTContext;
class Expr;
class Sema;

} // namespace clang

namespace gold {

class Sema;

// Builds a clang::Expr node out of a gold::Syntax node.
class ExprElaborator {
  clang::ASTContext &CxxAST;

  Sema &SemaRef;
public:
  ExprElaborator(clang::ASTContext &CxxContext, Sema &SemaRef);

  clang::Expr *elaborateExpr(const Syntax *S);

  clang::Expr *elaborateAtom(const AtomSyntax *S, clang::QualType ExplicitType);
  clang::Expr *elaborateCall(const CallSyntax *S);

  clang::Expr *elaborateBinOp(const CallSyntax *S, clang::BinaryOperatorKind Op);
  clang::Expr *elaborateCmpAssignOp(const CallSyntax *S,
                                    clang::BinaryOperatorKind Op);

  clang::Expr *elaborateBlockCondition(const ArraySyntax *Conditions);

private:
  clang::Expr *handleOperatorDotDot(const CallSyntax *Call);
};

} // namespace gold

#endif
