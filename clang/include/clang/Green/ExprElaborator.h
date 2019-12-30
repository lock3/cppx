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

#include "clang/AST/OperationKinds.h"

#include "clang/Green/Syntax.h"

namespace clang {

class ASTContext;
class Expr;
class Sema;

} // namespace clang

namespace green {

class GreenSema;

// Builds a clang::Expr node out of a green::Syntax node.
class ExprElaborator {
  clang::ASTContext &CxxAST;

  GreenSema &SemaRef;
public:
  ExprElaborator(clang::ASTContext &CxxContext, GreenSema &SemaRef);

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

} // namespace green

#endif
