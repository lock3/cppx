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
#include "clang/AST/Type.h"
#include "llvm/ADT/PointerUnion.h"

#include "clang/Gold/GoldSyntax.h"

#include <unordered_map>
#include <string>

namespace clang {

class ASTContext;
class Expr;
class Sema;
class TypeSourceInfo;

} // namespace clang

namespace gold {

class Declarator;
class Sema;
class SyntaxContext;

// Builds a C++ expression or C++ type expression, in the form of a clang::Expr*
// or clang TypeSourceInfo* respectively, out of a gold::Syntax node.
class ExprElaborator {
  SyntaxContext &Context;

  clang::ASTContext &CxxAST;

  Sema &SemaRef;
public:
  ExprElaborator(SyntaxContext &Context, Sema &SemaRef);

  // Represents a C++ expression, which may be either an object expression
  // or a type expression.
  using Expression = llvm::PointerUnion<clang::Expr *, clang::TypeSourceInfo *>;
  using TypeInfo = clang::TypeSourceInfo;

  //===--------------------------------------------------------------------===//
  //                        Value Expression Elaboration                      //
  //===--------------------------------------------------------------------===//
  Expression elaborateExpr(const Syntax *S);

  Expression elaborateAtom(const AtomSyntax *S, clang::QualType ExplicitType);
  Expression elaborateCall(const CallSyntax *S);

  Expression elaborateBinOp(const CallSyntax *S, clang::BinaryOperatorKind Op);

  Expression elaborateBlockCondition(const ArraySyntax *Conditions);

  Expression elaborateMacroExpression(const MacroSyntax *Macro);
  Expression elaborateClass(const MacroSyntax *Macro);

private:
  clang::Expr *handleOperatorDotDot(const CallSyntax *Call);

  //===--------------------------------------------------------------------===//
  //                        Type Expression Elaboration                       //
  //===--------------------------------------------------------------------===//
public:
  Expression elaborateTypeExpr(Declarator *D);

  Expression elaboratePointerType(Declarator *D, TypeInfo *Ty);
  Expression elaborateArrayType(Declarator *D, TypeInfo *Ty);
  Expression elaborateFunctionType(Declarator *D, TypeInfo *Ty);
  Expression elaborateExplicitType(Declarator *D, TypeInfo *Ty);

};

} // namespace gold

#endif
