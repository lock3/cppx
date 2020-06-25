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
class CppxNamespaceDecl;
class Expr;
class NamespaceDecl;
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

  /// This is only used when we have an explicitly specified name or explicit
  /// member access namespace look up.
  clang::DeclContext *CurrentLookUpContext = nullptr;
  Scope *OwningScope = nullptr;
public:
  ExprElaborator(SyntaxContext &Context, Sema &SemaRef,
      clang::DeclContext *DC = nullptr, Scope *GoldScope = nullptr);

  /// This is used partially during expression evaluation because there's always
  /// a possibility that we could return a namespace rather than a type.

  // Represents a C++ expression, which may be either an object expression
  // or a type expression.
  using Expression = llvm::PointerUnion<clang::Expr *, clang::TypeSourceInfo *,
                                        clang::NamespaceDecl *,
                                        clang::CppxNamespaceDecl *>;
  using TypeInfo = clang::TypeSourceInfo;

  //===--------------------------------------------------------------------===//
  //                        Value Expression Elaboration                      //
  //===--------------------------------------------------------------------===//
  Expression elaborateExpr(const Syntax *S);

  Expression elaborateAtom(const AtomSyntax *S, clang::QualType ExplicitType);
  Expression elaborateCall(const CallSyntax *S);

  Expression elaborateMemberAccess(const Syntax *LHS, const CallSyntax *Op,
                                   const Syntax *RHS);
  Expression elaborateNestedLookUpAccess(Expression Previous,
                                         const CallSyntax *Op,
                                         const Syntax *RHS);
  Expression elaborateNNS(clang::CppxNamespaceDecl *NS,
                          const CallSyntax *Op, const Syntax *RHS);
  Expression elaborateGlobalNNS(const CallSyntax *Op, const Syntax *RHS);


  // Expression elaborateOp(const CallSyntax *S, clang::BinaryOperatorKind Op);
  Expression elaborateUnaryOp(const CallSyntax *S, clang::UnaryOperatorKind Op);
  Expression elaborateBinOp(const CallSyntax *S, clang::BinaryOperatorKind Op);

  Expression elaborateBlockCondition(const ArraySyntax *Conditions);

  Expression elaborateMacro(const MacroSyntax *Macro);
  Expression elaborateClass(const MacroSyntax *Macro);

  Expression elaborateElementExpr(const ElemSyntax *Elem);
  Expression elaborateCastOp(const CallSyntax *CastOp);

private:
  clang::Expr *handleOperatorDotDot(const CallSyntax *S);

  //===--------------------------------------------------------------------===//
  //                        Type Expression Elaboration                       //
  //===--------------------------------------------------------------------===//
public:
  Expression elaborateTypeExpr(Declarator *D);

  Expression elaboratePointerType(Declarator *D, TypeInfo *Ty);
  Expression elaborateConstType(Declarator *D, TypeInfo *Ty);
  Expression elaborateRefType(Declarator *D, TypeInfo *Ty);
  Expression elaborateRRefType(Declarator *D, TypeInfo *Ty);
  Expression elaborateArrayType(Declarator *D, TypeInfo *Ty);
  Expression elaborateFunctionType(Declarator *D, TypeInfo *Ty);
  Expression elaborateExplicitType(Declarator *D, TypeInfo *Ty);

private:
  clang::TypeSourceInfo *handleOperatorConst(const CallSyntax *S);
  clang::TypeSourceInfo *handleRefType(const CallSyntax *S);
  clang::TypeSourceInfo *handleRRefType(const CallSyntax *S);
  clang::TypeSourceInfo *handleFunctionType(const CallSyntax *S);
  
private:
  /// Utility functions that handle operations assocated with type elaboration,
  /// but not the actual elaboration. These functions also handle error
  /// reporting so the result of any elaboration should be passed directly
  /// to them without need to check the result.
  ///{
  clang::TypeSourceInfo* makeConstType(Expression InnerType,
    const CallSyntax* ConstOpNode);
  clang::TypeSourceInfo* makeRefType(Expression Result,
    const CallSyntax* RefOpNode);
  clang::TypeSourceInfo* makeRRefType(Expression Result,
    const CallSyntax* RRefOpNode);
  ///}

};

void dumpExpression(ExprElaborator::Expression Expr, llvm::raw_ostream& Out);

} // namespace gold

#endif
