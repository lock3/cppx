//=== GoldExprElaborator.h - Elaboration for Gold Expressions -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license in formation.
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
  using TypeInfo = clang::TypeSourceInfo;

  //===--------------------------------------------------------------------===//
  //                        Value Expression Elaboration                      //
  //===--------------------------------------------------------------------===//
  clang::Expr *elaborateExpr(const Syntax *S);
  clang::Expr *elaborateExpectedConstantExpr(const Syntax* S);
  clang::Expr *elaborateAtom(const AtomSyntax *S, clang::QualType ExplicitType);

  clang::Expr *elaborateCall(const CallSyntax *S);

  /// This elaborates sizeof, alignof, noexcept used as an operator, constexpr
  /// as an operator, and decltype.
  /// This function also tests if the call is one of these operators, and returns
  /// a nullptr in the event that it's not.
  clang::Expr *elaborateBuiltinOperator(const CallSyntax *S);

  /// This function is responsible for the implementation of both sizeof and
  /// alignof operator implementations.
  clang::Expr *elaborateTypeTraitsOp(const AtomSyntax *Name, const CallSyntax *S,
                                     clang::UnaryExprOrTypeTrait Trait);

  clang::Expr *elaborateDeclTypeOp(const AtomSyntax *Name, const CallSyntax *S);
  clang::Expr *elaborateNoExceptOp(const AtomSyntax *Name, const CallSyntax *S);


  clang::Expr *elaborateMemberAccess(const Syntax *LHS, const CallSyntax *Op,
                                     const Syntax *RHS);
  clang::Expr *elaborateNestedLookupAccess(const clang::Expr *Previous,
                                           const Syntax *RHS);
  clang::Expr *elaborateNNS(clang::NamedDecl *NS,
                            const CallSyntax *Op, const Syntax *RHS);
  clang::Expr *elaborateNsAliasSpecifier(clang::CppxNamespaceDecl *NS,
                                         const CallSyntax *Op,
                                         const Syntax *RHS);
  clang::Expr *elaborateGlobalNNS(const CallSyntax *Op, const Syntax *RHS);
  clang::Expr *elaborateUnaryOp(const CallSyntax *S, clang::UnaryOperatorKind Op);
  clang::Expr *elaborateBinOp(const CallSyntax *S, clang::BinaryOperatorKind Op);

  clang::Expr *elaborateBlockCondition(const ArraySyntax *Conditions,
                                      bool IsConstExpr = false);

  clang::Expr *elaborateMacro(const MacroSyntax *Macro);
  clang::Expr *elaborateClass(const MacroSyntax *Macro);

  clang::Expr *elaborateElementExpr(const ElemSyntax *Elem);

  bool elaborateTemplateArugments(const ListSyntax *Args,
                                  clang::TemplateArgumentListInfo &ArgInfo,
              llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArgs);

  clang::Expr *elaborateCastOp(const CallSyntax *CastOp);



private:
  clang::Expr *handleRawBaseSpecifier(const CallSyntax *Op);

public:
  //===--------------------------------------------------------------------===//
  //                        Type Expression Elaboration                       //
  //===--------------------------------------------------------------------===//

  clang::Expr *elaborateTypeExpr(Declarator *D);

  clang::Expr *elaborateFunctionType(Declarator *D, clang::Expr *Ty);
  clang::Expr *elaborateExplicitType(Declarator *D, clang::Expr *Ty);

private:
  clang::Expr *handleOperatorConst(const CallSyntax *S);
  clang::Expr *handleRefType(const CallSyntax *S);
  clang::Expr *handleRRefType(const CallSyntax *S);
  clang::Expr *handleFunctionType(const CallSyntax *S);
  clang::Expr *handleArrayType(const CallSyntax *S);

private:
  /// Utility functions that handle operations assocated with type elaboration,
  /// but not the actual elaboration. These functions also handle error
  /// reporting so the result of any elaboration should be passed directly
  /// to them without need to check the result.
  ///{
  clang::Expr* makeConstType(clang::Expr *InnerType,
                             const CallSyntax* ConstOpNode);
  clang::Expr* makeRefType(clang::Expr *Result,
                           const CallSyntax* RefOpNode);
  clang::Expr* makeRRefType(clang::Expr *Result,
                            const CallSyntax* RRefOpNode);
  ///}
};

} // namespace gold

#endif
