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
  Expression elaborateCmpAssignOp(const CallSyntax *S,
                                    clang::BinaryOperatorKind Op);

  Expression elaborateBlockCondition(const ArraySyntax *Conditions);

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

  // Dictionary of built in types.
  //
  // FIXME: This should be initialized in the constructor.
  const std::unordered_map<std::string, clang::QualType> BuiltinTypes = {
    {"void", CxxAST.VoidTy},
    {"bool", CxxAST.BoolTy},
    {"char", CxxAST.CharTy},
    {"wchar_t", CxxAST.WideCharTy},
    {"wint_t", CxxAST.WIntTy},
    {"char8_t", CxxAST.Char8Ty},
    {"char16_t", CxxAST.Char16Ty},
    {"char32_t", CxxAST.Char32Ty},
    {"signed char", CxxAST.SignedCharTy},
    {"short", CxxAST.ShortTy},
    {"short int", CxxAST.ShortTy},
    {"int", CxxAST.IntTy},
    {"long", CxxAST.LongTy},
    {"long int", CxxAST.LongTy},
    {"long long", CxxAST.LongLongTy},
    {"long long int", CxxAST.LongLongTy},
    {"int128_t", CxxAST.Int128Ty},
    {"unsigned char", CxxAST.UnsignedCharTy},
    {"unsigned short", CxxAST.UnsignedShortTy},
    {"unsigned short int", CxxAST.UnsignedShortTy},
    {"unsigned", CxxAST.UnsignedIntTy},
    {"unsigned int", CxxAST.UnsignedIntTy},
    {"unsigned long", CxxAST.UnsignedLongTy},
    {"unsigned long int", CxxAST.UnsignedLongTy},
    {"unsigned long long", CxxAST.UnsignedLongLongTy},
    {"unsigned long long int", CxxAST.UnsignedLongLongTy},
    {"uint128_t", CxxAST.UnsignedInt128Ty},
    {"float", CxxAST.FloatTy},
    {"double", CxxAST.DoubleTy},
    {"long double", CxxAST.LongDoubleTy},
    {"float128_t", CxxAST.Float128Ty},
  };
};

} // namespace gold

#endif
