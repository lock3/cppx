//===- ExprCppx.h - Classes for representing expressions --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines the clang::Expr interface and subclasses for cppx expressions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_EXPRCPPX_H
#define LLVM_CLANG_AST_EXPRCPPX_H

#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"
#include "clang/AST/UnresolvedSet.h"
#include "clang/Basic/ExceptionSpecificationType.h"
#include "clang/Basic/ExpressionTraits.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Basic/TypeTraits.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>

namespace clang {

class ASTContext;

/// This is any type expression.
class CppxTypeLiteral : public Expr {
public:
  using ValueType = clang::TypeSourceInfo *;
private:
  ValueType Value;

  explicit CppxTypeLiteral(EmptyShell Empty)
    : Expr(CppxTypeLiteralClass, Empty) {}

public:
  CppxTypeLiteral(QualType K, ValueType T)
    : Expr(CppxTypeLiteralClass, K, VK_RValue, OK_Ordinary),
      Value(T) {
    setDependence(computeDependence(this));
  }

  ValueType getValue() const LLVM_READONLY {
    return Value;
  }

  SourceLocation getBeginLoc() const LLVM_READONLY;

  SourceLocation getEndLoc() const LLVM_READONLY;

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxTypeLiteralClass;
  }
  static CppxTypeLiteral* create(ASTContext &Context, QualType KindTy,
                                 ValueType Ty);

};

/// This represents a reference to a namespace expression.
class CppxDeclRefExpr : public Expr {
public:
  /// The actual type denoted by the literal.
  using ValueType = clang::Decl *;
private:
  ValueType DeclRef;

  /// The location of the namespace.
  SourceLocation Loc;

  explicit CppxDeclRefExpr(EmptyShell Empty)
    : Expr(CppxDeclRefExprClass, Empty) {}

public:
  CppxDeclRefExpr(QualType KindTy, ValueType D, SourceLocation L)
    : Expr(CppxDeclRefExprClass, KindTy, VK_RValue, OK_Ordinary),
      DeclRef(D), Loc(L) {
    setDependence(computeDependence(this));
  }

  ValueType getValue() const LLVM_READONLY {
    return DeclRef;
  }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return Loc;
  }

  SourceLocation getEndLoc() const LLVM_READONLY {
    return Loc;
  }

  /// Retrieve the location of the literal.
  SourceLocation getLocation() const { return Loc; }

  void setLocation(SourceLocation Location) { Loc = Location; }

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxDeclRefExprClass;
  }

  static CppxDeclRefExpr *Create(ASTContext &Context, QualType KindTy, 
                                 ValueType NsDecl, SourceLocation Loc);
};


} // namespace clang

#endif // LLVM_CLANG_AST_EXPRCPPX_H


