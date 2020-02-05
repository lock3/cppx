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

/// A builtin type.
class CppxTypeLiteral : public Expr {
  /// The actual type denoted by the literal.
  QualType Value;

  /// The location of the type literal.
  SourceLocation Loc;

  explicit CppxTypeLiteral(EmptyShell Empty)
    : Expr(CppxTypeLiteralClass, Empty) {}

public:
  CppxTypeLiteral(QualType K, QualType T, SourceLocation L)
    : Expr(CppxTypeLiteralClass, K, VK_RValue, OK_Ordinary,
           T->isDependentType(), false, T->isDependentType(),
           T->containsUnexpandedParameterPack()),
      Value(T),
      Loc(L) {}

  QualType getValue() const LLVM_READONLY {
    return Value;
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
    return T->getStmtClass() == IntegerLiteralClass;
  }
};


} // namespace clang

#endif // LLVM_CLANG_AST_EXPRCPPX_H
