//===- BlueDeclarator.h - Information about declarations ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Defines a structure used to represent declarations.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEDECLARATOR_H
#define CLANG_BLUE_BLUEDECLARATOR_H

#include "clang/Basic/SourceLocation.h"


namespace clang {
class Expr;
} // namespace clang

namespace blue {

class Scope;
class Syntax;

/// A declarator is a linked list of structures that provide information
/// about the kind and type of a declaration.
class Declarator {
public:
  enum Kind {
    Type,
    ImplicitType,
    Pointer,
    Array,
    Function,
    Template,
  };

  Declarator(Kind K, const Syntax *S)
    : Which(K), Info(S), Next() {}

  Declarator(Kind K, const Syntax *S, Declarator *D)
    : Which(K), Info(S), Next(D) {}

  Kind getKind() const {
    return Which;
  }

  bool declaresValue() const {
    return getKind() == Type || getKind() == Pointer || getKind() == Array
          || getKind() == ImplicitType;
  }

  bool declaresFunction() const {
    return getKind() == Function;
  }

  bool declaresTemplate() const {
    return getKind() == Template;
  }

  const Syntax *getInfo() const {
    return Info;
  }

  const clang::Expr *getExpression() const {
    return Val;
  }

  void setExpression(const clang::Expr *E) {
    assert(!Val);
    Val = E;
  }

  clang::SourceLocation getLocation() const;

  Declarator *getNext() {
    return Next;
  }

  const Declarator *getNext() const {
    return Next;
  }

  void dump() const;

  union {
    // For Function type, the scope of the parameters
    Scope *ParamScope;

  } DeclInfo;

private:
  Kind Which;
  const Syntax *Info;
  Declarator *Next;

  /// The expression that computes the value/type of the declarator. This
  /// is built up during elaboration and cached with the declarator fragment.
  const clang::Expr* Val;
};

} // namespace blue


#endif
