//===- BlueSyntaxVisitor.h - Visitors for Blue syntax ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Defines the concrete syntax tree for the Blue language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUESYNTAXVISITOR_H
#define CLANG_BLUE_BLUESYNTAXVISITOR_H

#include "llvm/ADT/STLExtras.h"

#include "clang/Blue/BlueSyntax.h"

namespace blue {

namespace syntaxvisitor {

template<template <typename> class Ptr, typename ImplClass, typename RetTy=void>
class Base {
public:
#define PTR(CLASS) typename Ptr<CLASS>::type
#define DISPATCH(NAME, CLASS) \
  return static_cast<ImplClass*>(this)->Visit##NAME(static_cast<PTR(CLASS)>(S))

  RetTy Visit(PTR(Syntax) S) {
    switch (S->getKind()) {
#define def_syntax(K) \
      case Syntax::K: DISPATCH(K ## Syntax, K ## Syntax);
#include "clang/Blue/BlueSyntax.def"
    }
  }

  // Generic visitor definition.
  RetTy VisitSyntax(PTR(Syntax) S) { return RetTy(); }

  // Derived visitors definitions. By default, these dispatch to the
  // generic visitor.
#define def_syntax(K) \
  RetTy Visit ## K ## Syntax(PTR(K ## Syntax) S) { DISPATCH(Syntax, Syntax); }
#include "clang/Blue/BlueSyntax.def"

  #undef PTR
  #undef DISPATCH
};
} // namespace syntaxvisitor

/// A simple visitor class that helps create syntax visitors.
template <typename ImplClass, typename RetTy = void>
class SyntaxVisitor
    : public syntaxvisitor::Base<std::add_pointer, ImplClass, RetTy> {};

/// A simple visitor class that helps create syntax visitors.
template <typename ImplClass, typename RetTy = void>
class ConstSyntaxVisitor
    : public syntaxvisitor::Base<llvm::make_const_ptr, ImplClass, RetTy> {};

} // namespace blue

#endif
