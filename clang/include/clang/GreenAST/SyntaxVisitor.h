//===- SyntaxVisitor.h - A tree traverser for Syntax nodes ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines a tree traverser for Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GSYNTAXVISITOR_H
#define CLANG_GREEN_GSYNTAXVISITOR_H

#include "clang/GreenAST/Syntax.h"
#include "llvm/ADT/STLExtras.h"

#include <utility>

namespace green {
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
      case Syntax::SK_ ## K: DISPATCH(K ## Syntax, K ## Syntax);
#include "clang/GreenAST/Syntax.def"
    }
  }

  // If a function is not implemented, fall back to the base.
#define def_syntax(K) \
  RetTy Visit ## K ## Syntax(PTR(K ## Syntax) S) { DISPATCH(Syntax, Syntax); }
#include "clang/GreenAST/Syntax.def"

  RetTy VisitSyntax(PTR(Syntax) S) { return RetTy(); }

  #undef PTR
  #undef DISPATCH
};
} // namespace syntaxvisitor

/// A simple visitor class that helps create syntax visitors.
///
/// This class does not preserve constness of Syntax pointers (see also
/// ConstSyntaxVisitor).
template <typename ImplClass, typename RetTy = void>
class GreenSyntaxVisitor
    : public syntaxvisitor::Base<std::add_pointer, ImplClass, RetTy> {};

/// A simple visitor class that helps create syntax visitors.
///
/// This class preserves constness of Syntax pointers (see also SyntaxVisitor).
template <typename ImplClass, typename RetTy = void>
class ConstGreenSyntaxVisitor
    : public syntaxvisitor::Base<llvm::make_const_ptr, ImplClass, RetTy> {};


} // namespace green

#endif
