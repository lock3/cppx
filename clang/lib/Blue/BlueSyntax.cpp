//===- BlueLexer.cpp - Concret -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Blue Lexer interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueSyntax.h"

#include "clang/AST/ASTDumper.h"

namespace blue {

const char *Syntax::getKindName() const {
  switch (getKind()) {
#define def_syntax(K) \
    case K: return # K;
#include "clang/Blue/BlueSyntax.def"
  }
}

Syntax::child_range Syntax::children() {
  switch (getKind()) {
#define def_syntax(K) \
    case K: return static_cast<K ## Syntax *>(this)->children();
#include "clang/Blue/BlueSyntax.def"
  }
}

Syntax::const_child_range Syntax::children() const {
  auto Children = const_cast<Syntax *>(this)->children();
  return const_child_range(Children.begin(), Children.end());
}

LLVM_DUMP_METHOD void Syntax::dump() const {
  clang::ASTDumper D(llvm::errs(), nullptr, nullptr, /*ShowColors=*/true);
  D.Visit(this);
}

} // namespace blue
