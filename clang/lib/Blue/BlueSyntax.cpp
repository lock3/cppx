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
#define def_syntax(K, B)                          \
    case K: return # K;
#include "clang/Blue/BlueSyntax.def"
  }
  llvm_unreachable("invalid syntax kind.");
}

clang::SourceLocation Syntax::getLocation() const {
  switch (getKind()) {
#define def_syntax(K, B)                                                  \
    case K: return static_cast<const K ## Syntax *>(this)->getLocation();
#include "clang/Blue/BlueSyntax.def"
  }
  llvm_unreachable("Invalid syntax location");
}

clang::SourceLocation Syntax::getBeginLocation() const {
  switch (getKind()) {
#define def_syntax(K, B)                                                  \
    case K: return static_cast<const K ## Syntax *>(this)->getBeginLocation();
#include "clang/Blue/BlueSyntax.def"
  }
  llvm_unreachable("Invalid syntax beginning location");
}

clang::SourceLocation Syntax::getEndLocation() const {
  switch (getKind()) {
#define def_syntax(K, B)                                                  \
    case K: return static_cast<const K ## Syntax *>(this)->getEndLocation();
#include "clang/Blue/BlueSyntax.def"
  }
  llvm_unreachable("Invalid syntax ending location");
}

Syntax::child_range Syntax::children() {
  switch (getKind()) {
#define def_syntax(K, B)                                                  \
    case K: return static_cast<K ## Syntax *>(this)->children();
#include "clang/Blue/BlueSyntax.def"
  }
  llvm_unreachable("Invalid syntax children");
}

Syntax::const_child_range Syntax::children() const {
  auto Children = const_cast<Syntax *>(this)->children();
  return const_child_range(Children.begin(), Children.end());
}

Syntax::reverse_child_range Syntax::reverseChildren() const {
  auto Children = const_cast<Syntax *>(this)->children();
  return reverse_child_range(reverse_child_iterator(Children.end()),
                             reverse_child_iterator(Children.begin()));
}
clang::SourceLocation DeclarationSyntax::getErrorLocation() const {
  if (getDeclarator()) {
    return getDeclarator()->getLocation();
  }

  if (getType()) {
    return getType()->getLocation();
  }

  if (getConstraint()) {
    return getConstraint()->getLocation();
  }

  if (getInitializer()) {
    return getInitializer()->getLocation();
  }


  

  return clang::SourceLocation();
  
}
bool DeclarationSyntax::declaratorIsThis() const {
  if (getDeclarator()) {
    if (auto Id = dyn_cast<AtomSyntax>(getDeclarator())) {
      if (Id->getToken().getSpelling() == "this") {
        return true;
      }
    }
  }
  return false;
}

Token DeclarationSyntax::getParamPassingSpecifier() const {
  if (ParamSpecs) {
    for (unsigned i = 0;i <NumParamSpecs;++i) {
      switch (ParamSpecs[i].getKind()) {
      case tok::InKeyword:
      case tok::InoutKeyword:
      case tok::OutKeyword:
      case tok::MoveKeyword:
      case tok::ForwardKeyword:
        return ParamSpecs[i];
      default:
        continue;
      }
    }
  }
  return {};
}

} // namespace blue
