//===- GoldSyntax.cpp - Classes for representing Gold syntax constructs ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GoldSyntax interface and subclasses.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldSyntax.h"

namespace gold {

Syntax *Syntax::error = new ErrorSyntax;

const char *
Syntax::getSyntaxKindName() const {

  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return # K;
#include "clang/Gold/GoldSyntax.def"
  }
}

Syntax::child_range
Syntax::children() {
  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return static_cast<K ## Syntax *>(this)->children();
#include "clang/Gold/GoldSyntax.def"
  }
}

clang::SourceLocation Syntax::getLoc() const {
  using clang::SourceLocation;

  switch(getKind()) {
  case SK_Error:
    return SourceLocation();
  case SK_Atom:
    return cast<AtomSyntax>(this)->getTokenLoc();
  case SK_List:
  case SK_Array:
    // FIXME: What is the SourceLocation in this case?
    return SourceLocation();
  case SK_Call:
    return cast<CallSyntax>(this)->getCalleeLoc();
  case SK_Elem:
    return cast<ElemSyntax>(this)->getObjectLoc();
  case SK_Macro:
    return cast<MacroSyntax>(this)->getCallLoc();
  case SK_File:
    return SourceLocation();
  }
}

std::size_t CallSyntax::getNumArguments() const {
  if (auto *List = dyn_cast<ListSyntax>(getArguments()))
    return List->getNumChildren();
  if (auto *Array = dyn_cast<ArraySyntax>(getArguments()))
    return Array->getNumChildren();
  llvm_unreachable("Invalid argument list");
}

Syntax* CallSyntax::getArgument(std::size_t N) {
  if (auto *List = dyn_cast<ListSyntax>(getArguments()))
    return List->getChild(N);
  if (auto *Array = dyn_cast<ArraySyntax>(getArguments()))
    return Array->getChild(N);
  llvm_unreachable("Invalid argument list");
}

} // namespace gold
