//===- GreenSema.h - Semantic Analysis of Green ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenSema class, which performs semantic analysis
//  for the Green language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENSEMA_H
#define CLANG_GREEN_GREENSEMA_H

#include "llvm/ADT/StringMap.h"

#include "clang/GreenAST/Syntax.h"

#include <memory>
#include <vector>

namespace clang {
class DeclContext;
class Preprocessor;
class Sema;
} // namespace clang

namespace usyntax {

using SyntaxVector = std::vector<Syntax *>;

class SyntaxContext;

// Semantic actions for the Green language.
class GreenSema {
  // A mapping of identifiers as strings to syntaxes.
  llvm::StringMap<Syntax *> Identifiers;

  // The context
  SyntaxContext &Context;

  // The clang preprocessor, for access to IdentifierInfos.
  clang::Preprocessor &PP;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &ClangSema;
public:
  // The current DeclContext in which we are allocating declarations.
  clang::DeclContext *CurContext;

public:
  GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, clang::Sema &S);

  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void MapIdentifiers(SyntaxVector &Syn);
};

} // namespace usyntax

#endif
