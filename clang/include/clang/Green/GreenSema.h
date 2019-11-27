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

#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/MapVector.h"

#include "clang/Green/SyntaxContext.h"

#include <memory>
#include <vector>

namespace clang {
class DeclContext;
class Preprocessor;
class Sema;
class Type;
} // namespace clang

namespace green {

class SyntaxContext;
struct Syntax;
struct ArraySyntax;

// Semantic actions for the Green language.
class GreenSema {
  friend class IdentifierMapper;

  // A mapping of identifiers as strings to syntaxes.
  llvm::MapVector<clang::IdentifierInfo *, const Syntax *> IdentifierMapping;

  // The context
  SyntaxContext &Context;

  // The clang preprocessor, for access to IdentifierInfos.
  clang::Preprocessor &PP;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &ClangSema;

public:
  GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, clang::Sema &S);

  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void MapIdentifiers(const ArraySyntax *S);

  // Iterate through the mapped identifiers and determine their type.
  void RequireTypes();

  clang::Preprocessor &getPP() { return PP; }
};

} // namespace usyntax

#endif
