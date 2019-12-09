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
#include "llvm/ADT/SmallVector.h"

#include "clang/Green/SyntaxContext.h"

#include <memory>
#include <vector>

namespace clang {
class DeclContext;
class LookupResult;
class Preprocessor;
class Sema;
class Type;
} // namespace clang

namespace green {

class SyntaxContext;
class GreenScope;
struct Syntax;
struct ArraySyntax;

// Semantic actions for the Green language.
class GreenSema {
  friend class IdentifierMapper;

  // The context
  SyntaxContext &Context;

  // The clang preprocessor, for access to IdentifierInfos.
  clang::Preprocessor &PP;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &ClangSema;

  // Stack of active GreenScopes.
  llvm::SmallVector<GreenScope *, 4> ScopeStack;

public:
  GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, clang::Sema &S);

  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void IdentifyDecls(const ArraySyntax *S);

  // Get the currently active GreenScope.
  GreenScope *getCurScope();
  void PushScope(GreenScope *S);
  GreenScope *PopScope();

  // Perform unqualified lookup of a name.
  bool LookupName(clang::LookupResult &R, GreenScope *S);

  // Iterate through the mapped identifiers and determine their type.
  void elaborateDecls();

  clang::Preprocessor &getPP() { return PP; }

  clang::Sema &getClangSema() { return ClangSema; }

public:
  // Tokenizations of commonly compared-against strings.
  const clang::IdentifierInfo *OperatorColonII;
  const clang::IdentifierInfo *OperatorExclaimII;
  const clang::IdentifierInfo *OperatorEqualsII;

  // A mapping of identifiers as strings to syntaxes.
  llvm::MapVector<clang::IdentifierInfo *, const Syntax *> IdentifierMapping;
};

} // namespace usyntax

#endif
