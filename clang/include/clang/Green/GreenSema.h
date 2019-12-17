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
class Decl;
class Type;

} // namespace clang

namespace green {

class SyntaxContext;
class GreenScope;
struct Syntax;
struct ArraySyntax;

/// Maintains the state of translation for a translation unit in the Green
/// Language.
class GreenSema {
  friend class IdentifierMapper;

  // The context
  SyntaxContext &Context;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &CxxSema;

  // Stack of active GreenScopes.
  llvm::SmallVector<GreenScope *, 4> ScopeStack;

public:
  GreenSema(SyntaxContext &Context, clang::Sema &S);

  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void IdentifyDecls(const ArraySyntax *S);

  // Scope management.

  /// Get the currently active GreenScope.
  GreenScope *getCurrentScope();

  /// Push a new scope.
  void pushScope(GreenScope *S);

  /// Enter a new scope corresponding to D.
  void enterScope(const Syntax *S, clang::Decl *D);

  /// Pop the current scope, returning it.
  GreenScope *popScope();

  /// Leave the current scope.
  void leaveScope(const Syntax *S);

  // Perform unqualified lookup of a name.
  bool LookupName(clang::LookupResult &R, GreenScope *S);

  // Iterate through the mapped identifiers and determine their type.
  void elaborateDecls();

  clang::Sema &getCxxSema() { return CxxSema; }

  SyntaxContext &getContext() { return Context; }

public:
  // Tokenizations of commonly compared-against strings.
  const clang::IdentifierInfo *OperatorColonII;
  const clang::IdentifierInfo *OperatorExclaimII;
  const clang::IdentifierInfo *OperatorEqualsII;
  const clang::IdentifierInfo *OperatorIfII;
  const clang::IdentifierInfo *OperatorElseII;

  // A mapping of identifiers as strings to syntaxes.
  llvm::MapVector<clang::IdentifierInfo *, const Syntax *> IdentifierMapping;
};

} // namespace usyntax

#endif
