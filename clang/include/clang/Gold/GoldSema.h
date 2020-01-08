//===- GoldSema.h - Semantic Analysis of Gold ASTs ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the gold::Sema class, which performs semantic analysis
//  for the Gold language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDSEMA_H
#define CLANG_GOLD_GOLDSEMA_H

#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"

#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldScope.h"

#include <memory>
#include <vector>

namespace clang {

class Decl;
class DeclContext;
class LookupResult;
class Preprocessor;
class Sema;
class Stmt;
class Type;

} // namespace clang

namespace gold {

class Declarator;
class Declaration;
struct Syntax;
struct ArraySyntax;
class SyntaxContext;

/// Maintains the state of Gold-to-C++ translation for a
/// translation unit in the Gold Language.
class Sema {
  friend class IdentifierMapper;

  // The context
  SyntaxContext &Context;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &CxxSema;

  // Stack of active Scopes.
  llvm::SmallVector<Scope *, 4> ScopeStack;

  // The declaration context.
  Declaration *CurrentDecl;

public:
  Sema(SyntaxContext &Context, clang::Sema &S);

  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void IdentifyDecls(const ArraySyntax *S);

  // Scope management.

  /// Get the currently active Scope.
  Scope *getCurrentScope();

  /// Push a new scope.
  void pushScope(Scope *S);

  /// Pop the current scope, returning it.
  Scope *popScope();

  /// Enter a new scope corresponding to the syntax S. This is primarily
  /// used for the elaboration of function and template parameters, which
  /// have no corresponding declaration at the point of elaboration.
  void enterScope(ScopeKind K, const Syntax *S);

  /// Leave the current scope. The syntax S must match the syntax for
  /// which the scope was initially pushed.
  void leaveScope(const Syntax *S);

  /// Leaves the current scope, but preserves the object for later use. This
  /// is primarily used to save lists of parameter declarations. The syntax
  /// S must match the syntax for which the scope was initially pushed.
  Scope *saveScope(const Syntax *S);

  // Name lookup

  // Perform unqualified lookup of a name in the current scope.
  bool lookupUnqualifiedName(clang::LookupResult &R);

  // Perform unqualified lookup of a name starting in S.
  bool lookupUnqualifiedName(clang::LookupResult &R, Scope *S);

  // Declaration context

  /// The current declaration.
  Declaration *getCurrentDecl() {
    return CurrentDecl;
  }

  /// The current C++ declaration.
  clang::DeclContext *getCurrentCxxDeclContext();

  /// Make D the current declaration.
  void pushDecl(Declaration *D);

  /// Make the owner of CurrentDecl current.
  void popDecl();


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

} // namespace gold

#endif
