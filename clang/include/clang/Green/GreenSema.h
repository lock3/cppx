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
class Decl;
class Type;

} // namespace clang

namespace green {

class SyntaxContext;
struct Syntax;
struct ArraySyntax;

/// A declaration is stores information about the declaration of an
/// identifier. It binds together the declaring operator, the declarator,
/// the definition, and the corresponding C++ declaration.
struct Declaration {
  /// The (binary) operator that introduces the definition. This is null
  /// for the top-level file declaration.
  Syntax *Operator = nullptr;

  /// The declarator (form of declaration).
  Syntax *Declarator = nullptr;

  /// The definition.
  Syntax *Definition = nullptr;

  /// The corresponding C++ declaration.
  clang::Decl* Cpp = nullptr;
};

/// Maintains the state of translation for a translation unit in the Green
/// Language.
class GreenSema {
  friend class IdentifierMapper;

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
  void IdentifyDecls(const ArraySyntax *S);

  // Iterate through the mapped identifiers and determine their type.
  void elaborateDecls();

  clang::Preprocessor &getPP() { return PP; }

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
