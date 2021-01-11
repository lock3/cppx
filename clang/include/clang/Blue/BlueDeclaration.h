//=== BlueDeclaration.h ---------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This class contains a declaration.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_DECLARATION_H
#define CLANG_BLUE_DECLARATION_H

#include "clang/Blue/BlueDeclarator.h"
#include "clang/Blue/BlueSyntax.h"


namespace clang {
  class Decl;
  class DeclContext;
  class IdentifierInfo;
  class Scope;
} // end namespace clang


namespace blue {
class Scope;
class Sema;

enum class Phase : std::size_t
{
  Unprocessed,
  Identification,
  Typing,
  Initialization
};

llvm::StringRef phaseToStr(Phase p);

struct Declaration {
  /// Use to create the initial file/global namespace.
  Declaration(const Syntax *File)
    : Ctx(nullptr), Def(nullptr), Decl(nullptr), Init(File)
  {}

  Declaration(Declaration *Ctx, const Syntax *Def, Declarator *Decl,
              const Syntax *Init)
    : Ctx(Ctx), Def(Def), Decl(Decl), Init(Init)
    {}

  // The owning context.
  Declaration *Ctx = nullptr;

  // The syntax where this declaration is declared.
  const Syntax *Def = nullptr;

  // The identifier for this decl.
  clang::IdentifierInfo *Id = nullptr;

  // The declarator of the declaration
  Declarator *Decl = nullptr;

  // The initializer syntax.
  const Syntax *Init = nullptr;

  clang::DeclContext *DeclaringContext = nullptr;

  /// The current phase of elaboration that this declaration has been elaborated
  /// to.
  Phase CurrentPhase = Phase::Unprocessed;

  /// If this declaration is currently being processed.
  bool IsElaborating = false;

  /// The list of members associated with this declaration.
  Scope *SavedScope = nullptr;

  /// This information is to aid with early elaboration. This allows the
  /// elabrotor to restore the state in which something was declared.
  ///
  /// This is the current clang scope that the clang declaration is part of.
  clang::Scope *ClangDeclaringScope = nullptr;

  /// This is the scope that this declaration is a member of.
  /// This is also the parent scope to the SavedScope, if set.
  Scope *ScopeForDecl = nullptr;


  clang::Decl *getCxx() {
    return Cxx;
  }

  void setCxx(Sema &SemaRef, clang::Decl *Cxx);

  /// The enclosing declaration.
  Declaration *getOwner() const {
    return Ctx;
  }

  bool IsVariableDecl() const;

  bool hasInitializer() const;

  const Syntax *getInitializer() const;

  /// Get the def as an IdentifierSyntax.
  const IdentifierSyntax *asId() const;

  /// Get the def as a DefSyntax.
  const DefSyntax *asDef() const;

private:
  /// The corresponding C++ declaration.
  clang::Decl *Cxx = nullptr;
};
Phase phaseOf(Declaration *D);

} // end namespace blue

#endif
