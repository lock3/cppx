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
} // end namespace clang

namespace blue {

class Sema;

struct Declaration {
  /// Use to create the initial file/global namespace.
  Declaration(const Syntax *File)
    : Ctx(nullptr), Def(nullptr), Decl(nullptr), Init(File)
  {}

  Declaration(Declaration *Ctx, const DefSyntax *Def, Declarator *Decl,
              const Syntax *Init)
    : Ctx(Ctx), Def(Def), Decl(Decl), Init(Init)
    {}

  // The owning context.
  Declaration *Ctx = nullptr;

  // The syntax where this declaration is declared.
  const DefSyntax *Def = nullptr;

  // The identifier for this decl.
  clang::IdentifierInfo *Id = nullptr;

  // The declarator of the declaration
  Declarator *Decl = nullptr;

  // The initializer syntax.
  const Syntax *Init = nullptr;

  clang::DeclContext *DeclaringContext = nullptr;

  clang::Decl *getCxx() {
    return Cxx;
  }

  void setCxx(Sema &SemaRef, clang::Decl *Cxx);

  /// The enclosing declaration.
  Declaration *getOwner() const {
    return Ctx;
  }

private:
  /// The corresponding C++ declaration.
  clang::Decl *Cxx = nullptr;
};

} // end namespace blue

#endif
