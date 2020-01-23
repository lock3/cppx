//===- BlueElaborator.h - Blue Language Elaborator ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Translates concrete syntax into C++ AST trees.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEELABORATOR_H
#define CLANG_BLUE_BLUEELABORATOR_H

#include "clang/Blue/BlueSyntax.h"

#include "clang/Sema/Sema.h"

namespace clang {

class Decl;
class DiagnosticsEngine;
class Expr;
class Sema;

} // namespace clang

namespace blue
{
  class Declarator;

  /// Transforms Blue syntax into C++ ASTs.
  class Elaborator
  {
  public:
    Elaborator(clang::Sema &S)
      : SemaRef(S)
    { }

    /// Returns the current state of C++ translation.
    clang::Sema &getCxxSema() {
      return SemaRef;
    }

    /// Returns the C++ AST context.
    clang::ASTContext &getCxxContext() {
      return getCxxSema().Context;
    }

    clang::Decl *elaborateTop(const Syntax *S);

    clang::Decl *elaborateDecl(const Syntax *S);
    clang::Decl *elaborateDefDecl(const DefSyntax *S);

    void elaborateParameters(const ListSyntax *S);
    void elaborateParameterGroup(const ListSyntax *S);
    void elaborateParameterList(const ListSyntax *S);
    clang::Decl *elaborateParameter(const Syntax *S);
    clang::Decl *elaborateTypedParameter(const DefSyntax *S);
    clang::Decl *elaborateUntypedParameter(const IdentifierSyntax *S);

    Declarator *getDeclaratorFromDecl(const DefSyntax *S);
    Declarator *getDeclaratorFromId(const IdentifierSyntax *S);
    Declarator *getDeclarator(const Syntax *S);
    Declarator *getUnaryDeclarator(const UnarySyntax *S);
    Declarator *getBinaryDeclarator(const BinarySyntax *S);
    Declarator *getLeafDeclarator(const Syntax *S);

  private:
    clang::Sema &SemaRef;
  };

} // namespace blue

#endif
