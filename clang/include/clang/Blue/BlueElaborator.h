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
  class Syntax;

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

  private:
    clang::Sema &SemaRef;
  };

} // namespace blue
