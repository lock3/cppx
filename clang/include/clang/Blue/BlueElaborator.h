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

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Sema/Sema.h"

namespace clang {

class Decl;
class DiagnosticsEngine;
class Expr;
class QualType;
class Sema;

} // namespace clang

namespace blue
{
class Declarator;

/// Transforms Blue syntax into C++ ASTs.
class Elaborator
{
public:
  Elaborator(Sema &S)
    : SemaRef(S), CxxSema(S.getCxxSema())
  { }

  /// Returns the current state of C++ translation.
  clang::Sema &getCxxSema() {
    return CxxSema;
  }

  /// Returns the C++ AST context.
  clang::ASTContext &getCxxContext() {
    return getCxxSema().Context;
  }

  // Elaboration
  clang::Decl *elaborateTop(const Syntax *S);

  clang::Decl *elaborateDecl(const Syntax *S);
  clang::Decl *elaborateDefDecl(const DefSyntax *S);

  void elaborateParameters(const ListSyntax *S);
  void elaborateParameterGroup(const ListSyntax *S);
  void elaborateParameterList(const ListSyntax *S);
  clang::Decl *elaborateParameter(const Syntax *S);

  Declarator *getDeclarator(const Syntax *S);
  Declarator *getUnaryDeclarator(const UnarySyntax *S);
  Declarator *getBinaryDeclarator(const BinarySyntax *S);
  Declarator *getLeafDeclarator(const Syntax *S);
  Declarator *getImplicitAutoDeclarator();

  clang::Expr *elaborateDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateTypeDeclarator(const Declarator *Dcl);
  clang::Expr *elaboratePointerDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateArrayDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateFunctionDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateTemplateDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateImplicitTypeDeclarator(const Declarator *Dcl);

  clang::Decl *makeValueDecl(const Syntax *S, Declarator *Dcl);
  clang::Decl *makeObjectDecl(const Syntax *S, Declarator *Dcl, clang::Expr *Ty);
  clang::Decl *makeTypeDecl(const Syntax *S, Declarator *Dcl, clang::QualType T);
  clang::Decl *makeFunctionDecl(const Syntax *S, Declarator *Dcl);
  clang::Decl *makeTemplateDecl(const Syntax *S, Declarator *Dcl);

  void elaborateDefinition(const Syntax *S);

  clang::Expr *elaborateExpression(const Syntax *S);
  clang::Expr *elaborateLiteralExpression(const LiteralSyntax *S);
  clang::Expr *elaborateIdentifierExpression(const IdentifierSyntax *S);
  clang::Expr *elaborateListExpression(const ListSyntax *S);
  clang::Expr *elaborateSeqExpression(const SeqSyntax *S);
  clang::Expr *elaborateUnaryExpression(const UnarySyntax *S);
  clang::Expr *elaborateBinaryExpression(const BinarySyntax *S);

  // Diagnostics

  void Error(clang::SourceLocation Loc, llvm::StringRef Msg);

private:
  Sema &SemaRef;

  clang::Sema &CxxSema;
};

} // namespace blue

#endif
