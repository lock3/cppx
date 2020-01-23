//===- BlueElaborator.cpp - Blue Language Elaborator ----------------------===//
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

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueSyntax.h"

#include <iostream>

namespace blue {

clang::Decl *Elaborator::elaborateTop(const Syntax *S)
{
  const TopSyntax *Top = cast<TopSyntax>(S);
  for (const Syntax *S : Top->children())
    elaborateDecl(S);

  return getCxxContext().getTranslationUnitDecl();
}

clang::Decl* Elaborator::elaborateDecl(const Syntax *S)
{
  switch (S->getKind()) {
  case Syntax::Def:
    return elaborateDefDecl(static_cast<const DefSyntax *>(S));
  default:
    break;
  }
  llvm_unreachable("invalid declaration");
}

clang::Decl *Elaborator::elaborateDefDecl(const DefSyntax *S) {
  const Token &Id = S->getIdentifier();
  // Decl
}

} // namespace blue
