//===- GreenSema.cpp - Semantic Analysis of Green ASTs --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GreenSema class, which performs semantic analysis
//  for the Green language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/GreenAST/Syntax.h"
#include "clang/GreenSema/GreenSema.h"
// #include "clang/GreenSema/IdentifierTree.h"
#include "clang/GreenSema/IdentifierMapper.h"


namespace usyntax {

using namespace llvm;

GreenSema::GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, 
                     clang::Sema &ClangSema)
  : Context(Context), PP(PP), ClangSema(ClangSema)
{}

// DumpClause and DumpFunction are temporary debugging tools.
static void DumpMacro(const SyntaxMacro *S);
static void DumpFunction(const SyntaxCall *S);

static void DumpClause(const Clause &C) {
  llvm::outs() << "ATTRIBUTES:\n";
  for (const Syntax *S: C.attrs)
    S->dump();

  llvm::outs() << "BODY\n";
  for (const Syntax *S: C.body) {
    S->dump();

    if (S && isa<SyntaxMacro>(S))
      DumpMacro(cast<SyntaxMacro>(S));
    else if (S && isa<SyntaxCall>(S))
      DumpFunction(cast<SyntaxCall>(S));
  }
}

void DumpMacro(const SyntaxMacro *S) {
  for (const Clause &C : S->clauses)
    DumpClause(C);
}

void DumpFunction(const SyntaxCall *S) {
  llvm::outs() << "parms:\n";
  for (auto Parm : S->call_parameters)
    Parm->dump();
}

static void DumpVector(const SyntaxVector const &Syn) {
  for (const Syntax *S : Syn) {
    S->dump();
    if (isa<SyntaxMacro>(S))
      DumpMacro(cast<SyntaxMacro>(S));
    else if (isa<SyntaxCall>(S))
      DumpFunction(cast<SyntaxCall>(S));
  }
}

void
GreenSema::MapIdentifiers(SyntaxVector &Syn) {
  IdentifierMapper M(Context, PP, ClangSema);
  M.MapSyntaxes(Syn);
}

} // namespace usyntax

