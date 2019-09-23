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
  : Context(Context), PP(PP), ClangSema(ClangSema) {
  CurContext = Context.ClangContext.getTranslationUnitDecl();
}


// DumpClause and DumpFunction are temporary debugging tools.
void DumpClause(const Clause &C) {
  llvm::outs() << "ATTRIBUTES:\n";
  for (const Syntax *S: C.attrs)
    S->dump();

  llvm::outs() << "BODY\n";
  for (const Syntax *S: C.body) {
    S->dump();

    if (S && isa<SyntaxMacro>(S)) {
      llvm::outs() << "BODY MACRO:\n";
      llvm::outs() << "CLAUSES:\n";
      for (const Clause &C : cast<SyntaxMacro>(S)->clauses) {
        DumpClause(C);
      }
      llvm::outs() << "END BODY MACRO\n";
    }
  }
}

void DumpFunction(const SyntaxCall *S) {
  llvm::outs() << "parms:\n";
  for (auto Parm : S->call_parameters) {
    Parm->dump();
  }
}
 
void
GreenSema::MapIdentifiers(SyntaxVector &Syn) {
  IdentifierMapper M(Context, PP, *this, ClangSema);
  M.MapSyntaxes(Syn);
}

} // namespace usyntax

