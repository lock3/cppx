//===- ParseGoldAST.h - Implement the ParseGoldAST method -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the gold::ParseGoldAST method.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldFile.h"
#include "clang/Gold/GoldLexer.h"
#include "clang/Gold/GoldParser.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/ParseGoldAST.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldSyntax.h"

#include "clang/Gold/GoldSyntaxVisitor.h"

namespace gold {

void ParseGoldAST(clang::ASTContext &ClangContext, clang::Preprocessor &PP,
                  clang::Sema &ClangSema) {
  using namespace std;

  // Parse the input file.
  clang::SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());

  SyntaxContext Context(ClangContext);

  Parser Parser(Context, SM, InputFile);
  Syntax *AST = Parser.parseFile();

  // FIXME: There's a -fdump-syntax flag that we should tie this too.

  // FIXME: We should handle -fsyntax-only here -- or maybe make a separate
  // front-end action that stops after parsing. Unfortunately, the flag
  // is in the FrontendOptions of the CompilerInstance, which doesn't seem
  // to be reachable from the arguments to this function.

  // Elaborate the resulting abstract syntax tree.
  Sema Sema(Context, ClangSema);
  Elaborator Elab(Context, Sema);
  clang::Decl *TU = Elab.elaborateFile(AST);
  TU->dump();
}

} // namespace gold
