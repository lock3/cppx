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
#include "clang/Gold/GoldTemplateCallback.h"

namespace gold {

void ParseGoldAST(clang::ASTContext &ClangContext, clang::Preprocessor &PP,
                  clang::Sema &ClangSema) {
  using namespace std;
  // Parse the input file.
  clang::SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());
  SyntaxContext Context(ClangContext);

  Parser Parser(Context, SM, InputFile, PP);
  Syntax *CST = Parser.parseFile();
  // file was empty
  if (!CST)
    return;
  // FIXME: There's a -fdump-syntax flag that we should tie this too.
  // CST->dump();
  // FIXME: We should handle -fsyntax-only here -- or maybe make a separate
  // front-end action that stops after parsing. Unfortunately, the flag
  // is in the FrontendOptions of the CompilerInstance, which doesn't seem
  // to be reachable from the arguments to this function.

  // Elaborate the resulting abstract syntax tree.
  Sema Sema(Context, ClangSema);
  Elaborator Elab(Context, Sema);

  // Initialize the template instantiation observer chain.
  ClangSema.TemplateInstCallbacks.push_back(
    std::unique_ptr<GoldTemplateInstCallback>(new GoldTemplateInstCallback));
  initialize(ClangSema.TemplateInstCallbacks, Sema);

  clang::TranslationUnitDecl *TU =
    cast<clang::TranslationUnitDecl>(Elab.elaborateFile(CST));
  clang::ASTConsumer *Consumer = &ClangSema.getASTConsumer();

  unsigned I = 0;
  for (auto *D : TU->decls()) {
    if (I++ < Elab.ImplicitSemaDecls)
      continue;

    auto DPtr = ClangSema.ConvertDeclToDeclGroup(D);
    if (D && !Consumer->HandleTopLevelDecl(DPtr.get()))
      return;
  }

  Consumer->HandleTranslationUnit(ClangSema.getASTContext());
  finalize(ClangSema.TemplateInstCallbacks, Sema);

}

} // namespace gold
