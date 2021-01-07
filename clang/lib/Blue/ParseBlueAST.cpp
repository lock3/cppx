//===- ParseBlueAST.cpp - Implement the ParseBlueAST method ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the blue::ParseBlueAST method.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/ParseBlueAST.h"
#include "clang/Blue/BlueFile.h"
#include "clang/Blue/BlueParser.h"
#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueSyntax.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"


namespace blue {

void ParseBlueAST(clang::ASTContext &CxxContext,
                  clang::Preprocessor &PP,
                  clang::Sema &CxxSema) {
  // llvm::outs() << "Parse blue ast\n";

  // Parse the input file.
  clang::SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());
  SyntaxContext Context(CxxContext);
  Parser Parser(SM, InputFile);
  Syntax *CST = Parser.parseFile();
  CST->dump();
  // Elaborate the resulting abstract syntax tree.
  Sema Sema(Context, CxxSema);
  Elaborator Elab(Sema);
  clang::Decl *PossibleTU = Elab.elaborateTop(CST);
  if (!PossibleTU)
    return;
  clang::TranslationUnitDecl *TU = cast<clang::TranslationUnitDecl>(PossibleTU);
  clang::ASTConsumer *Consumer = &CxxSema.getASTConsumer();

  for (auto *D : TU->decls()) {
    auto DPtr = CxxSema.ConvertDeclToDeclGroup(D);
    if (D && !Consumer->HandleTopLevelDecl(DPtr.get()))
      return;
  }

  Consumer->HandleTranslationUnit(CxxSema.getASTContext());
}

} // namespace blue
