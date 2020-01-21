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
#include "clang/Blue/BlueSyntax.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"


using namespace clang;

namespace blue {

void ParseBlueAST(ASTContext &ClangContext, Preprocessor &PP,
                  Sema &ClangSema) {
  // Parse the input file.
  clang::SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());
  Parser Parser(SM, InputFile);
  Syntax *AST = Parser.parseFile();

  AST->dump();

#if 0
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
#endif
}

}
