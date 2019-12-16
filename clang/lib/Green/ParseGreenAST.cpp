//===- ParseGreenAST.h - Implement the ParseGreenAST method ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the usyntax::ParseGreenAST method.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/Green/Elaborator.h"
#include "clang/Green/File.h"
#include "clang/Green/GreenLexer.h"
#include "clang/Green/GreenParser.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/IdentifierMapper.h"
#include "clang/Green/ParseGreenAST.h"
#include "clang/Green/SyntaxContext.h"
#include "clang/Green/Syntax.h"

#include "clang/Green/SyntaxVisitor.h"

using namespace clang;
using namespace green;

namespace lock3 {

void ParseGreenAST(ASTContext &ClangContext, Preprocessor &PP,
                   Sema &ClangSema) {
  using namespace std;

  // Parse the input file.
  SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());

  SyntaxContext Context(ClangContext);

  green::Parser Parser(Context, SM, InputFile);
  green::Syntax *AST = Parser.parseFile();

  // FIXME: There's a -fdump-syntax flag that we should tie this too.
  // if (AST)
  //   AST->dump();

  // FIXME: We should handle -fsyntax-only here -- or maybe make a separate
  // front-end action that stops after parsing. Unfortunately, the flag
  // is in the FrontendOptions of the CompilerInstance, which doesn't seem
  // to be reachable from the arguments to this function.

  // Elaborate the resulting abstract syntax tree.
  GreenSema Sema(Context, ClangSema);
  Elaborator Elab(Context, Sema);
  Elab.elaborateFile(AST);
}

} // namespace lock3
