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

// #include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenAST/Syntax.h"
#include "clang/GreenBasic/File.h"
#include "clang/GreenLexer/GreenLexer.h"
#include "clang/GreenLexer/GreenTokens.h"
#include "clang/GreenParse/ParseGreenAST.h"
#include "clang/GreenParse/GreenParser.h"
#include "clang/GreenSema/GreenSema.h"

#include "clang/GreenAST/SyntaxVisitor.h"

using namespace clang;

namespace lock3 {

void ParseGreenAST(ASTContext &ClangContext, Preprocessor &PP,
                   Sema &ClangSema) {
  using namespace std;

  SourceManager &SM = PP.getSourceManager();
  FileID MainFID = SM.getMainFileID();
  DiagnosticsEngine &Diags = SM.getDiagnostics();
  // green::lexer scan(SM, Input);
  green::parser parser(SM, Diags, {SM, MainFID});
  green::Syntax *AST = parser.parse_file();
  if (AST)
    AST->dump();
}

} // namespace lock3
