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

  SourceManager &SM = PP.getSourceManager();
  File InputFile(SM, SM.getMainFileID());
  SyntaxContext Context(ClangContext);
  GreenSema Sema(Context, PP, ClangSema);

  green::Parser Parser(SM, InputFile);
  green::Syntax *AST = Parser.parseFile();
  if (AST)
    AST->dump();

  // FIXME: We should handle -fsyntax-only here -- or maybe make a separate
  // front-end action that stops after parsing. Unfortunately, the flag
  // is in the FrontendOptions of the CompilerInstance, which doesn't seem
  // to be reachable from the arguments to this function.

#if 0
  // PHASE 1: Map names to the syntaxes that introduce them.
  IdentifierMapper Mapper(Context, Sema, PP);
  Mapper.identifyDecls(cast<FileSyntax>(AST));

  llvm::outs() << "Mappings:\n";
  for (auto MapIter : Sema.IdentifierMapping)
    llvm::outs() << MapIter.first->getName() << ": " << MapIter.second << '\n';

  // PHASE 2: Create a clang::Type and clang::Decl for each declaration.
  Elaborator Elab(Context, Sema);
  for (auto MapIter : Sema.IdentifierMapping)
    Elab.elaborateDecl(MapIter.second);
#endif
}

} // namespace lock3
