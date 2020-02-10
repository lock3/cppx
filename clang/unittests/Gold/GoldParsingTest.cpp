//=== GoldParsingTest.cpp - Elaboration for Gold Nodes ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "gtest/gtest.h"

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

#include "clang/Gold/GoldParser.h"
#include "clang/Gold/ParseGoldAST.h"
#include "clang/Tooling/Tooling.h"


#include "clang/AST/Mangle.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/LLVM.h"
#include "llvm/IR/DataLayout.h"
#include "clang/Gold/GoldFrontend.h"

#include <iostream>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldParserTest, ClassTesting) {

  StringRef Code = R"(
c : type = class:
  x : int
  y : bool

main() : int!
  return 0
  )";
  MatchFinder Finder;
  std::unique_ptr<FrontendActionFactory> Factory(
      newFrontendActionFactory<GoldSyntaxAction>());
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "gold"},
                            "temp.usyntax")){
    ASSERT_FALSE(true) << "Parsing error in \"" << Code.str() << "\"";
  }
  // auto AST =
  //     tooling::buildASTFromCodeWithArgs(Code, {"-x", "gold"}, "temp.usyntax", "gold");
  // ASTContext &Ctx = AST->getASTContext();
  // assert(Ctx.getTargetInfo().getDataLayout().getGlobalPrefix() &&
  //        "Expected target to have a global prefix");
  // // ASSERT_FALSE(true) << "Working on it.";
  // clang::ASTContext Context;
  // Parse
}