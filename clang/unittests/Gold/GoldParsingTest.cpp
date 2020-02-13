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

/*
TranslationUnitDecl 0x7ffff3816088 <<invalid sloc>> <invalid sloc>

|-CXXRecordDecl 0x7ffff38545e0 <bin/cpp_test.cpp:1:1, line:4:1> line:1:8 struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial needs_implicit
| | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveConstructor exists simple trivial needs_implicit
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7ffff3854708 <col:1, col:8> col:8 implicit struct c
| |-FieldDecl 0x7ffff38547b8 <line:2:3, col:7> col:7 x 'int'
| `-FieldDecl 0x7ffff3854818 <line:3:3, col:8> col:8 y 'bool'
|
`-FunctionDecl 0x7ffff38548d0 <line:6:1, line:8:1> line:6:5 main 'int ()'
  `-CompoundStmt 0x7ffff3854a20 <col:12, line:8:1>
    `-ReturnStmt 0x7ffff3854a10 <line:7:3, col:10>
      `-IntegerLiteral 0x7ffff38549f0 <col:10> 'int' 0
*/
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
}

TEST(GoldParserTest, ClassInstance) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool

main() : int!
  q : c
  return 0
  )";
  MatchFinder Finder;
  std::unique_ptr<FrontendActionFactory> Factory(
      newFrontendActionFactory<GoldSyntaxAction>());
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "gold"},
                            "temp.usyntax")){
    ASSERT_FALSE(true) << "Parsing error in \"" << Code.str() << "\"";
  }
}