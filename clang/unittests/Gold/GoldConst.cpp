//=== GoldConst.cpp - Test Gold const qualifier ----------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of const qualifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldConst, ConstPointer) {
  StringRef Code = R"(
x : const ^ const int = 0
)";

  SimpleGoldParseTest(Code);
}


TEST(GoldConst, ConstDeclarator) {
  StringRef Code = R"(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[const int]
)";
  DeclarationMatcher opMatches = varDecl(
    hasName("x"), hasType(asString("Ty<const int>")));
  ASSERT_TRUE(matches(Code.str(), opMatches));
}

TEST(GoldConst, DoubleConst) {
  StringRef Code = R"(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[const const int]
)";
  GoldFailureTest(Code);
}

TEST(GoldConst, PartOfDeclarator_DoubleConst) {
  StringRef Code = R"(
x:const const int
)";
  GoldFailureTest(Code);
}