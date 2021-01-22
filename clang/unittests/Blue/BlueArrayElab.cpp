//=== BlueArrayElab.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for array declaration/elaboration
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueArray, SimpleArrayDecl){
  StringRef Code = R"BLUE(
x : [2]int;
y : [2][4]int;
z : [2][4][8]int;
)BLUE";

  auto XMatch = varDecl(hasName("x"), hasType(asString("int [2]")));
  auto YMatch = varDecl(hasName("y"), hasType(asString("int [2][4]")));
  auto ZMatch = varDecl(hasName("z"), hasType(asString("int [2][4][8]")));
  ASSERT_TRUE(matches(Code.str(), XMatch));
  ASSERT_TRUE(matches(Code.str(), YMatch));
  ASSERT_TRUE(matches(Code.str(), ZMatch));
}
