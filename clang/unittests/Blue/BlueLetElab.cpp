//=== BlueComparisonOpElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing if statements
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueLet, NoBraces) {
  StringRef Code = R"BLUE(
main : () -> int = {
  let (y : int = 0) while (y < 10)
                      y+=1;
}
  )BLUE";
  auto ToMatch = compoundStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLet, Braces) {
  StringRef Code = R"BLUE(
main : () -> int = {
  let (y : int = 0) {
    while (y < 10)
      y+=1;
    y -= 10;
  }
}
  )BLUE";
  auto ToMatch = compoundStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
