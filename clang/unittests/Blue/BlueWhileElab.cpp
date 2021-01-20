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

TEST(BlueWhile, NoBraces) {
  StringRef Code = R"BLUE(
main : () -> int {
  y : int = 0;
  while (y < 10)
    y++;

  return y;
}
  )BLUE";
  auto ToMatch = whileStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueWhile, Braces) {
  StringRef Code = R"BLUE(
main : () -> int {
  y : int = 0;
  while (y < 10) {
    y++;
    y += 1;
  }

  return y;
}
  )BLUE";
  auto ToMatch = whileStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
