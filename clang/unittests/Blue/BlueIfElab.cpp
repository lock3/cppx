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

TEST(BlueIf, NoBraces) {
  StringRef Code = R"BLUE(
main : () -> int {
  if (1)
    return 1;
  else
    return 0;
}
  )BLUE";
  auto ToMatch = ifStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueIf, Braces) {
  StringRef Code = R"BLUE(
main : () -> int {
  if (1) {
    3;
    return 1;
  } else {
    10;
    return 0;
  }
}
  )BLUE";
  auto ToMatch = ifStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueIf, Staggered1) {
  StringRef Code = R"BLUE(
main : () -> int {
  if (1)
    return 1;
  else {
    10;
    return 0;
  }
}
  )BLUE";
  auto ToMatch = ifStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueIf, Staggered2) {
  StringRef Code = R"BLUE(
main : () -> int {
  if (1) {
    4;
    return 1;
  } else
    return 0;
}
  )BLUE";
  auto ToMatch = ifStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
