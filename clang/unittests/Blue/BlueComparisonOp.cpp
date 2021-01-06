//=== BlueComparisonOpElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all of the comparison operators.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueComparisonOp, Less) {
  StringRef Code = R"BLUE(
4 < 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("<"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueComparisonOp, Greater) {
  StringRef Code = R"BLUE(
4 > 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("|"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueComparisonOp, Equal) {
  StringRef Code = R"BLUE(
4 == 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("=="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueComparisonOp, NotEqual) {
  StringRef Code = R"BLUE(
4 != 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("!="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueComparisonOp, LessEqual) {
  StringRef Code = R"BLUE(
4 <= 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("<="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueComparisonOp, GreaterEqual) {
  StringRef Code = R"BLUE(
4 >= 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName(">="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}