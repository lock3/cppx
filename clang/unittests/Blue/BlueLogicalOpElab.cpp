//=== BlueLogicalOpElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all of the logical operators.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueLogicalOp, And) {
  StringRef Code = R"BLUE(
4 && 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("&&"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLogicalOp, Or) {
  StringRef Code = R"BLUE(
4 || 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("||"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLogicalOp, UnaryNot) {
  StringRef Code = R"BLUE(
!true
  )BLUE";
  auto ToMatch = unaryOperator(hasOperatorName("!"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}