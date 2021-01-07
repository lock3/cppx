//=== BlueArithmeticOpElab.cpp --------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing arithmetic operators.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueArithmeticOp, Plus) {
  StringRef Code = R"BLUE(
x:=4 + 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("+"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueArithmeticOp, Minus) {
  StringRef Code = R"BLUE(
x:=4 - 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("-"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueArithmeticOp, Multiplication) {
  StringRef Code = R"BLUE(
x:=4 * 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("*"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueArithmeticOp, Division) {
  StringRef Code = R"BLUE(
x:=4 / 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("/"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueArithmeticOp, Modulus) {
  StringRef Code = R"BLUE(
x:=4 % 5
  )BLUE";
  auto ToMatch = binaryOperator(hasOperatorName("%"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueArithmeticOp, UnaryPlus) {
  StringRef Code = R"BLUE(
x:=+5
  )BLUE";
  auto ToMatch = unaryOperator(hasOperatorName("+"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueArithmeticOp, UnaryMinus) {
  StringRef Code = R"BLUE(
x:=-5
  )BLUE";
  auto ToMatch = unaryOperator(hasOperatorName("-"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}