//=== BlueLiteralElab.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all elaboration of literals.
//
//===----------------------------------------------------------------------===//

#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueLiteral, IntLiteral) {
  StringRef Code = R"BLUE(
x:=4;
)BLUE";
  auto ToMatch = integerLiteral(equals(4));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, FloatingPoint) {
  StringRef Code = R"BLUE(
x:=4.5;
)BLUE";
  auto ToMatch = floatLiteral(equals(4.5));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, TrueKw) {
  StringRef Code = R"BLUE(
x:=true;
)BLUE";
  auto ToMatch = 	cxxBoolLiteral(equals(true));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, FalseKw) {
  StringRef Code = R"BLUE(
x:=false;
)BLUE";
  auto ToMatch = 	cxxBoolLiteral(equals(false));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, NullKw) {
  StringRef Code = R"BLUE(
x:=null;
)BLUE";
  auto ToMatch = 	cxxNullPtrLiteralExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, String) {
  StringRef Code = R"BLUE(
x:="x";
)BLUE";
  auto ToMatch = 	stringLiteral(hasSize(1));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueLiteral, Char) {
  StringRef Code = R"BLUE(
x:='x';
)BLUE";
  auto ToMatch = 	characterLiteral(equals(120u));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}