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
func main : () int = {
  var y : int = 0;
  while (y < 10)
    y+=1;

  return y;
}
  )BLUE";
  auto ToMatch = whileStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueWhile, Braces) {
  StringRef Code = R"BLUE(
func main : () int = {
  var y : int = 0;
  while (y < 10) {
    y += 1;
    y += 1;
  }

  return y;
}
  )BLUE";
  auto ToMatch = whileStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDoWhile, Braces) {
  StringRef Code = R"BLUE(
func main : () int = {
  var x : int = 0;

  do {
    x += 2;
  } while(x < 10);

  return x;
}
  )BLUE";
  auto ToMatch = doStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDoWhile, NoBraces) {
  StringRef Code = R"BLUE(
func main : () int = {
  var x : int = 0;

  do
    x += 2;
  while(x < 10);

  return x;
}
  )BLUE";
  auto ToMatch = doStmt();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
