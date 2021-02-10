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

// TEST(BlueFor, NoBraces) {
//   StringRef Code = R"BLUE(
// main : () int = {
//   y : int = 0;
//   for (x : int = 0; x < 10; x++)
//     x = y;

//   return y;
// }
//   )BLUE";
//   auto ToMatch = forStmt();
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueFor, Braces) {
//   StringRef Code = R"BLUE(
// main : () int = {
//   y : int = 0;
//   for (x : int = 0; x < 10; x++) {
//     y++;
//     x = y;
//   }

//   return y;
// }
//   )BLUE";
//   auto ToMatch = forStmt();
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueFor, NoVar) {
//   StringRef Code = R"BLUE(
// main : () int = {
//   y : int = 0;
//   for (;y < 10; y++) {
//     y++;
//   }

//   return y;
// }
//   )BLUE";
//   auto ToMatch = forStmt();
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueFor, NoInc) {
//   StringRef Code = R"BLUE(
// main : () int = {
//   y : int = 0;
//   for (x : int = 0; x < 10; ) {
//     x++;
//   }

//   return y;
// }
//   )BLUE";
//   auto ToMatch = forStmt();
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueFor, Infinite) {
//   StringRef Code = R"BLUE(
// main : () int = {
//   y : int = 0;
//   for (;;) {
//     x++;
//   }

//   return y;
// }
//   )BLUE";
//   auto ToMatch = forStmt();
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
