//=== GoldFor.cpp - Test Gold for loops ------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of for loops.
//
//===----------------------------------------------------------------------===//

#include "ParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ForRange, SimpleForRangeArray) {
  StringRef Code = R"(
main() : int!
  xs : [3]int = array{0, 1, 2}
  y : int = 0
  for (x : int in xs):
    y += x
)";

  SimpleGoldParseTest(Code);
}

TEST(ForRange, AutoForRangeArray) {
  StringRef Code = R"(
main() : int!
  xs = array{0, 1, 2}
  y : int = 0
  for (x in xs):
    y += x
)";

  SimpleGoldParseTest(Code);
}

TEST(ForRange, CartesianFor) {
  StringRef Code = R"(
main() : int!
  for (x in 0 .. 99):
    0
  for (x in 0..99):
    0
  for (x : int in 0..99):
    0
)";

  SimpleGoldParseTest(Code);
}
