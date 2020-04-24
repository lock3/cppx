//=== GoldWhile.cpp - Test Gold while loops --------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of while loops.
//
//===----------------------------------------------------------------------===//

#include "ParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(While, SimpleWhile) {
  StringRef Code = R"(
main() : int!
  i = 0
  while (i <> 10):
    i += 1
  return i
)";

  SimpleGoldParseTest(Code);
}

TEST(While, SimpleBlockWhile) {
  StringRef Code = R"(
main() : int!
  i = 0
  j = 0
  while:
    i <> 4
    j <> 4
  do:
    i += 1
    j += 2
  return i + j
)";

  SimpleGoldParseTest(Code);
}
