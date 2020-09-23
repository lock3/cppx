//=== GoldAttributes.cpp - Test Gold attributes ----------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of attributes.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Attributes, SimpleAttributes) {
  StringRef Code = R"(
x<(i < 10)> = 10
y<(10 == 10)> = 10
)";

  SimpleGoldParseTest(Code);
}

TEST(Attributes, LineAttributes) {
  StringRef Code = R"(
[20]
[10]
x : int = 0

[520]
[10]
y() : int!
  return 0
)";

  SimpleGoldParseTest(Code);
}
