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

#include "ParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Attributes, SimpleAttributes) {
  StringRef Code = R"(
x<(i < 10)> = 10
x<i> = 10
x<(10 == 10)> = 10
x < 20
x<i> < 10
x<z<10>><(i < 10)> < y<10>
)";

  SimpleGoldParseTest(Code);
}
