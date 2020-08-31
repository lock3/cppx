//=== GoldParsing.cpp - Tests General Parsing ------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Parsing, MixedSyntax) {
  StringRef Code = R"(
main() : int! {
  t : type = class {
    x : int = 0
  }

  u : type = class:
    x : int = 0;

  if (1):
    return 1;
  else:
    return 0
}

x : int = 0
)";

  SimpleGoldParseTest(Code);
}
