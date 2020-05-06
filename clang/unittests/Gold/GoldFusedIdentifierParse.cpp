//===- unittest/Gold/GoldFusedIdentifierParse.cpp -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


// These tests are just to make sure that we can process the code,
// it doesn't matter if it has real meaning or not.
TEST(GoldFusedIdentifer, DoubleQuoteFusedIdentifier) {
  StringRef Code = R"(
x"x " : int
)";
  SimpleGoldParseTest(Code);
}

TEST(GoldFusedIdentifer, SingleQuoteFusedIdentifier) {
  StringRef Code = R"(
x'x' : int
)";
  SimpleGoldParseTest(Code);
}