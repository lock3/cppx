//=== GoldArrayElab.cpp ----------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for automatic type deduction in Gold.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ArrayMacro, ArrayIndexingOperatr) {
  StringRef Code = R"(
main() : int!
  a :[3]int = array{0, 1, 2}
  return a[1]
)";

  ASSERT_TRUE(matches(Code.str(), arraySubscriptExpr()));
}