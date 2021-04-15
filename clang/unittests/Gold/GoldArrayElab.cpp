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

TEST(ArrayMacro, ArrayIndexingOperator) {
  StringRef Code = R"(
main() : int!
  a :[3]int = array{0, 1, 2}
  return a[1]
)";

  ASSERT_TRUE(matches(Code.str(), arraySubscriptExpr()));
}

TEST(ArrayType, MultidimensionalArray) {
  StringRef Code = R"(
main() : int!
  x : [1, 1, 3]int
  y = x[0, 0, 2]
)";

  auto YMatcher = arraySubscriptExpr(
    hasBase(implicitCastExpr(hasSourceExpression(declRefExpr()))));

  ASSERT_TRUE(matches(Code.str(), YMatcher));
}

TEST(ArrayMacro, ArrayImplicitRHS) {
  StringRef Code = R"(
x[] : int = array{1, 2, 3};
)";

  ASSERT_TRUE(matches(Code.str(), varDecl(hasType(asString("int [3]")))));
}

TEST(ArrayMacro, ArrayImplicitLHS) {
  StringRef Code = R"(
x : []int = array{1, 2, 3};
)";

  ASSERT_TRUE(matches(Code.str(), varDecl(hasType(asString("int [3]")))));
}
