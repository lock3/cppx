//=== GoldConstExprIfStmtElab.cpp -----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration of the constexpr version of an if statement.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConstExprIf, NameAttr) {
  StringRef Code = R"(
foo() : int!
  if<constexpr> (true):
    return 1
)";
  auto ToMatch = ifStmt(hasCondition(constantExpr(has(cxxBoolLiteral()))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldConstExprIf, LineAttr) {
  StringRef Code = R"(
foo() : int!
  [constexpr]
  if(true):
    return 1
  else:
    return 0
)";
  auto ToMatch = ifStmt(hasCondition(constantExpr(has(cxxBoolLiteral()))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldConstExprIf, IfDo_LineAttr) {
  StringRef Code = R"(
foo() : int!
  [constexpr]
  if:
    true
    true
  do:
    return 1
  else:
    return 0
)";

  auto ToMatch = ifStmt(hasCondition(
                          constantExpr(
                            has(binaryOperator(
                                  has(cxxBoolLiteral()), has(cxxBoolLiteral())
                                  ))
                            )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldConstExprIf, IfDo_NameAttr) {
  StringRef Code = R"(
foo() : int!
  if<constexpr>:
    true
    true
  do:
    return 1
  else:
    return 0
)";

  auto ToMatch = ifStmt(hasCondition(
                          constantExpr(
                            has(binaryOperator(
                                  has(cxxBoolLiteral()), has(cxxBoolLiteral())
                                  ))
                            )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
