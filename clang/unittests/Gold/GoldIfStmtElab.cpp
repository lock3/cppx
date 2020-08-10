//=== GoldIfStmtElab.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all of the different forms of an if statement
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldIf, ColonSyntax) {
  StringRef Code = R"(
foo() : int!
  if (true):
    return 1
)";
  auto ToMatch = ifStmt(hasCondition(cxxBoolLiteral()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldIf, CurlyBraceSyntax) {
  StringRef Code = R"(
foo() : int!
  if (true) {
    return 1;
  }
)";
  auto ToMatch = ifStmt(hasCondition(cxxBoolLiteral()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldIf, ElseColonSyntax) {
  StringRef Code = R"(
foo() : int!
  if (true):
    return 1
  else:
    return 0
)";
  auto ToMatch = ifStmt(hasCondition(cxxBoolLiteral()),
                        hasElse(compoundStmt(has(returnStmt()))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldIf, ElseCurlyBraceSyntax) {
  StringRef Code = R"(
foo() : int!
  if (true) {
    return 1;
  } else {
    return 3;
  }
)";
  auto ToMatch = ifStmt(hasCondition(cxxBoolLiteral()),
                        hasElse(compoundStmt(has(returnStmt()))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldIf, ifDoSyntax) {
  StringRef Code = R"(
foo(x:bool, y:bool) : int!
  if:
    x
    y
  do:
    return 1;
  return 0
)";
  auto ToMatch = ifStmt(hasCondition(binaryOperator()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}