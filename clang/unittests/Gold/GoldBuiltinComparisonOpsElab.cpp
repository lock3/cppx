//=== GoldBuiltinComparisonOpsElab.cpp -------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements comparison operator tests for builtin types.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

static void doCmpOpTest(const std::string &TypeName,
    const std::string &Constant1, const std::string& Constant2) {
  using namespace std::string_literals;
  std::string Code = "foo():void!\n"s +
"  x:" + TypeName + " = " + Constant1 + "\n"
"  y:" + TypeName + " = " + Constant2 + "\n"
"  lt:bool = x < y\n"
"  gt:bool = x > y\n"
"  le:bool = x <= y\n"
"  ge:bool = x >= y\n"
"  eq:bool = x == y\n"
"  ne:bool = x <> y\n"
"";
  DeclarationMatcher opMatches = translationUnitDecl(
    
    hasDescendant(varDecl(
      hasName("lt"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("<"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("gt"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName(">"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("le"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("<="),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("ge"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName(">="),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("eq"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("=="),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("ne"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("!="),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an logical operators.";
}

TEST(ComparisonOp, Builtin_int) {
  doCmpOpTest("int", "5", "6");
}
TEST(ComparisonOp, Builtin_int8) {
  doCmpOpTest("int8", "5", "6");
}
TEST(ComparisonOp, Builtin_int16) {
  doCmpOpTest("int16", "5", "6");
}
TEST(ComparisonOp, Builtin_int32) {
  doCmpOpTest("int32", "5", "6");
}
TEST(ComparisonOp, Builtin_int64) {
  doCmpOpTest("int64", "5", "6");
}
TEST(ComparisonOp, Builtin_int128) {
  doCmpOpTest("int128", "5", "6");
}

TEST(ComparisonOp, Builtin_uint) {
  doCmpOpTest("uint", "5", "6");
}
TEST(ComparisonOp, Builtin_uint8) {
  doCmpOpTest("uint8", "5", "6");
}
TEST(ComparisonOp, Builtin_uint16) {
  doCmpOpTest("uint16", "5", "6");
}
TEST(ComparisonOp, Builtin_uint32) {
  doCmpOpTest("uint32", "5", "6");
}
TEST(ComparisonOp, Builtin_uint64) {
  doCmpOpTest("uint64", "5", "6");
}
TEST(ComparisonOp, Builtin_uint128) {
  doCmpOpTest("uint128", "5", "6");
}

TEST(ComparisonOp, Builtin_char) {
  doCmpOpTest("char", "5", "6");
}
TEST(ComparisonOp, Builtin_char8) {
  doCmpOpTest("char8", "5", "6");
}
TEST(ComparisonOp, Builtin_char16) {
  doCmpOpTest("char16", "5", "6");
}
TEST(ComparisonOp, Builtin_char32) {
  doCmpOpTest("char32", "5", "6");
}

TEST(ComparisonOp, Builtin_float16) {
  doCmpOpTest("float16", "5", "6");
}
TEST(ComparisonOp, Builtin_float32) {
  doCmpOpTest("float32", "5", "6");
}

#if 0
TEST(ComparisonOp, Builtin_float64) {
  doCmpOpTest("float64", "5", "6");
}
TEST(ComparisonOp, Builtin_float128) {
  doCmpOpTest("float128", "5", "6");
}
#endif

TEST(ComparisonOp, Builtin_bool) {
  doCmpOpTest("bool", "true", "false");
}
