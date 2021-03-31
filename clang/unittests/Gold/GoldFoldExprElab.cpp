//=== GoldFoldExprElab.cpp -------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Gold fold expression testing.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFoldExpr, NestedUnaryRightFoldExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z : bool = ((x || ...))
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryRightFoldExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z :bool
  z = (x || ...)
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryRightFoldExpr_CommaOperator) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  (x , ...)
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldFoldExpr, NestedExpressionRightFoldExpr) {
  StringRef Code = R"(
foo[T:type](x:rref T) : bool!
  return false

f[T:type...](x:rref T...): void!
  z = ((foo(x) || ...))
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(callExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryLeftFoldExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (... || x)
)";
  auto ToMatch = cxxFoldExpr(hasRHSExpr(declRefExpr()));

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, BinaryLeftFoldExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (false || ... || x)
)";
  auto ToMatch = cxxFoldExpr(hasRHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, BinaryRightFoldExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (x || ... || true)
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, InvalidUseOfFoldInNonSubExprParen) {
  StringRef Code = R"(
foo(x:bool):void!
  ;

f[T:type...](x:rref T...): void!
  foo(false || ... || x)
)";
  GoldFailureTest(Code);
}


static const std::string RightCodePartOne = R"Gld(
f[T:type...](x:rref T...): void!
  z = (x )Gld";

static const std::string RightCodePartTwo = R"Gld( true)
)Gld";

static void doBinaryRightFoldExprTest(const std::string &OpStr,
                                    clang::BinaryOperatorKind ExpectedOpKind)
{
  std::string Code = RightCodePartOne + OpStr + " ... " + OpStr + RightCodePartTwo;
  auto ToMatch = cxxFoldExpr(hasOperator(ExpectedOpKind),
    hasLHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code, ToMatch));
}

static const std::string LeftCodePartOne = R"Gld(
f[T:type...](x:rref T...): void!
  z = (true )Gld";

static const std::string LeftCodePartTwo = R"Gld( x)
)Gld";

static void doBinaryLeftFoldExprTest(const std::string &OpStr,
                                    clang::BinaryOperatorKind ExpectedOpKind)
{
  std::string Code = LeftCodePartOne + OpStr + " ... " + OpStr + LeftCodePartTwo;
  auto ToMatch = cxxFoldExpr(hasOperator(ExpectedOpKind),
      hasRHSExpr(declRefExpr()));
  ASSERT_TRUE(matches(Code, ToMatch));
}
// + - * / % ^ & | = < > << >> += -= *= /= %= ^= &= |= <<= >>= == != <= >= && ||
TEST(GoldFoldExpr, BinaryRight_Add) {
  doBinaryRightFoldExprTest("+", clang::BinaryOperatorKind::BO_Add);
}
TEST(GoldFoldExpr, BinaryRight_Sub) {
  doBinaryRightFoldExprTest("-", clang::BinaryOperatorKind::BO_Sub);
}
TEST(GoldFoldExpr, BinaryRight_Mul) {
  doBinaryRightFoldExprTest("*", clang::BinaryOperatorKind::BO_Mul);
}
TEST(GoldFoldExpr, BinaryRight_Div) {
  doBinaryRightFoldExprTest("/", clang::BinaryOperatorKind::BO_Div);
}
TEST(GoldFoldExpr, BinaryRight_Rem) {
  doBinaryRightFoldExprTest("%", clang::BinaryOperatorKind::BO_Rem);
}
TEST(GoldFoldExpr, BinaryRight_Assign) {
  doBinaryRightFoldExprTest("=", clang::BinaryOperatorKind::BO_Assign);
}
TEST(GoldFoldExpr, BinaryRight_LT) {
  doBinaryRightFoldExprTest("<", clang::BinaryOperatorKind::BO_LT);
}
TEST(GoldFoldExpr, BinaryRight_GT) {
  doBinaryRightFoldExprTest(">", clang::BinaryOperatorKind::BO_GT);
}
TEST(GoldFoldExpr, BinaryRight_AddAssign) {
  doBinaryRightFoldExprTest("+=", clang::BinaryOperatorKind::BO_AddAssign);
}
TEST(GoldFoldExpr, BinaryRight_SubAssign) {
  doBinaryRightFoldExprTest("-=", clang::BinaryOperatorKind::BO_SubAssign);
}
TEST(GoldFoldExpr, BinaryRight_MulAssign) {
  doBinaryRightFoldExprTest("*=", clang::BinaryOperatorKind::BO_MulAssign);
}
TEST(GoldFoldExpr, BinaryRight_DivAssign) {
  doBinaryRightFoldExprTest("/=", clang::BinaryOperatorKind::BO_DivAssign);
}
TEST(GoldFoldExpr, BinaryRight_RemAssign) {
  doBinaryRightFoldExprTest("%=", clang::BinaryOperatorKind::BO_RemAssign);
}
TEST(GoldFoldExpr, BinaryRight_LE) {
  doBinaryRightFoldExprTest("<=", clang::BinaryOperatorKind::BO_LE);
}
TEST(GoldFoldExpr, BinaryRight_GE) {
  doBinaryRightFoldExprTest(">=", clang::BinaryOperatorKind::BO_GE);
}
TEST(GoldFoldExpr, BinaryRight_EQ) {
  doBinaryRightFoldExprTest("==", clang::BinaryOperatorKind::BO_EQ);
}
TEST(GoldFoldExpr, BinaryRight_NE) {
  doBinaryRightFoldExprTest("<>", clang::BinaryOperatorKind::BO_NE);
}
TEST(GoldFoldExpr, BinaryRight_LAND) {
  doBinaryRightFoldExprTest("&&", clang::BinaryOperatorKind::BO_LAnd);
}
TEST(GoldFoldExpr, BinaryRight_LOr) {
  doBinaryRightFoldExprTest("||", clang::BinaryOperatorKind::BO_LOr);
}
TEST(GoldFoldExpr, BinaryLeft_Add) {
  doBinaryLeftFoldExprTest("+", clang::BinaryOperatorKind::BO_Add);
}
TEST(GoldFoldExpr, BinaryLeft_Sub) {
  doBinaryLeftFoldExprTest("-", clang::BinaryOperatorKind::BO_Sub);
}
TEST(GoldFoldExpr, BinaryLeft_Mul) {
  doBinaryLeftFoldExprTest("*", clang::BinaryOperatorKind::BO_Mul);
}
TEST(GoldFoldExpr, BinaryLeft_Div) {
  doBinaryLeftFoldExprTest("/", clang::BinaryOperatorKind::BO_Div);
}
TEST(GoldFoldExpr, BinaryLeft_Rem) {
  doBinaryLeftFoldExprTest("%", clang::BinaryOperatorKind::BO_Rem);
}

TEST(GoldFoldExpr, BinaryLeft_Assign) {
  doBinaryLeftFoldExprTest("=", clang::BinaryOperatorKind::BO_Assign);
}

TEST(GoldFoldExpr, BinaryLeft_LT) {
  doBinaryLeftFoldExprTest("<", clang::BinaryOperatorKind::BO_LT);
}
TEST(GoldFoldExpr, BinaryLeft_GT) {
  doBinaryLeftFoldExprTest(">", clang::BinaryOperatorKind::BO_GT);
}
TEST(GoldFoldExpr, BinaryLeft_AddAssign) {
  doBinaryLeftFoldExprTest("+=", clang::BinaryOperatorKind::BO_AddAssign);
}
TEST(GoldFoldExpr, BinaryLeft_SubAssign) {
  doBinaryLeftFoldExprTest("-=", clang::BinaryOperatorKind::BO_SubAssign);
}
TEST(GoldFoldExpr, BinaryLeft_MulAssign) {
  doBinaryLeftFoldExprTest("*=", clang::BinaryOperatorKind::BO_MulAssign);
}
TEST(GoldFoldExpr, BinaryLeft_DivAssign) {
  doBinaryLeftFoldExprTest("/=", clang::BinaryOperatorKind::BO_DivAssign);
}
TEST(GoldFoldExpr, BinaryLeft_RemAssign) {
  doBinaryLeftFoldExprTest("%=", clang::BinaryOperatorKind::BO_RemAssign);
}
TEST(GoldFoldExpr, BinaryLeft_LE) {
  doBinaryLeftFoldExprTest("<=", clang::BinaryOperatorKind::BO_LE);
}
TEST(GoldFoldExpr, BinaryLeft_GE) {
  doBinaryLeftFoldExprTest(">=", clang::BinaryOperatorKind::BO_GE);
}
TEST(GoldFoldExpr, BinaryLeft_EQ) {
  doBinaryLeftFoldExprTest("==", clang::BinaryOperatorKind::BO_EQ);
}
TEST(GoldFoldExpr, BinaryLeft_NE) {
  doBinaryLeftFoldExprTest("<>", clang::BinaryOperatorKind::BO_NE);
}
TEST(GoldFoldExpr, BinaryLeft_LAnd) {
  doBinaryLeftFoldExprTest("&&", clang::BinaryOperatorKind::BO_LAnd);
}
TEST(GoldFoldExpr, BinaryLeft_LOr) {
  doBinaryLeftFoldExprTest("||", clang::BinaryOperatorKind::BO_LOr);
}

// Unary expansion with parsing with subexpression
TEST(GoldFoldExpr, UnaryRight_NestedSubexpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z : bool = ((x + 2) || ...)
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(parenExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryLeft_NestedSubexpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z : bool = (... || (x + 2))
)";
  auto ToMatch = cxxFoldExpr(hasRHSExpr(parenExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryRight_NonNestedUnaryExpression) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z : bool = (x + 2 || ...)
)";
  GoldFailureTest(Code);
}

TEST(GoldFoldExpr, UnaryLeft_NonNestedUnaryExpression) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z : bool = (... || x + 2)
)";
  GoldFailureTest(Code);
}

// Binary Initialization expression test with additional operators
TEST(GoldFoldExpr, BinaryRightFoldExpr_WithComplexInitExpr_LowerPresidence) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (x + ... + true && false)
)";
  GoldFailureTest(Code);
}

TEST(GoldFoldExpr, BinaryRightFoldExpr_WithComplexPackExpr) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = ( !x || ... || true)
)";
  auto ToMatch = cxxFoldExpr(hasLHSExpr(unaryOperator(
    hasOperatorName("!"),
    has(declRefExpr())
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldFoldExpr, BinaryRightFoldExpr_WithComplexInitExpr_HigherPresedence) {
  StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (x * ... * (4 + 43))
)";
  auto ToMatch = cxxFoldExpr(hasRHSExpr(parenExpr()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}