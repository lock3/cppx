//=== GoldBuiltinBitwiseOpsElab.cpp ----------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements bitwise operator tests for builtin types.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

static void doBitwiseOpTest(const std::string &TypeName,
    const std::string &ExpectedType,
    const std::string &Constant1, const std::string& Constant2) {
  using namespace std::string_literals;

// x & y 
// x | y 
// x ^ y 


  std::string Code = "foo():void!\n"s +
"  x:" + TypeName + " = " + Constant1 + "\n"
"  y:" + TypeName + " = " + Constant2 + "\n"
"  bwAnd:" + TypeName + " = x & y\n"
"  bwOr:" + TypeName + " = x | y\n"
"  bwXOr:" + TypeName + " = x ^ y\n"
"  x &= y\n"
"  x |= y\n"
"  x ^= y\n"
"";
// "  # lShift:bool = x << y\n"
// "  # rShift:bool = x >> y\n"
// "  # bwNot = ~x"
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(varDecl(
      hasName("bwAnd"),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&"),
        hasType(asString(ExpectedType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("bwOr"),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("|"),
        hasType(asString(ExpectedType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("bwXOr"),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("^"),
        hasType(asString(ExpectedType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("&="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("|="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("^="),
        isAssignmentOperator()
    ))
    // ,
    // FIXME: The left and right shift operators need to be designed.
    // hasDescendant(varDecl(
    //   hasName("lShift:bool"),
    //   hasType(asString("_Bool")),
    //   hasInitializer(findAll(binaryOperator(
    //     hasOperatorName("<<"),
    //     hasType(asString("_Bool")),
    //     hasLHS(implicitCastExpr()),
    //     hasRHS(implicitCastExpr())
    //   )))
    // )),
    // hasDescendant(varDecl(
    //   hasName("rShift:bool"),
    //   hasType(asString("_Bool")),
    //   hasInitializer(findAll(binaryOperator(
    //     hasOperatorName(">>"),
    //     hasType(asString("_Bool")),
    //     hasLHS(implicitCastExpr()),
    //     hasRHS(implicitCastExpr())
    //   )))
    // )),
    // FIXME: The syntax for this hasn't been decided yet.
    // hasDescendant(varDecl(
    //   hasName("bwNot"),
    //   hasType(asString("_Bool")),
    //   hasInitializer(findAll(unaryOperator(
    //     hasOperatorName("~"),
    //     hasType(asString("_Bool")),
    //     hasUnaryOperand(implicitCastExpr(
    //       hasImplicitDestinationType(asString("_Bool")),
    //       hasCastKind(CK_FloatingToBoolean)
    //     ))
    //   )))
    // ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an logical operators.";
}

TEST(BitWiseOp, Builtin_int) {
  doBitwiseOpTest("int", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_int8) {
  doBitwiseOpTest("int8", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_int16) {
  doBitwiseOpTest("int16", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_int32) {
  doBitwiseOpTest("int32", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_int64) {
  doBitwiseOpTest("int64", "long", "5", "6");
}
TEST(BitWiseOp, Builtin_int128) {
  doBitwiseOpTest("int128", "__int128", "5", "6");
}

TEST(BitWiseOp, Builtin_uint) {
  doBitwiseOpTest("uint", "unsigned int", "5", "6");
}
TEST(BitWiseOp, Builtin_uint8) {
  doBitwiseOpTest("uint8", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_uint16) {
  doBitwiseOpTest("uint16", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_uint32) {
  doBitwiseOpTest("uint32", "unsigned int", "5", "6");
}
TEST(BitWiseOp, Builtin_uint64) {
  doBitwiseOpTest("uint64", "unsigned long", "5", "6");
}
TEST(BitWiseOp, Builtin_uint128) {
  doBitwiseOpTest("uint128", "unsigned __int128", "5", "6");
}

TEST(BitWiseOp, Builtin_char) {
  doBitwiseOpTest("char", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_char8) {
  doBitwiseOpTest("char8", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_char16) {
  doBitwiseOpTest("char16", "int", "5", "6");
}
TEST(BitWiseOp, Builtin_char32) {
  doBitwiseOpTest("char32", "unsigned int", "5", "6");
}
