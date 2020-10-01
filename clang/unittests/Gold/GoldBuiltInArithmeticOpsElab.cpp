//=== GoldQualifiedMemberAccessElab.cpp - Testing qualified member lookup --==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests to make sure that qualified member access works
//  correctly.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

static void doIntegerBinaryTest(const std::string &TypeName,
                        const std::string &ExpectedTypeName,
                        const std::string &ExpectedOperatorType,
                        const std::string &Constant1,
                        const std::string &Constant2) {
  using namespace std::string_literals;
  std::string Code = "foo():void!\n"s +
"  x:" + TypeName + " = " + Constant1 + "\n"
"  y:" + TypeName + " = " + Constant2 + "\n"
"  add:" + TypeName + " = x + y\n"
"  sub:" + TypeName + " = x - y\n"
"  mul:" + TypeName + " = x * y\n"
"  div:" + TypeName + " = x / y\n"
"  mod:" + TypeName + " = x % y\n"
"  unaryPlus:" + TypeName + " = +x\n"
"  unaryMinus:" + TypeName + " = -x\n"
"  x += y\n"
"  x -= y\n"
"  x *= y\n"
"  x /= y\n"
"  x %= y\n"
"";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(varDecl(
      hasName("add"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("+"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("sub"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("-"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("mul"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("*"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("div"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("/"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("mod"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("%"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),

    hasDescendant(varDecl(
      hasName("unaryPlus"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("+"),
        hasType(asString(ExpectedOperatorType)),
        hasUnaryOperand(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("unaryMinus"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("-"),
        hasType(asString(ExpectedOperatorType)),
        hasUnaryOperand(implicitCastExpr())
      )))
    )),
    hasDescendant(
      binaryOperator(
        hasOperatorName("+="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("-="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("*="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("/="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("%="),
        isAssignmentOperator()
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an arithmetic operator use.";
}

// Signed integers
TEST(ArithmeticOp, BuiltinType_int) {
  doIntegerBinaryTest("int", "int", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_int8) {
  doIntegerBinaryTest("int8", "signed char", "int","5", "6");
}

TEST(ArithmeticOp, BuiltinType_int16) {
  doIntegerBinaryTest("int16", "short", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_int32) {
  doIntegerBinaryTest("int32", "int", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_int64) {
  doIntegerBinaryTest("int64", "long", "long", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_int128) {
  doIntegerBinaryTest("int128", "__int128", "__int128", "5", "6");
}


// Unsigned integers
TEST(ArithmeticOp, BuiltinType_uint) {
  doIntegerBinaryTest("uint", "unsigned int", "unsigned int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_uint8) {
  doIntegerBinaryTest("uint8", "unsigned char", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_uint16) {
  doIntegerBinaryTest("uint16", "unsigned short", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_uint32) {
  doIntegerBinaryTest("uint32", "unsigned int", "unsigned int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_uint64) {
  doIntegerBinaryTest("uint64", "unsigned long", "unsigned long", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_uint128) {
  doIntegerBinaryTest("uint128", "unsigned __int128",
                      "unsigned __int128", "5", "6");
}

// Character types
TEST(ArithmeticOp, BuiltinType_char) {
  doIntegerBinaryTest("char", "unsigned char", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_char8) {
  doIntegerBinaryTest("char8", "unsigned char", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_char16) {
  doIntegerBinaryTest("char16", "unsigned short", "int", "5", "6");
}

TEST(ArithmeticOp, BuiltinType_char32) {
  doIntegerBinaryTest("char32", "unsigned int", "unsigned int", "5", "6");
}

static void doFloatingPointBinaryTest(const std::string &TypeName,
                        const std::string &ExpectedTypeName,
                        const std::string &ExpectedOperatorType,
                        const std::string &Constant1,
                        const std::string &Constant2) {
  using namespace std::string_literals;
  std::string Code = "foo():void!\n"s +
"  x:" + TypeName + " = " + Constant1 + "\n"
"  y:" + TypeName + " = " + Constant2 + "\n"
"  add:" + TypeName + " = x + y\n"
"  sub:" + TypeName + " = x - y\n"
"  mul:" + TypeName + " = x * y\n"
"  div:" + TypeName + " = x / y\n"
"  unaryPlus:" + TypeName + " = +x\n"
"  unaryMinus:" + TypeName + " = -x\n"
"  x += y\n"
"  x -= y\n"
"  x *= y\n"
"  x /= y\n"
"";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(varDecl(
      hasName("add"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("+"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("sub"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("-"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("mul"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("*"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("div"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("/"),
        hasType(asString(ExpectedOperatorType)),
        hasLHS(implicitCastExpr()),
        hasRHS(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("unaryPlus"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("+"),
        hasType(asString(ExpectedOperatorType)),
        hasUnaryOperand(implicitCastExpr())
      )))
    )),
    hasDescendant(varDecl(
      hasName("unaryMinus"),
      hasType(asString(ExpectedTypeName)),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("-"),
        hasType(asString(ExpectedOperatorType)),
        hasUnaryOperand(implicitCastExpr())
      )))
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("+="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("-="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("*="),
        isAssignmentOperator()
    )),
    hasDescendant(binaryOperator(
        hasOperatorName("/="),
        isAssignmentOperator()
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an arithmetic operator use.";
}



TEST(ArithmeticOp, BuiltinType_float16) {
  doFloatingPointBinaryTest("float16", "__fp16", "float", "5.", ".6");
}

TEST(ArithmeticOp, BuiltinType_float32) {
  doFloatingPointBinaryTest("float32", "float", "float", "5.", ".6");
}

TEST(ArithmeticOp, BuiltinType_float64) {
  doFloatingPointBinaryTest("float64", "double", "double", "5.", ".6");
}

#if 0
TEST(ArithmeticOp, BuiltinType_float128) {
  doFloatingPointBinaryTest("float128", "__float128", "__float128", "5.", ".6");
}
#endif
