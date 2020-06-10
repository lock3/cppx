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

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;
#define GOLD_BINARY_ARITHMETIC_TEST(GOLD_TYPE, CPP_TYPE, CONSTANT1, CONSTANT2, OP, OP_NAME)\
TEST(BuiltinArithmeticOp, GOLD_TYPE##_##OP_NAME) {\
  StringRef Code = "foo():void!\n"\
"  x:" #GOLD_TYPE " = " CONSTANT1 "\n"\
"  y:" #GOLD_TYPE " = " CONSTANT2 "\n"\
"  z:" #GOLD_TYPE " = x " #OP " y\n"\
"";\
  DeclarationMatcher operatorUse = varDecl(\
    hasName("z"),\
    hasType(asString(CPP_TYPE)),\
    hasInitializer(binaryOperator(\
      hasOperatorName(#OP),\
      hasType(asString(CPP_TYPE)),\
      hasLHS(implicitCastExpr()),\
      hasRHS(implicitCastExpr())\
    ))\
  );\
  ASSERT_TRUE(matches(Code.str(), operatorUse));\
}
GOLD_BINARY_ARITHMETIC_TEST(int, "int", "5", "6", +, Addition)
GOLD_BINARY_ARITHMETIC_TEST(int, "int", "5", "6", -, Subtraction)
GOLD_BINARY_ARITHMETIC_TEST(int, "int", "5", "6", *, Multiplication)
GOLD_BINARY_ARITHMETIC_TEST(int, "int", "5", "6", /, Division)
GOLD_BINARY_ARITHMETIC_TEST(int, "int", "5", "6", %, Modulus)

GOLD_BINARY_ARITHMETIC_TEST(float, "float", "5.", ".6", +, Addition)
GOLD_BINARY_ARITHMETIC_TEST(float, "float", "5.", ".6", -, Subtraction)
GOLD_BINARY_ARITHMETIC_TEST(float, "float", "5.", ".6", *, Multiplication)
GOLD_BINARY_ARITHMETIC_TEST(float, "float", "5.", ".6", /, Division)