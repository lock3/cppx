//=== GoldBitsElab.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing bitfields.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldBitsAttribute, BitFieldMember) {
  StringRef Code = R"(
Clx : type = class:
  x<bits(19)>:int
)";
  DeclarationMatcher ToMatch = fieldDecl(hasName("x"),
    	isBitField(), hasBitWidth(19));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldBitsAttribute, BitFieldWithConstantExprAsArg) {
  StringRef Code = R"(
P:const int = 10
Q:const int = 9
Clx : type = class:
  x<bits(P+Q)>:int
)";
  DeclarationMatcher ToMatch = fieldDecl(hasName("x"),
    	isBitField(), hasBitWidth(19));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldBitsAttribute, InvalidBitfieldDepndsOnNonConstantVariable) {
  StringRef Code = R"(
P:const int = 10
Q:int = 9
Clx : type = class:
  x<bits(P+Q)>:int
)";
  GoldFailureTest(Code);
}

TEST(GoldBitsAttribute, BitFieldOfInvalidType) {
  StringRef Code = R"(
NotAnInteger : type = class:
  ;
Clx : type = class:
  x<bits(5)>:NotAnInteger
)";
  GoldFailureTest(Code);
}

TEST(GoldBitsAttribute, ChainOfConstantExpressionVars) {
  StringRef Code = R"(
A:const int = 9
P:const int = 10
Q:const int = A
Clx : type = class:
  x<bits(P+Q)>:int
)";
  DeclarationMatcher ToMatch = fieldDecl(hasName("x"),
    	isBitField(), hasBitWidth(19));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}