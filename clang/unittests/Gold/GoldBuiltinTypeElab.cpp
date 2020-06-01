//=== GoldBuiltinTypeElab.cpp - Testing to make sure all builtin types work-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language elaborators.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


#define GOLD_BUILTIN_TYPE_TEST(TYPE_STRING, TEST_SUFFIX, EXPECTED_TYPE_STR)\
TEST(BuiltinTypeElab, BuiltInType_##TEST_SUFFIX) {\
  StringRef Code = "i:" TYPE_STRING ;\
  DeclarationMatcher VarIMatcher = varDecl(\
    hasName("i"), hasType(asString(EXPECTED_TYPE_STR))\
  );\
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));\
}

#define GOLD_BUILTIN_TYPE_SIMPLE_TEST(TYPE_STRING, TEST_SUFFIX)\
  GOLD_BUILTIN_TYPE_TEST(TYPE_STRING, TEST_SUFFIX, TYPE_STRING)

#define GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST(TYPE_STRING, TEST_SUFFIX)\
  GOLD_BUILTIN_TYPE_SIMPLE_TEST(TYPE_STRING, TEST_SUFFIX)\
  GOLD_BUILTIN_TYPE_SIMPLE_TEST("unsigned " TYPE_STRING, unsigned_##TEST_SUFFIX)\
  GOLD_BUILTIN_TYPE_TEST("signed " TYPE_STRING, signed_##TEST_SUFFIX, TYPE_STRING)

GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("char", char)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("short", short)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("short int", short_int)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("int", int)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("long", long)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("long int", long_int)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("long long", long_long)
GOLD_BUILTIN_TYPE_ARITHMETIC_VARIABLE_TEST("long long int", long_long_int)

TEST(BuiltinTypeElab, BuiltInType_unsigned) {
    StringRef Code = "i:unsigned";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("unsigned int"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

TEST(BuiltinTypeElab, BuiltInType_signed) {
  StringRef Code = "i:signed";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("int"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}
