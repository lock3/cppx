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





GOLD_BUILTIN_TYPE_TEST("bool", bool, "_Bool")
GOLD_BUILTIN_TYPE_TEST("int8", int8, "signed char")
// GOLD_BUILTIN_TYPE_SIMPLE_TEST("int8", int8_t)
// GOLD_BUILTIN_TYPE_SIMPLE_TEST("int", int)
// GOLD_BUILTIN_TYPE_SIMPLE_TEST("long", long)
// GOLD_BUILTIN_TYPE_SIMPLE_TEST("short", short)

// int
// uint



// uint8 
// uint16 
// uint32 
// uint64 
// uint64 
// uint128 

// GOLD_BUILTIN_TYPE_TEST("unsigned", unsigned, "unsigned int")
// GOLD_BUILTIN_TYPE_TEST("signed", signed, "int")

// GOLD_BUILTIN_TYPE_SIMPLE_TEST("float", float)
// GOLD_BUILTIN_TYPE_SIMPLE_TEST("double", double)
// GOLD_BUILTIN_TYPE_TEST("uint128_t", uint128_t, "unsigned __int128")