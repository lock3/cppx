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
GOLD_BUILTIN_TYPE_TEST("null_t", null_t, "nullptr_t")

GOLD_BUILTIN_TYPE_TEST("int", int, "int")
GOLD_BUILTIN_TYPE_TEST("int8", int8, "signed char")
GOLD_BUILTIN_TYPE_TEST("int16", int16, "short")
GOLD_BUILTIN_TYPE_TEST("int32", int32, "int")
GOLD_BUILTIN_TYPE_TEST("int64", int64, "long")
GOLD_BUILTIN_TYPE_TEST("int128", int128, "__int128")

GOLD_BUILTIN_TYPE_TEST("uint", uint, "unsigned int")
GOLD_BUILTIN_TYPE_TEST("uint8", uint8, "unsigned char")
GOLD_BUILTIN_TYPE_TEST("uint16", uint16, "unsigned short")
GOLD_BUILTIN_TYPE_TEST("uint32", uint32, "unsigned int")
GOLD_BUILTIN_TYPE_TEST("uint64", uint64, "unsigned long")
GOLD_BUILTIN_TYPE_TEST("uint128", uint128, "unsigned __int128")

GOLD_BUILTIN_TYPE_TEST("char", char, "unsigned char")
GOLD_BUILTIN_TYPE_TEST("char8", char8, "unsigned char")
GOLD_BUILTIN_TYPE_TEST("char16", char16, "unsigned short")
GOLD_BUILTIN_TYPE_TEST("char32", char32, "unsigned int")

GOLD_BUILTIN_TYPE_TEST("float16", float16, "__fp16")
GOLD_BUILTIN_TYPE_TEST("float32", float32, "float")
GOLD_BUILTIN_TYPE_TEST("float64", float64, "double")
#if 0
GOLD_BUILTIN_TYPE_TEST("float128", float128, "__float128")
#endif

TEST(BuiltinTypeElab, BuiltInType_InvalidTypeName) {
  llvm::StringRef Code = R"Gold(
i:sponge
)Gold";
  GoldFailureTest(Code);
}

