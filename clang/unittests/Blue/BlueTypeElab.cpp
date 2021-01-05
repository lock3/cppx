//=== BlueTypeElab.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all of the different kinds of types to make sure that they all work
//  and create the correct type within the C++ AST.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

#define BLUE_BUILTIN_TYPE_TEST(TYPE_STRING, TEST_SUFFIX, EXPECTED_TYPE_STR)\
TEST(BlueType, BuiltInType_##TEST_SUFFIX) {\
  StringRef Code = "i:" TYPE_STRING ;\
  DeclarationMatcher VarIMatcher = varDecl(\
    hasName("i"), hasType(asString(EXPECTED_TYPE_STR))\
  );\
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));\
}

BLUE_BUILTIN_TYPE_TEST("bool", bool, "_Bool")
BLUE_BUILTIN_TYPE_TEST("null_t", null_t, "nullptr_t")

BLUE_BUILTIN_TYPE_TEST("int", int, "int")
BLUE_BUILTIN_TYPE_TEST("int8", int8, "signed char")
BLUE_BUILTIN_TYPE_TEST("int16", int16, "short")
BLUE_BUILTIN_TYPE_TEST("int32", int32, "int")
BLUE_BUILTIN_TYPE_TEST("int64", int64, "long")
BLUE_BUILTIN_TYPE_TEST("int128", int128, "__int128")

BLUE_BUILTIN_TYPE_TEST("uint", uint, "unsigned int")
BLUE_BUILTIN_TYPE_TEST("uint8", uint8, "unsigned char")
BLUE_BUILTIN_TYPE_TEST("uint16", uint16, "unsigned short")
BLUE_BUILTIN_TYPE_TEST("uint32", uint32, "unsigned int")
BLUE_BUILTIN_TYPE_TEST("uint64", uint64, "unsigned long")
BLUE_BUILTIN_TYPE_TEST("uint128", uint128, "unsigned __int128")

BLUE_BUILTIN_TYPE_TEST("char", char, "unsigned char")
BLUE_BUILTIN_TYPE_TEST("char8", char8, "unsigned char")
BLUE_BUILTIN_TYPE_TEST("char16", char16, "unsigned short")
BLUE_BUILTIN_TYPE_TEST("char32", char32, "unsigned int")

BLUE_BUILTIN_TYPE_TEST("float16", float16, "__fp16")
BLUE_BUILTIN_TYPE_TEST("float32", float32, "float")
BLUE_BUILTIN_TYPE_TEST("float64", float64, "double")
BLUE_BUILTIN_TYPE_TEST("float", float, "double")

#if 0
BLUE_BUILTIN_TYPE_TEST("float128", float128, "__float128")
#endif

#undef BLUE_BUILTIN_TYPE_TEST

TEST(BlueType, BuiltInType_IntegerCompilerFunction_Signed) {
  StringRef Code = "i:integer[1, signed]";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("signed char"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

TEST(BlueType, BuiltInType_IntegerCompilerFunction_Unsigned) {
  StringRef Code = "i:integer[1, unsigned]";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("unsigned char"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

TEST(BlueType, BuiltInType_CharacterCompilerFunction_ASCII) {
  StringRef Code = "i:character[1, ascii]";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("signed char"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

// FIXME: I need to double check this.
TEST(BlueType, BuiltInType_CharacterCompilerFunction_UTF8) {
  StringRef Code = "i:character[1, utf8]";
  DeclarationMatcher VarIMatcher = varDecl(
    // TODO: Double check this to double check the actual type of char8_t
    hasName("i"), hasType(asString("char8_t"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

TEST(BlueType, BuiltInType_FloatCompilerFunction_Binary_Float) {
  StringRef Code = "i:float[4, binary]";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("float"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}

TEST(BlueType, BuiltInType_FloatCompilerFunction_Binary_Double) {
  StringRef Code = "i:float[8, binary]";
  DeclarationMatcher VarIMatcher = varDecl(
    hasName("i"), hasType(asString("double"))
  );
  ASSERT_TRUE(matches(Code.str(), VarIMatcher));
}