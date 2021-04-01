//=== GoldStringLiteralElab.cpp --------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  String literal tests.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldStringLiteral, EmptyStringLiteral) {
  StringRef Code = R"(i:^ const char = "")";
  DeclarationMatcher IntCmp = varDecl(
    hasName("i"), hasType(asString("const unsigned char *")),
    hasInitializer(hasDescendant(stringLiteral(stringHasValue(u8""))))
  );
  ASSERT_TRUE(matches(Code.str(), IntCmp));
}

TEST(GoldStringLiteral, NotEmpty) {
  StringRef Code = R"(i:^ const char ="value")";
  DeclarationMatcher IntCmp = varDecl(
    hasName("i"), hasType(asString("const unsigned char *")),
    hasInitializer(hasDescendant(stringLiteral(stringHasValue(u8"value"))))
  );
  ASSERT_TRUE(matches(Code.str(), IntCmp));
}

TEST(GoldStringLiteral, EscapedTabCharacter) {
  StringRef Code = R"(i:^ const char ="value\t")";
  DeclarationMatcher IntCmp = varDecl(
    hasName("i"), hasType(asString("const unsigned char *")),
    hasInitializer(hasDescendant(stringLiteral(stringHasValue("value\t"))))
  );
  ASSERT_TRUE(matches(Code.str(), IntCmp));
}

TEST(GoldStringLiteral, UnterminatedString) {
  StringRef Code = R"(i:^ const char ="value)";
  GoldFailureTest(Code);
}

TEST(GoldStringLiteral, StringWithNewlineInside) {
  StringRef Code = R"(i:^ const char ="value
")";
  GoldFailureTest(Code);
}