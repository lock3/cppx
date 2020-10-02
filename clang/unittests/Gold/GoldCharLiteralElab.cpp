//=== GoldCharLiteralElab.cpp - Testing to make sure all builtin types work-==//
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


// FIXME: We need hex, octal, UTF-8 implementation tests for this as well as a
// possible wide char implementation.

TEST(CharLiteralElab, SimpleCharacter_LowerCase) {
  StringRef Code = "i:char = 'a'";
  DeclarationMatcher CharacterCmp = varDecl(
    hasName("i"), hasType(asString("unsigned char")),
    hasInitializer(characterLiteral(equals(unsigned('a'))))
  );
  ASSERT_TRUE(matches(Code.str(), CharacterCmp));
}

TEST(CharLiteralElab, SimpleCharacter_UpperCase) {
  StringRef Code = "i:char = 'A'";
  DeclarationMatcher CharacterCmp = varDecl(
    hasName("i"), hasType(asString("unsigned char")),
    hasInitializer(characterLiteral(equals(65u)))
  );
  ASSERT_TRUE(matches(Code.str(), CharacterCmp));
}

static bool testEscapedCharacterConstant(const std::string& EscapedCharacterStr,
    char ExpectedValue) {
  std::string Code = "i:char = '" + EscapedCharacterStr + "'";
  DeclarationMatcher CharacterCmp = varDecl(
    hasName("i"), hasType(asString("unsigned char")),
    hasInitializer(characterLiteral(equals(unsigned(ExpectedValue))))
  );
  if (!matches(Code, CharacterCmp))
    return testing::AssertionFailure()
        << "Test failed for Escaped character sequence \""
        << EscapedCharacterStr << "\"";
  return testing::AssertionSuccess();
}

TEST(CharLiteralElab, Character_EscapedCharacters) {
  ASSERT_TRUE(testEscapedCharacterConstant("\\a",'\a'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\b",'\b'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\f",'\f'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\n",'\n'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\r",'\r'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\t",'\t'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\v",'\v'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\\"",'\"'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\'",'\''));
  ASSERT_TRUE(testEscapedCharacterConstant("\\?",'\?'));
  ASSERT_TRUE(testEscapedCharacterConstant("\\\\",'\\'));
}

TEST(CharLiteralElab, Character_Hex) {
  StringRef Code = "i:char = '\\x21'";
  DeclarationMatcher CharacterCmp = varDecl(
    hasName("i"), hasType(asString("unsigned char")),
    hasInitializer(characterLiteral(equals(unsigned('!'))))
  );
  ASSERT_TRUE(matches(Code.str(), CharacterCmp));
}


TEST(CharLiteralElab, Character_Octal) {
  StringRef Code = "i:char = '\\041'";
  DeclarationMatcher CharacterCmp = varDecl(
    hasName("i"), hasType(asString("unsigned char")),
    hasInitializer(characterLiteral(equals(unsigned('!'))))
  );
  ASSERT_TRUE(matches(Code.str(), CharacterCmp));
}

TEST(CharLiteralElab, InvalidCharacterLiteral) {
  StringRef Code = "i:char = ''";
  GoldFailureTest(Code);
}


