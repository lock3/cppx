//=== GoldEnumElab.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This tests all of the ways we can declare an enum.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldEnum, EnumForwardDecl) {
  StringRef Code = R"(
E : type = enum
)";
  DeclarationMatcher ToMatch = enumDecl(
    hasName("E"), unless(isDefinition())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, EnumForwardDeclWithUnderlyingType) {
  StringRef Code = R"(
E : type = enum(uint64)
)";
  DeclarationMatcher ToMatch = enumDecl(
    hasName("E"), underlyingIntegerType(asString("unsigned long")),
    unless(isDefinition())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, Definition) {
  StringRef Code = R"(
E : type = enum:
  x
  y
  z
)";
  DeclarationMatcher ToMatch = enumDecl(
    hasName("E"), isScoped(),
    has(enumConstantDecl(hasName("x"))),
    has(enumConstantDecl(hasName("y"))),
    has(enumConstantDecl(hasName("z")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, WithUnderlyingType) {
  StringRef Code = R"(
E : type = enum(uint64):
  x
  y
  z
)";
  DeclarationMatcher ToMatch = enumDecl(
    hasName("E"), underlyingIntegerType(asString("unsigned long")),
    has(enumConstantDecl(hasName("x"))),
    has(enumConstantDecl(hasName("y"))),
    has(enumConstantDecl(hasName("z")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, WithInitExpr) {
  StringRef Code = R"(
E : type = enum:
  x = 4
  y
  z
)";
  DeclarationMatcher ToMatch = enumDecl(
    hasName("E"),
    has(enumConstantDecl(hasName("x"), has(integerLiteral()))),
    has(enumConstantDecl(hasName("y"))),
    has(enumConstantDecl(hasName("z")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, UseAsAType) {
  StringRef Code = R"(
E : type = enum:
  x = 4
  y
  z

V : E
)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(enumDecl(
      hasName("E"),
      has(enumConstantDecl(hasName("x"), has(integerLiteral()))),
      has(enumConstantDecl(hasName("y"))),
      has(enumConstantDecl(hasName("z")))
    )),
    has(varDecl(hasName("V"), hasType(asString("enum E"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldEnum, HasMemberFunction) {
  StringRef Code = R"(
E : type = enum:
  x = 4
  y
  z
  foo():void
)";
  GoldFailureTest(Code);
}


TEST(GoldEnum, ValueAccess) {
  StringRef Code = R"(
E : type = enum:
  x = 4
  y
  z

V : E = E.y
)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(enumDecl(
      hasName("E"),
      has(enumConstantDecl(hasName("x"), has(integerLiteral()))),
      has(enumConstantDecl(hasName("y"))),
      has(enumConstantDecl(hasName("z")))
    )),
    has(varDecl(hasName("V"), hasType(asString("enum E")),
        hasInitializer(hasDescendant(declRefExpr(to(enumConstantDecl(hasName("y"))))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}