//=== GoldTypeExpressionsTest.cpp - Testing Expressions involving types ----==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file tests how we handle type evaluation.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldTypeExpr, TypeAlias) {
  StringRef Code = R"(
x : type = int
)";

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(typeAliasDecl(hasName("x"), hasType(asString("int"))))
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}

TEST(GoldTypeExpr, TypeAlias_ThenUsed) {
  StringRef Code = R"(
x : type = int
y : x
)";

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(typeAliasDecl(hasName("x"), hasType(asString("int")))),
    hasDescendant(varDecl(hasName("y"), hasType(asString("x"))))
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}

TEST(GoldTypeExpr, TypeAliasOfAClass) {
  StringRef Code = R"(
c : type = class:
  y : bool = 0

x : type = c
)";

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(typeAliasDecl(hasName("x"), hasType(asString("struct c"))))
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}


TEST(GoldTypeExpr, TypeAliasOfAClass_ThenUsed) {
  StringRef Code = R"(
c : type = class:
  y : bool = 0

x : type = c
main() : int!
  q : x
  return q.y
)";

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(typeAliasDecl(hasName("x"), hasType(asString("struct c")))),
    hasDescendant(functionDecl(hasName("main"),
      hasDescendant(varDecl(hasName("q"), hasType(asString("x"))))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}