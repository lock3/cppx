//=== GoldTemplateTypeAliasElab.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This tests the implementation for a template type alias implementation.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldTemplateTypeAlias, Declaration) {
  StringRef Code = R"(
TCls[T:type] : type = class:
  ;

TTA[T:type] : type = TCls[T]
)";
  DeclarationMatcher ToMatch = typeAliasTemplateDecl(
    hasName("TTA"), has(typeAliasDecl(has(templateSpecializationType())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateTypeAlias, NonDependentAlias) {
  StringRef Code = R"(
TTA[T:type] : type = int
)";
  DeclarationMatcher ToMatch = typeAliasTemplateDecl(hasName("TTA"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateTypeAlias, Use) {
  StringRef Code = R"(
TTA[T:type] : type = int

foo() :TTA[int]
)";
  DeclarationMatcher ToMatch = functionDecl(
    hasType(asString("TTA<int> (void)"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateTypeAlias, InvalidForwardDecl) {
  StringRef Code = R"(
TTA[T:type] : type
)";
  GoldFailureTest(Code);
}

TEST(GoldTemplateTypeAlias, DependentAlias) {
  StringRef Code = R"(
c[T : type] = class {
  j : type = T
}

main() : int!
  int_type : type = c[int].j
  x : int_type = int_type(73)
)";

  SimpleGoldParseTest(Code);
  DeclarationMatcher int_type =
    typeAliasDecl(hasName("int_type"),
                  hasType(asString("c<int>::j")));
  ASSERT_TRUE(matches(Code.str(), int_type));
}
