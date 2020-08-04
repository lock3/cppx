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

/*
|-ClassTemplateDecl 0x7fffe623c9d0 <cpp_test.cpp:10:1, line:13:1> line:11:8 A
| |-TemplateTypeParmDecl 0x7fffe623c870 <line:10:10, col:19> col:19 referenced typename depth 0 index 0 X
| `-CXXRecordDecl 0x7fffe623c938 <line:11:1, line:13:1> line:11:8 struct A definition
|   |-DefinitionData aggregate standard_layout trivially_copyable trivial
|   | |-DefaultConstructor exists trivial needs_implicit
|   | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
|   | |-MoveConstructor exists simple trivial needs_implicit
|   | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
|   | |-MoveAssignment exists simple trivial needs_implicit
|   | `-Destructor simple irrelevant trivial needs_implicit
|   |-CXXRecordDecl 0x7fffe623cc10 <col:1, col:8> col:8 implicit struct A
|   `-FieldDecl 0x7fffe623ccb8 <line:12:3, col:5> col:5 y 'X'
`-TypeAliasTemplateDecl 0x7fffe623ced0 <line:15:1, line:16:14> col:1 B
  |-TemplateTypeParmDecl 0x7fffe623cd20 <line:15:10, col:19> col:19 referenced typename depth 0 index 0 T
  `-TypeAliasDecl 0x7fffe623ce70 <line:16:1, col:14> col:7 B 'A<T>'
    `-TemplateSpecializationType 0x7fffe623cdd0 'A<T>' dependent A
      `-TemplateArgument type 'T'
*/

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