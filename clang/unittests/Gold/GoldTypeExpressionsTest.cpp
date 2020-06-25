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
// TEST(GoldTypeExpr, AliasTemplate) {
//   StringRef Code = R"(
// A[X:type] : type = class:
//   y : X

// B[T:type] : type = A[T]

// )";

//   DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
//     hasDescendant(typeAliasDecl(hasName("x"), hasType(asString("struct c")))),
//     hasDescendant(functionDecl(hasName("main"),
//       hasDescendant(varDecl(hasName("q"), hasType(asString("x"))))
//     ))
//   );

//   ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
// }