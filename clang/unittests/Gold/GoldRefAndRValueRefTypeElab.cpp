//=== GoldRefAndRValueRefTypeElab.cpp --------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing for declaration of reference types. Both L & R value
//  reference types.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldRef, LValueRef) {
  std::string Code = R"Gold(
x:int = 0
y:ref int = x
)Gold";
  DeclarationMatcher opMatches = varDecl(
    hasName("y"), hasType(asString("int &")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, LValueRefParam) {
  std::string Code = R"Gold(
x:int = 0
foo(y:ref int): void
)Gold";
  DeclarationMatcher opMatches = parmVarDecl(
    hasName("y"), hasType(asString("int &")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, RValueRefParam) {
  std::string Code = R"Gold(
move(var:rref int):rref int
)Gold";
  DeclarationMatcher opMatches = parmVarDecl(
    hasName("var"), hasType(asString("int &&")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, LValueRefAsDeclaratorOnly) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[ref int]
)Gold";
  DeclarationMatcher opMatches = varDecl(
    hasName("x"), hasType(asString("Ty<int &>")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, RValueRefAsDeclaratorOnly) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[rref int]
)Gold";
  DeclarationMatcher opMatches = varDecl(
    hasName("x"), hasType(asString("Ty<int &&>")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, SubTypeExpr_DoubleRef) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[ref ref int]
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, SubTypeExpr_Ref_NoType) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[ref]
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, SubTypeExpr_RefRValueRef) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[ref rref int]
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, SubTypeExpr_RValueRefRef) {
  std::string Code = R"Gold(
Ty[T:type] :type = class:
  i:int

foo() :void!
  x:Ty[rref ref int]
)Gold";
  GoldFailureTest(Code);
}



TEST(GoldRef, PartOfDeclarator_DoubleRef) {
  std::string Code = R"Gold(
x:ref ref int
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, PartOfDeclarator_Ref_NoType) {
  std::string Code = R"Gold(
x:ref
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, PartOfDeclarator_RefRValueRef) {
  std::string Code = R"Gold(
x:ref rref int
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldRef, PartOfDeclarator_RValueRefRef) {
  std::string Code = R"Gold(
x:rref ref int
)Gold";
  GoldFailureTest(Code);
}