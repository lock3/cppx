//=== GoldMutableAttributeElab.cpp -----------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the mutable attribute for class members.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldMutableAttr, MemberVariable) {
  std::string Code = R"Gold(
Cls : type = class:
  x<mutable> :int
)Gold";
  DeclarationMatcher ToMatch = cxxRecordDecl(
    hasName("Cls"),
    has(fieldDecl(hasName("x"), isMutable()))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldMutableAttr, InvalidNonMemberMutable) {
  std::string Code = R"Gold(
x<mutable> :int
)Gold";
  GoldFailureTest(Code);
}