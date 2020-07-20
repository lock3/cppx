//=== GoldExplicitAttributeElab.cpp ----------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the explicit attribute for constructors, and conversion operators.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldExplicitAttr, OnAConstructor) {
  std::string Code = R"Gold(
Cls : type = class:
  constructor(i:int)<explicit>: void!
    ;
  
)Gold";
  DeclarationMatcher ToMatch = cxxRecordDecl(
    hasName("Cls"),
    has(cxxConstructorDecl(isExplicit()))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldExplicitAttr, OnInvalidMemberFunction) {
  std::string Code = R"Gold(
Cls : type = class:
  foo()<explicit>:void!
    ;
  
)Gold";
  GoldFailureTest(Code);
}