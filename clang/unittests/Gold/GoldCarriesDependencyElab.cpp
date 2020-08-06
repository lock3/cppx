//=== GoldCarriesDependencyElab.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the carries_dependency attribute
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldCarriesDependency, OnFunction) {
  StringRef Code = R"(
[carries_dependency]
foo(i:int) : int
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
      hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCarriesDependency, OnParameter) {
  StringRef Code = R"(
foo(x:^int, y<carries_dependency>:^int):int
)";
  DeclarationMatcher ToMatch =
    parmVarDecl(hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCarriesDependency, AsCall) {
  StringRef Code = R"(
f(i:int)<carries_dependency()> : int
)";
  DeclarationMatcher ToMatch =
    parmVarDecl(hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
