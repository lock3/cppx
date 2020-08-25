//=== GoldNestedNameSpecifierOnDeclElab.cpp --------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of declarations with nested name
//  specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldCompileRun.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldNestedNameDecl, Basic) {
  StringRef Code = R"(
x = namespace:
  foo():void

x.foo():void!
  ;
)";
  auto Matcher = functionDecl(hasName("x::foo"), isDefinition());
  ASSERT_TRUE(matches(Code.str(), Matcher));
}