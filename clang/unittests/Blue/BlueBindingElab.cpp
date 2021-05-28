//=== BlueBindingElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for binding declarations and use.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueBinding, Simple) {
  StringRef Code = R"BLUE(
z : [2]int;

main:() -> int = {
  z[0] = 1;
  z[1] = 2;
  (x, y) := z;
}
)BLUE";

  auto MatchX = bindingDecl(hasName("x"), hasType(asString("int")));
  auto MatchY = bindingDecl(hasName("y"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), MatchX));
  ASSERT_TRUE(matches(Code.str(), MatchY));
}

TEST(BlueBinding, ExplicitType) {
  StringRef Code = R"BLUE(
z : [2]int;

main:() -> int = {
  z[0] = 1;
  z[1] = 2;
  (x, y is bool) := z;
}
)BLUE";

  auto MatchX = bindingDecl(hasName("x"), hasType(asString("int")));
  auto MatchY = bindingDecl(hasName("y"), hasType(asString("bool")));
  ASSERT_TRUE(matches(Code.str(), MatchX));
  ASSERT_TRUE(matches(Code.str(), MatchY));
}
