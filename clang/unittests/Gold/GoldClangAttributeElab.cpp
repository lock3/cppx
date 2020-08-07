//=== GoldClangAttributeElab.cpp ------------------------------------------===//
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

TEST(GoldClangAttr, ReturnsNonNull) {
  StringRef Code = R"(
[gnu.returns_nonnull]
foo(i:int) : ^void
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
      hasAttr(clang::attr::ReturnsNonNull));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldClangAttr, Nullable) {
//   StringRef Code = R"(
// foo(i<_Nullable>:^int) : ^void
// )";
//   DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
//       hasAttr(clang::attr::Nullable));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
TEST(GoldClangAttr, gsl_suppress) {
  StringRef Code = R"(
[gsl.suppress("Rh-public")]
foo(i:^int) : ^void
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
      hasAttr(clang::attr::Suppress));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
// 