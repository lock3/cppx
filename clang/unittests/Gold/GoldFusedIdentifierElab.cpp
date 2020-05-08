//===- unittest/Gold/GoldFusedIdentifierElab.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFusedIdentiferElab, DoubleQuoteFusedIdentifier) {
  StringRef Code = R"(
x"x " : int
)";
  DeclarationMatcher FusedIdentifier = varDecl(
    hasName("x\"x \"")
  );
  ASSERT_TRUE(matches(Code.str(), FusedIdentifier));
}

TEST(GoldFusedIdentiferElab, SingleQuoteFusedIdentifier) {
  StringRef Code = R"(
x'x' : int
)";
  DeclarationMatcher FusedIdentifier = varDecl(
    hasName("x'x'")
  );
  ASSERT_TRUE(matches(Code.str(), FusedIdentifier));
}
