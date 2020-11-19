//=== GoldLambda.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing lambda expressions.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// Yes, there is a such thing as a global local lambda.
TEST(GoldLambda, SimpleGlobalLocalLambda) {
  StringRef Code = R"(
fn = lambda{}(x : int){ return x; }

main() : int! {
  test = fn(42);
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")),
            hasDescendant(integerLiteral(equals(42))));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, SimpleCapture) {
  StringRef Code = R"(
main() : int! {
  y : int = 42
  fn = lambda{ y  }(){ return y; }
  test = fn();
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")),
            hasDescendant(integerLiteral(equals(42))));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, InitCapture) {
  StringRef Code = R"(
main() : int! {
  y : int = 42
  fn = lambda{ y = 20 }(){ return y; }
  test = fn();
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")),
            hasDescendant(integerLiteral(equals(20))));
  ASSERT_TRUE(matches(Code.str(), Test));
}
