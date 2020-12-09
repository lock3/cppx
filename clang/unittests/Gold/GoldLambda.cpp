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
    varDecl(hasName("test"), hasType(asString("int")));
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
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, ExplicitGeneric) {
  StringRef Code = R"(
main() : int! {
  fn = lambda[T : type]{}(m : T){ return m; }
  test = fn(10)
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, ExplicitGenericFailure) {
  StringRef Code = R"(
main() : int! {
  fn = lambda[T : type]{}(m : T){ return m; }
  test = fn[int](10)
}
)";

  GoldFailureTest(Code.str());
}

TEST(GoldLambda, AutoParam) {
  StringRef Code = R"(
main() : int! {
  fn = lambda{}(m : auto, n : auto){ return m + n; }
  test = fn(10, 10)
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, AutoDepth) {
  StringRef Code = R"(
main() : int! {
  fn = lambda{}(m : auto, n : auto){
    yn = lambda{}(m : auto, n : auto) {
      return m + n;
    }

    return yn(m, n);
  }
  test = fn(10, 10)
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, StarThisCapture) {
  StringRef Code = R"(
T = class:
  x : int = 42

  fn()!
    yn = lambda{^this}(){ return x; }
    return yn()

main() : int! {
  t : T
  test = t.fn()
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, ThisCapture) {
  StringRef Code = R"(
T = class:
  x : int = 42

  fn()!
    yn = lambda{this}(){ return x; }
    return yn()

main() : int! {
  t : T
  test = t.fn()
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldLambda, DoubleThisCapture) {
  StringRef Code = R"(
T = class:
  x : int = 42

  fn()!
    yn = lambda{this, this}(){ return x; }
    return yn()

main() : int! {
  t : T
  return t.fn()
}
)";

  GoldFailureTest(Code.str());
}

TEST(GoldLambda, DoubleThisCaptureWithCaret) {
  StringRef Code = R"(
T = class:
  x : int = 42

  fn()!
    yn = lambda{this, ^this}(){ return x; }
    return yn()

main() : int! {
  t : T
  return t.fn()
}
)";

  GoldFailureTest(Code.str());
}

TEST(GoldLambda, ListCapture) {
  StringRef Code = R"(
main() : int! {
  x : int = 10
  y : int = 10
  z : int = 10
  yn = lambda{x, (y, z)}(){ return x + y + z; }
  test = yn()
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}
