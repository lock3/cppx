//=== GoldWNNS.cpp - Test Gold nested name specifiers ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of nested name specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(NNS, SimpleNNS) {
  StringRef Code = R"(
ns : namespace = namespace:
  ns2 : namespace = namespace:
    x : int = 1
  y : int = 1

main() : int!
  return ns.ns2.x + ns.y
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, GlobalNNS) {
  StringRef Code = R"(
x : int = 9

main() : int!
  x : int = 10
  return x + .x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, GlobalNested) {
  StringRef Code = R"(
ns : namespace = namespace:
  x : int = 0

main() : int!
  x : int = 10
  return .ns.x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, AutoNNS) {
  StringRef Code = R"(
ns = namespace:
  x : int = 5

main() : int!
  x : int = 4
  return .ns.x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, PrefixBaseSpecifier) {
  StringRef Code = R"(
A : type = class:
  i : int = 9

B: type = class (A):
  ;

main() : int!
  b : B
  result = b.(A)i
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(NNS, PrefixBaseSpecifierTempl) {
  StringRef Code = R"(
A[T : type] : type = class:
  i : T = 9

B[T : type] : type = class (A[T]):
  ;

main() : int!
  b : B[int]
  result = b.(A[int])i
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}
