//=== GoldUsing.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing using declarations and using directives
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldUsingDeclaration, Decl) {
  StringRef Code = R"(
ns = namespace {
  y : int;
}

main() : int! {
  using {ns.y};
}
)";

  DeclarationMatcher Match =
    usingDecl(hasAnyUsingShadowDecl(hasName("y")));
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(GoldUsingDeclaration, Ambiguous) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
}

ys = namespace {
  y() : int!
    return 0
}

main() : int! {
  using {ys.y, ns.y};
  return y()
}
)";

  GoldFailureTest(Code);
}

TEST(GoldUsingDeclaration, Overload) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
  y(p : int) : int!
    return p
}

main() : int! {
  using {ns.y};
  x = y(42)
}
)";

  DeclarationMatcher X = varDecl(hasName("x"),
                                 hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), X));
}


TEST(GoldUsingDeclaration, OverloadMistype) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
  y(p : int) : int!
    return p
}

main() : int! {
  using {ns.y};
  return y
}
)";

  GoldFailureTest(Code);
}
