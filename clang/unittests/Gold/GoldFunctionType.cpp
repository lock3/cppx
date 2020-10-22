//=== GoldFunctionType.cpp - Test Gold function types ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of function types.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFunctionType, SimpleFnType) {
  StringRef Code = R"(
void_void : (() -> void)
intint_int : ((int, int) -> int)
)";

  DeclarationMatcher VV = varDecl(hasName("void_void"),
                                  hasType(asString("void (*)(void)")));
  DeclarationMatcher II_I = varDecl(hasName("intint_int"),
                                    hasType(asString("int (*)(int, int)")));
  ASSERT_TRUE(matches(Code.str(), VV));
  ASSERT_TRUE(matches(Code.str(), II_I));
}

TEST(GoldFunctionType, SimpleMethodType) {
  StringRef Code = R"(
T : type = class:
  foo(x : int) : int!
    return x;
  bar() : int!
    return 10

main() : int!
  foo_ptr : T.(int) -> int = &T.foo
  bar_ptr : T.() -> int = &T.bar
  t : T

  foo_test = (t.^foo_ptr)(10)
  bar_test = (t.^bar_ptr)()
)";

  DeclarationMatcher foo_test = varDecl(hasName("foo_test"),
                                        hasType(asString("int")));
  DeclarationMatcher bar_test = varDecl(hasName("bar_test"),
                                        hasType(asString("int")));
  DeclarationMatcher foo_ptr = varDecl(hasName("foo_ptr"),
                                        hasType(asString("int (struct T::*)(int)")));
  DeclarationMatcher bar_ptr = varDecl(hasName("bar_ptr"),
                                        hasType(asString("int (struct T::*)(void)")));
  ASSERT_TRUE(matches(Code.str(), foo_test));
  ASSERT_TRUE(matches(Code.str(), bar_test));
  ASSERT_TRUE(matches(Code.str(), foo_ptr));
  ASSERT_TRUE(matches(Code.str(), bar_ptr));
}
