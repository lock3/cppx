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
