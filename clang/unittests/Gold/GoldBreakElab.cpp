//=== GoldBreakElab.cpp ---------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for the the break statement.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldBreak, WithinWhileLoop) {
  StringRef Code = R"(
main() : int!
  i = 0
  while (i <> 10):
    i += 1
    break
  return i
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("main"), isMain(),
                                                  hasDescendant(breakStmt())
                                                 );
  ASSERT_TRUE(matches(Code.str(), ToMatch));  
}

TEST(GoldBreak, WithinForLoop) {
  StringRef Code = R"(
main() : int!
  xs : [3]int = array{0, 1, 2}
  y : int = 0
  for (x : int in xs):
    y += x
    break
  return 0
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("main"), isMain(),
                                                  hasDescendant(breakStmt())
                                                 );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldBreak, InvalidBreakLocation) {
  StringRef Code = R"(
main() : int!
  break
  return i
)";
  GoldFailureTest(Code);
}