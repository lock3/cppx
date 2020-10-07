//=== GoldBlockMacrosElab.cpp ----------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests the syntax for the catch as part of a macro.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldBlockMacro, MultipleNestings) {
    StringRef Code = R"(
foo():void!
  {
    h:int = 4
    {
      i:int = 5
    }
    j:int = 3
  }
  k:int = 2
)";
  auto ToMatch = compoundStmt(
    has(compoundStmt(
      has(compoundStmt(
        has(declStmt(has(varDecl(hasName("i")))))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldBlockMacro, InvalidBlockIsntADecl) {
    StringRef Code = R"(
{
  h:int = 4
  {
    i:int = 5
  }
  j:int = 3
}
)";
  GoldFailureTest(Code);
}