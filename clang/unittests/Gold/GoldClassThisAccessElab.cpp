//=== GoldClassThisAccessElab.cpp ------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This tests how classes are elaborated.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassParsing, This_MemberAccess) {
  StringRef Code = R"(
c : type = class:
  x : int
  foo() : int!
    return this.x
  

)";
  DeclarationMatcher ThisUse = recordDecl( 
    hasDescendant(
      cxxMethodDecl(
        hasName("foo"),
        hasDescendant(cxxThisExpr(hasType(asString("struct c *"))))
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), ThisUse));
}

TEST(ClassParsing, InvalidUseOfThis) {
  StringRef Code = R"(
foo() : int!
  return this.x
  

)";
  GoldFailureTest(Code);
}
