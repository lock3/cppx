//=== GoldInPlaceNewAndDeleteElab.cpp --------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration for the inplace new operator.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldNew, PlacementNew) {
  std::string Code = R"Gold(
foo(x:^int):void!
  x.construct(43)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("__GoldInplaceNew"))),
    hasDescendant(cxxNewExpr())
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNew, DestructorCall) {
  std::string Code = R"Gold(
ToDestory : type = class:
  ;
foo(x:^ToDestory):void!
  x.destruct()
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("__GoldInplaceNew"))),
    hasDescendant(cxxMemberCallExpr())
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}