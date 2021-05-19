//=== BluePointerElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing declaration of pointer for elaboration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BluePlacementNew, InitAVariable) {
  StringRef Code = R"BLUE(
foo: () -> void = {
  x : ^ int = null;
  __inplace_new(x, int, 4);
}
)BLUE";

  auto ToMatch = translationUnitDecl(
    hasDescendant(cxxNewExpr()),
    has(functionDecl(hasName("__BuiltinBlueInPlaceNew")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BluePlacementNew, DestructorCall) {
  std::string Code = R"Gold(
ToDestroy : type = {
}

foo:(x:^ToDestroy) -> void = {
  __inplace_delete(x);
}
)Gold";
  auto ToMatch = memberExpr(member(hasName("~ToDestroy")));
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(BluePlacementNew, DestructUsedWithinTemplate) {
  std::string Code = R"Gold(
bar:[T:type]->(x:^T) -> void = {
  __inplace_delete(x);
}
)Gold";
  auto ToMatch = cxxDependentScopeMemberExpr();
  ASSERT_TRUE(matches(Code, ToMatch));
}