//=== BlueFunctionCallTransformElab.cpp =----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for member access, and member access with nested name specifiers.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueMemberTransform, TestingBasicTransform) {
  StringRef Code = R"BLUE(
A : type = {
}

foo:(x:A) -> void = {
}


bar:()->void = {
  x : A;
  x.foo();
}

)BLUE";
  auto ToMatch = callExpr(callee(functionDecl(hasName("foo"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}