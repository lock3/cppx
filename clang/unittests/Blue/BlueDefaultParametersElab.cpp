//=== BlueDefaultParameterElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for Functions with default arguments.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueDefaultParameter, DefaultParam) {
  StringRef Code = R"BLUE(
func foo:(x:int = 5) = {
  return x;
})BLUE";
  auto ToMatch = parmVarDecl(hasDescendant(integerLiteral(equals(5))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}