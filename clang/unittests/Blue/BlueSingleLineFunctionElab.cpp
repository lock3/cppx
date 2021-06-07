//=== BlueSingleLineFunctionElab.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for namespace alias declaration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueSingleLineFunction, SimpleDecl) {
  StringRef Code = R"BLUE(
foo:()->int = 4;
)BLUE";
  auto ToMatch = translationUnitDecl(
    hasDescendant(functionDecl(hasName("foo"), hasType(asString("int (void)")))),
    hasDescendant(returnStmt())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}