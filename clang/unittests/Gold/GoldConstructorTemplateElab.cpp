//=== GoldConstructorTemplateElab.cpp --------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for constructors with template parameters.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConstructorTemplate, CtorTemplateInsideNonTemplateClass) {
  StringRef Code = R"(
C : type = class:
  constructor[T:type](x:ref T)! {
    z = x;
    y = bool(x);
  }
  z : int
  y : bool
)";
  auto ToMatch = cxxRecordDecl(
    hasName("C"),
    has(functionTemplateDecl(
      has(cxxConstructorDecl())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}