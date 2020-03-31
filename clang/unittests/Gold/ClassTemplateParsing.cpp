//=== ClassTempalteParsing.cpp - Elaboration for Gold Nodes ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "ParseUtil.h"
#include "ASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassTemplate, ClassTemplateDeclaration) {
StringRef Code = R"(
c[x:type] : type = class:
  z : int
  y : bool

main() : int!
  temp : c[int]
  return 0
)";
  DeclarationMatcher ClassC = recordDecl();
  ASSERT_TRUE(matches(Code, ClassC));
  ASSERT_FALSE(true) << "Figure out what clang should output for this.";
}

