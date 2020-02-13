//=== GoldParsingTest.cpp - Elaboration for Gold Nodes ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for paring of global variables.
//
//===----------------------------------------------------------------------===//

#include "ParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GlobalVariables, SimpleDecl) {
  StringRef Code = R"(
x : int
)";
  SimpleGoldParseTest(Code);
}

TEST(GlobalVariables, SimpleDeclWithInit) {
  StringRef Code = R"(
x : int = 9
  )";
  SimpleGoldParseTest(Code);
}