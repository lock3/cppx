//=== GoldConst.cpp - Test Gold const qualifier ----------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of const qualifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Const, ConstPointer) {
  StringRef Code = R"(
x : const ^ const int = 0
)";

  SimpleGoldParseTest(Code);
}

