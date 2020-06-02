//=== GoldWNNS.cpp - Test Gold nested name specifiers ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of nested name specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(NNS, SimpleNNS) {
  StringRef Code = R"(
ns : namespace = namespace:
  ns2 : namespace = namespace:
    x : int = 1
  y : int = 1

main() : int!
  return ns.ns2.x + ns.y
)";

  SimpleGoldParseTest(Code);
}
