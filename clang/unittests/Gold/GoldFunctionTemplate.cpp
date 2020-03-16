//=== FunctionTemplate.cpp - Elaboration for Gold Function Templates -------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of function templates.
//
//===----------------------------------------------------------------------===//

#include "ParseUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFunctionTemplate, MixedTypeParameters) {
  StringRef Code = R"(
f[T : type, z : int](x : T) : T!
  return x + z
)";
  SimpleGoldParseTest(Code);
}
