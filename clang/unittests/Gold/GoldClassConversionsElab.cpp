//=== GoldClassConversionsElab.cpp -----------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests to make sure that qualified member access works
//  correctly.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldClassConversions, ImplicitConversionToBase) {
  // FIXME: Adding this so I don't forget to implement it.
  ASSERT_FALSE(true)
    << "Implement me!";

}