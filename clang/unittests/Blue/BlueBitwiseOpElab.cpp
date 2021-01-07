//=== BlueBitwiseOpElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing all of the bitwise operators.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

// TODO: FIX me when I have VarDeclRef working.
// TEST(BlueBitwiseOp, BWAnd) {
//   StringRef Code = R"BLUE(
// x:=4 & 5
//   )BLUE";
//   auto ToMatch = binaryOperator(hasOperatorName("&"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
// TEST(BlueBitwiseOp, BWOr) {
//   StringRef Code = R"BLUE(
// x:=4 | 5
//   )BLUE";
//   auto ToMatch = binaryOperator(hasOperatorName("|"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueBitwiseOp, BWXOr) {
//   StringRef Code = R"BLUE(
// x:=4 ^ 5
//   )BLUE";
//   auto ToMatch = binaryOperator(hasOperatorName("^"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueBitwiseOp, LeftShift) {
//   StringRef Code = R"BLUE(
// x:=4 << 5
//   )BLUE";
//   auto ToMatch = binaryOperator(hasOperatorName("<<"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueBitwiseOp, BWRightShift) {
//   StringRef Code = R"BLUE(
// x:=4 >> 5
//   )BLUE";
//   auto ToMatch = binaryOperator(hasOperatorName(">>"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueBitwiseOp, BWComposite) {
//   StringRef Code = R"BLUE(
// x:=~4
//   )BLUE";
//   auto ToMatch = unaryOperator(hasOperatorName("~"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }