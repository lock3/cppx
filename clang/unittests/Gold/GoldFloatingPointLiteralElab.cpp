//=== GoldFloatingPointLiteralElab.cpp - Testing to make sure all builtin types work-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language elaborators.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// FIXME: floating point literals don't seem to be working correctly.
// TEST(FloatingPointElab, NormalNotation) {
//   StringRef Code = R"(
// i:float = 100.0
// )";
//   DeclarationMatcher ClassC = varDecl(
//     hasName("i"), hasType(asString("float")),
//     hasInitializer(has(floatLiteral(equals(100.0))))
  
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(FloatingPointElab, ScientificNotation) {
//   StringRef Code = R"(
// i:float = 1e2
// )";
//   DeclarationMatcher ClassC = varDecl(
//     hasName("i"), hasType(asString("float")),
//     hasInitializer(has(floatLiteral(equals(100.0))))
  
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }