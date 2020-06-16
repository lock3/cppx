//=== GoldUserDefinedOperatorOverloadElab.cpp ------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Test's for different types of casting implementations.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


// TEST(GoldUserDefinedOperator, UnaryPlus) {
//   ASSERT_FALSE(true) << "Implement me!";
//   using namespace std::string_literals;
//   std::string Code = R"Gold(foo():void!

// )Gold";
//   DeclarationMatcher opMatches = hasDescendant(cxxStaticCastExpr());
//   ASSERT_TRUE(matches(Code, opMatches))
//     << "Static cast failed";
// }