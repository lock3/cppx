//=== GoldEnumElab.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This tests all of the ways we can declare an enum.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// TEST(GoldEnumSyntax, EnumForwardDecl) {
//   StringRef Code = R"(
// x : type = enum
// )";
//   DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
//     unless(hasBody(compoundStmt())));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
