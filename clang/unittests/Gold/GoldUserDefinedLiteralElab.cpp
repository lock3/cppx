//=== GoldUserDefinedLiteralElab.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing literal declarations.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// Working on it.
// TEST(GoldUnion, SimpleDecl) {
//   StringRef Code = R"(


// )";

//   DeclarationMatcher ToMatch = tagDecl(hasName("U"), isUnion(), isDefinition());
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
