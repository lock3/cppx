//=== GoldCompatibilityTypesElab.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing to make sure that we support some built in types inside C/C++.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldCompatibilityType, cchar) {
  StringRef Code = R"(
i:cchar
)";
  DeclarationMatcher Match = varDecl(hasName("i"), hasType(asString("char")));
  ASSERT_TRUE(matches(Code.str(), Match));
}
