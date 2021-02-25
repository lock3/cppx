//=== BlueTypeAliasElab.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration of type aliases
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueUsingDirective, Wildcard) {
  StringRef Code = R"(
namespace ns:namespace = {
  var x:int = 7;
}

using ns._;
)";

  auto Match = usingDirectiveDecl();
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(BlueUsingDirective, Specific) {
  StringRef Code = R"(
namespace ns:namespace = {
  var x:int = 7;
}

using ns._;
)";

  auto Match = usingDirectiveDecl();
  ASSERT_TRUE(matches(Code.str(), Match));
}
