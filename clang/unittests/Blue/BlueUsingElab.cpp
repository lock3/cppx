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
ns:namespace = {
  x:int = 7;
}

using ns._;
)";

  auto Match = usingDirectiveDecl();
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(BlueUsingDirective, Specific) {
  StringRef Code = R"(
ns:namespace = {
  x:int = 7;
}

using ns._;
)";

  auto Match = usingDirectiveDecl();
  ASSERT_TRUE(matches(Code.str(), Match));
}


TEST(BlueUsingDirective, NamespaceAlias) {
  StringRef Code = R"(
ns:namespace = {
  x:int = 7;
}

using N = ns;
)";

  auto ToMatch = namespaceAliasDecl(hasName("N"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueUsingDirective, NamespaceAliasReferenceThroughIt) {
  StringRef Code = R"(
ns:namespace = {
  x:int = 7;
}

using N = ns;
foo:()={
  N.x = 5;
}
)";

  auto ToMatch = namespaceAliasDecl(hasName("N"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}