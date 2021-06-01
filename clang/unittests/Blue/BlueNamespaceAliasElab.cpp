//=== BlueNamespaceAliasElab.cpp ------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for namespace alias declaration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueNamespaceAlias, SimpleAlias) {
  StringRef Code = R"BLUE(
n1:namespace = {
}

Ns : namespace = n1;
)BLUE";
  auto ToMatch = namespaceAliasDecl(hasName("Ns"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}