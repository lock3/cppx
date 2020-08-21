//=== GoldNamespaceAliasElab.cpp -------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for namespace aliase tests.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldNamespaceAlias, GlobalDeclaration) {
    StringRef Code = R"(
ns : namespace = namespace { ; }
newNs : namespace = ns
)";
  auto ToMatch = namespaceAliasDecl(hasName("newNs"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceAlias, WithinAnotherNamespace) {
    StringRef Code = R"(
ns : namespace = namespace { ; }
ns2 : namespace = namespace:
  newNs : namespace = ns

)";
  auto ToMatch = namespaceDecl(hasName("ns2"),
      has(namespaceAliasDecl(hasName("newNs")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceAlias, DoubleAlias) {
    StringRef Code = R"(
ns : namespace = namespace { ; }
ns2 : namespace = namespace:
  newNs : namespace = ns
  newNs2 : namespace = newNs
)";
  auto ToMatch = namespaceDecl(hasName("ns2"),
      has(namespaceAliasDecl(hasName("newNs"))),
      has(namespaceAliasDecl(hasName("newNs2")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldNamespaceAlias, UseThroughGlobalAlias) {
    StringRef Code = R"(
ns : namespace = namespace:
  i:const int = 1

newNs : namespace = ns

foo() :int!
  return ns.i

)";
  auto ToMatch = translationUnitDecl(
    has(namespaceDecl(hasName("ns"))),
    has(functionDecl(hasName("foo"),
      hasDescendant(returnStmt(hasDescendant(declRefExpr())))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceAlias, UseThroughNestedAlias) {
    StringRef Code = R"(
ns : namespace = namespace:
  i:const int = 1
ns2 : namespace = namespace:
  newNs : namespace = ns
  newNs2 : namespace = newNs

foo() :int!
  return ns2.newNs.i

)";
  auto ToMatch = translationUnitDecl(
    has(namespaceDecl(hasName("ns2"),
      has(namespaceAliasDecl(hasName("newNs"))),
      has(namespaceAliasDecl(hasName("newNs2")))
    )),
    has(functionDecl(hasName("foo"),
      hasDescendant(returnStmt(hasDescendant(declRefExpr())))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}