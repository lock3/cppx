//=== GoldNamespaceDeclElab.cpp --------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for namespace declarations tests.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldNamespaceDecl, DeclWithTemplateParameters) {
    StringRef Code = R"(
NS[T:type] = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, DeclWithSpecialization) {
    StringRef Code = R"(
NS[int] = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, NNSWithTemplateParameters) {
    StringRef Code = R"(
X[T:type].NS = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, NNSWithSpecialization) {
    StringRef Code = R"(
X[int].NS = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, NamespaceNoBody) {
    StringRef Code = R"(
NS = namespace
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, NNSIsAClass) {
    StringRef Code = R"(
X = class:
  ;
X.NS = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, GloballyQualifiedNestedNamespace) {
    StringRef Code = R"(
.X.NS : namespace = namespace { ; }
)";
  GoldFailureTest(Code);
}

TEST(GoldNamespaceDecl, ExistingNamespace) {
    StringRef Code = R"(
X = namespace{ ; }
X.NS : namespace = namespace { ; }
)";
  auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceDecl, ImplicitNestedNamespaceDecl) {
    StringRef Code = R"(
X.NS : namespace = namespace { ; }
)";
  auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceDecl, ImplicitNestedNamespaceDecl_withinNamespace) {
    StringRef Code = R"(
X.NS : namespace = namespace { NS2.NS3 = namespace { ; } }
)";
  auto ToMatch = namespaceDecl(hasName("X"),
    has(namespaceDecl(hasName("NS"),
      has(namespaceDecl(hasName("NS2"),
        has(namespaceDecl(hasName("NS3")))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceDecl, ImplicitNestedNamespaceDecl_WithExternC) {
    StringRef Code = R"(
X.NS<extern("C")> : namespace = namespace { ; }
)";
  auto ToMatch = namespaceDecl(hasName("X"), has(linkageSpecDecl(
    has(namespaceDecl(hasName("NS")))
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldNamespaceDecl, ImplicitNestedNamespaceDecl_Use) {
    StringRef Code = R"(
X.NS : namespace = namespace {
  i<inline>:int = 4;
}

foo():int!
  return X.NS.i
)";
  auto ToMatch = translationUnitDecl(
    has(namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))))),
    has(functionDecl(hasName("foo"), hasDescendant(declRefExpr(to(varDecl(hasName("i")))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldNamespaceDecl, ExistingNamespaceAlreadyExists_UseOfDecls) {
    StringRef Code = R"(
X = namespace{ ; }
X.NS : namespace = namespace {
  i<inline>:int = 4;
}

foo():int!
  return X.NS.i
)";
  auto ToMatch = translationUnitDecl(
    has(namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))))),
    has(functionDecl(hasName("foo"), hasDescendant(declRefExpr(to(varDecl(hasName("i")))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}