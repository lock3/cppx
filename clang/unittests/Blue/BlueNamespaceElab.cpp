//=== BlueNamespaceElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for namespace declaration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueNamespace, EmptyNamespace) {
  StringRef Code = R"BLUE(
namespace n1:namespace = { }
)BLUE";
  auto ToMatch = namespaceDecl(hasName("n1"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueNamespace, NonEmptyNamespace) {
  StringRef Code = R"BLUE(
namespace n1:namespace = {
  var x:int;
}
)BLUE";
  auto ToMatch = namespaceDecl(hasName("n1"),
    has(
      varDecl(hasName("x"), hasType(asString("int")))
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueNamespace, UsingNSQualifier) {
  StringRef Code = R"BLUE(
namespace n1:namespace = {
  var x:int;
}
foo:() void = {
  var y:= n1.x;
}
)BLUE";
  auto ToMatch = translationUnitDecl(has(namespaceDecl(hasName("n1"),
    has(
      varDecl(hasName("x"), hasType(asString("int")))
    )
  )),
  has(functionDecl(
    hasDescendant(varDecl(hasName("y"))))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}