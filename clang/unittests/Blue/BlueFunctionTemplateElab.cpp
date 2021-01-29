//=== BlueFunctionTemplateElab.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for function declaration/elaboration
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueFunctionTemplate, SimpleFunctionDecl) {
  StringRef Code = R"BLUE(
foo:[T:type] => (x:T) -> void {
}
)BLUE";

  auto ToMatch = functionTemplateDecl(
    has(functionDecl(
      hasName("foo"),
      hasType(asString("void (type-parameter-0-0)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunctionTemplate, Use) {
  StringRef Code = R"BLUE(
foo:[T:type] => (x:T) -> void {
}
bar:() {
  foo(34);
}
)BLUE";

  auto ToMatch = functionTemplateDecl(
    has(functionDecl(
      hasName("foo"),
      hasType(asString("void (type-parameter-0-0)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunctionTemplate, UseProvidingTemplateArguments) {
  StringRef Code = R"BLUE(
foo:[T:type] => (x:T) -> void {
}
bar:() {
  foo[int](34);
}
)BLUE";

  auto ToMatch = functionTemplateDecl(
    has(functionDecl(
      hasName("foo"),
      hasType(asString("void (type-parameter-0-0)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}