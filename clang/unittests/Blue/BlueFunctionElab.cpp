//=== BlueFunctionElab.cpp -------------------------------------------------===//
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

TEST(BlueFunction, SimpleFunctionDecl){
  StringRef Code = R"BLUE(
foo:() -> void{
}
)BLUE";

  auto ToMatch = functionDecl(hasName("foo"), hasType(asString("void ()")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunction, WithParameter){
  StringRef Code = R"BLUE(
foo:(x:int) -> void{
}
)BLUE";

  auto ToMatch = functionDecl(hasName("foo"), hasType(asString("void (int)")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunction, WithParameters){
  StringRef Code = R"BLUE(
foo:(x:int, y:int) -> void{
}
)BLUE";

  auto ToMatch = functionDecl(hasName("foo"), hasType(asString("void (int, int)")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunction, Overloaded){
  StringRef Code = R"BLUE(
foo:(x:int, y:int) -> void{ }
foo:(x:int) -> void { }
)BLUE";

  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("foo"), hasType(asString("void (int, int)")))),
    has(functionDecl(hasName("foo"), hasType(asString("void (int)"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueFunction, NoBody) {
  // TODO: I'm not sure if this is an error or not.

  StringRef Code = R"BLUE(
foo:(x:int, y:int) -> void
)BLUE";
  BlueFailureTest(Code);
}