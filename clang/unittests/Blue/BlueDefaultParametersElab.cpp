//=== BlueDefaultParameterElab.cpp -----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for Functions with default arguments.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueDefaultParameter, DefaultArg) {
  StringRef Code = R"BLUE(
func foo:(x:int = 5) = {
  return x;
})BLUE";
  auto ToMatch = parmVarDecl(hasDescendant(integerLiteral(equals(5))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultParameter, CallWithDefaultArgument) {
  StringRef Code = R"BLUE(
func foo:(x:int = 5) = {
  return x;
}

func bar:() = {
  return foo();
})BLUE";
  auto ToMatch = callExpr(callee(
    functionDecl(hasName("foo"), hasDescendant(
      parmVarDecl(hasDescendant(integerLiteral(equals(5))))
    ))
  ));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultParameter, GivenArgumentForDefault) {
  StringRef Code = R"BLUE(
func foo:(x:int = 5) = {
  return x;
}

func bar:() = {
  return foo(3);
})BLUE";
  auto ToMatch = callExpr(callee(
    functionDecl(hasName("foo"))
  ));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultParameter, DefaultArgForMemberFunctionCall) {
  StringRef Code = R"BLUE(
type X:class= {

  func foo:(in this, x:int = 5) = {
    return x;
  }
}

func bar:(t:^X) = {
  return t.foo();
})BLUE";
  auto ToMatch = 	cxxMemberCallExpr(callee(
    cxxMethodDecl(hasName("foo"))
  ));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultParameter, InvalidDefaultThis) {
  StringRef Code = R"BLUE(
type X:class= {

  func foo:(in this := 4, x:int = 5) = {
    return x;
  }
}

func bar:(t:^X) = {
  return t.foo();
})BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDefaultParameter, DefaultArgumentsWithTrailingNonDefaults) {
  StringRef Code = R"BLUE(
func foo:(x:int = 5, y:int) = {
  return x;
}
)BLUE";
  BlueFailureTest(Code);
}