//===- unittest/Gold/GoldDefaultFunctionArgumentsElab.cpp -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Tests for default arguments to functions.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldDefaultArguments, SingleDefaultArg) {
  StringRef Code = R"(
foo(x:int = 4): int!
  return x
)";
  DeclarationMatcher ToMatch = parmVarDecl(hasName("x"), hasDefaultArgument());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDefaultArguments, InvalidRedeclaration) {
  StringRef Code = R"(
foo(y:int) : int!
  return y

foo(x:int = 4): int!
  return x
)";
  // DeclarationMatcher ToMatch = parmVarDecl(hasName("q"), hasDefaultArgument());
  // ASSERT_TRUE(matches(Code.str(), ToMatch));
  GoldFailureTest(Code);
}

TEST(GoldDefaultArguments, OverloadConflict) {
  StringRef Code = R"(
foo(y:float32 = 1.2) : int!
  return y
foo(x:int = 4): int!
  return x
bar() :void!
  foo()
)";
  GoldFailureTest(Code);
}

TEST(GoldDefaultArguments, DefaultArgumentForMemberFunctions) {
  StringRef Code = R"(
Cls : type = class:
  foo(x:int = 4): int!
    return x

bar():void!
  V:Cls
  V.foo()
)";
  DeclarationMatcher ToMatch = parmVarDecl(hasName("x"), hasDefaultArgument());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}