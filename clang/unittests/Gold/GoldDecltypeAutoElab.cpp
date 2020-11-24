//===- unittest/Gold/GoldDecltypeAutoElab.cpp -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Errors associated with declarator elaboration.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldDecltypeAuto, FunctionReturnType) {
  StringRef Code = R"(
foo() : decltype(auto)!
  return 3

foo2(x:ref int) : decltype(auto)!
  return x
)";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("foo"), hasType(asString("int (void)")))),
    has(functionDecl(hasName("foo2"), hasType(asString("int &(int &)"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}