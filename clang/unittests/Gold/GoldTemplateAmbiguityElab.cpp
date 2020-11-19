//===- unittest/Gold/GoldTemplateAmbiguityElab.cpp ------------------------===//
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

TEST(GoldTemplateAmbiguity, DependentDotIsNestedName) {
  StringRef Code = R"(
foo[T:type]():void!
  Y : type = T.X
  var : Y

T1 = class:
  X : type = int

bar():void!
  foo[T1]()
)";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("foo"), hasType(asString("int (void)")))),
    has(functionDecl(hasName("foo2"), hasType(asString("int &(int &)"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentDotIsNestedName_NameIsMember) {
  StringRef Code = R"(
foo[T:type]():void!
  Y : int= T.X

T1 = class:
  X <static>: const int = 4

bar():void!
  foo[T1]()
)";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("foo"), hasType(asString("int (void)")))),
    has(functionDecl(hasName("foo2"), hasType(asString("int &(int &)"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}