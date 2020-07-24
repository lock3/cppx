//=== GoldThreadLocalAttributeElab.cpp ------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the thread_local attribute.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Goldthread_localAttribute, VarDecl) {
  StringRef Code = R"(
x<thread_local>:int
)";
  DeclarationMatcher ToMatch = varDecl(hasName("x"),
                                       hasThreadStorageDuration());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(Goldthread_localAttribute, StaticClassMember) {
  StringRef Code = R"(
Cls : type = class:
  x<static><thread_local>:int
)";
  DeclarationMatcher ToMatch = varDecl(hasName("x"),
                                       hasThreadStorageDuration());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(Goldthread_localAttribute, WithinAFunctionDef) {
  StringRef Code = R"(
foo() : void!
  x<static><thread_local>:int
)";
  DeclarationMatcher ToMatch = varDecl(hasThreadStorageDuration());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(Goldthread_localAttribute, AttributeOnAClass) {
  StringRef Code = R"(
Cls<thread_local> : type = class:
  ;
)";
  GoldFailureTest(Code);
}