//=== GoldAlignAsAttributeElab.cpp ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the alignas attribute.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldAlignasAttribute, VarDecl) {
  StringRef Code = R"(
x<alignas(8)>:int
)";
  DeclarationMatcher ToMatch = varDecl(hasName("x"), isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignasAttribute, ClassMember) {
  StringRef Code = R"(
Cls : type = class:
  x<alignas(32)>:int
)";
  DeclarationMatcher ToMatch = fieldDecl(hasName("x"), isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignasAttribute, ClassWithAlignment) {
  StringRef Code = R"(
Cls <alignas(32)>: type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls"), isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignasAttribute, VerifyUnaligned) {
  StringRef Code = R"(
Cls1 : type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls1"),
                                             unless(isAligned()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignasAttribute, ClassAlignedUsingAnotherClass) {
  StringRef Code = R"(
Cls1 <alignas(8)> :type = class:
  ;
Cls <alignas(Cls1)>: type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls"), isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}