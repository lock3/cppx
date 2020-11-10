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

TEST(GoldAlignAsAttribute, VarDecl) {
  StringRef Code = R"(
x<alignas(128)>:int
)";
  DeclarationMatcher ToMatch = varDecl(hasName("x"), valueDeclAlignedTo(32));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignAsAttribute, ClassMember) {
  StringRef Code = R"(
Cls : type = class:
  x<alignas(32)>:int
)";
  DeclarationMatcher ToMatch = fieldDecl(hasName("x"), valueDeclAlignedTo(32));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignAsAttribute, ClassWithAlignment) {
  StringRef Code = R"(
Cls <alignas(32)>: type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls"),
                                             typeDeclAlignedTo(256));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignAsAttribute, VerifyUnaligned) {
  StringRef Code = R"(
Cls1 : type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls1"),
                                             unless(isAligned()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldAlignAsAttribute, ClassAlignedUsingAnotherClass) {
  StringRef Code = R"(
Cls1 <alignas(8)> :type = class:
  ;
Cls <alignas(Cls1)>: type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls"),
                                             isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignAsAttribute, AlignAsOnVariableOfAnUnaligedClass) {
  StringRef Code = R"(
Cls :type = class:
  ;
main() : int!
  X <alignas(8)>:Cls
)";
  DeclarationMatcher ToMatch = varDecl(hasName("X"),
                                       valueDeclAlignedTo(8));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldAlignAsAttribute, AlignedBitfield) {
  StringRef Code = R"(
Cls : type = class:
  x<alignas(32)><bits(2)>:int
)";
  GoldFailureTest(Code);
}

TEST(GoldAlignAsAttribute, BitfieldAligned) {
  StringRef Code = R"(
Cls : type = class:
  x<bits(2)><alignas(32)>:int
)";
  GoldFailureTest(Code);
}

TEST(GoldAlignAsAttribute, InvalidAlignment) {
  StringRef Code = R"(
Cls : type = class:
  x<alignas(1)>:int
)";
  GoldFailureTest(Code);
}

TEST(GoldAlignAsAttribute, AlignAsParameterPack) {
  StringRef Code = R"(
Cls[T:type...]<alignas(T...)> : type = class:
  ;
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("Cls"),
                                             isAligned());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}