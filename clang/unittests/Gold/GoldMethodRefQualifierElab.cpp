//=== GoldMethodRefQualifierElab.cpp -------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing to make sure that member function reference qualifiers work.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldMethodRefQualifier, Ref_Qualifier) {
  StringRef Code = R"(
x : type = class:
  x()<ref> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRefQualifier()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldMethodRefQualifier, Ref_QualifierWithConstSecond) {
  StringRef Code = R"(
x : type = class:
  x()<ref><const> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRefQualifier(), isConst()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldMethodRefQualifier, Ref_QualifierWithConstFirst) {
  StringRef Code = R"(
x : type = class:
  x()<const><ref> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRefQualifier(), isConst()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldMethodRefQualifier, RRef_Qualifier) {
  StringRef Code = R"(
x : type = class:
  x()<rref> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRRefQualifier()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}


TEST(GoldMethodRefQualifier, RRef_QualifierWithConstSecond) {
  StringRef Code = R"(
x : type = class:
  x()<rref><const> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRRefQualifier(), isConst()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldMethodRefQualifier, RRef_QualifierWithConstFirst) {
  StringRef Code = R"(
x : type = class:
  x()<const><rref> : int
)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("x"), methodHasRRefQualifier(), isConst()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}


TEST(GoldMethodRefQualifier, Ref_QualifierOnInvalidType) {
  StringRef Code = R"(
x : type = class:
  x<ref> : int
)";
  GoldFailureTest(Code);
}

TEST(GoldMethodRefQualifier, Ref_UsedAsACall) {
  StringRef Code = R"(
x : type = class:
  x()<ref()> : int
)";
  GoldFailureTest(Code);
}

TEST(GoldMethodRefQualifier, ConflictingQualifier) {
  StringRef Code = R"(
x : type = class:
  foo()<ref><rref> : int!
    return 4
)";
  GoldFailureTest(Code);
}

TEST(GoldMethodRefQualifier, Ref_NonMethodFunctionRefQualifier) {
  StringRef Code = R"(
foo()<ref> : int !
  return 4
)";
  GoldFailureTest(Code);
}