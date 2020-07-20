//=== GoldInlineAttribute.cpp ----------------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the inline attribute.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldInlineAttr, InlineFunction) {
  std::string Code = R"Gold(
foo(X:int)<inline>:int!
  return X + 4
)Gold";
  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"), isInline())
    )
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldInlineAttr, IsNotInlineFunction) {
  std::string Code = R"Gold(
foo(X:int):int!
  return X + 4
)Gold";
  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"), unless(isInline())))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldInlineAttr, IsImplicitlyInlineFunction) {
  std::string Code = R"Gold(
Cls : type = class:
  foo(X:int):int!
    return X + 4
  

)Gold";
  DeclarationMatcher ToMatch = recordDecl(
    hasName("Cls"),
    has(cxxMethodDecl(hasName("foo"), isInline()))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldInlineAttr, IsExplicitlyInlineFunction) {
  std::string Code = R"Gold(
Cls : type = class:
  foo(X:int)<inline>:int!
    return X + 4
  

)Gold";
  DeclarationMatcher ToMatch = recordDecl(
    hasName("Cls"),
    has(cxxMethodDecl(hasName("foo"), isInline()))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldInlineAttr, InlinedVariable) {
  std::string Code = R"Gold(
x<inline>:int = 5
)Gold";
  DeclarationMatcher ToMatch = varDecl(
    hasName("x"),isInlinedVar()
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldInlineAttr, InlinedNonStaticClass) {
  std::string Code = R"Gold(
Cls : type = class:
  x<inline>:int = 5
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldInlineAttr, InlinedStaticMember) {
  std::string Code = R"Gold(
Cls : type = class:
  x<static><inline>:float64 = 5.0

)Gold";
  DeclarationMatcher ToMatch = cxxRecordDecl(
    has(varDecl(
      hasName("x"),isInlinedVar()
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}
