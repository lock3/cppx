//===- unittest/Gold/GoldOverrideAttributeElab.cpp ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Tests for the override attribute. This attribute may only be applied to
//  an overridden virtual function.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldOverrideAttr, OverrideAVirtualFunc) {
  StringRef Code = R"(
Base : type = class:
  foo()<virtual>: void!
    ;
  destructor()<virtual> : void!
    ;
  
Derived : type = class(Base):
  foo()<override>:void!
    ;
  

)";
  DeclarationMatcher OvrrideMatches = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("Base"),
      has(cxxMethodDecl(hasName("foo"), isVirtual())),
      has(cxxDestructorDecl(isVirtual()))
    )),
    has(cxxRecordDecl(
      hasName("Derived"),
      has(cxxMethodDecl(hasName("foo"), isOverride()))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), OvrrideMatches));
}

TEST(GoldOverrideAttr, OverrideANonVirtualFunc) {
  StringRef Code = R"(
Base : type = class:
  foo(): void!
    ;
  destructor()<virtual> : void!
    ;
  
Derived : type = class:
  foo()<override>:void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldOverrideAttr, OverrideOnAConstructor) {
  StringRef Code = R"(
Base : type = class:
  constructor()<override>: void!
    ;

)";
  GoldFailureTest(Code);
}

TEST(GoldOverrideAttr, OverrideOnADestructor) {
  StringRef Code = R"(
Base : type = class:
  destructor()<override>: void!
    ;
  
)";
  GoldFailureTest(Code);
}

TEST(GoldOverrideAttr, OverrideOnAField) {
  StringRef Code = R"(
Base : type = class:
  x<override>:int = 5
  
)";
  GoldFailureTest(Code);
}

TEST(GoldOverrideAttr, DuplicateOverride) {
  StringRef Code = R"(
Base : type = class:
  foo()<virtual>: void!
    ;
  destructor()<virtual> : void!
    ;
  
Derived : type = class:
  foo()<override><override>:void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldOverrideAttr, OverrideOnAStaticMemberFunction) {
  StringRef Code = R"(
Base : type = class:
  foo()<static>: void!
    ;
  destructor()<virtual> : void!
    ;
  
Derived : type = class:
  foo()<override>:void!
    ;
  

)";
  GoldFailureTest(Code);
}


TEST(GoldOverrideAttr, OverrideOnUsedOnAStaticFunction) {
  StringRef Code = R"(
Base : type = class:
  foo()<virtual>: void!
    ;
  destructor()<virtual> : void!
    ;
  
Derived : type = class:
  foo()<static><override>:void!
    ;
  

)";
  GoldFailureTest(Code);
}
