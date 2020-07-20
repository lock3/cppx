//===- unittest/Gold/GoldVirtualFunctionElab.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file tests how we handle type evaluation.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldVritualAttr, LineAttr_VirtualOnMemberFunction) {
  StringRef Code = R"(
c : type = class:
  [virtual]
  foo():void!
    ;
  

)";
  DeclarationMatcher ClassC = recordDecl(hasName("c"),
    has(cxxMethodDecl(hasName("foo"), isVirtual()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(GoldVritualAttr, LineAttr_VirtualOnDestructor) {
  StringRef Code = R"(
c : type = class:
  [virtual]
  destructor():void!
    ;
  

)";
  DeclarationMatcher ClassC = recordDecl(hasName("c"),
    has(cxxDestructorDecl(isVirtual()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(GoldVritualAttr, VirtualOnConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor()<virtual>:void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldVritualAttr, DoubleVirtual) {
  StringRef Code = R"(
c : type = class:
  foo()<virtual><virtual>:void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldVritualAttr, VirtualOnStaticFunction) {
  StringRef Code = R"(
c : type = class:
  [static]
  [virtual]
  something():void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldVritualAttr, VirtualOnFreeFunction) {
  StringRef Code = R"(
[static]
something<virtual>():void!
  ;

)";
  GoldFailureTest(Code);
}