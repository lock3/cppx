//===- unittest/Gold/GoldFinalAttributeElab.cpp ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file tests the final attribute applied to both classes and functions.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFinalAttr, FinalClass) {
  StringRef Code = R"(
c <final>: type = class:
  ;
)";
  DeclarationMatcher ClassC = cxxRecordDecl(hasName("c"), isFinal() );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(GoldFinalAttr, InheritingFromAFinalClass) {
  StringRef Code = R"(
C1<final> : type = class:
  ;

C2 :type = class(C1):
  ;

)";
  GoldFailureTest(Code);
}

TEST(GoldFinalAttr, FinalMethod) {
  StringRef Code = R"(
C1 : type = class:
  foo()<final>:void!
    ;
  

)";
  GoldFailureTest(Code);
}

TEST(GoldFinalAttr, DoubleFinal) {
  StringRef Code = R"(
C1<final><final> : type = class:
  ;
)";
  GoldFailureTest(Code);
}