//=== GoldConversionOperatorElab.cpp ---------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of the conversion operator
//  testing
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldCompileRun.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConversionOperatorElab, InvalidConversionOperatorTwoTypes) {
  StringRef Code = R"(
conversion'int':int
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, Decl_InvalidConversionOperatorDefinedOutsideOfClassNoNNS) {
  StringRef Code = R"(
conversion'int' ()!
  ;
)";
  GoldFailureTest(Code);
}


TEST(GoldConversionOperatorElab, Decl_InvalidConversionOperatorDeclaredOutsideOfClassNoNNS) {
  StringRef Code = R"(
conversion'int' ()
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, Decl_ConversionOperatorToInteger) {
  StringRef Code = R"(
C = class:
  conversion'int' ()!
    return 5
)";
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("int")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldConversionOperatorElab, Decl_ConversionOperatorToReferenceType) {
  StringRef Code = R"(
C = class:
  i:int
  conversion'ref int' ()!
    return i
)";
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("int &")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldConversionOperatorElab, Decl_ConversionToUDT) {
  StringRef Code = R"(
X = class:
  ;

C = class:
  conversion'X' ()!
    return X()

)";
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("struct X")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldConversionOperatorElab, Decl_ConversionDefinedOutsideOfClass) {
  StringRef Code = R"(
X = class:
  ;

C = class:
  conversion'X' ()

C.conversion'X' ()!
  return X()

)";
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("struct X")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldConversionOperatorElab, Use_ConversionDefinedOutsideOfClass) {
  StringRef Code = R"(
X = class:
  ;

C = class:
  conversion'X' ()

C.conversion'X' ()!
  return X()

x.foo():void!
  ;

foo() : void!
  y : C = C()
  s : X = y
)";
  // TODO: Figure out what an explicit conversion operator looks like in the AST.
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("struct X")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}