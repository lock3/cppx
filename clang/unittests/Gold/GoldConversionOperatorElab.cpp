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

TEST(GoldConversionOperatorElab, InvalidConversionOperatorTwoTypesAsIdentifier) {
  StringRef Code = R"(
conversion'int':float32
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, ConversionOperatorUsedAsNameForAssignment) {
  StringRef Code = R"(
conversion'int' = 4
)";
  GoldFailureTest(Code);
}

// TODO: File this as a bug report for the crash.
// TEST(GoldConversionOperatorElab, ConversionOperatorUsedInsideOfEnum) {
//   StringRef Code = R"(
// x = enum:
//   conversion'int'
// )";
//   GoldFailureTest(Code);
// }

// TEST(GoldConversionOperatorElab, ConversionUsedAsFieldNameInClass) {
//   StringRef Code = R"(
// x = class:
//   conversion'int'
// )";
//   GoldFailureTest(Code);
// }

TEST(GoldConversionOperatorElab, ConversionOperatorAssignmentInsideFunction) {
  StringRef Code = R"(
foo() : void
  conversion'int' = 1
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, ConversionOperatorDeclInsideFunction) {
  StringRef Code = R"(
foo() : void
  conversion'int'():int
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, StaticConversionOperatorDecl) {
  StringRef Code = R"(
C = class:
  conversion'int'()<static>:int!
    return 3
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, InvalidConversionOperatorTwoTypesAsFunction) {
  StringRef Code = R"(
conversion'int'():float32
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

TEST(GoldConversionOperatorElab, ConversionOperatorWithParameter) {
  StringRef Code = R"(
C = class:
  conversion'int' (i:int)!
    return 5
)";
  GoldFailureTest(Code);
}


TEST(GoldConversionOperatorElab, ConversionOperatorArrayType) {
  StringRef Code = R"(
C = class:
  conversion'[3]int' ()!
    return 5
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, ConversionOperatorFunctionType) {
  StringRef Code = R"(
C = class:
  conversion'int=>int' ()!
    return 5
)";
  GoldFailureTest(Code);
}

TEST(GoldConversionOperatorElab, ConversionOperatorArrayTypeThroughTypedef) {
  StringRef Code = R"(
C = class:
  x : type = [3] int
  conversion'x' ()!
    return 5
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
    cxxConversionDecl(hasType(asString("int (void)")))
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
    cxxConversionDecl(hasType(asString("int &(void)")))
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
    cxxConversionDecl(hasType(asString("struct X (void)")))
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
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(hasDescendant(
      cxxConversionDecl(hasType(asString("struct X (void)")))
    ))),
    has( cxxConversionDecl(hasType(asString("struct X (void)"))))
  );
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

foo() : void!
  y : C = C()
  s : X = y
)";
  // TODO: Figure out what an explicit conversion operator looks like in the AST.
  auto Matcher = translationUnitDecl(has(
    cxxRecordDecl(hasDescendant(
      cxxConversionDecl(hasType(asString("struct X (void)")))
    ))),
    has( cxxConversionDecl(hasType(asString("struct X (void)"))))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldConversionOperatorElab, Decl_ConversionOperatorTemplate) {
  StringRef Code = R"(
X = class:
  conversion'T'[T:type]()!
    return 4

)";
  auto Matcher = cxxRecordDecl(hasDescendant(
    cxxConversionDecl(hasType(asString("type-parameter-0-0 (void)")))
  ));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}