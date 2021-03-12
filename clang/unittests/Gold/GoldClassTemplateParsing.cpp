//=== GoldClassTempalteParsing.cpp - Elaboration for Gold Nodes ------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassTemplate, ClassTemplateDeclaration_SingleParameter_NoTemplateParameterUse) {
  StringRef Code = R"(
c[T:type] : type = class:
  z : int
  y : bool
)";
  DeclarationMatcher ClassC = classTemplateDecl(
    hasName("c"),
    has(templateTypeParmDecl(
      hasName("T")
    )),
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      has(fieldDecl(
        hasName("z"), hasType(asString("int"))
      )),
      has(fieldDecl(
        hasName("y"), hasType(asString("_Bool"))
      ))
    ))
  );
  
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(ClassTemplate, ClassTemplateDeclaration_SingleParameter_TemplateParamUsedInDef) {
  StringRef Code = R"(
c[T:type] : type = class:
  z : T
  y : bool
)";
  DeclarationMatcher ClassC = classTemplateDecl(
    hasName("c"),
    has(templateTypeParmDecl(
      hasName("T")
    )),
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      has(fieldDecl(
        hasName("z"), hasType(asString("type-parameter-0-0"))
      )),
      has(fieldDecl(
        hasName("y"), hasType(asString("_Bool"))
      ))
    ))
  );
  
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(ClassTemplate, ClassTemplateDeclaration_Instantiation) {
  StringRef Code = R"(
c[T:type] : type = class:
  z : T
  y : bool = 0

main() : int!
  q : c[int]
  return 0
)";
  DeclarationMatcher ClassCTemplate = classTemplateDecl(
    hasName("c"),
    has(templateTypeParmDecl(
      hasName("T")
    )),
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      has(fieldDecl(
        hasName("z"), hasType(asString("type-parameter-0-0"))
      )),
      has(fieldDecl(
        hasName("y"), hasType(asString("_Bool"))
      ))
    ))
  );
  
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("c<int>")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(ClassCTemplate),
    hasDescendant(MainFnMatcher)
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}


TEST(ClassTemplate, ClassTemplateDeclaration_NonTypeParameter) {
  StringRef Code = R"(
c[T:int] : type = class:
  z : int = T
  y : bool = 0

main() : int!
  q : c[3]
  return 0
)";
  DeclarationMatcher ClassCTemplate = classTemplateDecl(
    hasName("c"),
    has(nonTypeTemplateParmDecl(
      hasName("T")
    )),
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      has(fieldDecl(
        hasName("z"), hasType(asString("int"))
      )),
      has(fieldDecl(
        hasName("y"), hasType(asString("_Bool"))
      ))
    ))
  );
  
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("c<3>")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher TemplateAndInstantiationMatch = translationUnitDecl(
    hasDescendant(ClassCTemplate),
    hasDescendant(MainFnMatcher)
  );

  ASSERT_TRUE(matches(Code.str(), TemplateAndInstantiationMatch));
}