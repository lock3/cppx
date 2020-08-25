//=== ClassTempalteParsing.cpp - Elaboration for Gold Nodes ----------------==//
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

/*
File 0x7fffdaed0e98
`-Call 0x7fffdaed0e78
  |-Atom 0x7fffdaed0e38 operator'='
  `-List 0x7fffdaed0e60
    |-Call 0x7fffdaed0cb0
    | |-Atom 0x7fffdaed0c70 operator':'
    | `-List 0x7fffdaed0c98
    |   |-Elem 0x7fffdaed0c40
    |   | |-Atom 0x7fffdaed0b80 c
    |   | `-List 0x7fffdaed0c28
    |   |   `-Call 0x7fffdaed0c08
    |   |     |-Atom 0x7fffdaed0bc8 operator':'
    |   |     `-List 0x7fffdaed0bf0
    |   |       |-Atom 0x7fffdaed0b98 x
    |   |       `-Literal 0x7fffdaed0bb0 type
    |   `-Literal 0x7fffdaed0c58 type
    `-Macro 0x7fffdaed0e18
      |-Atom 0x7fffdaed0cc8 class
      |-Array 0x7fffdaed0e00
      | |-Call 0x7fffdaed0d50
      | | |-Atom 0x7fffdaed0d10 operator':'
      | | `-List 0x7fffdaed0d38
      | |   |-Atom 0x7fffdaed0ce0 z
      | |   `-Literal 0x7fffdaed0cf8 int
      | `-Call 0x7fffdaed0dd8
      |   |-Atom 0x7fffdaed0d98 operator':'
      |   `-List 0x7fffdaed0dc0
      |     |-Atom 0x7fffdaed0d68 y
      |     `-Literal 0x7fffdaed0d80 bool
      `-<<<NULL>>>



Single parameter AST:
Elem 0x7fffe7441c60
|-Atom 0x7fffe7441ba0 c
`-List 0x7fffe7441c48
  `-Call 0x7fffe7441c28
    |-Atom 0x7fffe7441be8 operator':'
    `-List 0x7fffe7441c10
      |-Atom 0x7fffe7441bb8 x
      `-Literal 0x7fffe7441bd0 type


Multiple parameter AST:
Elem 0x7fffbe56ecf0
|-Atom 0x7fffbe56eba0 c
`-List 0x7fffbe56ecd8
  |-Call 0x7fffbe56ec28
  | |-Atom 0x7fffbe56ebe8 operator':'
  | `-List 0x7fffbe56ec10
  |   |-Atom 0x7fffbe56ebb8 x
  |   `-Literal 0x7fffbe56ebd0 type
  `-Call 0x7fffbe56ecb0
    |-Atom 0x7fffbe56ec70 operator':'
    `-List 0x7fffbe56ec98
      |-Atom 0x7fffbe56ec40 q
      `-Literal 0x7fffbe56ec58 type

*/
