//=== GoldVariadicTemplateParameterElab.cpp =-------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Gold variadic template parameter declaration testing.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldVariadicTemplateParam, Class_TypeParameterPack) {
  StringRef Code = R"(
x[T:type...] = class:
  ;
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack()))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, Class_NonTypeTemplateParamPack) {
  StringRef Code = R"(
x[T:int...] = class:
  ;
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(nonTypeTemplateParmDecl(hasName("T"), isParameterPack()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, Class_TemplateTemplateParmPack) {
  StringRef Code = R"(
x[T[Z : type] : type...] = class:
  ;
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(templateTemplateParmDecl(hasName("T"), isParameterPack()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, Function) {
  StringRef Code = R"(
x[T:type...](Parm:rref T...) : void!
  ;
)";
  auto ToMatch = functionTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack())),
    has(functionDecl(
      hasDescendant(parmVarDecl(
        hasName("Parm")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, ExpansionExpression) {
  StringRef Code = R"(
foo[T:type, U:type](A:rref T, B:rref U) : void!
  ;

x[T:type...](Parm:rref T...) : void!
  foo(Parm...)
)";
  auto ToMatch = functionTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack())),
    has(functionDecl(
      hasDescendant(parmVarDecl(
        hasName("Parm")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, ExpressionExpansion) {
  StringRef Code = R"(
bar[T:type](A:rref T) : void!
  ;

x[T:type...](Parm:rref T...) : void!
  bar(Parm)...
)";
  auto ToMatch = functionTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack())),
    has(functionDecl(
      hasDescendant(parmVarDecl(
        hasName("Parm")
      )),
      hasDescendant(packExpansionExpr())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariadicTemplateParam, InvalidUnexpandibleType_InExpr) {
  StringRef Code = R"(
bar[T:type](A:rref T) : void!
  ;

x[T:type](Parm:rref T) : void!
  bar(Parm)...
)";
  GoldFailureTest(Code);
}

TEST(GoldVariadicTemplateParam, InvalidUnexpandibleType_OnParameter) {
  StringRef Code = R"(
bar[T:type](A:rref T) : void!
  ;

x[T:type](Parm:rref T) : void!
  bar(Parm...)
)";
  GoldFailureTest(Code);
}

TEST(GoldVariadicTemplateParam, RecursiveExpansion) {
  StringRef Code = R"(
adder[T : type](v : T) : T!
  return v

adder[T : type, Args:type...](first : T, a : Args...) : T!
  return first + adder(a...)

main() : int! {
  test = adder(1, 2, 3)
}
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Test));
}
