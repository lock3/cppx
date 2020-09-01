//=== GoldNestedNameSpecifierOnDeclElab.cpp --------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of declarations with nested name
//  specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldCompileRun.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldNestedNameDecl, NamespaceNameFunctionDef) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x.foo():void!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(functionDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImproperFunctionLookup) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x.foo():void!
  ;
test():void!
  foo()
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, NamespaceNameClassDef) {
  StringRef Code = R"(
x : namespace = namespace:
  Ty : type = class

x.Ty : type = class:
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(cxxRecordDecl(hasName("Ty"), unless(isDefinition())))
    )),
    has(cxxRecordDecl(hasName("Ty"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, NamespaceExternVariableWithLaterDecl) {
  StringRef Code = R"(
x : namespace = namespace:
  Var<extern>:int

x.Var:int = 5

)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(varDecl(hasName("Var"), unless(isDefinition())))
    )),
    has(varDecl(hasName("Var"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, FunctionDeclWithinClassDefOutside) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo() : int

c.foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      has(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}