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


TEST(GoldNestedNameDecl, NamespaceNameFunctionDefThroughNSAlias) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

X2:namespace = x
X2.foo():void!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(functionDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("X2"))))
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

TEST(GoldNestedNameDecl, Method_FromClassTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

c[T:type].foo() : T!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassTemplate_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

c[T:type].foo() : T!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassTemplateParameterNameSwitch) {
  StringRef Code = R"(
c[T:type, U:type] : type = class:
  x : T
  y : bool
  foo() : T

c[U:type, T:type].foo() : T!
  return x
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, Method_FromClassExplicitSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[int] : type = class {
  x : int
  y : bool
  foo() : int
}

c[int].foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassExplicitSpecialization_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[int] : type = class {
  x : int
  y : bool
  foo() : int
}

Q: type = c[int]

Q.foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassPartialSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[T:type][^T] : type = class:
  x : ^T
  y : bool
  foo() : T

c[T:type][^T].foo() : T!
  return ^x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, FunctionTemplate_DefOutsideOfClass) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo[T:type]() : int

c.foo[T:type]() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionTemplateDecl(has(cxxMethodDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ConstructorDefOutsideOfClass) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c.constructor()!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxConstructorDecl(unless(isDefinition())))
    )),
    has(cxxConstructorDecl(
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassPartialSpecialization_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[T:type][^T] : type = class:
  x : ^T
  y : bool
  foo() : T

Q[T:type] : type = c[^T]

Q[T:type].foo() : T!
  return ^x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}



// Special error cases
TEST(GoldNestedNameDecl, InvalidNamespaceWithTemplateParameters) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x[T:type].foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, ReturnedValueIsARefToAVariable) {
  StringRef Code = R"(
x = 4
x.foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidNamespaceWithSpecializtion) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x[int].foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, TypeCannotBeUsedAsANNS) {
  StringRef Code = R"(
int.something()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidTypeNotATemplate) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c[T:type].constructor()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidTypeNotATemplateSpecialization) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c[int].constructor()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, IncorrectSpecialziation) {
  StringRef Code = R"(
c[T:type] : type = class:
  constructor()
  y : bool

c[int].constructor()!
  ;
)";
  GoldFailureTest(Code);
}