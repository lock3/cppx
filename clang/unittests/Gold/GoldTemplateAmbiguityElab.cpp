//===- unittest/Gold/GoldTemplateAmbiguityElab.cpp ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Errors associated with declarator elaboration.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldTemplateAmbiguity, DependentDotIsNestedName) {
  StringRef Code = R"(
foo[T:type]():void!
  Y : type = T.X
  var : Y

T1 = class:
  X : type = int

bar():void!
  foo[T1]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentDotIsNestedName_NameIsMember) {
  StringRef Code = R"(
foo[T:type]():void!
  Y : int = T.X

T1 = class:
  X <static>: const int = 4

bar():void!
  foo[T1]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(declRefExpr())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentDotIsNestedName_MismatchedTypes) {
  StringRef Code = R"(
foo[T:type]():void!
  Y : T.X

T1 = class:
  X <static>: const int = 4

bar():void!
  foo[T1]()
)";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, MemberExprWithNestedNameSpecifier) {
  StringRef Code = R"(
T2[T:type] = class (T):
  X : int
  foo() : void!
    Q : type = T2[T].(T)X

T1 = class:
  X : type = int

bar():void!
  v : T2[T1]
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberExprWithNestedNameSpecifierNonTemplateClasss) {
  StringRef Code = R"(
T2 = class (T1):
  X : int
  foo()[T:type]: void!
    Q : type = T2.(T)X

T1 = class:
  X : type = int

bar():void!
  v : T2[T1]
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentTemplateNameExpression) {
  StringRef Code = R"(
T2 = class:
  T3[T:type] = class:
    x : type = T

foo[T:type]():void!
  Y : type = T.T3[int].x

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentStaticArrayExpression) {
  StringRef Code = R"(
T1 = class:
  x:int
T2 = class:
  T3 <static>: [2]T1

foo[T:type]():void!
  Y : int = T.T3[2].x

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, DependentFunctionCallExpression) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    foo()<static>:void!
      ;

foo[T:type]():void!
  T.T2.foo()

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentConstructorCall) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    foo = class:
      ;

foo[T:type]():void!
  x = T.T2.foo()

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, StaticMemberThatSupportsFunctionCallOperator) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;
T1 = class:
  T2 = class:
    foo<static> <inline> : Callable

foo[T:type]():void!
  T.T2.foo()

bar():void!
  foo[T2]()
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MultiplyNestedMemberAccess) {
  StringRef Code = R"Gold(
T3 = class:
  i : int
T2 = class:
  t3 : T3
T1 = class:
  t2 : T2

foo[T:type](v:T):void!
  v.t2.t3

bar():void!
  foo[T2]()
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, MultiplyNestedMemberAccess_AllArePointers) {
  StringRef Code = R"Gold(
T3 = class:
  i : ^int
T2 = class:
  t3 : ^T3
T1 = class:
  t2 : ^T2

foo[T:type](v:^T):void!
  v.t2.t3

bar():void!
  foo[T2]()
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, DependentTemplateWithDependentArguments) {
  StringRef Code = R"(
T2 = class:
  T3[T:type] = class:
    x : type = T
T4 = class:
  x : type = int

foo[T:type, U:type]():void!
  Y : type = T.T3[U.x].x

bar():void!
  foo[T2, T4]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentTempalateCallingAFunctionWithTemplateTemplateParam) {
  StringRef Code = R"(
T2 = class:
  T3[T:type] = class:
    x : type = T

foo2[T:[:type]=>type]:void!
  Y : type = T[int].x

foo[T:type]():void!
  foo2[T.T3]

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, ImplicitMemberAccess) {
  StringRef Code = R"(
T1[T:type] = class:
  x : type = T
  i : int
  foo():int!
    return i

bar():void!
  x = T1[int]
  x.foo()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, ExplicitMemberAccess) {
  StringRef Code = R"(
T1[T:type] = class:
  x : type = T
  i : int
  foo():int!
    return this.i

bar():void!
  x = T1[int]
  x.foo()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, MemberFunctionPtr) {
  StringRef Code = R"(
T1[T:type] = class:
  bar():void!
    x = &T1[T].foo
  foo():void!
    ;

bar():void!
  x = T1[int]
  x.bar()
)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberFunctionLookup) {
  StringRef Code = R"(
T1[T:type] = class:
  bar(i:int):void!
    ;
  bar(f:float64):void!
    ;

foo[T:type]():void!
  x : T
  x.bar(1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}