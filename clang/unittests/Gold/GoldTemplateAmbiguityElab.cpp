//===- unittest/Gold/GoldTemplateAmbiguityElab.cpp ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Testing special instantiation situatutions.
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
  v.foo()
)";
  auto ToMatch = cxxMethodDecl(
    hasName("foo"),
    hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentNameWithDependentNestedNameSpecifierExpr) {
  StringRef Code = R"(
T2[T:type] = class (T.X):
  X : int
  foo() : void!
    Q : type = T2[T].(T.X)X

T3:type = class:
  X : type = int
T1 = class:
  X : type = T3

bar():void!
  v : T2[T1]
  v.foo()
)";
  auto ToMatch = cxxMethodDecl(
    hasName("foo"),
    hasDescendant(typeAliasDecl(hasType(asString("T3::X"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberExprWithNestedNameSpecifierNonTemplateClass) {
  StringRef Code = R"(
T2 = class (T1):
  X : int
  foo[T:type](): void!
    Q : type = T2.(T)X

T1 = class:
  X : type = int

bar():void!
  v : T2
  v.foo[T1]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(cxxMethodDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T1::X"))))))
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
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(
      hasName("Y"),
      hasType(asString("T2::T3<int>::x"))
    ))))
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
  Y : int = T.T3[1].x

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"),
      hasDescendant(arraySubscriptExpr())
    ))
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
  foo[T1]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"), hasDescendant(callExpr())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentFunctionCallExpression_WithParensOnId) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    foo()<static>:void!
      ;

foo[T:type]():void!
  (T.T2.foo)()

bar():void!
  foo[T1]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"), hasDescendant(callExpr())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentConstructorCall) {
  StringRef Code = R"(

foo[T:type]():void!
  x = T.T2.foo()

bar():void!
  foo[T1]()

T1 = class:
  T2 = class:
    foo = class:
      bar = class:
        ;

)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"), hasDescendant(cxxConstructExpr())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentConstructorCall_InAssignment) {
  StringRef Code = R"(

foo[T:type]():void!
  x :T.T2.foo
  x = T.T2.foo()

bar():void!
  foo[T1]()

T1 = class:
  T2 = class:
    foo = class:
      bar = class:
        ;

)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"), hasDescendant(cxxConstructExpr())
    ))
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
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("()")))
    ))
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
  x:T1
  foo(x)
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(declRefExpr())))
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
  v:T1
  foo(&v)
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(declRefExpr())))
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
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(typeAliasDecl(hasType(asString("T2::T3<int>::x"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentTempalateCallingAFunctionWithTemplateTemplateParam) {
  StringRef Code = R"(
T2 = class:
  T3[T:type] = class:
    x : type = T

foo2[T[x:type] : type]():void!
  Y : type = T[int].x

foo[T:type]():void!
  foo2[T.T3]()

bar():void!
  foo[T2]()
)";
  auto ToMatch = functionTemplateDecl(
    hasName("foo2"),
    has(functionDecl(hasName("foo2"), hasDescendant(typeAliasDecl(hasType(asString("T2::T3<int>::x"))))))
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
  x = T1[int]()
  x.foo()
)";
  auto ToMatch = cxxMethodDecl(
    hasName("foo"),
    hasDescendant(memberExpr(member(hasName("i")),
                  has(cxxThisExpr())))
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
  x = T1[int]()
  x.foo()
)";
  auto ToMatch = cxxMethodDecl(
    hasName("foo"),
    hasDescendant(memberExpr(member(hasName("i")),
                  has(cxxThisExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, MemberFunctionPtr) {
  StringRef Code = R"(
T1[T:type] = class:
  bar():void!
    x: T1[T].() -> void = &T1[T].foo
  foo():void!
    ;

bar():void!
  x = T1[int]()
  x.bar()
)";
  auto ToMatch = cxxMethodDecl(
      hasName("bar"),
      hasDescendant(varDecl())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberFunctionLookup) {
  StringRef Code = R"(
T1 = class:
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
    hasDescendant(cxxMemberCallExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      )),
      on(declRefExpr(to(varDecl(hasName("x")))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberFunctionTemplateLookup) {
  StringRef Code = R"(
T1 = class:
  bar(i:int):void!
    ;
  bar[T:type](f:^T):void!
    ;

foo[T:type]():void!
  x : T
  x.bar(1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxMemberCallExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      )),
      on(declRefExpr(to(varDecl(hasName("x")))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, MemberFunctionTemplateLookup_MultipleTemplateLookup) {
  StringRef Code = R"(
T1 = class:
  bar[T:type](i:T):void!
    ;

  bar[T:type](f:^T):void!
    ;

foo[T:type]():void!
  x : T
  x.bar(1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxMemberCallExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      )),
      on(declRefExpr(to(varDecl(hasName("x")))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, MemberFunctionTemplateLookup_ExplicitTemplateArgs) {
  StringRef Code = R"(
T1 = class:
  bar[T:type](i:T):void!
    ;

  bar[T:type](f:^T):void!
    ;

foo[T:type]():void!
  x : T
  x.bar[int](1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxMemberCallExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      )),
      on(declRefExpr(to(varDecl(hasName("x")))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, StaticMemberFunctionTemplateLookup_MultipleTemplateLookup) {
  StringRef Code = R"(
T1 = class:
  [static]
  bar[T:type](i:T):void!
    ;

  [static]
  bar[T:type](f:^T):void!
    ;

foo[T:type]():void!
  T.bar[int](1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(callExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, StaticMemberFunctionTemplateLookup_ExplicitTemplateArgs) {
  StringRef Code = R"(
T1 = class:
  [static]
  bar[T:type](i:T):void!
    ;

  [static]
  bar[T:type](f:^T):void!
    ;

foo[T:type]():void!
  T.bar[int](1)

bar():void!
  foo[T1]()

)";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(callExpr(
      callee(cxxMethodDecl(
        hasName("bar"),
        hasType(asString("void (int)"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, Error_MemberAccessToType) {
  StringRef Code = R"(
T2 = class:
  x:int

T1 = class:
  var : type = T2

foo[T:type](x:T):void!
  x.var

bar():void!
  foo[T1]()
)";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, DependentStaticArrayToFunctionCallOperator) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;

T1 = class:
  T2 = class:
    foo<static> <inline> : [3]Callable

foo[T:type]():void!
  T.T2.foo[1]()

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("()")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentTemplateToConstructorCall) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    foo [T:type]= class:
      ;

foo[T:type]():void!
  T.T2.foo[int]()

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(cxxConstructExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, DependentTemplateToConstructorCallWithParensAroundType) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    foo [T:type]= class:
      ;

foo[T:type]():void!
  (T.T2.foo[int])()

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(cxxConstructExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentMemberAccess_ToConstruct) {
  StringRef Code = R"Gold(

T3 = class:
  ;
T1 = class:
  T2[T:type] = class:
    x <static> <inline> : ^T = null

foo[T:type, U:type]():void!
  T.T2[U].x.construct()

bar():void!
  foo[T1, T3]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(cxxNewExpr(hasAnyPlacementArg(anything()))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentMemberAccess_ToDestruct) {
  StringRef Code = R"Gold(

T3 = class:
  ;
T1 = class:
  T2[T:type] = class:
    x <static> <inline> : ^T = null

foo[T:type, U:type]():void!
  T.T2[U].x.destruct()

bar():void!
  foo[T1, T3]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasName("foo"), hasDescendant(
      memberExpr(member(cxxDestructorDecl()))
    )))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, Error_ExpectedTemplateGivenArray) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;

T1 = class:
  T2 = class:
    foo<static> <inline> : [3]Callable

foo[T:type]():void!
  T.T2.foo[int]()

bar():void!
  foo[T1]()
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, Error_InstanciatedTypeIsNotATag) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;

T1 = class:
  T2 = class:
    foo : int

foo[T:type]():void!
  T.T2.foo.x

bar():void!
  foo[T1]()
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, Error_InstanciatedExpressionIsNotATemplateOrArray) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;

T1 = class:
  T2 = class:
    foo : int

foo[T:type]():void!
  T.T2.foo.x

bar():void!
  foo[T1]()
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, PassingTemplateTemplateParameterToDependentName) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;

T1 = class:
  T2[T:type] = class:
    ;
  T3[Container[T : type] : type] = class:
    T : type = Container[int]

foo[T:type]():void!
  x : T.T3[T.T2].T

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(varDecl(
        hasType(asString("T1::T3<T2>::T")))
      ))
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentMemberExpressionInsideOfNewType) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    ;

foo[T:type]():void!
  x = new [T.T2]()
  delete x
bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentExplicitOperatorCall) {
  StringRef Code = R"Gold(
T1 = class:
  operator"+"(Y:ref T1):T1!
    return T1()

foo[T:type](var:T):void!
  x : T1
  var.operator"+"(x)

bar():void!
  x : T1
  foo(x)
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxMemberCallExpr(
      callee(cxxMethodDecl(
        hasName("operator+")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, OperarandsAreDependentConstructorCalls) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    operator"+"(Y:ref const T2)<const>:bool!
      return false


foo[T:type](var:T):void!
  T.T2() + T.T2()

bar():void!
  x : T1
  foo(x)
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    hasDescendant(cxxOperatorCallExpr(
      hasOverloadedOperatorName("+")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, OperandsAreTypes) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    operator"+"(Y:ref T2):T2!
      return T2()

foo[T:type](var:T):void!
  x : T1
  T.T2 + T.T2

bar():void!
  x : T1
  foo(x)
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, PossibleConflictingOperatorOverloads) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    operator"^"():ref T2!
      return ^this
    operator"*"(other:T2):int32!
      return 1
    operator"^"(other:T2):bool!
      return false

foo[T:type](var : ^T.T2, var2 : ^T.T2):void!
  ^var

bar():void!
  x :^T1.T2
  y :^T1.T2
  foo[T1](x, y)
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(hasType(asString("void (struct T1::T2 *, struct T1::T2 *)"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, DependentVariableTemplateAccesExpr) {
  StringRef Code = R"Gold(
T1 = class:
  X[T:type] <constexpr> <static> : const T = T(4)
  X[int] <constexpr> <static> : const int = 4

foo[T:type]():void!
  x = T.X[int]

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = declRefExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, EnumPassedAsAType) {
  StringRef Code = R"Gold(
E1 = enum:
  x
  y
  z

foo[T:type]():void!
  x = T.z

bar():void!
  foo[E1]()
)Gold";
  auto ToMatch = declRefExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldTemplateAmbiguity, LookupUpEnumInsideClassScope) {
  StringRef Code = R"Gold(
T1 = class:
  E1 = enum:
    x
    y
    z

foo[T:type]():void!
  x = T.E1.x

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = declRefExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


// I need tests for const/ref/rref dependent type that doesn't evaluate to a type.
TEST(GoldTemplateAmbiguity, ConstDependentExpression) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    ;
foo[T:type]():void!
  x :type = const T.T2

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(typeAliasDecl(
        hasName("x"),
        hasType(asString("const struct T1::T2"))
        ))
      ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, RefDependentExpression) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    ;
foo[T:type]():void!
  x :type = ref T.T2

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(typeAliasDecl(
        hasName("x"),
        hasType(asString("struct T1::T2 &"))
        ))
      ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, RRefDependentExpression) {
  StringRef Code = R"Gold(
T1 = class:
  T2 = class:
    ;
foo[T:type]():void!
  x :type = rref T.T2

bar():void!
  foo[T1]()
)Gold";
  auto ToMatch = functionTemplateDecl(
    hasName("foo"),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(typeAliasDecl(
        hasName("x"),
        hasType(asString("struct T1::T2 &&"))
        ))
      ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldTemplateAmbiguity, PassingDependentTypeExpressionAsFunctionArgument) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    ;

s[T:type](x:T):void!
  ;
foo[T:type]():void!
  s(T.T2)

bar():void!
  foo[T1]()
)";
  GoldFailureTest(Code);
}

TEST(GoldTemplateAmbiguity, CyclicDecltypeFunction) {
  StringRef Code = R"(
foo():int!
  return 0

foo(x:decltype(foo())):void!
  ;
)";
  GoldFailureTest(Code);
}