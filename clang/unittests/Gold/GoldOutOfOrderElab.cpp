//===- unittest/Gold/GoldOutOfOrderElab.cpp -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Testing commom places where I've create/located bugs for out of order
//  elaboration.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldOutOfOrder, CyclicalDependantVariableDecl) {
  StringRef Code = R"(
b = a
a = b
)";
  GoldFailureTest(Code);
}

TEST(GoldOutOfOrder, CyclicalDependantVariableDeclWithNoDependentType) {
  StringRef Code = R"(
b = a
a : int = b
)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("a"),
      hasType(asString("int"))
    )),
    has(varDecl(
      hasName("b"),
      hasType(asString("int"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, MemberCorssReferenceThroughPtrs) {
  StringRef Code = R"(
T1 = class:
  x :^T2

T2 = class:
  x :^T1
)";
  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl( hasName("T1"),
      hasDescendant(fieldDecl(
        hasName("x"),
        hasType(asString("struct T2 *"))
      ))
    )),
    has(cxxRecordDecl( hasName("T2"),
      hasDescendant(fieldDecl(
        hasName("x"),
        hasType(asString("struct T1 *"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, NonPtrMembers) {
  StringRef Code = R"(
T1 = class:
  x :T2

T2 = class:
  x :T1
)";
  GoldFailureTest(Code);
}

TEST(GoldOutOfOrder, InvalidMemberReferencing) {
  StringRef Code = R"(
T1 = class:
  x :T2

T2 = class:
  x :T1
)";
  GoldFailureTest(Code);
}


TEST(GoldOutOfOrder, UsingCompletedTypeBetweenMemberFunctions) {
  StringRef Code = R"(
T1 = class:
  foo():void!
    x : T2

T2 = class:
  foo():void!
    x : T1
)";
  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl( hasName("T1"),
      hasDescendant(cxxMethodDecl(
        hasName("foo")
      ))
    )),
    has(cxxRecordDecl( hasName("T2"),
      hasDescendant(cxxMethodDecl(
        hasName("foo")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, BinaryOpratorNotElaboratedYet_NonMember) {
  StringRef Code = R"(
V = T1() + T1()

T1 = class:
  foo():void!
    ;

operator"+"(x:ref const T1, y:ref const T1):bool!
  return false
)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("V"),
      hasInitializer(hasDescendant(
        cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, UnaryOpratorNotElaboratedYet_NonMember) {
  StringRef Code = R"(
V = + (T1())

T1 = class:
  foo():void!
    ;

operator"+"(y:ref const T1):bool!
  return false
)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("V"),
      hasInitializer(hasDescendant(
        cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldOutOfOrder, BinaryOpratorNotElaboratedYet_Member) {
  StringRef Code = R"(
V = T1() + T1()

T1 = class:
  foo():void!
    ;

  operator"+"(x:ref const T1):bool!
    return false
)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("V"),
      hasInitializer(hasDescendant(
        cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, UnaryOpratorNotElaboratedYet_Member) {
  StringRef Code = R"(
V = + (T1())

T1 = class:
  foo():void!
    ;
  operator"+"():bool!
    return false

)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("V"),
      hasInitializer(hasDescendant(
        cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldOutOfOrder, UnaryOperatorDeclaredWithinAnotherNamespaceDeclaration) {
  StringRef Code = R"(
x = namespace:
  T1 = class:
    ;

V = +x.T1()

x = namespace:
  operator"+"(y:ref const T1):bool!
    return false


)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
      hasName("V"),
      hasInitializer(hasDescendant(
        cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, CrossTypeBoundryAliasReference) {
  StringRef Code = R"(
T1 = class:
  X : type = int
  v:T2.Y

T2 = class:
  Y : type = int
  v:T1.X
)";
  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl( hasName("T1"),
      hasDescendant(fieldDecl(
        hasName("v"), hasType(asString("T2::Y"))
      ))
    )),
    has(cxxRecordDecl( hasName("T2"),
      hasDescendant(fieldDecl(
        hasName("v"), hasType(asString("T1::X"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, FunctionOverloadCycleInsideNamespaceDecl) {
  StringRef Code = R"(
V = N.foo()
N.foo():int!
  ;
N.foo(p1:int32):void!
  ;
N.foo(p1:int64):void!
  ;

N = namespace:
  foo():int
  foo(p1:int32):void
  foo(p1:int64):void
)";
  auto ToMatch = translationUnitDecl(
    has(varDecl(
        hasName("V"), hasType(asString("int"))
      ))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldOutOfOrder, MemberDefBeforClassDef) {
  StringRef Code = R"(

N.foo():int!
  ;

N.foo(p1:int32):void!
  ;

N.foo(p1:int64):void!
  ;

N = class:
  foo():int
  foo(p1:int32):void
  foo(p1:int64):void
)";
  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl( hasName("N"),
      hasDescendant(cxxMethodDecl(
        hasName("foo"), hasType(asString("int (void)"))
      )),
      hasDescendant(cxxMethodDecl(
        hasName("foo"), hasType(asString("void (int)"))
      )),
      hasDescendant(cxxMethodDecl(
        hasName("foo"), hasType(asString("void (long)"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldOutOfOrder, Constexpr_cyclicDependency) {
  StringRef Code = R"(
a<constexpr>:float64 = b;
b<constexpr>:float64 = a;
)";
  GoldFailureTest(Code);
}

TEST(GoldOutOfOrder, ConstexprAuto_cyclicDependency) {
  StringRef Code = R"(
a<constexpr> = b;
b<constexpr> = a;
)";
  GoldFailureTest(Code);
}

TEST(GoldOutOfOrder, Constexpr_Constructor_Type_DeclDefBeforeUse) {
  StringRef Code = R"(
T1 = class:
  x :int
  y : float64
  constructor()<constexpr>:void


T1.constructor()<constexpr>:void!
  x = 5
  y = 57.0

X<constexpr>:int = T1().x

T2 = class:
  ;
y : [X]T2
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("struct T2 [5]")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, Constexpr_FunctionDeclBeforeDefAfter) {
  StringRef Code = R"(
foo()<constexpr>: int
b <constexpr> :int = foo()

foo()<constexpr>:int!
  return 4

)";
  auto ToMatch = varDecl(hasName("b"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, Constexpr_FuncInNamespaceDeclDefOutside) {
  StringRef Code = R"(
x = namespace:
  foo()<constexpr>: int
b <constexpr> :int = x.foo()

foo(x:int64) :void!
  ;
foo(x:float32) :void!
  ;

foo(x:int) :void!
  ;
x.foo()<constexpr>:int!
  return 4

)";
  auto ToMatch = varDecl(hasName("b"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, Constexpr_DeclBefore_DefAfter) {
  StringRef Code = R"(
T1 = class:
  x :int
  y : float64
  constructor()<constexpr>:void


X<constexpr>:int = T1().x

T1.constructor()<constexpr>:void!
  x = 5
  y = 57.0

T2 = class:
  ;
y : [X]T2
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("struct T2 [5]")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldOutOfOrder, Constexpr_ConstructorForcedEvaluation_ThroughTemplate) {
  StringRef Code = R"(
T1 = class:
  x :int
  y : float64
  constructor()<constexpr>!
    x = 5
    y = 57.0

X<constexpr>:int = T1().x

T2[V:int] = class:
  ;

y : T2[X]
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("T2<X>")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, Constexpr_ConstructorForcedEvaluation_ThroughArray) {
  StringRef Code = R"(
T1 = class:
  x :int
  y : float64
  constructor()<constexpr>!
    x = 5
    y = 57.0

X<constexpr>:int = T1().x

T2 = class:
  ;
y : [X]T2
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("struct T2 [5]")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldOutOfOrder, Constexpr_DeclInNamespace_DifferentDeclSameNameOutsideNamespace) {
  StringRef Code = R"(
ns = namespace:
  T1 = class:
    x : int
    constructor()<constexpr>
    y : float64

X<constexpr>:int = ns.T1().x

T1 : int = 5

ns.T1.constructor()<constexpr>!
  x = 5
  y = 57.0

T2 = class:
  ;
y : [X]T2
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("struct T2 [5]")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// Create a test for a class declared inside of a namespace but defined outside
// of that namespace.
TEST(GoldOutOfOrder, Constexpr_CallChain_OutOfOrderDecl) {
  StringRef Code = R"(
foo<constexpr>():int

ns = namespace:
  T1 = class:
    x : int
    constructor()<constexpr>
    y : float64

X<constexpr>:int = ns.T1().x

T1 : int = 5

foo<constexpr>():int!
  return 3

ns.T1.constructor()<constexpr>!
  x = foo()
  y = 57.0
T2 = class:
  ;
y : [X]T2
)";
  auto ToMatch = varDecl(hasName("y"), hasType(asString("struct T2 [3]")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}