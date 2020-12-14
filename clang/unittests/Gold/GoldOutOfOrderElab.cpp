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