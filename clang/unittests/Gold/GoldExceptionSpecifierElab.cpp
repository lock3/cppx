//=== GoldNoExceptFunctionDeclElab.cpp -------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing to make sure that we can declare/define noexcept functions.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldExceptionSpec, noexcept_ImplicitNoArgs) {
  StringRef Code = R"(
foo()<noexcept> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_ExplicitWithNoArgs) {
  StringRef Code = R"(
foo()<noexcept()> : int !
  return 4
)";
  GoldFailureTest(Code);
}

TEST(GoldExceptionSpec, noexcept_ParameterDependentTrueConstantExpr) {
  StringRef Code = R"(
foo(i:int)<noexcept(noexcept(i + 1))> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, LineAttr_noexcept_ImplicitNoArgs) {
  StringRef Code = R"(
[noexcept]
foo() : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, LineAttr_noexcept_ParameterDependentTrueConstantExpr) {
  StringRef Code = R"(
[noexcept(noexcept(i + 1))]
foo(i:int) : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_ParameterDependentFalse) {
  StringRef Code = R"(
Cls : type = class:
  constructor()<noexcept(false)>:void!
    ;

  bar()<noexcept(false)>:void!
    ;
  

foo(i:ref Cls)<noexcept(noexcept(i.bar()))> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_DependentExpression) {
  StringRef Code = R"(
foo[T:type](i:T)<noexcept(noexcept(i.bar()))> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_PossibleLateElaboration) {
  StringRef Code = R"(
C1 : type = class:
  foo()<noexcept>: void


C2 : type = class:
  bar()<noexcept(noexcept(y.foo()))>:void!
    y.foo()
  x:int
  y:C1

)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("bar"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}


TEST(GoldExceptionSpec, noexcept_LateElaborationOfFunctionDecl) {
  StringRef Code = R"(
C2 : type = class:
  constructor()<noexcept(false)>:void
  bar()<noexcept(noexcept(this.foo()))>:void
  foo() : void!
    ;
  x:int

)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("bar"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_ElaborationOfFunctionDecl) {
  StringRef Code = R"(
C2 : type = class:
  constructor()<noexcept(false)>:void
  bar()<noexcept(noexcept(foo()))>:void
  foo() : void!
    ;
  x:int

)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("bar"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}


TEST(GoldExceptionSpec, noexcept_MemberFunctionInSimpleOrder) {
  StringRef Code = R"(
C2 : type = class:
  constructor()<noexcept(false)>:void
  foo() : void!
    ;
  bar()<noexcept(noexcept(this.foo()))>:void
  x:int

)";
  DeclarationMatcher NoExceptAttr = cxxMethodDecl(
    hasName("bar"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_InvalidConstantExpr) {
  StringRef Code = R"(
Cls : type = class:
  constructor()<noexcept(false)>:void!
    ;

  bar()<noexcept(false)>:void!
    ;
  

foo(i:ref Cls)<noexcept(i.bar())> : int !
  return 4
)";
  GoldFailureTest(Code);
}

TEST(GoldExceptionSpec, noexcept_TrueConstexpr) {
  StringRef Code = R"(
foo()<noexcept(1)> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_FalseConstexpr) {
  StringRef Code = R"(
foo()<noexcept(0)> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), unless(isNoThrow())
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, noexcept_ToManyArguments) {
  StringRef Code = R"(
foo()<noexcept(1, 2)> : int !
  return 4
)";
  GoldFailureTest(Code);
}


TEST(GoldExceptionSpec, throw_DynamicNoThrow) {
  StringRef Code = R"(
foo()<throw()> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), isNoThrow(), hasDynamicExceptionSpec()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, throw_DynamicThrows) {
  StringRef Code = R"(
FakeException : type = class:
  i:int

foo()<throw(FakeException)> : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), hasDynamicExceptionSpec()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, LineAttr_throw_DynamicThrows) {
  StringRef Code = R"(
FakeException : type = class:
  i:int

[throw(FakeException)]
foo() : int !
  return 4
)";
  DeclarationMatcher NoExceptAttr = functionDecl(
    hasName("foo"), hasDynamicExceptionSpec()
  );
  ASSERT_TRUE(matches(Code.str(), NoExceptAttr));
}

TEST(GoldExceptionSpec, throw_InvalidExceptionSpec) {
  StringRef Code = R"(
foo()<throw(1)> : int !
  return 4
)";
  GoldFailureTest(Code);
}