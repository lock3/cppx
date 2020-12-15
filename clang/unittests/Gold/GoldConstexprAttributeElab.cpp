//=== GoldExplicitAttributeElab.cpp ----------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the explicit attribute for constructors, and conversion operators.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConstexprAttr, OnAConstructor) {
  std::string Code = R"Gold(
Cls : type = class:
  constructor()<constexpr>: void!
    ;
  
)Gold";
  DeclarationMatcher ToMatch = cxxRecordDecl(
    hasName("Cls"),
    has(cxxConstructorDecl(isConstexpr()))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldConstexprAttr, OnADestructor) {
  std::string Code = R"Gold(
Cls : type = class:
  destructor()<constexpr>: void!
    ;
  
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, OnAVirtualFunction_Virtual_ConstExpr) {
  std::string Code = R"Gold(
Cls : type = class:
  something()<virtual><constexpr>: void!
    ;
  
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, OnAVirtualFunction_ConstExpr_Virtual) {
  std::string Code = R"Gold(
Cls : type = class:
  something()<constexpr><virtual>: void!
    ;
  
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, OnAVirtualFunction_ImplicitVirtual) {
  std::string Code = R"Gold(
Base : type = class:
  something()<virtual>:void!
    ;
Cls : type = class(Base):
  something()<constexpr>: void!
    ;
  
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, OnAMemberVariable) {
  std::string Code = R"Gold(
Cls : type = class:
  x<static><constexpr>:int = 5

)Gold";
  DeclarationMatcher ToMatch = varDecl( hasName("x"), isConstexpr());
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldConstexprAttr, NonStaticMemberVariable) {
  std::string Code = R"Gold(
Cls : type = class:
  x<constexpr>:int = 5

)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, TypeAlias) {
  std::string Code = R"Gold(
x<constexpr>:type = int
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldConstexprAttr, FreeVariable) {
  std::string Code = R"Gold(
x<constexpr>:int = 5
)Gold";
  DeclarationMatcher ToMatch = varDecl(hasName("x"), hasType(asString("int")),
                                       isConstexpr());
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldConstexprAttr, FreeFunction) {
  std::string Code = R"Gold(
foo()<constexpr>:int!
  return 5
)Gold";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), isConstexpr());
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldConstexprAttr, FreeFunctionTemplate) {
  std::string Code = R"Gold(
foo[T:type]()<constexpr>:int!
  return 5
)Gold";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), isConstexpr());
  ASSERT_TRUE(matches(Code, ToMatch));
}

// TEST(GoldConstexprAttr, MultipleReturnStmts) {
//   std::string Code = R"Gold(
// foo(x:int)<constexpr>:int!
//   if (x):
//     return 5
//   else:
//     return 3
// )Gold";
//   GoldFailureTest(Code);
// }

TEST(GoldConstexprAttr, VirtualBaseClass) {
  std::string Code = R"Gold(
T1 = class:
  ;
T2 = class(T1<virtual>):
  construct()<constexpr>!
    ;
)Gold";
  GoldFailureTest(Code);
}