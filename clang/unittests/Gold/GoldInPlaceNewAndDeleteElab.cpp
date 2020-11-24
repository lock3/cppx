//=== GoldInPlaceNewAndDeleteElab.cpp --------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration for the inplace new operator.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConstruct, PlacementNew) {
  std::string Code = R"Gold(
foo(x:^int):void!
  x.construct(43)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("__GoldInplaceNew"))),
    hasDescendant(cxxNewExpr())
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldDestruct, DestructorCall) {
  std::string Code = R"Gold(
ToDestroy : type = class:
  ;
foo(x:^ToDestroy):void!
  x.destruct()
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(hasName("__GoldInplaceNew"))),
    hasDescendant(cxxMemberCallExpr())
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldDestruct, DtorCalledOnInteger) {
  std::string Code = R"Gold(
foo(x:^int):void!
  x.destruct()
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldDestruct, UserDefinedDestructor) {
  std::string Code = R"Gold(
ToDestroy : type = class:
  destructor()!
    ;
foo(x:^ToDestroy):void!
  x.destruct()
)Gold";
  auto ToMatch =
  translationUnitDecl(
    hasDescendant(cxxMemberCallExpr(
      on(hasType(asString("struct ToDestroy *"))),
      has(memberExpr(member(hasName("~ToDestroy"))))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldDestruct, DeletedDestructor) {
  std::string Code = R"Gold(
ToDestroy : type = class:
  destructor() = delete

foo(x:^ToDestroy):void!
  x.destruct()
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldDestruct, DestructorTypeGivenAsNameQualifier) {
  std::string Code = R"Gold(
ToDestroy : type = class:
  destructor()<virtual>!
    ;
ToDestroy2 : type = class(ToDestroy):
  destructor()!
    ;
foo(x:^ToDestroy2):void!
  (x.(ToDestroy)destruct)()
)Gold";
  auto ToMatch =
  translationUnitDecl(
    hasDescendant(cxxMemberCallExpr(
      on(hasType(asString("struct ToDestroy2 *"))),
      has(memberExpr(member(hasName("~ToDestroy"))))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}