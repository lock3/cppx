//=== GoldClassIndirectAccessElab.cpp --------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file provides a means of testing what would be the -> operator in C++.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassParsing, ObjPtr_MemberUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  x:int

foo(Var:^Ty):int !
  return Var.x
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr(member(hasName("x")), isArrow())
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
  
}

TEST(ClassParsing, ObjPtr_MethodUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  foo(): int!
    return 4
  

bar(Var:^Ty):int !
  return Var.foo()
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr(member(hasName("foo")), isArrow())
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
}


TEST(ClassParsing, ObjRef_MemberUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  x:int

foo(Var:ref Ty):int !
  return Var.x
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr(member(hasName("x")), unless(isArrow()))
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
  
}

TEST(ClassParsing, ObjRef_MethodUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  foo(): int!
    return 4
  

bar(Var:ref Ty):int !
  return Var.foo()
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr()
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
}

TEST(ClassParsing, ObjRRef_MemberUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  x:int

foo(Var:rref Ty):int !
  return Var.x
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr(member(hasName("x")), unless(isArrow()))
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
}

TEST(ClassParsing, ObjRRef_MethodUse_OutsideOfClass) {
  StringRef Code = R"(
Ty : type = class:
  foo(): int!
    return 4
  

bar(Var:rref Ty):int !
  return Var.foo()
)";
  StatementMatcher MemberExprRef(findAll(
      memberExpr()
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberExprRef));
}