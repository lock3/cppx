//=== BlueConstructorElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for constructor declaration and use.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueConstructor, Decl) {
  StringRef Code = R"BLUE(
C : type = {
  operator= : (out this) = { }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxConstructorDecl(isDefaultConstructor(),
                  unless(isImplicit())))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueConstructor, MoveConstructor) {
  StringRef Code = R"BLUE(
C : type = {
  operator= : (out this, move that:C) = { }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxConstructorDecl(isMoveConstructor(),
                  unless(isImplicit())))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueConstructor, DefaultCtorCall) {
  StringRef Code = R"BLUE(
C : type = {
  operator= : (out this) = { }
}

foo:()->void = {
  x:C = ();
}
)BLUE";
  auto ToMatch = cxxConstructExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueConstructor, SingleArgumentCall) {
  StringRef Code = R"BLUE(
C : type = {
  operator= : (out this, x:int) = { }
}

foo:()->void = {
  x:C = (5);
}
)BLUE";
  auto ToMatch = cxxConstructExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(BlueConstructor, MultiArgumentCall) {
  StringRef Code = R"BLUE(
C : type = {
  operator= : (out this, x:int, y:int) = { }
}

foo:()->void = {
  x:C = (5, 65);
}
)BLUE";
  auto ToMatch = cxxConstructExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueCopyConstructor, ThatParamImp) {
  StringRef Code = R"BLUE(
point: type = {
    x: int = 0;
    y: int = 0;
    operator=: (out this, that)    = { x=that.x; y=that.y; }
}
)BLUE";
  auto ToMatch = cxxConstructorDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueCopyConstructor, ThatParamExp) {
  StringRef Code = R"BLUE(
point: type = {
    x: int = 0;
    y: int = 0;
    operator=: (out this, that : point)    = { x=that.x; y=that.y; }
}
)BLUE";
  auto ToMatch = cxxConstructorDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
