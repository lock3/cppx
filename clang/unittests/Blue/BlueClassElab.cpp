//=== BlueClassElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for basic class elaboration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueClass, SimpleClassDecl) {
  StringRef Code = R"BLUE(
C : class = { }
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("C"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassWithMemberDecl) {
  StringRef Code = R"BLUE(
C : class = {
  x:int;
}
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("C"),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
    isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassDeclUse_DefaultInit) {
  StringRef Code = R"BLUE(
C : class = {
  x:int;
}
x:C;

)BLUE";
  auto ToMatch = translationUnitDecl(has(cxxRecordDecl(
    hasName("C"),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
    )),
    varDecl(hasName("x"), hasType(asString("struct C")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassDeclUse_DefaultCtor) {
  StringRef Code = R"BLUE(
C : class = {
  x:int;
}
x:C = ();

)BLUE";
  auto ToMatch = translationUnitDecl(has(cxxRecordDecl(
    hasName("C"),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
    )),
    varDecl(hasName("x"), hasType(asString("struct C")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}