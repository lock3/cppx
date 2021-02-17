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
type C : class = {
  func operator= : (out this) = { }
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
type C : class = {
  func operator= : (out this, move that:C) = { }
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


