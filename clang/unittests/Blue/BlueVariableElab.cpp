//=== BlueVariableElab.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for elaboration and parsing of Blue variable declarations.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueVariableDecl, ImplicitAuto) {
  StringRef Code = R"BLUE(
X := 4;
  )BLUE";

  auto ToMatch = varDecl(
    hasName("X"),
    hasType(asString("int")),
    hasInitializer(integerLiteral(equals(4)))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueVariableDecl, VariableDeclAndUse){
  StringRef Code = R"BLUE(
X : int = 4;
Y := X;

)BLUE";

  auto ToMatch = varDecl(
    hasName("Y"),
    hasType(asString("int")),
    hasInitializer(hasDescendant(declRefExpr(to(varDecl(hasName("X"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueVariableDecl, UndeclaredIdentifier){
  StringRef Code = R"BLUE(
Y := Z;

)BLUE";
  BlueFailureTest(Code);
}


// FIXME: do forced phase 2 elaboration of variable declarations.
TEST(BlueVariableDecl, OutOfOrderVariableUse) {
  StringRef Code = R"BLUE(
Y := Z;
Z:= 3;

)BLUE";

  auto ToMatch = varDecl(
    hasName("Y"),
    hasType(asString("int")),
    hasInitializer(hasDescendant(declRefExpr(to(varDecl(hasName("Z"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueVariableDecl, ConflictingVariableDecls) {
  StringRef Code = R"BLUE(
Y := Z;
Z:= 3;

)BLUE";

  auto ToMatch = varDecl(
    hasName("Y"),
    hasType(asString("int")),
    hasInitializer(hasDescendant(declRefExpr(to(varDecl(hasName("Z"))))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}