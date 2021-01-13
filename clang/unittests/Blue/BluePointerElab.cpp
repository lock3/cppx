//=== BluePointerElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing declaration of pointer for elaboration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BluePointer, NullptrInit) {
  StringRef Code = R"BLUE(
X :^int = null;
)BLUE";

  auto ToMatch = varDecl(
    hasName("X"),
    hasType(asString("int *")),
    hasInitializer(hasDescendant(cxxNullPtrLiteralExpr()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BluePointer, NoInitializer) {
  StringRef Code = R"BLUE(
X :^int;
)BLUE";

  auto ToMatch = varDecl(
    hasName("X"),
    hasType(asString("int *"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BluePointer, DereferenceExpression) {
  StringRef Code = R"BLUE(
X :^int = null;
Y := X^;
)BLUE";
  auto ToMatch = varDecl(
    hasName("Y"),
    hasType(asString("int")),
    hasInitializer(hasDescendant(unaryOperator(hasOperatorName("*"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BluePointer, PointerTypeAlias) {
  StringRef Code = R"BLUE(
X :type = ^int;
)BLUE";
  auto ToMatch = typeAliasDecl(
    hasName("X"),
    hasType(asString("int *"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BluePointer, InvalidPtrDecl) {
  StringRef Code = R"BLUE(
x : ^3;
)BLUE";
  BlueFailureTest(Code);
}

TEST(BluePointer, InvalidTypeAliasPtrDecl) {
  StringRef Code = R"BLUE(
x := '3'^;
)BLUE";
  BlueFailureTest(Code);
}

TEST(BluePointer, InvalidDereference) {
  StringRef Code = R"BLUE(
X :int = 3;
Y := X^;
)BLUE";
  BlueFailureTest(Code);
}