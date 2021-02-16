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
var X :^int = null;
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
var X :^int;
)BLUE";

  auto ToMatch = varDecl(
    hasName("X"),
    hasType(asString("int *"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BluePointer, DereferenceExpression) {
  StringRef Code = R"BLUE(
var X :^int = null;
var Y := X^;
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
var X :type = ^int;
)BLUE";
  auto ToMatch = typeAliasDecl(
    hasName("X"),
    hasType(asString("int *"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BluePointer, AddressOf) {
  StringRef Code = R"BLUE(
func f:() = {
  var Y:int;
  var X :^int = ^Y;
}
)BLUE";
  auto ToMatch = unaryOperator(hasOperatorName("&"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BluePointer, InvalidPtrDecl) {
  StringRef Code = R"BLUE(
var x : ^3;
)BLUE";
  BlueFailureTest(Code);
}

TEST(BluePointer, InvalidTypeAliasPtrDecl) {
  StringRef Code = R"BLUE(
var x := '3'^;
)BLUE";
  BlueFailureTest(Code);
}

TEST(BluePointer, InvalidDereference) {
  StringRef Code = R"BLUE(
var X :int = 3;
var Y := X^;
)BLUE";
  BlueFailureTest(Code);
}
