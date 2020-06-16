//=== GoldClassConversionsElab.cpp -----------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests to make sure that qualified member access works
//  correctly.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldClassConversions, ImplicitConversionToBase) {
  std::string Code = R"Gold(
Base : type = class:
  a:int

Derived : type = class(Base):
  b:int

foo(D : ^ Derived):void!
  B : ^Base = D
)Gold";
  DeclarationMatcher opMatches = hasDescendant(implicitCastExpr(
          hasImplicitDestinationType(asString("struct Base *")),
          hasCastKind(CK_DerivedToBase)));
  ASSERT_TRUE(matches(Code, opMatches))
    << "Reinterpert cast failed";

}