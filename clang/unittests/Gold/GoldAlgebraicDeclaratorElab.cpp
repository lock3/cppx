//=== GoldAlgebraicDeclaratorElab.cpp - Elaboration for Gold Nodes ---------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file tests balancing declarators and declarator algebra.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(DeclaratorAlgebra, TypeArray) {
  StringRef Code = R"(
x[3] : [4]type;
)";

  DeclarationMatcher X = varDecl(
    hasName("x"),
    hasType(asString("type[3][4]")));

  ASSERT_TRUE(matches(Code.str(), X));
}

TEST(DeclaratorAlgebra, TypeArray2) {
  StringRef Code = R"(
x[3] : type;
)";

  DeclarationMatcher X = varDecl(
    hasName("x"),
    hasType(asString("type[3]")));

  ASSERT_TRUE(matches(Code.str(), X));
}

TEST(DeclaratorAlgebra, ArrayLikeSpecialization) {
  StringRef Code = R"(
x[T : int] : type = class:
  ;

x[3] : type = class:
  ;
)";

  DeclarationMatcher X = classTemplateSpecializationDecl(
    hasName("x"));

  ASSERT_TRUE(matches(Code.str(), X));
}

TEST(DeclaratorAlgebra, DeclareArrayAsSpecializationFail) {
  StringRef Code = R"(
x[T : int] : type = class:
  ;

x[3] : [4]type;
)";

  GoldFailureTest(Code.str());
}

TEST(DeclaratorAlgebra, DeclareArrayAsSpecializationFail2) {
  StringRef Code = R"(
x[T : int] : type = class:
  ;

x[3] : type;
)";

  GoldFailureTest(Code.str());
}

