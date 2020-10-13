//=== GoldFoldExprElab.cpp -------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Gold fold expression testing.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldFoldExpr, UnaryRightFoldExpr) {
    StringRef Code = R"(
f[T:type...](x:rref T...): void!
  i = 3
  j = 4
  q = false
  z = i * (j || q)
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack()))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, UnaryLeftFoldExpr) {
    StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (... || x)
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack()))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFoldExpr, BinaryLeftFoldExpr) {
    StringRef Code = R"(
f[T:type...](x:rref T...): void!
  z = (false || ... || x)
)";
  auto ToMatch = classTemplateDecl(
    hasName("x"),
    has(templateTypeParmDecl(hasName("T"), isParameterPack()))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}