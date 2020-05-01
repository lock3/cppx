//=== FunctionTemplate.cpp - Elaboration for Gold Function Templates -------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of function templates.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFunctionTemplate, MixedTypeParameters) {
  StringRef Code = R"(
f[T : type, z : int](x : T) : T!
  return x + z
)";
  SimpleGoldParseTest(Code);
}


TEST(GoldFunctionTemplate, WithExplicitCall) {
  StringRef Code = R"(
f[z : int]() : int!
  return z

main() : int!
  return f[4]()
)";
  StatementMatcher StmtMatcher(compoundStmt(hasDescendant(
    returnStmt(
      has(callExpr(
        has(implicitCastExpr(
          has(declRefExpr(
            to(functionDecl())

          ))
        ))
      ))
    )
  )));
  ASSERT_TRUE(matches(Code, StmtMatcher));
}
