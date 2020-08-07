//=== FunctionTemplateSpec.cpp - Elaboration for Gold Function Tmp Spec ----==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of function
//  template specializations.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFunctionTemplateSpec, Basic) {
  StringRef Code = R"(
f[T : type](x : T) : T!
  return T()

f[int](x : int) : int!
  return x

main() : int!
  result = f[int](42)
)";

  SimpleGoldParseTest(Code);
  DeclarationMatcher f =
    functionDecl(hasName("f"),
                 isExplicitTemplateSpecialization());

  StatementMatcher
    ResultMatcher(hasDescendant(
                    varDecl(hasName("result"),
                            hasType(asString("int"))
                      )
                    )
      );
  ASSERT_TRUE(matches(Code.str(), ResultMatcher));
  ASSERT_TRUE(matches(Code.str(), f));
}

TEST(GoldFunctionTemplateSpec, BasicNonExplicit) {
  StringRef Code = R"(
f[T : type](x : T) : T!
  return T()

f[](x : int) : int!
  return x

f[](x : double) : double!
  return 24.0

main() : int!
  result = f[int](42)
)";

  SimpleGoldParseTest(Code);
  DeclarationMatcher f =
    functionDecl(hasName("f"),
                 isExplicitTemplateSpecialization());

  StatementMatcher
    ResultMatcher(hasDescendant(
                    varDecl(hasName("result"),
                            hasType(asString("int"))
                      )
                    )
      );
  ASSERT_TRUE(matches(Code.str(), ResultMatcher));
  ASSERT_TRUE(matches(Code.str(), f));
}
