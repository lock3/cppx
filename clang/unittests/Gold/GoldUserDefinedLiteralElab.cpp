//=== GoldUserDefinedLiteralElab.cpp --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing literal declarations.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldUserDefinedLiteral, Decl) {
  StringRef Code = R"(
literal"_x"(p:uint64):uint64

)";

  DeclarationMatcher Match = functionDecl(
    hasName("operator\"\"_x"),
    unless(isDefinition())
  );
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(GoldUserDefinedLiteral, InvalidParameterType) {
  StringRef Code = R"(
literal"_x"(p:int64):int64

)";
  GoldFailureTest(Code);
}


TEST(GoldUserDefinedLiteral, NonFunctionDeclaration) {
  StringRef Code = R"(
literal"_x":int64

)";
  GoldFailureTest(Code);
}

TEST(GoldUserDefinedLiteral, InvalidNameNoUnderscore) {
  StringRef Code = R"(
literal"X"(p:uint64):uint64
)";
  GoldFailureTest(Code);
}


TEST(GoldUserDefinedLiteral, Def) {
  StringRef Code = R"(
literal"_x"(p:uint64):uint64!
  return 4

)";

  DeclarationMatcher Match = functionDecl(
    hasName("operator\"\"_x"),
    isDefinition()
  );
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(GoldUserDefinedLiteral, Use) {
  StringRef Code = R"(
literal"_x"(p:uint64):int!
  return 4

x:int = 5_x
)";

  DeclarationMatcher Match = functionDecl(
    hasName("operator\"\"_x"),
    isDefinition()
  );
  ASSERT_TRUE(matches(Code.str(), Match));
}