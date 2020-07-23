//===- unittest/Gold/GoldVariableDeclElab.cpp -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldVarDecl, InvalidTypeName) {
    StringRef Code = R"(
x : V
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDecl, PointerToAnArrayOf3Elements) {
    StringRef Code = R"(
x : ^[3]const int
)";
  DeclarationMatcher ToMatch = varDecl(hasName("x"),
    hasType(
      pointerType(
        pointee(
          constantArrayType(
            hasSize(3),
            hasElementType(asString("const int"))
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}