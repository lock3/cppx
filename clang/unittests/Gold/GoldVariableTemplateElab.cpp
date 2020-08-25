//=== GoldVariableTemplateElab ---------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing elaboration of variable templates.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldVariableTemplate, Simple) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
)";

  DeclarationMatcher ToMatch = varTemplateDecl(hasName("X"),
    has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, NoInit) {
  StringRef Code = R"(
X[T:type] : const T
)";

  DeclarationMatcher ToMatch = varTemplateDecl(hasName("X"),
    has(varDecl())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, UseOfDeclWithNoConst) {
  StringRef Code = R"(
X[T:type] : T

foo() : void!
  v = X[int]
)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    has(varTemplateDecl(hasName("X"))),
    has(functionDecl(hasName("foo"),
      hasDescendant(varDecl())
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
