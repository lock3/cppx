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


TEST(GoldVariableTemplate, Specialization) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
X[int] : const int = 12

)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(varTemplateDecl(hasName("X"),
      has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
    )),
    hasDescendant(varTemplateSpecializationDecl(hasName("X")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, PartialSpecialization) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
X[T:type][^T] : const int = 12
)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(varTemplateDecl(hasName("X"),
      has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
    )),
    hasDescendant(varTemplatePartialSpecializationDecl(hasName("X")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, MultipleSpecializations) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
X[int] : const int = 12
X[float64] : const int = 11
)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(varTemplateDecl(hasName("X"),
      has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
    )),
    hasDescendant(varTemplateSpecializationDecl(hasName("X")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, InvalidSpecialization) {
  StringRef Code = R"(
X : const float32 = 4
X[int] : const int = 12
)";
  GoldFailureTest(Code);
}



TEST(GoldVariableTemplate, PartialSpecialization_Use) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
X[T:type][^T] : const int = 12

main() : int!
  return X[^int]
)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(varTemplateDecl(hasName("X"),
      has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
    )),
    hasDescendant(varTemplatePartialSpecializationDecl(hasName("X")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldVariableTemplate, Specialization_Use) {
  StringRef Code = R"(
X[T:type] : const T = T(4)
X[int] : const int = 12

main() : int!
  return X[int]
)";

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(varTemplateDecl(hasName("X"),
      has(varDecl(hasInitializer(hasType(asString("type-parameter-0-0")))))
    )),
    hasDescendant(varTemplateSpecializationDecl(hasName("X")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}