//===- unittest/Gold/GoldVarDeclStorageClass.cpp --------------------------===//
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

TEST(GoldVarDeclStorageClass, Static) {
  StringRef Code = R"(
foo<static> : int
)";
  DeclarationMatcher StaticVar = varDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}

TEST(GoldVarDeclStorageClass, ConflictingStorageClass) {
  StringRef Code = R"(
foo<static><extern> : int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, External) {
  StringRef Code = R"(
foo<extern>: int
)";
  DeclarationMatcher ExternVar = varDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}


TEST(GoldVarDeclStorageClass, LineAttr_Static) {
  StringRef Code = R"(
[static]
foo : int
)";
  DeclarationMatcher StaticVar = varDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}

TEST(GoldVarDeclStorageClass, LineAttr_External) {
  StringRef Code = R"(
[extern]
foo: int
)";
  DeclarationMatcher ExternVar = varDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}
