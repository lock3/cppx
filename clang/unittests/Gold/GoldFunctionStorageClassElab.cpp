//===- unittest/Gold/GoldFunctionStorageClassElab.cpp ---=//
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

TEST(GoldFunctionStorageClass, StaticFreeFunction) {
  StringRef Code = R"(
foo()<static> : int!
  return 4
)";
  DeclarationMatcher StaticFunction = functionDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code, StaticFunction));
}

TEST(GoldFunctionStorageClass, StaticFreeFunctionDecl) {
  StringRef Code = R"(
foo()<static> : int
)";
  DeclarationMatcher StaticFunction = functionDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code, StaticFunction));
}

TEST(GoldFunctionStorageClass, ExternFreeFunction) {
  StringRef Code = R"(
foo()<extern> : int!
  return 4
)";
  DeclarationMatcher ExternFunction = functionDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code, ExternFunction));
}

TEST(GoldFunctionStorageClass, ExternFreeFunctionDecl) {
  StringRef Code = R"(
foo()<extern>: int
)";
  DeclarationMatcher ExternFunction = functionDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code, ExternFunction));
}


// TEST(GoldFunctionStorageClass, ExternC_FreeFunctionDecl) {
//   StringRef Code = R"(
// foo()<language"C">: int
// )";
//   DeclarationMatcher ExternFunction = functionDecl(
//     hasName("foo"), isExternStorageClass());
//   ASSERT_TRUE(matches(Code, ExternFunction));
// }

// TEST(GoldFunctionStorageClass, ExternC_FreeFunction) {
//   StringRef Code = R"(
// foo()<language"C">: int!
//   return 4
// )";
//   DeclarationMatcher ExternFunction = functionDecl(
//     hasName("foo"), isExternStorageClass());
//   ASSERT_TRUE(matches(Code, ExternFunction));
// }

