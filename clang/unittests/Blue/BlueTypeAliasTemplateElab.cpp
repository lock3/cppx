//=== BlueTypeAliasTemplateElab.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing type alias template declarations.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueTypeAliasTemplate, ExplicitTypeAlias) {
  StringRef Code = R"BLUE(
C:[T:type] => class {
}
x : [T:type] => type = C[T];
)BLUE";
  auto ToMatch = typeAliasTemplateDecl(
    hasName("x"), has(typeAliasDecl(has(templateSpecializationType())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(BlueTypeAlias, ImplicitTypeAlias) {
//   StringRef Code = R"BLUE(
// x : = int;
// )BLUE";
//   auto ToMatch = typeAliasDecl(
//     hasName("x"),
//     hasType(asString("int"))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueTypeAlias, TypeAliasMissingInitializer) {
//   StringRef Code = R"BLUE(
// x : type;
// )BLUE";
//   BlueFailureTest(Code);
// }

// TEST(BlueTypeAlias, TypeAliasOfAClass) {
//   StringRef Code = R"BLUE(
// Y : class{ }
// x : = Y;
// )BLUE";
//   auto ToMatch = typeAliasDecl(
//     hasName("x"),
//     hasType(asString("struct Y"))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
