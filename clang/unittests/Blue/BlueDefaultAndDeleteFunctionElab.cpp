
//=== BlueDefaultAndDeleteFunctionElab.cpp --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration of type aliases
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueDefaultFunction, DefaultDefaultConstructor) {
  StringRef Code = R"(
c : type = {
  operator= : (out this) = default;
})";
  auto ToMatch = cxxConstructorDecl(isDefaultConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultFunction, DefaultMoveConstructor) {
  StringRef Code = R"(
c : type = {
  operator= : (out this, move that:c) = default;
})";
  auto ToMatch = cxxConstructorDecl(isMoveConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultFunction, DefaultCopyConstructor) {
  StringRef Code = R"(
c : type = {
  operator= : (out this, in that:c) = default;
})";
  auto ToMatch = cxxConstructorDecl(isCopyConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(BlueDefaultFunction, DefaultMoveAssignment) {
  StringRef Code = R"(
c : type = {
  operator= : (inout this, move that:c) = default;
})";
  auto ToMatch = cxxMethodDecl(isMoveAssignmentOperator(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDefaultFunction, DefaultCopyAssignment) {
  StringRef Code = R"(
c : type = {
  operator= : (inout this, in that:c) = default;
})";
  auto ToMatch = cxxMethodDecl(isCopyAssignmentOperator(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

