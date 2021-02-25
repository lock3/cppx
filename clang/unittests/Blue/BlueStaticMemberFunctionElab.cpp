//=== BlueStaticMemberFunctionElab.cpp =-----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for identifiying and elaborating implicitly static methods.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueStaticMethod, SimpleStaticMethod) {
  StringRef Code = R"BLUE(
type X:class= {
  func foo:(x:int) = {
    return x;
  }
}
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("X"),
    has(cxxMethodDecl(hasName("foo"), isStaticStorageClass()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}