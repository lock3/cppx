//=== BlueWithCppExternElab.cpp =====--------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for include C++ header files inside of blue.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueExternCpp, SimpleDecl) {
  StringRef Code = R"BLUE(
extern "C++" {
}
)BLUE";
  SimpleBlueParseTest(Code.str());
}

TEST(BlueExternCpp, ClassDecl) {
  StringRef Code = R"BLUE(
extern "C++" {
  class A { };
}

foo:()->void = {
  x:A;
}
)BLUE";
  auto ToMatch = varDecl(hasName("x"), hasType(asString("class A")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(BlueExternCpp, TestingCppInclude) {
//   StringRef Code = R"BLUE(
// extern "C++" {
//   #include <iostream>
//   class A { };
// }

// foo:()->void = {
//   x:A;
// }
// )BLUE";
//   auto ToMatch = varDecl(hasName("x"), hasType(asString("class A")));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
TEST(BlueExternCpp, ShiftOperatorTest) {
  StringRef Code = R"BLUE(
extern "C++" {
  struct x {
    x &operator<<(int rhs) {
      return *this;
    }
  };
}
foo:()->void = {
  y : x;
  y << 5;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOperatorName("<<"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
