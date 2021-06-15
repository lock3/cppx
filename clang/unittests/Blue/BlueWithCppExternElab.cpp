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

TEST(BlueExternCpp, TestingCppInclude) {
  StringRef Code = R"BLUE(
extern "C++" {
  #include <iostream>
  class A { };
  void bar() {
  }
}

foo:()->void = {
  x:A;
  y : ^const char = "Also hello world.\n";
  std.cout << y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOperatorName("<<"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueExternCpp, MultipleExternCppBlocks) {
  StringRef Code = R"BLUE(
extern "C++" {
  class A { };
}

extern "C++" {
  class B { };
}

foo:()->void = {
  x:A;
  y:B;
}
)BLUE";
  // auto ToMatch = cxxOperatorCallExpr(hasOperatorName("<<"));
  auto ToMatch = translationUnitDecl(
    hasDescendant(varDecl(hasName("x"), hasType(asString("class A")))),
    hasDescendant(varDecl(hasName("y"), hasType(asString("class B"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueExternCpp, MissingIncludeFile) {
  StringRef Code = R"BLUE(
extern "C++" {
  #include <foo>
}

)BLUE";
  BlueFailureTest(Code);
}


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


TEST(BlueExternCpp, JustAnInclude) {
  StringRef Code = R"BLUE(
#include <iostream>

foo:()->void = {
  y : ^const char = "Also hello world.\n";
  std.cout << y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOperatorName("<<"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueExternCpp, MakingSureWeCanCallPrintf) {
  StringRef Code = R"BLUE(
#include <cstdio>

foo:()->void = {
  std.printf("hello world\n");
}
)BLUE";
  auto ToMatch = callExpr(callee(functionDecl(hasName("printf"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}