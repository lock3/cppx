//=== BlueClassTemplateElab.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing class template declaration and use.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueClassTemplate, SimpleClassTemplateDecl) {
  StringRef Code = R"BLUE(
type C:[T:type] class = {
}
)BLUE";

  auto ToMatch = classTemplateDecl(
    has(cxxRecordDecl(
      hasName("C")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClassTemplate, ClassTemplateWithDependentMember) {
  StringRef Code = R"BLUE(
type C:[T:type] class = {
  var x:T;
}
)BLUE";

  auto ToMatch = classTemplateDecl(
    has(cxxRecordDecl(
      hasName("C"),
      has(fieldDecl(hasName("x")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClassTemplate, UseInTypeAlias) {
  StringRef Code = R"BLUE(
type C:[T:type] class = {
  var x:T;
}
type x:type = C[int];
)BLUE";

  auto ToMatch = typeAliasDecl(
    hasName("x"),
    hasType(asString("C<int>"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClassTemplate, UseAsVariableType) {
  StringRef Code = R"BLUE(
type C:[T:type] class = {
  var x:T;
}
var x:C[int];
)BLUE";

  auto ToMatch = varDecl(
    hasName("x"),
    hasType(asString("C<int>"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
