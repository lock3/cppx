//===- unittest/Gold/GoldConstAttrElab.cpp --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Tests for the const attribute, the const attribute may only be applied to
//  non-static member functions within a class.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldConstAttr, ConstMemberFunctionDecl) {
  StringRef Code = R"(
c : type = class:
  foo()<const>:void
)";
  DeclarationMatcher ClassC = cxxRecordDecl(hasName("c"), 
    has(cxxMethodDecl(hasName("foo"), isConst()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(GoldConstAttr, ConstMemberFunctionDeclWithExceptionSpecInfo) {
  StringRef Code = R"(
c : type = class:
  foo()<noexcept><const>:void
)";
  DeclarationMatcher ClassC = cxxRecordDecl(hasName("c"), 
    has(cxxMethodDecl(hasName("foo"), isConst(), isNoThrow()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(GoldConstAttr, ConstMemberFunctionDef) {
  StringRef Code = R"(
c : type = class:
  foo()<const>:void!
    ;
  
)";
  DeclarationMatcher ClassC = cxxRecordDecl(hasName("c"), 
    has(cxxMethodDecl(hasName("foo"), isConst()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

