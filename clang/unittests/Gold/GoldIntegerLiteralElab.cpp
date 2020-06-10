//=== GoldIntegerLiteralElab.cpp - -----------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language elaborators.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


// FIXME: We need hex, octal, UTF-8 implementation tests for this as well as a
// possible wide char implementation.

TEST(IntegerLiteralElab, PositiveInteger) {
  StringRef Code = "i:int = 1";
  DeclarationMatcher IntCmp = varDecl(
    hasName("i"), hasType(asString("int")),
    hasInitializer(integerLiteral(equals(1)))
  
  );
  ASSERT_TRUE(matches(Code.str(), IntCmp));
}

