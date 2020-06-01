//=== GoldCharLiteralElab.cpp - Testing to make sure all builtin types work-==//
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


TEST(CharLiteralElab, SimpleCharacter) {
  StringRef Code = R"(
i:char = 'a'
)";
  DeclarationMatcher ClassC = varDecl(
    hasName("i"), hasType(asString("char")),
    hasInitializer(has(characterLiteral(equals('a'))))
  
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

// FIXME: We need hex, octal, UTF-8 implementation tests for this as well as a
// possible wide char implementation.
