//=== GoldLiteralSuffix.cpp - Test Gold literals with suffixes -------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of literal suffixes.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldUtil.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Suffix, DecimalInt) {
  StringRef Code = R"(
main() : int!
  s = 42s64
  u = 42u64
)";

  static const pair<const char *, const char *> Variables[] = {
    {"s", "long"},
    {"u", "unsigned long"},
  };

  for (auto Var : Variables) {
    StatementMatcher
      VarMatcher(hasDescendant(
                      varDecl(hasName(Var.first),
                              hasType(asString(Var.second)),
                              hasDescendant(integerLiteral(equals(42)))
                        )
                      ));

    ASSERT_TRUE(matches(Code.str(), VarMatcher));
  }
}

TEST(Suffix, DecimalFloat) {
  StringRef Code = R"(
main() : int!
  d = 42d
  f = 42f
)";

  static const pair<const char *, const char *> Variables[] = {
    {"d", "double"},
    {"f", "float"},
  };

  for (auto Var : Variables) {
    StatementMatcher
      VarMatcher(hasDescendant(
                      varDecl(hasName(Var.first),
                              hasType(asString(Var.second)),
                              hasDescendant(floatLiteral(equals(42.0)))
                        )
                      ));

    ASSERT_TRUE(matches(Code.str(), VarMatcher));
  }
}

TEST(Suffix, Exponent) {
  StringRef Code = R"(
main() : int!
  ex = 4.2e1
  ex_f = 4.2e1f
  ex_d = 4.2e1d
)";

  static const pair<const char *, const char *> Variables[] = {
    {"ex", "float"},
    {"ex_f", "float"},
    {"ex_d", "double"}
  };

  for (auto Var : Variables) {
    StatementMatcher
      VarMatcher(hasDescendant(
                      varDecl(hasName(Var.first),
                              hasType(asString(Var.second)),
                              hasDescendant(floatLiteral(equals(4.2e1)))
                        )
                      ));

    ASSERT_TRUE(matches(Code.str(), VarMatcher));
  }
}

TEST(Suffix, Hex) {
  StringRef Code = R"(
main() : int!
  s = 0xbeefs64
  u = 0xbeefu64
)";

  static const pair<const char *, const char *> Variables[] = {
    {"s", "long"},
    {"u", "unsigned long"},
  };

  for (auto Var : Variables) {
    StatementMatcher
      VarMatcher(hasDescendant(
                      varDecl(hasName(Var.first),
                              hasType(asString(Var.second)),
                              hasDescendant(integerLiteral(equals(0xbeef)))
                        )
                      ));

    ASSERT_TRUE(matches(Code.str(), VarMatcher));
  }
}
