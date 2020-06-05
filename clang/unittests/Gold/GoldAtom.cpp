//=== GoldFor.cpp - Test Gold for loops ------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of for loops.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Expr.h"

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(Atom, Char) {
  StringRef Code = R"(
main() : int!
  x : char = 'a'
)";

  SimpleGoldParseTest(Code);
}

template <typename T, typename U>
struct pair {
  T first;
  U second;
};

TEST(Atom, CharEscapes) {
  StringRef Code = R"(
main() : int!
  single_quote       : char = '\''
  double_quote       : char = '\"'
  question_mark      : char = '\?'
  backslash          : char = '\\'
  audible_bell       : char = '\a'
  backspace          : char = '\b'
  feed               : char = '\f'
  newline            : char = '\n'
  carriage_return    : char = '\r'
  horiz_tab          : char = '\t'
  vert_tab           : char = '\v'
)";

  static const pair<StringRef, int> Escapes[] = {
    {"single_quote", 0x27},
    {"double_quote", 0x22},
    {"question_mark", 0x3f},
    {"backslash", 0x5c},
    {"audible_bell", 0x07},
    {"backspace", 0x08},
    {"feed", 0x0c},
    {"newline", 0x0a},
    {"carriage_return", 0x0d},
    {"horiz_tab", 0x09},
    {"vert_tab", 0x0b}
  };

  for (auto Seq : Escapes) {
    StatementMatcher
      EscapeMatcher(hasDescendant(
                      varDecl(hasName(Seq.first.str()),
                          hasType(asString("char")),
                          hasDescendant(integerLiteral(equals(Seq.second)))
                      )
                   ));
    ASSERT_TRUE(matches(Code.str(), EscapeMatcher));
  }
}

TEST(Atom, BoolLiteral) {
  StringRef Code = R"(
main() : int!
  t = true
  f = false
)";

  StatementMatcher
    TrueMatcher(hasDescendant(
                  varDecl(hasName("t"), hasType(asString("_Bool")),
                          hasDescendant(integerLiteral(equals(true))))));
  StatementMatcher
    FalseMatcher(hasDescendant(
                  varDecl(hasName("f"), hasType(asString("_Bool")),
                          hasDescendant(integerLiteral(equals(false))))));
  ASSERT_TRUE(matches(Code.str(), TrueMatcher)
              && matches(Code.str(), FalseMatcher));
}
