//===- SymbolTable.h - Definitions for classifying symbols ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines useful enumerations and data structures for classifying
//  and storing symbols.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_SYMBOLTABLE_H
#define CLANG_GREEN_SYMBOLTABLE_H

#include "llvm/ADT/StringSwitch.h"

#include "clang/GreenBasic/GreenBasic.h"
#include "clang/GreenParse/TokenInfo.h"

#include <string>

namespace usyntax {

// Reserved words.
//------------------------------------------------------------------------------

enum res_t : nat8 {
  res_none = 0,
  res_of,
  res_not,
  res_and,
  res_or,
  res_where,
  res_if,

  res_else,
  res_catch,
  res_do,
  res_in,
  res_otherwise,
  res_returns,
  res_then,
  res_until,
  res_at,
  res_at_special
};

static constexpr const char *scan_reserved[] =
{
  "",      "of", "not", "and",       "or",      "where", "if",    "else",
  "catch", "do", "in",  "otherwise", "returns", "then",  "until", 0
};

struct reserved_t {
  static constexpr intp n = 16;
  static constexpr nat16 m0 = 1683, m1 = 1854;
  static constexpr nat8 hash(char8 c0, char8 c1) noexcept {
    return ((m0 * c0 + m1 * c1) >> 8) & (n - 1);
  }

  res_t table[n];
  constexpr reserved_t() : table{} {
    for (nat8 i = 0; i < n; i++)
      table[i] = res_none;
    for (nat8 i = 1; scan_reserved[i]; i++)
      table[hash(scan_reserved[i][0], scan_reserved[i][1])] = res_t(i);
  }

  constexpr res_t operator[](const char8 *s) const noexcept {
    nat8 i = table[hash(s[0], s[1])], j = 0;
    const char *r = scan_reserved[i];
    for (; r[j]; j++)
      if (s[j] != r[j])
        return res_none;
    return res_t(i & -((chars.flags[s[j]] & cf_idtail) == 0));
  }

  constexpr const char *operator[](res_t i) const noexcept {
    return scan_reserved[i];
  }

  /*static void find_coefficients() noexcept {
          for(intp m1=0;; m1++) for(intp m0=0; m0<m1; m0++) {
                  bool tmp[16]={},bad=false;
                  for(intp i=0; scan_reserved[i] && !bad; i++) {
                          nat8
  j=((m0*scan_reserved[i][0]+m1*scan_reserved[i][1])>>8)&15;
                          bad=tmp[j],tmp[j]=1;
                  }
                  if(!bad && !tmp[0])
                          log("m0=",m0," m1=",m1);
          }
  }*/
};


inline bool IsCPlusPlusBuiltinTypeKeyword(const std::string &Identifier) {
  return llvm::StringSwitch<bool>(Identifier)
    .Cases("void", "bool", "char", "int", "short", true)
    .Cases("long", "unsigned", "float", "double", true)
    .Default(false);
}

inline bool IsUsyntaxReservedWord(const std::string &Identifier) {
  return llvm::StringSwitch<bool>(Identifier)
    .Cases("", "of", "not", "and", "or", true)
    .Cases("where", "if", "else", "catch", "do", true)
    .Cases("in",  "otherwise", "returns", "then", "until", true)
    .Default(false);
}

inline bool IsAnyKeyword(const std::string &Identifier) {
  return IsUsyntaxReservedWord(Identifier) ||
    IsCPlusPlusBuiltinTypeKeyword(Identifier);
}

constexpr auto Reserved = reserved_t{};

} // namespace usyntax

#endif
