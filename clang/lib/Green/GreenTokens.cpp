//===- GreenTokens.cpp - Green Token Interface ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Token interface used exclusively by
//  the Green Language.
//
//===----------------------------------------------------------------------===//

#include "clang/Green/GreenTokens.h"

#include <iostream>

namespace green
{
  char const* display_name(token_kind k)
  {
    switch (k) {
    case tok_eof: return "end-of-file";
#define def_token(K) \
    case tok_ ## K: return # K;
#include "clang/Green/GreenTokens.def"
    case tok_invalid: return "invalid";
    }
    assert(false);
  }

  bool has_unique_spelling(token_kind k)
  {
    switch (k)
    {
    case tok_newline: return true;
    case tok_separator: return true;
    case tok_indent: return true;
    case tok_dedent: return true;
#define def_puncop(K, S) \
    case tok_ ## K: return true;
#include "clang/Green/GreenTokens.def"
    default: return false;
    }
  }

  bool has_multiple_spellings(token_kind k)
  {
    return !has_unique_spelling(k);
  }

  char const* spelling(token_kind k)
  {
    switch (k)
    {
    case tok_eof: return "end-of-file";
    case tok_unknown: return "unknown";
    case tok_indent: return "indent";
    case tok_dedent: return "dedent";
    case tok_separator: return "separator";
#define def_puncop(K, S) \
    case tok_ ## K: return S;
#include "clang/Green/GreenTokens.def"
    case tok_identifier: return "identifier";
    case tok_binary_integer: return "binary-integer";
    case tok_decimal_integer: return "decimal-integer";
    case tok_hexadecimal_integer: return "hexadecimal-integer";
    case tok_decimal_float: return "decimal-float";
    case tok_hexadecimal_float: return "hexadecimal-float";
    case tok_character: return "character";
    case tok_string: return "string";
    case tok_invalid: return "invalid";
    default: break;
    }
    assert(false);
  }

  template<std::size_t N>
  static constexpr std::size_t string_literal_length(char const (&arr)[N])
  {
    return N - 1;
  }

  std::size_t token_length(token_kind k)
  {
    switch (k) {
    default: return 0;
    case tok_unknown: return 1;
#define def_puncop(K, S) \
    case tok_ ## K: return string_literal_length(S);
#include "clang/Green/GreenTokens.def"
    }
  }

  void token::dump() const
  {
    dump(std::cerr);
  }

  void token::dump(std::ostream& os, bool nl) const
  {
    os << '<';
    os << display_name(kind());
    if (*this && has_multiple_spellings(kind()))
      os << ':' << spelling();
    os << '>';
    if (nl)
      os << '\n';
  }

} // namespace green
