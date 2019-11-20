//===- Symbol.h - Symbol Interface ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the symbol interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_SYMBOL_H
#define CLANG_GREEN_SYMBOL_H

#include <cassert>
#include <iosfwd>
#include <string>

/// A unique string in the language, or an empty string.
struct symbol
{
  static symbol empty;

  explicit symbol()
    : str(empty.str)
  {
    assert(str);
  }

  explicit symbol(std::string const* s)
    : str(s)
  {
    assert(s);
  }

  char const* data() const
  {
    return str->data();
  }

  std::size_t size() const
  {
    return str->size();
  }

  std::string const* str;
};

inline
bool operator==(symbol a, symbol b)
{
  return a.str == b.str;
}

inline
bool operator!=(symbol a, symbol b)
{
  return a.str != b.str;
}

/// Returns a symbol for `str`.
symbol get_symbol(char const* str);

/// Returns a symbol for the characters in `[first, last)`.
symbol get_symbol(char const* first, char const* last);

/// Returns a symbol for the characters in `[str, str + n)`.
symbol get_symbol(char const* str, std::size_t n);

/// Returns a symbol for `str`.
symbol get_symbol(std::string const& str);

// Streaming

std::ostream& operator<<(std::ostream& os, symbol sym);

#endif
