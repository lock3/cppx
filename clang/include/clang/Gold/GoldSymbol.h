//===- GoldSymbol.h - Symbol Interface ------------------------------------===//
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

#ifndef CLANG_GOLD_SYMBOL_H
#define CLANG_GOLD_SYMBOL_H

#include <cassert>
#include <iosfwd>
#include <string>

namespace gold {

/// A unique string in the language, or an empty string.
class Symbol {
public:
  static Symbol Empty;

  explicit Symbol() : Ptr(Empty.Ptr) {
    assert(Ptr);
  }

  explicit Symbol(std::string const* S) : Ptr(S) {
    assert(S);
  }

  char const* data() const {
    return Ptr->data();
  }

  std::size_t size() const {
    return Ptr->size();
  }

  std::string const& str() const {
    return *Ptr;
  }

  friend bool operator==(Symbol a, Symbol b) {
    return a.Ptr == b.Ptr;
  }

  friend bool operator!=(Symbol a, Symbol b) {
    return a.Ptr != b.Ptr;
  }

private:
  /// A pointer to a unique string.
  std::string const* Ptr;
};

/// Returns a symbol for `str`.
Symbol getSymbol(char const* str);

/// Returns a symbol for the characters in `[first, last)`.
Symbol getSymbol(char const* first, char const* last);

/// Returns a symbol for the characters in `[str, str + n)`.
Symbol getSymbol(char const* str, std::size_t n);

/// Returns a symbol for `str`.
Symbol getSymbol(std::string const& str);

// Streaming

std::ostream& operator<<(std::ostream& os, Symbol sym);

} // namespace gold

#endif
