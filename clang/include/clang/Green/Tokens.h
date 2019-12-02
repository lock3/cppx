//===- Tokens.h - Lock3 Token Interface -----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the base class for Tokens used in Lock3 compilers.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_TOKENS_H
#define CLANG_GREEN_TOKENS_H

#include "clang/Basic/SourceLocation.h"

#include "clang/Green/Symbol.h"

using token_flags = unsigned short;

/// No flags are set.
constexpr token_flags  tf_none = 0x0;

/// The token is fused; it is comprised of other tokens).
constexpr token_flags  tf_fused = 0x01;

/// The last common token flag. Derived languages can use bits above
/// this value by left-shifting `n` past `tf_last`.
constexpr token_flags tf_last = tf_fused;

/// The valid of for end-of-file tokens.
constexpr unsigned short eof_token_kind = 0;

/// The valid of for invalid tokens.
constexpr unsigned short invalid_token_kind = -1;

/// A token represents a symbol in the language, the end of file, or an
/// invalid construct. Tokens are discriminated by their kind.
///
/// The kind value 0 designates the end-of-file, which should typically have
/// a valid source location. A lexer yields the end-of-file token when there
/// are no more significant symbols in the input.
///
/// The kind value -1 designates an invalid token, which does not have a valid
/// source location. A lexer should never yield invalid tokens to a parser,
/// although they may be internally created to represent error conditions
/// in the lexer, or to represent unmatched symbols in the parser.
///
/// A fused token is one that is comprised of other tokens or symbols in
/// the language. For example...
///
/// \todo Only tokens whose patterns generate a set of strings need to store
/// the symbol. We could reuse that storage for other properties. On the other
/// hand, it's convenient to store the spelling in general.
struct token
{
  /// Constructs an invalid token.
  token()
    : kind(-1), flags(), loc(), ptr()
  { }

  /// Constructs a normal token.
  token(unsigned k, clang::SourceLocation L, symbol s)
    : kind(k), flags(), loc(L), EndLoc(L), sym(s)
  { }

  token(unsigned k, clang::SourceLocation L, clang::SourceLocation E, symbol s)
    : kind(k), flags(), loc(L), EndLoc(E), sym(s)
  { }

  template<typename T>
  token(unsigned k, clang::SourceLocation L, T* data)
    : kind(k), flags(tf_fused), loc(L), EndLoc(L), ptr(data)
  { }

  template<typename T>
  token(unsigned k, clang::SourceLocation L, clang::SourceLocation E, T* data)
    : kind(k), flags(tf_fused), loc(L), EndLoc(E), ptr(data)
  { }

  /// True if the token is neither invalid nor end-of-file.
  explicit operator bool() const
  {
    return !is_invalid() && !is_eof();
  }

  bool has_kind(unsigned k) const
  {
    return kind == k;
  }

  bool is_eof() const
  {
    return has_kind(eof_token_kind);
  }

  bool is_invalid() const
  {
    return has_kind(invalid_token_kind);
  }

  bool has_spelling(char const* str) const
  {
    return sym == get_symbol(str);
  }

  char const* spelling() const;

  bool is_fused() const
  {
    return flags & tf_fused;
  }

  /// The kind of token.
  unsigned short kind;

  /// A small set of flags associated with a token.
  unsigned short flags;

  /// The location of the token.
  clang::SourceLocation loc;

  // FIXME: Remove this.
  clang::SourceLocation EndLoc;

  /// Data associated with the token.
  union
  {
    // For non-fused tokens, this is its underlying spelling (or lexeme).
    symbol sym;

    // For fused tokens, this is a pointer to its associated data.
    void* ptr;
  };
};

#endif
