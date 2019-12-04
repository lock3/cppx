//===- GreenTokens.h - Green Token Interface ------------------------------===//
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

#ifndef CLANG_GREEN_GREENTOKENS_H
#define CLANG_GREEN_GREENTOKENS_H

#include "clang/Green/Tokens.h"

#include <array>
#include <iosfwd>

namespace green
{
  /// Set if the token is the first token of a new line.
  constexpr token_flags tf_starts_line = tf_last << 1;

  /// Kinds of tokens.
  enum token_kind : unsigned short
  {
    tok_eof = eof_token_kind,
#define def_token(K) \
    tok_ ## K,
#include "GreenTokens.def"
    tok_invalid = invalid_token_kind,
  };

  /// Returns a printable version of the token name.
  char const* display_name(token_kind k);

  /// Returns true if the token name has a unique spelling.
  bool has_unique_spelling(token_kind k);

  /// Returns true if the token name has multiple spellings (e.g., identifier).
  bool has_multiple_spellings(token_kind k);

  /// Returns a spelling of the token name. If a token has multiple spellings,
  /// this returns its grammatical name.
  char const* spelling(token_kind k);

  /// Returns the length of a token of kind `k` or 0 if there are multiple
  /// or zero spellings of the token.
  std::size_t token_length(token_kind k);

  // Token class

  /// A token (or other lexical unit) of the language. Note that space,
  /// comments, and preprocessing symbols are considered tokens, although
  /// many are removed or transformed in various stages of translation.
  struct token : ::token
  {
    /// Constructs an end-of-file token with no location. This is typically
    /// used to return unmatched tokens in the parser.
    token()
      : ::token()
    { }

    /// Constructs an end-of-file token for the given source location. This is
    /// used by the lexer to construct a valid endpoint for a source file.
    token(clang::SourceLocation loc)
      : ::token(tok_eof, loc, symbol())
    { }

    /// Constructs a token.
    token(token_kind k, clang::SourceLocation loc, symbol sym)
      : ::token(k, loc, sym)
    { }

    token(token_kind k, clang::SourceLocation BeginLoc,
          clang::SourceLocation EndLoc, symbol sym)
      : ::token(k, BeginLoc, EndLoc, sym)
    { }

    token_kind kind() const
    {
      return static_cast<token_kind>(::token::kind);
    }

    bool starts_line() const
    {
      return flags & tf_starts_line;
    }

    bool isUnknown() const {
      return has_kind(tok_unknown);
    }

    bool is_newline() const
    {
      return has_kind(tok_newline);
    }

    bool is_space() const
    {
      return has_kind(tok_space);
    }

    bool is_comment() const
    {
      return has_kind(tok_line_comment) || has_kind(tok_block_comment);
    }

    void dump() const;
    void dump(std::ostream& os, bool nl = true) const;
  };

  /// Returns a human-readable name for the token. For simple tokens,
  /// this is simply its spelling. Otherwise, it is the grammatical name
  /// of the token.
  char const* readable_name(token const& tok);

} // namespace green

#endif

