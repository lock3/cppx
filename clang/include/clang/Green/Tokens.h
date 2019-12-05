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

#include "clang/Green/Symbol.h"
#include "clang/Basic/SourceLocation.h"

#include <iosfwd>

namespace green {

namespace tok {
/// Kinds of tokens.
enum TokenKind : unsigned short {
#define def_token(K) \
  K,
#include "clang/Green/Tokens.def"
};
} // namespace tok

using TokenKind = tok::TokenKind;

/// Returns a printable version of the token name.
char const* getDisplayName(TokenKind k);

/// Returns true if the token name has a unique spelling.
bool hasUniqueSpelling(TokenKind k);

/// Returns true if the token name has multiple spellings (e.g., identifier).
bool hasMultipleSpellings(TokenKind k);

/// Returns a spelling of the token name. If a token has multiple spellings,
/// this returns its grammatical name.
char const* getSpelling(TokenKind k);

/// Returns the length of a token of kind `k` or 0 if there are multiple
/// or zero spellings of the token.
std::size_t getTokenLength(TokenKind k);

/// Properties of tokens.
enum TokenFlags : unsigned short {
  /// No flags are set.
  TF_None = 0x0,

  /// The token is comprised of other tokens.
  TF_Fused = 0x01,

  /// Set if the token is the first token of a new line.
  TF_StartsLine = 0x02,
};

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
struct Token
{
  /// Constructs an invalid token.
  Token()
    : Kind(tok::Invalid), Flags(), Loc(), Ptr()
  { }

  /// Constructs a normal token.
  Token(TokenKind K, clang::SourceLocation Loc, symbol Sym)
    : Kind(K), Flags(), Loc(Loc), Sym(Sym)
  { }

  /// Constructs a fused token.
  template<typename T>
  Token(TokenKind K, clang::SourceLocation Loc, T* Data)
    : Kind(K), Flags(TF_Fused), Loc(Loc), Ptr(Data)
  { }

  /// True if the token is neither invalid nor end-of-file.
  explicit operator bool() const {
    return !isInvalid() && !isEndOfFile();
  }

  TokenKind getKind() const {
    return static_cast<TokenKind>(Kind);
  }

  bool hasKind(TokenKind K) const {
    return getKind() == K;
  }

  bool isEndOfFile() const {
    return hasKind(tok::EndOfFile);
  }

  bool isInvalid() const {
    return hasKind(tok::Invalid);
  }

  bool isUnknown() const {
    return hasKind(tok::Unknown);
  }

  bool isNewline() const {
    return hasKind(tok::Newline);
  }

  bool isSpace() const {
    return hasKind(tok::Space);
  }

  bool isComment() const {
    return hasKind(tok::LineComment) || hasKind(tok::BlockComment);
  }

  bool isFused() const {
    return Flags & TF_Fused;
  }

  bool isAtStartOfLine() const {
    return Flags & TF_StartsLine;
  }

  clang::SourceLocation getLocation() const {
    return Loc;
  }

  symbol getSymbol() const {
    assert(!isFused());
    return Sym;
  }

  bool hasSpelling(char const* Str) const {
    return getSymbol() == get_symbol(Str);
  }

  /// Returns the spelling of the token.
  char const* getSpelling() const;

  /// Returns a human-readable name for the token. For simple tokens,
  /// this is simply its spelling. Otherwise, it is the grammatical name
  /// of the token.
  char const* getRedableName();

  void dump() const;
  void dump(std::ostream& os, bool Nl = true) const;

  /// The kind of token.
  unsigned short Kind;

  /// A small set of flags associated with a token.
  unsigned short Flags;

  /// The location of the token.
  clang::SourceLocation Loc;

  /// Data associated with the token.
  union
  {
    // For non-fused tokens, this is its underlying spelling (or lexeme).
    symbol Sym;

    // For fused tokens, this is a pointer to its associated data.
    void* Ptr;
  };
};

} // namespace green

#endif
