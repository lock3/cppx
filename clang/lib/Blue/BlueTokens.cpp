//===- BlueTokens.cpp - Blue Token Implementation -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines Token functions.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueTokens.h"

#include <iostream>

namespace blue {

char const* getDisplayName(TokenKind K) {
  switch (K) {
#define def_token(TK) \
  case tok::TK: return # TK;
#include "clang/Green/Tokens.def"
  }
  assert(false);
}

bool hasUniqueSpelling(TokenKind K) {
  switch (K) {
  case tok::Newline: return true;
#define def_puncop(TK, S) \
  case tok::TK: return true;
#include "clang/Green/Tokens.def"
  default: return false;
  }
}

bool hasMultipleSpellings(TokenKind K) {
  return !hasUniqueSpelling(K);
}

const char* getSpelling(TokenKind K) {
  switch (K) {
  case tok::EndOfFile: return "end-of-file";
  case tok::Unknown: return "unknown";
#define def_puncop(TK, S) \
  case tok::TK: return S;
#include "clang/Green/Tokens.def"
  case tok::Identifier: return "identifier";
  case tok::BinaryInteger: return "binary-integer";
  case tok::DecimalInteger: return "decimal-integer";
  case tok::HexadecimalInteger: return "hexadecimal-integer";
  case tok::DecimalFloat: return "decimal-float";
  case tok::HexadecimalFloat: return "hexadecimal-float";
  case tok::Character: return "quoted-character";
  case tok::String: return "quoted-string";
  case tok::Invalid: return "invalid";
  default: break;
  }
  assert(false);
}

template<std::size_t N>
static constexpr std::size_t getSize(char const (&A)[N]) {
  return N - 1;
}

std::size_t getTokenLength(TokenKind K) {
  switch (K) {
  default: return 0;
  case tok::Unknown: return 1;
#define def_puncop(TK, S) \
  case tok::TK: return getSize(S);
#include "clang/Green/Tokens.def"
  }
}

llvm::StringRef Token::getSpelling() const {
  // FIXME: Generate a spelling for fused tokens? This would probably
  // need to be a std::string instead of a character pointer.
  assert(!isFused());
  return Sym.data();
}

void Token::dump() const {
  dump(std::cerr);
}

void Token::dump(std::ostream& OS, bool Nl) const {
  OS << '<';
  OS << getDisplayName(getKind());
  if (*this && hasMultipleSpellings(getKind()))
    OS << ':' << getSpelling().data();
  OS << '>';
  if (Nl)
    OS << '\n';
}

} // namespace blue
