//===- BlueLexer.cpp - Blue Language Lexer Implementation -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Blue Lexer interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticLex.h"
#include "clang/Basic/SourceManager.h"

#include "clang/Blue/BlueLexer.h"

#include <iostream>
#include <stdexcept>
#include <string>

namespace blue {

namespace {

bool isSpace(char C) {
  return C == ' ' || C == '\t' || C == '\r' || C == '\v' || C == '\f';
}

bool isNewline(char C) {
  return C == '\n';
}

bool isIdentifierStart(char C) {
  return std::isalpha(C) || C == '_';
}

bool isIdentifierRest(char C) {
  return std::isalpha(C) || std::isdigit(C) || C == '_';
}

bool isDecimalDigit(char C) {
  return std::isdigit(C);
}

bool isHexadecimalDigit(char C) {
  return std::isxdigit(C);
}

bool isDecimalExponent(char C) {
  return C == 'e' || C == 'E';
}

bool isSign(char C) {
  return C == '+' || C == '-';
}

/// The byte order mark.
char BOM[] = {'\xef', '\xbe', '\xbb'};

} // namespace

Lexer::Lexer(clang::SourceManager &SM, File const& F)
  : Input(&F), First(F.getText().data()), Last(First + F.getText().size()),
    Line(1), Column(1), SM(SM) {
  // Bypass the byte order mark if present.
  //
  // FIXME: Check for UTF-16 and UTF-32 byte order marks, and probably
  // diagnose those as ill-formed (or optionally convert the file).
  if (std::equal(BOM, BOM + 3, First, Last))
    First += 3;
}

Token Lexer::operator()() {
  while (!isDone()) {
    // Establish this point as the start of the current token.
    StartingPosition Pos(*this);

    switch (getLookahead()) {
    case ' ':
    case '\t':
    case '\r':
    case '\v':
    case '\f':
      // Ignore space (including carriage returns).
      matchSpace();
      continue;

    case '\n':
      matchNewline();
      continue;

    case '(':
      return matchToken(tok::LeftParen);

    case ')':
      return matchToken(tok::RightParen);

    case '[':
      return matchToken(tok::LeftBracket);

    case ']':
      return matchToken(tok::RightBracket);

    case '{':
      return matchToken(tok::LeftBrace);

    case '}':
      return matchToken(tok::RightBrace);

    case ':':
      return matchToken(tok::Colon);

    case ';':
      return matchToken(tok::Semicolon);

    case ',':
      return matchToken(tok::Comma);

    case '.':
      if (isDecimalDigit(getLookahead(1)))
        return matchDecimalNumber();
      return matchToken(tok::Dot);

    case '?':
      return matchToken(tok::Question);

    case '+':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::PlusEqual);
      return matchToken(tok::Plus);

    case '-':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::MinusEqual);
      if (nthCharacterIs(1, '>'))
        return matchToken(tok::MinusGreater);
      return matchToken(tok::Minus);

    case '*':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::StarEqual);
      return matchToken(tok::Star);

    case '/':
      if (nthCharacterIs(1, '/')) {
        matchLineComment();
        continue;
      }
      if (nthCharacterIs(1, '*')) {
        matchBlockComment();
        continue;
      }
      if (nthCharacterIs(1, '='))
        return matchToken(tok::SlashEqual);
      return matchToken(tok::Slash);

    case '%':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::PercentEqual);
      return matchToken(tok::Percent);

    case '&':
      if (nthCharacterIs(1, '&'))
        return matchToken(tok::AmpersandAmpersand);
      return matchToken(tok::Ampersand);

    case '|':
      if (nthCharacterIs(1, '|'))
        return matchToken(tok::BarBar);
      return matchToken(tok::Bar);

    case '<':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::LessEqual);
      if (nthCharacterIs(1, '<'))
        return matchToken(tok::LessLess);
      return matchToken(tok::Less);

    case '>':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::GreaterEqual);
      if (nthCharacterIs(1, '>'))
        return matchToken(tok::GreaterGreater);
      return matchToken(tok::Greater);

    case '~':
      return matchToken(tok::Tilde);

    case '=':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::EqualEqual);
      else if (nthCharacterIs(1, '>'))
        return matchToken(tok::EqualGreater);
      return matchToken(tok::Equal);

    case '!':
      if (nthCharacterIs(1, '='))
        return matchToken(tok::BangEqual);
      return matchToken(tok::Bang);

    case '\'':
      return matchCharacter();

    case '"':
      return matchString();

    case '0':
      if (nthCharacterIs(1, 'x') || nthCharacterIs(1, 'X'))
        return matchHexadecimalNumber();
      LLVM_FALLTHROUGH;

    default:
      if (isIdentifierStart(getLookahead()))
        return matchWord();
      if (isDecimalDigit(getLookahead()))
        return matchNumber();

      // Return an unknown token.
      //
      // FIXME: This could be an ill-formed UTF-8 character. We should
      // detect and identify those characters.
      return matchToken(tok::Unknown);
    }
  }

  return matchEof();
}

void Lexer::matchSpace() {
  consumeCharacter();
  while (isSpace(getLookahead()))
    consumeCharacter();
}

void Lexer::matchNewline() {
  assert(isNewline(getLookahead()));
  ++First;
  ++Line;
  Column = 1;
}

void Lexer::matchLineComment() {
  consumeCharacters(2); // Consume the '//' characters.
  while (!nextCharacterIs('\n'))
    consumeCharacter();
}

void Lexer::matchBlockComment() {
  consumeCharacters(2); // Consume the '/*' characters.
  while (nextCharacterIsNot('*') && nthCharacterIsNot(1, '/')) {
    char c = getLookahead();
    consumeCharacter();
    if (isNewline(c)) {
      ++Line;
      Column = 1;
    }
  }

  if (isDone()) {
    getDiagnostics().Report(getInputLocation(),
                            clang::diag::err_unterminated_block_comment);
  }

  consumeCharacters(2); // Consume the '*/' characters.
}

Token Lexer::makeToken(TokenKind K, char const* F, char const* L) {
  return makeToken(K, F, L - F);
}

Token Lexer::makeToken(TokenKind K, char const* S, std::size_t N) {
  clang::SourceLocation Loc = getSourceLocation(S);
  Symbol Sym = getSymbol(S, N);
  return Token(K, Loc, Sym);
}

Token Lexer::matchEof() {
  clang::SourceLocation Loc = SM.getLocForEndOfFile(Input->getID());
  return Token(tok::EndOfFile, Loc, Symbol());
}

Token Lexer::matchToken(TokenKind K) {
  std::size_t Len = getTokenLength(K);
  consumeCharacters(Len);
  return makeToken(K, Start, Len);
}

Token Lexer::matchWord() {
  assert(isIdentifierStart(getLookahead()));
  consumeCharacter();
  while (isIdentifierRest(getLookahead()))
    consumeCharacter();

  // FIXME: This could be a keyword.
  return makeToken(tok::Identifier, Start, First);
}

Token Lexer::matchNumber() {
  // FIXME: The specification also allows for hex ASCII (ish?) and
  // Unicode hex literals.
  if (getLookahead(0) == '0') {
    if (getLookahead(1) == 'x')
      return matchHexadecimalNumber();
  }
  return matchDecimalNumber();
}

Token Lexer::matchDecimalNumber() {
  if (nextCharacterIs('.')) {
    // Matches '. decimal-digit-seq ...'
    return matchDecimalFraction();
  } else {
    // Matches 'decimal-digit-seq [. decimal-digit-seq] ...]'
    matchDecimalDigitSeq();
    if (nextCharacterIs('.')) {
      if (isDecimalDigit(getLookahead()))
        return matchDecimalFraction();

      if (isDecimalExponent(getLookahead()))
        return matchDecimalExponent();

      consumeCharacter();
      return makeToken(tok::DecimalFloat, Start, First);
    }
  }

  // Matches 'decimal-digit-seq (e|E) ...'
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();

  return makeToken(tok::DecimalInteger, Start, First);
}

Token Lexer::matchDecimalFraction() {
  requireCharacter('.');
  matchDecimalDigitSeq();
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();
  return makeToken(tok::DecimalFloat, Start, First);
}

Token Lexer::matchDecimalExponent() {
  requireCharacterIf(isDecimalExponent);
  matchCharacterIf(isSign);
  // FIXME: There could be an error here.
  matchDecimalDigitSeq();
  return makeToken(tok::DecimalFloat, Start, First);
}

Token Lexer::matchHexadecimalNumber() {
  consumeCharacters(2); // Matches '0x'.

  // FIXME: Match hex floats?
  // FIXME: Wrong error.
  if (!isHexadecimalDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(),
                           clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchHexadecimalDigitSeq();

  return makeToken(tok::HexadecimalInteger, Start, First);
}

Token Lexer::matchCharacter() {
  assert(nextCharacterIs('\''));

  consumeCharacter(); // '\''
  while (nextCharacterIsNot('\'')) {
    // Diagnose newlines, but continue lexing the token.
    if (isNewline(getLookahead())) {
      // FIXME: Diagnose this error.
      assert(false && "Newline in character literal");
      consumeCharacter();
    }

    if (nextCharacterIs('\\')) {
      matchEscapeSequence();
      continue;
    }

    consumeCharacter();
  }

  if (isDone()) {
      // FIXME: Note the start of the character.
      getDiagnostics().Report(
        getInputLocation(), clang::diag::ext_unterminated_char_or_string);
    return matchEof();
  }

  consumeCharacter(); // '\''

  return makeToken(tok::Character, Start, First);
}

Token Lexer::matchString() {
  assert(nextCharacterIs('"'));

  consumeCharacter(); // '"'
  while (nextCharacterIsNot('"')) {
    // Diagnose newlines, but continue lexing the token.
    if (isNewline(getLookahead())) {
      // FIXME: Diagnose this error.
      assert(false && "Newline in character literal");
      consumeCharacter();
    }

    if (nextCharacterIs('\\')) {
      matchEscapeSequence();
      continue;
    }

    consumeCharacter();
  }

  if (isDone()) {
    // FIXME: Note the start of the string.
    getDiagnostics().Report(
      getInputLocation(), clang::diag::ext_unterminated_char_or_string);
    return matchEof();
  }

  consumeCharacter(); // '"'

  return makeToken(tok::String, Start, First);
}

void Lexer::matchEscapeSequence() {
  consumeCharacter(); // '\\'
  switch (getLookahead()) {
  case 'r':
  case 'n':
  case 't':
  case '\'':
  case '"':
    consumeCharacter();
    break;

  default:
    // FIXME: Emit an error!
    // error (getInputLocation(), "invalid escape character '{}'",
    //        std::to_string(getLookahead()));
    assert(false && "Invalid escape sequence");
    consumeCharacter();
    break;
  }
}

void Lexer::matchDecimalDigitSeq() {
  // FIXME: Allow digit separators?

  if (!matchCharacterIf(isDecimalDigit)) {
    // FIXME: Wrong error.
    getDiagnostics().Report(getInputLocation(), clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid number");s
    return;
  }

  while (nextCharacterMatches(isDecimalDigit))
    consumeCharacter();
}

void Lexer::matchDecimalDigitSeqOpt() {
  if (isDecimalDigit(getLookahead()))
    matchDecimalDigitSeq();
}

void Lexer::matchHexadecimalDigitSeq() {
  // FIXME: Allow digit separators?
  requireCharacterIf(isHexadecimalDigit);
  while (nextCharacterMatches(isHexadecimalDigit))
    consumeCharacter();
}

clang::SourceLocation Lexer::getSourceLocation(char const* Loc) {
  llvm::StringRef Buf = Input->getText();
  return SM.getComposedLoc(Input->getID(), Loc - Buf.data());
}

} // namespace blue
