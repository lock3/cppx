//===- GreenLexer.cpp - Green Language Lexer ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenLexer interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticLex.h"
#include "clang/Basic/SourceManager.h"

#include "clang/Green/GreenLexer.h"

#include <iostream>
#include <stdexcept>
#include <string>

namespace green {

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
};

bool isHexadecimalDigit(char C) {
  return std::isxdigit(C);
};

bool isDecimalExponent(char C) {
  return C == 'e' || C == 'E';
};

bool isSign(char C) {
  return C == '+' || C == '-';
};

/// The byte order mark.
char BOM[] = {'\xef', '\xbe', '\xbb'};

} // namespace

CharacterScanner::CharacterScanner(clang::SourceManager &SM, File const& F)
  : Input(&F),
    First(F.getText().data()),
    Last(First + F.getText().size()),
    Line(1),
    Column(1),
    SM(SM) {
  // Bypass the byte order mark.
  if (std::equal(BOM, BOM + 3, First, Last))
    First += 3;
}

token CharacterScanner::operator()() {
  while (!isDone())
  {
    // Establish this point as the start of the current token.
    StartingPosition pos(*this);

    switch (getLookahead()) {
    case ' ':
    case '\t':
      return matchSpace();

    case '\n':
      // FIXME: Handle the optional \r\n.
      return matchNewline();

    case '#':
      return matchLineComment();

    case '(':
      return matchToken(tok_left_paren);
    case ')':
      return matchToken(tok_right_paren);
    case '[':
      return matchToken(tok_left_bracket);
    case ']':
      return matchToken(tok_right_bracket);
    case '{':
      return matchToken(tok_left_brace);
    case '}':
      return matchToken(tok_right_brace);
    case ':':
      return matchToken(tok_colon);
    case ';':
      return matchToken(tok_semicolon);
    case ',':
      return matchToken(tok_comma);

    case '.':
      if (isDecimalDigit(getLookahead(1)))
        return matchDecimalNumber();
      return matchToken(tok_dot);

    case '?':
      return matchToken(tok_question);
    case '+':
      if (getLookahead(1) == '=')
        return matchToken(tok_plus_equal);
      return matchToken(tok_plus);

    case '-':
      if (getLookahead(1) == '=')
        return matchToken(tok_minus_equal);
      if (getLookahead(1) == '>')
        return matchToken(tok_minus_greater);
      return matchToken(tok_minus);

    case '*':
      if (getLookahead(1) == '=')
        return matchToken(tok_star_equal);
      return matchToken(tok_star);

    case '/':
      if (getLookahead(1) == '=')
        return matchToken(tok_slash_equal);
      return matchToken(tok_slash);

    case '%':
      if (getLookahead(1) == '=')
        return matchToken(tok_percent_equal);
      return matchToken(tok_percent);

    case '&':
      if (getLookahead(1) == '&')
        return matchToken(tok_ampersand_ampersand);
      return matchToken(tok_ampersand);

    case '|':
      if (getLookahead(1) == '|')
        return matchToken(tok_bar_bar);
      return matchToken(tok_bar);

    case '<':
      if (getLookahead(1) == '#')
        return matchBlockComment();
      else if (getLookahead(1) == '=')
        return matchToken(tok_less_equal);
      else if (getLookahead(1) == '>')
        return matchToken(tok_less_greater);
      return matchToken(tok_less);

    case '>':
      if (getLookahead(1) == '=')
        return matchToken(tok_greater_equal);
      return matchToken(tok_greater);

    case '~':
      return matchToken(tok_tilde);

    case '=':
      if (getLookahead(1) == '=')
        return matchToken(tok_equal_equal);
      else if (getLookahead(1) == '>')
        return matchToken(tok_equal_greater);
      return matchToken(tok_equal);

    case '!':
      if (getLookahead(1) == '=')
        return matchToken(tok_bang_equal);
      return matchToken(tok_bang);

    case '\'':
      return matchCharacter();
    case '"':
      return matchString();

    case '0':
      if (nthCharacterIs(1, 'x') || nthCharacterIs(1, 'X'))
        return matchHexadecimalNumber();
      if (nthCharacterIs(1, 'c'))
        return matchHexadecimalCharacter();
      if (nthCharacterIs(1, 'u'))
        return matchUnicodeCharacter();
      LLVM_FALLTHROUGH;

    default:
      if (isIdentifierStart(getLookahead()))
        return matchWord();
      if (isDecimalDigit(getLookahead()))
        return matchNumber();

      // Return an unknown token.
      //
      // FIXME: This could be an ill-formed UTF-8 character.
      return matchToken(tok_unknown);
    }
  }

  return matchEof();
}

token CharacterScanner::makeToken(token_kind K, char const* F, char const* L) {
  return makeToken(K, F, L - F);
}

token CharacterScanner::makeToken(token_kind K, char const* S, std::size_t N) {
  clang::SourceLocation Loc = getSourceLocation(S);
  symbol Sym = get_symbol(S, N);
  token Tok(K, Loc, Sym);

  // Update line flags.
  if (Column - N == 1)
    Tok.flags |= tf_starts_line;

  return Tok;
}

token CharacterScanner::matchEof() {
  // TODO: replace with getendoffile
  return token(getSourceLocation(First));
}

token CharacterScanner::matchSpace() {
  consume();
  while (isSpace(getLookahead()))
    consume();
  return makeToken(tok_space, Start, First);
}

token CharacterScanner::matchNewline() {
  assert(isNewline(getLookahead()));

  // FIXME: Handle the \r\n case.
  consume();
  token tok = makeToken(tok_newline, Start, 1);

  // Update the line and reset the column.
  ++Line;
  Column = 1;

  return tok;
}

token CharacterScanner::matchLineComment() {
  assert(nextCharacterIs('#'));
  consume();
  while (!isDone() && !isNewline(getLookahead()))
    consume();
  return makeToken(tok_line_comment, Start, First);
}

token CharacterScanner::matchBlockComment() {
  auto BeginLoc = getSourceLocation(First);
  consume(2); // '<#'
  while (!isDone() && nextCharacterIsNot('#') && nthCharacterIsNot(1, '>')) {
    char c = getLookahead();
    consume();
    if (isNewline(c)) {
      ++Line;
      Column = 1;
    }
  }
  if (isDone()) {
    // FIXME: We need a better diagnostic because this explicitly
    // refers to C-style block comments.
    getDiagnostics().Report(getInputLocation(),
                            clang::diag::err_unterminated_block_comment);
    return matchEof();
  }
  consume(2); // '#>'
  auto EndLoc = getSourceLocation(First);

  // Build the block comment.
  symbol Sym = get_symbol(Start, First);
  return token(tok_block_comment, BeginLoc, EndLoc, Sym);
}

token CharacterScanner::matchToken(token_kind K) {
  std::size_t Len = token_length(K);
  consume(Len);
  return makeToken(K, Start, Len);
}

token CharacterScanner::matchWord() {
  assert(isIdentifierStart(getLookahead()));
  consume();
  while (isIdentifierRest(getLookahead()))
    consume();
  return makeToken(tok_identifier, Start, First);
}

token CharacterScanner::matchNumber() {
  // FIXME: The specification also allows for hex ASCII (ish?) and
  // Unicode hex literals.
  if (getLookahead(0) == '0')
  {
    if (getLookahead(1) == 'x')
      return matchHexadecimalNumber();
  }
  return matchDecimalNumber();
}

token CharacterScanner::matchDecimalNumber() {
  if (nextCharacterIs('.'))
  {
    // Matches '. decimal-digit-seq ...'
    return matchDecimalFraction();
  } else {
    // Matches 'decimal-digit-seq [. decimal-digit-seq] ...]'
    matchDecimalDigitSeq();
    if (nextCharacterIs('.'))
    {
      if (isDecimalDigit(getLookahead()))
        return matchDecimalFraction();

      if (isDecimalExponent(getLookahead()))
        return matchDecimalExponent();

      consume();
      return makeToken(tok_decimal_float, Start, First);
    }
  }

  // Matches 'decimal-digit-seq (e|E) ...'
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();

  return makeToken(tok_decimal_integer, Start, First);
}

token CharacterScanner::matchDecimalFraction() {
  require('.');
  matchDecimalDigitSeq();
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();
  return makeToken(tok_decimal_float, Start, First);
}

token CharacterScanner::matchDecimalExponent() {
  requireIf(isDecimalExponent);
  matchIf(isSign);
  // FIXME: There could be an error here.
  matchDecimalDigitSeq();
  return makeToken(tok_decimal_float, Start, First);
}

token CharacterScanner::matchHexadecimalNumber() {
  consume(2); // Matches '0x'.

  // FIXME: Match hex floats?

  if (!isHexadecimalDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(), clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchHexadecimalDigitSeq();

  return makeToken(tok_hexadecimal_integer, Start, First);
}

token CharacterScanner::matchCharacter() {
  assert(nextCharacterIs('\''));

  consume(); // '\''
  while (!isDone() && nextCharacterIsNot('\''))
  {
    // Diagnose newlines, but continue lexing the token.
    if (isNewline(getLookahead())) {
      // error (getInputLocation(), "newline in character literal");
      consume();
    }

    if (nextCharacterIs('\\')) {
      matchEscapeSequence();
      continue;
    }

    // FIXME: Match the '{0cXX}' and '{0uXXXX}' cases. These are
    // interesting because the nested codes are other tokens. We could
    // leave these in place to be lexed later, or attach them to the
    // token in some interesting way. See comments in match_string also.

    consume();
  }

  if (isDone()) {
      // FIXME: Note the start of the character.
      getDiagnostics().Report(
        getInputLocation(), clang::diag::ext_unterminated_char_or_string);
    return matchEof();
  }
  consume(); // '\''

  return makeToken(tok_character, Start, First);
}

token CharacterScanner::matchString() {
  assert(nextCharacterIs('"'));

  consume(); // '"'
  while (!isDone() && nextCharacterIsNot('"')) {
    // Diagnose newlines, but continue lexing the token.
    if (isNewline(getLookahead())) {
      // error (getInputLocation(), "newline in string literal");
      consume();
    }

    if (nextCharacterIs('\\')) {
      matchEscapeSequence();
      continue;
    }

    // FIXME: Match nested tokens in '{ ... '}'. We're going to have to
    // do something pretty interesting for string tokens (i.e., storing
    // interpolation ranges in a side buffer somewhere so we don't copy
    // dynamic objects).

    consume();
  }
  if (isDone()) {
    // FIXME: Note the start of the character.
    getDiagnostics().Report(
      getInputLocation(), clang::diag::ext_unterminated_char_or_string);
    return matchEof();
  }
  consume(); // '"'

  return makeToken(tok_string, Start, First);
}

void CharacterScanner::matchEscapeSequence() {
  consume(); // '\\'
  switch (getLookahead())
  {
  case 'r':
  case 'n':
  case 't':
  case '\'':
  case '"':
  case '{':
  case '}':
  case '<':
  case '>':
  case '&':
  case '~':
  case '#':
    consume();
    break;

  default:
    // FIXME: Emit an error!
    // error (getInputLocation(), "invalid escape character '{}'",
    //        std::to_string(getLookahead()));
    consume();
    break;
  }
}

token CharacterScanner::matchHexadecimalCharacter() {
  // sorry(getInputLocation(), "hexadecimal characters not supported");
  return {};
}

token CharacterScanner::matchUnicodeCharacter() {
  // sorry(getInputLocation(), "unicode characters not supported");
  return {};
}

void CharacterScanner::matchDecimalDigitSeq() {
  // FIXME: Allow digit separators?

  if (!matchIf(isDecimalDigit)) {
    // FIXME: Wrong error.
    getDiagnostics().Report(getInputLocation(), clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid number");s
    return;
  }

  while (matchIf(isDecimalDigit))
    ;
}

void CharacterScanner::matchDecimalDigitSeqOpt() {
  if (isDecimalDigit(getLookahead()))
    matchDecimalDigitSeq();
}

void CharacterScanner::matchHexadecimalDigitSeq() {
  // FIXME: Allow digit separators?
  requireIf(isHexadecimalDigit);
  while (matchIf(isHexadecimalDigit))
    consume();
}

clang::SourceLocation
CharacterScanner::getSourceLocation(char const* Loc) {
  llvm::StringRef Buf = Input->getText();
  return SM.getComposedLoc(Input->getID(), Loc - Buf.data());
}

// Line scanner

token LineScanner::operator()() {
  token tok;
  bool startsLine = false;
  while (true) {
    tok = Scanner();

    // Space at the beginning of a line cannot be discarded here.
    if (tok.is_space() && tok.starts_line())
      break;

    // Propagate a previous line-start flag to this next token.
    if (startsLine) {
      tok.flags |= tf_starts_line;
      startsLine = false;
    }

    // Empty lines are discarded.
    if (tok.is_newline() && tok.starts_line())
      continue;

    // Errors, space, and comments are discardable. If a token starts a
    // line, the next token will become the new start of line.
    if (tok.is_invalid() || tok.is_space() || tok.is_comment()) {
      startsLine = tok.starts_line();
      continue;
    }

    // All other tokens are retained.
    break;
  };

  return tok;
}

// Block scanner

token BlockScanner::operator()() {
  // Get the next token, possibly one that we've buffered.
  token Tok;
  if (Lookahead)
    Tok = std::exchange(Lookahead, {});
  else
    Tok = Scanner();

  // Check for a newline followed by indentation.
  if (Tok.is_newline()) {
    token Next = Scanner();
    if (Next.is_space()) {
      // A newline followed by space is either an indent or a dedent.
      // For example:
      //
      //    if (x < y): <newline>
      //      stuff <newline>
      //
      // Combine the tokens to create the appropriate level of indentation.
      Tok = combineSpace(Tok, Next);
    } else {
      // At the top-level a newline followed by a token implies the
      // presence of a separator or dedent. For example:
      //
      //    x = 1 <newline>
      //    y = 2 <newline>
      //
      // For dedents, we might have this:
      //
      //    if (x < y): <newline>
      //      stuff <newline>
      //    more_stuff
      //
      // We want to replace the first newline token with a separator.
      // However, we have to preserve the token we just found.
      //
      // Note that the second newline is followed by eof, so we'd
      // probably want to do the same.
      Tok = combineSpace(Tok, {});

      // Buffer the next token for the next read.
      Lookahead = Next;
    }
  }

  assert(!Tok.is_newline());
  return Tok;
}

token BlockScanner::matchSeparator(token const& nl) {
  return token(tok_separator, nl.loc, nl.sym);
}

token BlockScanner::matchIndent(token const& nl) {
  return token(tok_indent, nl.loc, nl.sym);
}

token BlockScanner::matchDedent(token const& nl) {
  return token(tok_dedent, nl.loc, nl.sym);
}

static symbol getSymbol(token const& Tok) {
  return Tok.is_invalid() ? symbol() : Tok.sym;
}

/// True if `A` and `B` have the same spellings.
static bool equalSpelling(token const& A, token const& B) {
  return getSymbol(A) == getSymbol(B);
}

/// True if `sym` starts with `pre` (i.e., is lexicographically greater).
static bool startsWith(symbol Sym, symbol Pre) {
  return Sym.str->compare(*(Pre.str)) > 0;
}

/// True if the lexeme of `tok` has the lexeme of `pre` has a prefix.
static bool startsWith(token const& Tok, token const& Pre) {
  return startsWith(getSymbol(Tok), getSymbol(Pre));
}

token BlockScanner::combineSpace(token const& Nl, token const& NewIndent) {
  // Emit queued dedents.
  if (!Dedents.empty())
    return popDedent();

  token PrevIndent = currentIndentation();

  // If the indentations are the same, this is a separator. Note
  // that we replace the current prefix for diagnostics purposes.
  if (equalSpelling(NewIndent, PrevIndent)) {
    if (!Prefix.empty())
      Prefix.back() = NewIndent;
    return matchSeparator(Nl);
  }

  // If the new indentation starts with the previous (i.e., new is longer),
  // then indent.
  if (startsWith(NewIndent, PrevIndent)) {
    pushIndentation(NewIndent);
    return matchIndent(Nl);
  }

  // The previous indentation starts with the new (i.e., previous is longer),
  // then dendent. Note that this can entail multiple dedents: one for each
  // level that does not have the same spelling.
  if (startsWith(PrevIndent, NewIndent)) {
    do {
      popIndentation();
      PrevIndent = currentIndentation();
      pushDedent(matchDedent(Nl));
    } while (!Prefix.empty() && !equalSpelling(NewIndent, PrevIndent));

    // Update the location of the last indent for diagnostic purposes.
    if (!Prefix.empty())
      Prefix.back() = NewIndent;

    return popDedent();
  }

  // FIXME: Note the previous indentation depth.
  getDiagnostics().Report(NewIndent.loc, clang::diag::err_invalid_indentation);

  // Return a line separator just in case.
  return matchSeparator(Nl);
}

} // namespace green
