 //===- GoldLexer.cpp - Gold Language Lexer --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GoldLexer interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticLex.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringMap.h"

#include "clang/Lex/Token.h"
#include "clang/Lex/Preprocessor.h"

#include "clang/Gold/GoldLexer.h"
#include "clang/Gold/GoldSyntaxContext.h"

#include <iostream>
#include <stdexcept>
#include <string>

namespace gold {

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

bool isDigitSeparator(char C) {
  return C == '\'';
}

bool isHexadecimalDigit(char C) {
  return std::isxdigit(C);
}

bool isBinaryDigit(char C) {
  return C == '0' || C == '1';
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

CharacterScanner::CharacterScanner(clang::SourceManager &SM, File const &F,
                                   SyntaxContext &Ctx, clang::Preprocessor &PP)
  : Input(&F),
    First(F.getText().data()),
    Last(First + F.getText().size()),
    Line(1),
    Column(1),
    SM(SM),
    Ctx(Ctx),
    PP(PP)
  {
  // Bypass the byte order mark.
  if (std::equal(BOM, BOM + 3, First, Last))
    First += 3;

  // Build the keyword table.
#define def_keyword(K, S)                       \
  Keywords.try_emplace(S, tok::K ## Keyword);
#include "clang/Gold/GoldTokens.def"
}

Token CharacterScanner::operator()() {
  while (!isDone())
  {
    // Establish this point as the start of the current token.
    StartingPosition Pos(*this);

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
      else if (getLookahead(1) == '.') {
        if (getLookahead(2) == '.')
          return matchToken(tok::Ellipsis);
        return matchToken(tok::DotDot);
      } else if (getLookahead(1) == '^')
        return matchToken(tok::DotCaret);

      return matchToken(tok::Dot);

    case '?':
      return matchToken(tok::Question);
    case '+':
      if (getLookahead(1) == '=')
        return matchToken(tok::PlusEqual);
      return matchToken(tok::Plus);

    case '-':
      if (getLookahead(1) == '=')
        return matchToken(tok::MinusEqual);
      if (getLookahead(1) == '>')
        return matchToken(tok::MinusGreater);
      return matchToken(tok::Minus);

    case '*':
      if (getLookahead(1) == '=')
        return matchToken(tok::StarEqual);
      return matchToken(tok::Star);

    case '/':
      if (getLookahead(1) == '=')
        return matchToken(tok::SlashEqual);
      return matchToken(tok::Slash);

    case '%':
      if (getLookahead(1) == '=')
        return matchToken(tok::PercentEqual);
      return matchToken(tok::Percent);

    case '&':
      if (getLookahead(1) == '&')
        return matchToken(tok::AmpersandAmpersand);
      if (getLookahead(1) =='=')
        return matchToken(tok::AmpersandEqual);
      return matchToken(tok::Ampersand);

    case '|':
      if (getLookahead(1) == '|')
        return matchToken(tok::BarBar);
      if (getLookahead(1) =='=')
        return matchToken(tok::BarEqual);
      return matchToken(tok::Bar);

    case '<':
      if (getLookahead(1) == '#')
        return matchBlockComment();
      else if (getLookahead(1) == '=')
        return matchToken(tok::LessEqual);
      else if (getLookahead(1) == '>')
        return matchToken(tok::LessGreater);
      else if (getLookahead(1) == '<') {
        if (getLookahead(2) == '=')
          return matchToken(tok::LessLessEqual);
        else
          return matchToken(tok::LessLess);
      }
      return matchToken(tok::Less);

    case '>':
      if (getLookahead(1) == '=')
        return matchToken(tok::GreaterEqual);
      else if (getLookahead(1) == '>') {
        if (getLookahead(2) == '=')
          return matchToken(tok::GreaterGreaterEqual);
        else
          return matchToken(tok::GreaterGreater);
      }
      return matchToken(tok::Greater);

    case '~':
      return matchToken(tok::Tilde);
    case '^':
      if (getLookahead(1) == '=')
        return matchToken(tok::CaretEqual);
      return matchToken(tok::Caret);

    case '=':
      if (getLookahead(1) == '=')
        return matchToken(tok::EqualEqual);
      else if (getLookahead(1) == '>')
        return matchToken(tok::EqualGreater);
      return matchToken(tok::Equal);

    case '!':
      return matchToken(tok::Bang);

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
      if (nthCharacterIs(1, 'b'))
        return matchBinaryNumber();
      LLVM_FALLTHROUGH;

    default:
      if (isIdentifierStart(getLookahead()))
        return matchWord();
      if (isDecimalDigit(getLookahead()))
        return matchNumber();

      // Return an unknown token.
      //
      // FIXME: This could be an ill-formed UTF-8 character.
      return matchToken(tok::Unknown);
    }
  }

  return matchEof();
}

Token CharacterScanner::makeToken(TokenKind K, char const* F, char const* L) {
  assert(!Fused);
  return makeToken(K, F, L - F);
}

Token CharacterScanner::makeToken(TokenKind K, char const* F, char const* L,
                                  llvm::SmallVectorImpl<llvm::StringRef> &Suf) {
  assert(!Fused);
  Token Tok = makeToken(K, F, L - F);
  Tok.setSuffixes(Suf);
  return Tok;
}

static const llvm::StringMap<tok::FusionKind> FusionBases = {
  {"operator", tok::Operator},
  {"conversion", tok::Conversion},
  {"literal", tok::Literal},
};

Token CharacterScanner::makeFusedToken(Token Base, Token **Data,
                                       unsigned Count) {
  assert(Fused);
  auto It = FusionBases.find(Base.getSpelling());
  if (It == FusionBases.end()) {
    unsigned DiagID =
      getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                       "invalid fused token base");
    getDiagnostics().Report(Base.getLocation(), DiagID);
    return Token();
  }

  tok::FusionKind FK = It->getValue();
  llvm::StringRef Inner(FusionStart, FusionEnd - FusionStart);
  FusionStart = FusionEnd = nullptr;
  Fused = false;
  return Token(tok::Identifier, Base.getLocation(), FK, Data, Count, Inner);
}

Token CharacterScanner::makeToken(TokenKind K, char const* S, std::size_t N) {
  assert(!Fused);
  clang::SourceLocation Loc = getSourceLocation(S);
  Symbol Sym = getSymbol(S, N);
  Token Tok(K, Loc, Sym);

  // Update line flags.
  if (Column - N == 1)
    Tok.Flags |= TF_StartsLine;

  return Tok;
}

Token CharacterScanner::matchEof() {
  clang::SourceLocation Loc = SM.getLocForEndOfFile(Input->getID());
  return Token(tok::EndOfFile, Loc, Symbol());
}

Token CharacterScanner::matchSpace() {
  consume();
  while (isSpace(getLookahead()))
    consume();
  return makeToken(tok::Space, Start, First);
}

Token CharacterScanner::matchNewline() {
  assert(isNewline(getLookahead()));

  // FIXME: Handle the \r\n case.
  consume();
  Token tok = makeToken(tok::Newline, Start, 1);

  // Update the line and reset the column.
  ++Line;
  Column = 1;

  return tok;
}

Token CharacterScanner::matchLineComment() {
  assert(nextCharacterIs('#'));
  consume();
  while (!isDone() && !isNewline(getLookahead()))
    consume();

  // Creating and dumping a clang token into the PP for things that interact
  // with comments.
  clang::Token CTok;
  CTok.setKind(clang::tok::comment);
  CTok.setLocation(getSourceLocation(Start));
  const unsigned TokLen = First - Start;
  CTok.setLength(TokLen);

  PP.HandleComment(CTok, clang::SourceRange(getSourceLocation(Start),
                                            getSourceLocation(First)));
  return makeToken(tok::LineComment, Start, First);
}

Token CharacterScanner::matchBlockComment() {
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

  // Creating and dumping a clang token into the PP for things that interact
  // with comments.
  clang::Token CTok;
  CTok.setKind(clang::tok::comment);
  CTok.setLocation(getSourceLocation(Start));
  const unsigned TokLen = First - Start;
  CTok.setLength(TokLen);

  PP.HandleComment(CTok, clang::SourceRange(getSourceLocation(Start),
                                            getSourceLocation(First)));
  // Build the block comment.
  Symbol Sym = getSymbol(Start, First);
  return Token(tok::BlockComment, BeginLoc, Sym);
}

Token CharacterScanner::matchToken(TokenKind K) {
  std::size_t Len = getTokenLength(K);
  consume(Len);
  return makeToken(K, Start, Len);
}

Token CharacterScanner::matchFusionArg(Token Base) {
  assert(getLookahead() == '"' || getLookahead() == '\'');
  bool DoubleQuote = getLookahead() == '"';
  consume();
  FusionStart = First;

  llvm::SmallVector<Token *, 4> Tokens;
  unsigned TokenCount = 0;

  auto insideQuote = [this](bool DoubleQuote) -> bool {
    return (DoubleQuote && this->getLookahead() != '"') ||
      (!DoubleQuote && this->getLookahead() != '\'');
  };

  while(insideQuote(DoubleQuote)) {
    Fused = false;

    Token T = operator()();
    if (T.isSpace())
      continue;

    Tokens.push_back(
      new (Ctx) Token(T.getKind(), T.getLocation(), T.getSymbol()));
    ++TokenCount;

    Fused = true;
  }

  // Consume the remaining quote.
  FusionEnd = First;
  consume();

  Token **TokenArray = new (Ctx) Token *[Tokens.size()];
  std::copy(Tokens.begin(), Tokens.end(), TokenArray);
  return makeFusedToken(Base, TokenArray, TokenCount);
}

Token CharacterScanner::matchWord() {
  assert(isIdentifierStart(getLookahead()));
  do consume(); while (isIdentifierRest(getLookahead()));

  // Building fused identifiers.
  auto It = FusionBases.find(llvm::StringRef(Start, First - Start));
  if (It != FusionBases.end()) {
    Token Base = makeToken(tok::Identifier, Start, First);
    Fused = true;

    if (getLookahead() == '"' || getLookahead() == '\'')
      return matchFusionArg(Base);
    else
      Fused = false;
  }

  // This might be a keyword.
  llvm::StringRef Str(Start, First - Start);
  // Adding special handling for a Character that doesn't conform to
  // typical matching, and doesn't fit into our AST structure very well.
  if (Str == "sizeof") {
    // Reading the size of pack token.
    if (getLookahead() == '.'
        && getLookahead(1) == '.'
        && getLookahead(2) == '.') {
      consume();
      consume();
      consume();
      return makeToken(tok::SizeOfPack, Start, First);
    }
  }
  auto Iter = Keywords.find(Str);
  if (Iter != Keywords.end())
    return makeToken(Iter->second, Start, First);
  return makeToken(tok::Identifier, Start, First);
}

Token CharacterScanner::matchNumber() {
  // FIXME: The specification also allows for hex ASCII (ish?) and
  // Unicode hex literals.
  if (getLookahead(0) == '0')
  {
    if (getLookahead(1) == 'x')
      return matchHexadecimalNumber();
  }
  return matchDecimalNumber();
}

Token CharacterScanner::matchDecimalNumber() {
  if (nextCharacterIs('.')) {
    // Matches '. decimal-digit-seq ...'
    return matchDecimalFraction();
  } else {
    // Matches 'decimal-digit-seq [. decimal-digit-seq] ...]'
    matchDecimalDigitSeq();
    if (nextCharacterIs('.') && !nthCharacterIs(1, '.')) {
      if (isDecimalDigit(getLookahead(1)))
        return matchDecimalFraction();

      if (isDecimalExponent(getLookahead(1)))
        return matchDecimalExponent();

      consume();
      return makeToken(tok::DecimalFloat, Start, First);
    }
  }

  // Matches 'decimal-digit-seq (e|E) ...'
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();

  const char *const LiteralEnd = First;
  llvm::SmallVector<llvm::StringRef, 4> Suffixes;
  matchLiteralSuffixSeq(Suffixes);

  if (!Suffixes.empty())
    return makeToken(tok::DecimalInteger, Start, LiteralEnd, Suffixes);

  return makeToken(tok::DecimalInteger, Start, First);
}

Token CharacterScanner::matchDecimalFraction() {
  require('.');
  matchDecimalDigitSeq();
  if (isDecimalExponent(getLookahead()))
    return matchDecimalExponent();

  const char *const LiteralEnd = First;
  llvm::SmallVector<llvm::StringRef, 4> Suffixes;
  matchLiteralSuffixSeq(Suffixes);

  if (!Suffixes.empty())
    return makeToken(tok::DecimalInteger, Start, LiteralEnd, Suffixes);

  return makeToken(tok::DecimalFloat, Start, First);
}

Token CharacterScanner::matchDecimalExponent() {
  requireIf(isDecimalExponent);
  matchIf(isSign);
  // FIXME: There could be an error here.
  matchDecimalDigitSeq();

  const char *const LiteralEnd = First;
  llvm::SmallVector<llvm::StringRef, 4> Suffixes;
  matchLiteralSuffixSeq(Suffixes);

  if (!Suffixes.empty())
    return makeToken(tok::DecimalExponent, Start, LiteralEnd, Suffixes);

  return makeToken(tok::DecimalExponent, Start, First);
}

Token CharacterScanner::matchHexadecimalNumber() {
  consume(2); // Matches '0x'.

  // FIXME: Match hex floats?
  // FIXME: Wrong error.
  if (!isHexadecimalDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(),
                           clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchHexadecimalDigitSeq();

  const char *const LiteralEnd = First;
  llvm::SmallVector<llvm::StringRef, 4> Suffixes;
  matchLiteralSuffixSeq(Suffixes);

  if (!Suffixes.empty())
    return makeToken(tok::HexadecimalInteger, Start, LiteralEnd, Suffixes);

  return makeToken(tok::HexadecimalInteger, Start, First);
}

Token CharacterScanner::matchBinaryNumber() {
  consume(2); // Matches '0x'.

  // FIXME: Match hex floats?
  // FIXME: Wrong error.
  if (!isBinaryDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(),
                           clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchBinaryDigitSeq();

  const char *const LiteralEnd = First;
  llvm::SmallVector<llvm::StringRef, 4> Suffixes;
  matchLiteralSuffixSeq(Suffixes);

  if (!Suffixes.empty())
    return makeToken(tok::BinaryInteger, Start, LiteralEnd, Suffixes);

  return makeToken(tok::BinaryInteger, Start, First);
}

Token CharacterScanner::matchCharacter() {
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

  return makeToken(tok::Character, Start, First);
}

Token CharacterScanner::matchString() {
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

  return makeToken(tok::String, Start, First);
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

Token CharacterScanner::matchHexadecimalCharacter() {
  consume(2); // Matches '0c'.

  if (!isHexadecimalDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(),
                           clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchHexadecimalDigitSeq();

  return makeToken(tok::HexadecimalCharacter, Start, First);
}

Token CharacterScanner::matchUnicodeCharacter() {
  consume(2); // Matches '0u'.

  if (!isHexadecimalDigit(getLookahead())) {
    getDiagnostics().Report(getInputLocation(),
                           clang::diag::err_bad_string_encoding);
    // error(getInputLocation(), "invalid hexadecimal number");
    return {};
  }

  matchHexadecimalDigitSeq();

  return makeToken(tok::UnicodeCharacter, Start, First);
}

void CharacterScanner::matchDecimalDigitSeq() {
  assert(isDecimalDigit(getLookahead()) || isDigitSeparator(getLookahead())
         && "invalid number");

  while (matchIf(isDecimalDigit) || matchIf(isDigitSeparator))
    ;
}

void CharacterScanner::matchDecimalDigitSeqOpt() {
  if (isDecimalDigit(getLookahead()))
    matchDecimalDigitSeq();
}

void CharacterScanner::matchHexadecimalDigitSeq() {
  requireIf(isHexadecimalDigit);
  while (matchIf(isHexadecimalDigit) || matchIf(isDigitSeparator))
    ;
}

void CharacterScanner::matchBinaryDigitSeq() {
  requireIf(isBinaryDigit);
  while (matchIf(isBinaryDigit) || matchIf(isDigitSeparator))
    ;
}

void CharacterScanner::matchLiteralSuffixSeq(
  llvm::SmallVectorImpl<llvm::StringRef> &Suffixes) {
  while (std::isalnum(getLookahead())) {
    const char *SuffixFragBegin = First;
    const char *SuffixFragEnd;

    switch (getLookahead()) {
    case 'u':
    case 'U':
    case 's':
    case 'S':
      consume();
      matchDecimalDigitSeq();
      SuffixFragEnd = First;
      break;

    case 'f':
    case 'F':
    case 'd':
    case 'D':
    case 'h':
    case 'H':
    case 'q':
    case 'Q':
      consume();
      SuffixFragEnd = First;
      break;

    default:
      getDiagnostics()
        .Report(getInputLocation(), clang::diag::err_invalid_suffix_constant)
        << std::string(1, getLookahead()) << 0;
      return;
    }

    Suffixes.emplace_back(SuffixFragBegin, SuffixFragEnd - SuffixFragBegin);
  }
}

clang::SourceLocation
CharacterScanner::getSourceLocation(char const* Loc) {
  llvm::StringRef Buf = Input->getText();
  return SM.getComposedLoc(Input->getID(), Loc - Buf.data());
}

// Line scanner

static llvm::StringMap<bool> InfixKeywords {
  {"where", true},
  {"otherwise", true},
  {"returns", true},
  {"until", true},
  {"using", true},
  {"catch", true}
};

static bool isInfix(Token Op, bool GTIO) {
  switch (Op.getKind()) {
  case tok::Plus:
  case tok::Minus:
  case tok::Star:
  case tok::Slash:
  case tok::Percent:
  case tok::Ampersand:
  case tok::Bar:
  case tok::Less:
  case tok::Equal:
  case tok::EqualEqual:
  case tok::LessEqual:
  case tok::GreaterEqual:
  case tok::AmpersandAmpersand:
  case tok::BarBar:
  case tok::ColonEqual:
  case tok::PlusEqual:
  case tok::MinusEqual:
  case tok::StarEqual:
  case tok::SlashEqual:
  case tok::PercentEqual:
  case tok::MinusGreater:
  case tok::EqualGreater:
  case tok::LeftParen:
  case tok::LeftBracket:
  case tok::Comma:
  case tok::LessLess:
  case tok::GreaterGreater:
  case tok::LessLessEqual:
  case tok::GreaterGreaterEqual:
  case tok::CatchKeyword:
    return true;
  case tok::Identifier: {
    auto It = InfixKeywords.find(Op.getSymbol().data());
    if (It == InfixKeywords.end())
      return false;
    return true;
  }

  case tok::Greater:
    return GTIO;

  default:
    return false;
  }
}

Token LineScanner::operator()() {
  Token Tok;
  bool StartsLine = false;
  while (true) {
    Tok = Scanner();

    if (!Tok.isSpace() && !Tok.isNewline())
      Current = Tok;

    // Space at the beginning of a line cannot be discarded here.
    if (Tok.isSpace() && Tok.isAtStartOfLine() &&
        !isInfix(Current, GreaterThanIsOperator))
      break;

    // Propagate a previous line-start flag to this next token.
    if (StartsLine) {
      Tok.Flags |= TF_StartsLine;
      StartsLine = false;
    }

    // Empty lines are discarded.
    if (Tok.isNewline() && Tok.isAtStartOfLine())
      continue;

    // Errors, space, and comments are discardable. If a token starts a
    // line, the next token will become the new start of line.
    if (Tok.isInvalid() || Tok.isSpace() || Tok.isComment()) {
      StartsLine = Tok.isAtStartOfLine();
      continue;
    }

    // Discard space between a line-broken infix operator. e.g.)
    //
    // \code
    //   x = 2 + 2 *
    //       2 + 2
    // \endcode
    if ((Tok.isSpace() || Tok.isNewline()) &&
        isInfix(Current, GreaterThanIsOperator)) {
      continue;
    }

    // All other tokens are retained.
    break;
  };

  return Tok;
}

// Block scanner

Token BlockScanner::operator()() {
  // Get the next token, possibly one that we've buffered.
  Token Tok;
  if (Lookahead)
    Tok = std::exchange(Lookahead, {});
  else
    Tok = Scanner();

  // Check for a newline followed by indentation.
  if (Tok.isNewline()) {
    Token Next = Scanner();
    if (Next.isSpace()) {
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
  } else if (!Dedents.empty()) {
    Lookahead = Tok;
    Tok = combineSpace({}, {});
  }

  assert(!Tok.isNewline());
  return Tok;
}

Token BlockScanner::matchSeparator(Token const& Tok) {
  return Token(tok::Separator, Tok.getLocation(), Tok.getSymbol());
}

Token BlockScanner::matchIndent(Token const& Tok) {
  return Token(tok::Indent, Tok.getLocation(), Tok.getSymbol());
}

Token BlockScanner::matchDedent(Token const& Tok) {
  return Token(tok::Dedent, Tok.getLocation(), Tok.getSymbol());
}

static Symbol getSymbol(Token const& Tok) {
  return Tok.isInvalid() ? Symbol() : Tok.getSymbol();
}

/// True if `A` and `B` have the same spellings.
static bool equalSpelling(Token const& A, Token const& B) {
  return getSymbol(A) == getSymbol(B);
}

/// True if `sym` starts with `pre` (i.e., is lexicographically greater).
static bool startsWith(Symbol Sym, Symbol Pre) {
  return Sym.str().compare(Pre.str()) > 0;
}

/// True if the lexeme of `tok` has the lexeme of `pre` has a prefix.
static bool startsWith(Token const& Tok, Token const& Pre) {
  return startsWith(getSymbol(Tok), getSymbol(Pre));
}

Token BlockScanner::combineSpace(Token const& Nl, Token const& NewIndent) {
  // Emit queued dedents.
  if (!Dedents.empty())
    return popDedent();

  Token PrevIndent = currentIndentation();

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
    return matchIndent(NewIndent);
  }

  // The previous indentation starts with the new (i.e., previous is longer),
  // then dendent. Note that this can entail multiple dedents: one for each
  // level that does not have the same spelling.
  if (startsWith(PrevIndent, NewIndent)) {
    do {
      popIndentation();
      PrevIndent = currentIndentation();
      pushDedent(matchDedent(NewIndent));
    } while (!Prefix.empty() && !equalSpelling(NewIndent, PrevIndent));

    // Update the location of the last indent for diagnostic purposes.
    if (!Prefix.empty())
      Prefix.back() = NewIndent;

    return popDedent();
  }

  // FIXME: Note the previous indentation depth. This is the reason we
  // overwrite the last entry in the stack: to get the nearest indentation
  // of the expected length.
  getDiagnostics().Report(NewIndent.getLocation(),
                          clang::diag::err_invalid_indentation);

  // Return a line separator just in case.
  return matchSeparator(Nl);
}

} // namespace gold
