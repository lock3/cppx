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
      if (getLookahead(1) == '>') {
        return matchToken(tok::ColonGreater);
      }
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
  auto BeginLoc = First;
  consume(2); // '<#'
  bool Terminated = false;
  while (!isDone()) {
    // Deal with nested blocks.
    if (nextCharacterIs('<') && nthCharacterIs(1, '#'))
      matchBlockComment();

    if (nextCharacterIs('#') && nthCharacterIs(1, '>')) {
      Terminated = true;
      break;
    }

    char c = getLookahead();
    consume();
    if (isNewline(c)) {
      ++Line;
      Column = 1;
    }
  }

  if (isDone() && !Terminated) {
    unsigned DiagID =
      getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                       "unterminated <# comment");
    getDiagnostics().Report(getInputLocation(), DiagID);
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
  return makeToken(tok::BlockComment, BeginLoc, TokLen);
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

static bool isTerminatingTextTokenCharacter(char c) {
    switch (c) {
      case '>':
      case '\0':
      case '{':
      case '}':
      case '#':
      case '<':
      case '&':
      case '~':
      case '\\':
        return true;
      default:
        return false;
    }
}

// Token CharacterScanner::matchInTextMode() {
//   assert(TextTokenMode && "Invalid call to text node matching");
//   switch(*First) {
//     case '\\': {
//       return matchEscapedCharInTextMode();
//     }
//     default:
//       // There may be special cases after this that don't conform
//       // to this pattern. They are handled by the default big grab functionality.
//       break;
//   }
//   return matchText();
// }

// Token CharacterScanner::matchEscapedCharInTextMode() {
//   auto NextChar = First + 1;
//   matchEscapeSequence();
//   return makeToken(tok::EscapedTextChar, Start, 2);
// }

// Token CharacterScanner::matchText() {
//   std::size_t Len = 0;
//   while(*First && !isTerminatingTextTokenCharacter(*First)) {
//     consume();
//     ++Len;
//   }
//   TextTokenMode = false;
//   auto Ret = makeToken(tok::Text, Start, Len);
//   return Ret;
// }


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

} // namespace gold
