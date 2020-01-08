//===- GoldLexer.h - Gold Language Lexer ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GoldLexer interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDLEXER_H
#define CLANG_GOLD_GOLDLEXER_H

#include "clang/Basic/SourceLocation.h"

#include "clang/Gold/GoldFile.h"
#include "clang/Gold/GoldTokens.h"

#include <deque>
#include <stack>

namespace clang {

class DiagnosticsEngine;
class SourceManager;

} // namespace clang

// We want to think about this as a staged grammar, like C++. In general, we
// need to retain various kinds of white space, line breaks, and comments
// until later stages where we can discard them.

namespace gold {
  /// Transforms characters into tokens.
  struct CharacterScanner
  {
    CharacterScanner(clang::SourceManager &SM, File const& F);

    Token operator()();

    clang::SourceLocation getInputLocation() {
      return getSourceLocation(First);
    }

    bool isDone() const {
      return First == Last;
    }

    char getLookahead() const {
      if (isDone())
        return 0;
      return *First;
    }

    char getLookahead(int N) const {
      if (N >= Last - First)
        return 0;
      return First[N];
    }

    bool nextCharacterIs(char C) const {
      return getLookahead() == C;
    }

    bool nextCharacterIsNot(char C) const {
      return !nextCharacterIs(C);
    }

    bool nthCharacterIs(int N, char C) const {
      return getLookahead(N) == C;
    }

    bool nthCharacterIsNot(int N, char C) const {
      return !nthCharacterIs(N, C);
    }

    char consume() {
      char C = *First;
      ++First;
      ++Column;
      return C;
    }

    void consume(int N) {
      assert(N <= Last - First);
      First += N;
      Column += N;
    }

    char match(char C) {
      if (nextCharacterIs(C))
        return consume();
      return 0;
    }

    template<typename F>
    char matchIf(F Pred) {
      if (Pred(getLookahead()))
        return consume();
      return 0;
    }

    char require(char C) {
      assert(nextCharacterIs(C));
      return consume();
    }

    template <typename F>
    char requireIf(F Pred) {
      assert(Pred(getLookahead()));
      return consume();
    }

    void skipUntil(char C);

    void consumeSpace();
    void consumeBlock_comment();
    void consumeLine_comment();

    Token makeToken(TokenKind K, char const* F, char const* L);
    Token makeToken(TokenKind K, char const* S, std::size_t N);

    Token matchEof();
    Token matchSpace();
    Token matchNewline();
    Token matchLineComment();
    Token matchBlockComment();
    Token matchToken(TokenKind K);
    Token matchWord();
    Token matchNumber();
    Token matchDecimalNumber();
    Token matchDecimalFraction();
    Token matchDecimalExponent();
    Token matchHexadecimalNumber();

    void matchDecimalDigitSeq();
    void matchDecimalDigitSeqOpt();
    void matchHexadecimalDigitSeq();

    Token matchCharacter();
    Token matchHexadecimalCharacter();
    Token matchUnicodeCharacter();
    Token matchString();

    void matchEscapeSequence();

    clang::SourceLocation getSourceLocation(char const* Loc);

    clang::DiagnosticsEngine& getDiagnostics() {
      return SM.getDiagnostics();
    }

    /// The source file being lexed.
    File const* Input;

    /// The current character of the input text.
    char const* First;

    /// Past the end of the last character of the input text.
    char const* Last;

    /// The start of the current token.
    char const* Start;

    /// The current line number (1-based).
    int Line;

    /// The current column (1-based).
    int Column;

    /// Used to manage the starting position of a token. This class will
    /// also cache a previous position, allowing for nested token lexing.
    struct StartingPosition
    {
      StartingPosition(CharacterScanner& CS)
        : Scanner(CS), Prev(CS.Start) {
        Scanner.Start = Scanner.First;
      }

      ~StartingPosition() {
        Scanner.Start = Prev;
      }

      /// The scanner.
      CharacterScanner& Scanner;

      /// The previous character.
      char const* Prev;
    };

    /// The base location in the input file. Used to compute source locations
    /// as tokens are matched.
    clang::SourceLocation FileLoc;

    /// Provides context for generating source locations and emitting
    /// diagnostics.
    clang::SourceManager &SM;
  };


  /// Removes empty lines and comments from a translation unit.
  struct LineScanner {
    LineScanner(clang::SourceManager &SM, File const& F)
      : Scanner(SM, F)
    { }

    Token operator()();

    clang::DiagnosticsEngine& getDiagnostics() {
      return Scanner.getDiagnostics();
    }

    /// The underlying scanner.
    CharacterScanner Scanner;
  };


  /// Combines newline and whitespace to create indents, dedents, and
  /// separators. Note that there are no newlines in the translation unit
  /// returned from this scanner.
  struct BlockScanner {
    BlockScanner(clang::SourceManager &SM, File const& F)
      : Scanner(SM, F), Prefix() { }

    Token operator()();

    Token combineSpace(Token const& nl, Token const& sp);
    Token matchSeparator(Token const& nl);
    Token matchIndent(Token const& nl);
    Token matchDedent(Token const& nl);

    /// The current level of indentation. If the indentation stack is empty,
    /// return an empty token.
    Token currentIndentation() const {
      if (Prefix.empty())
        return {};
      return Prefix.back();
    }

    /// Push a new indentation level.
    void pushIndentation(Token const& Tok) {
      assert(Tok.isSpace());
      Prefix.push_back(Tok);
    }

    /// Pops the current indentation level.
    Token popIndentation() {
      Token Tok = Prefix.back();
      Prefix.pop_back();
      return Tok;
    }

    /// Save a dedent token.
    void pushDedent(Token Tok) {
      Dedents.push_back(Tok);
    }

    /// Get the next saved dedent token.
    Token popDedent() {
      Token Tok = Dedents.back();
      Dedents.pop_back();
      return Tok;
    }

    clang::DiagnosticsEngine& getDiagnostics() {
      return Scanner.getDiagnostics();
    }

    /// The underlying scanner.
    LineScanner Scanner;

    /// A buffered lookahead token.
    Token Lookahead;

    /// The indentation stack.
    std::vector<Token> Prefix;

    /// A single newline/space can match multiple dedents.
    std::vector<Token> Dedents;
  };


  // Lexer

  /// The lexer is ultimately responsible for producing tokens from
  /// input source. This is defined by a stack of scanners, each of
  /// which applies a phase of translation.
  struct Lexer {
    Lexer(clang::SourceManager &SM, File const& F)
      : Scanner(SM, F)
    { }

    Token operator()()
    {
      return Scanner();
    }

    BlockScanner Scanner;
  };

} // namespace gold

#endif

