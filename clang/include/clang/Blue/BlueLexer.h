//===- BlueLexer.h - Blue Language Lexer ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the BlueLexer interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUELEXER_H
#define CLANG_BLUE_BLUELEXER_H

#include "clang/Blue/BlueTokens.h"
#include "clang/Blue/BlueFile.h"

#include <deque>
#include <stack>

namespace clang {

class DiagnosticsEngine;
class SourceManager;

} // namespace clang

// We want to think about this as a staged grammar, like C++. In general, we
// need to retain various kinds of white space, line breaks, and comments
// until later stages where we can discard them.

namespace blue {

  /// Transforms characters into tokens.
  struct Lexer
  {
    Lexer(clang::SourceManager &SM, File const& F);

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

    template<typename P>
    bool nextCharacterMatches(P Pred) const {
      return Pred(getLookahead());
    }

    bool nextCharacterIsNot(char C) const {
      return !nextCharacterIs(C);
    }

    template<typename P>
    bool nextCharacterDiffers(P Pred) const {
      return !Pred(getLookahead());
    }

    bool nthCharacterIs(int N, char C) const {
      return getLookahead(N) == C;
    }

    bool nthCharacterIsNot(int N, char C) const {
      return !nthCharacterIs(N, C);
    }

    char consumeCharacter() {
      char C = *First;
      ++First;
      ++Column;
      return C;
    }

    void consumeCharacters(int N) {
      assert(N <= Last - First);
      First += N;
      Column += N;
    }

    char matchCharacter(char C) {
      if (nextCharacterIs(C))
        return consumeCharacter();
      return 0;
    }

    template<typename P>
    char matchCharacterIf(P Pred) {
      if (Pred(getLookahead()))
        return consumeCharacter();
      return 0;
    }

    char requireCharacter(char C) {
      assert(nextCharacterIs(C));
      return consumeCharacter();
    }

    template<typename P>
    char requireCharacterIf(P Pred) {
      assert(Pred(getLookahead()));
      return consumeCharacter();
    }

    void skipUntil(char C);

    Token makeToken(TokenKind K, char const* F, char const* L);
    Token makeToken(TokenKind K, char const* S, std::size_t N);

    void matchSpace();
    void matchNewline();
    void matchLineComment();
    void matchBlockComment();

    Token matchEof();
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
      StartingPosition(Lexer& CS)
        : Scanner(CS), Prev(CS.Start) {
        Scanner.Start = Scanner.First;
      }

      ~StartingPosition() {
        Scanner.Start = Prev;
      }

      /// The scanner.
      Lexer& Scanner;

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

} // namespace blue

#endif

