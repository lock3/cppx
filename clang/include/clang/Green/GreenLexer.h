//===- GreenLexer.h - Green Language Lexer --------------------------------===//
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

#ifndef CLANG_GREEN_GREENLEXER_H
#define CLANG_GREEN_GREENLEXER_H

#include "clang/Basic/SourceLocation.h"

#include "clang/Green/GreenTokens.h"
#include "clang/Green/File.h"

#include <deque>
#include <stack>

namespace clang {

class DiagnosticsEngine;
class SourceManager;

} // namespace clang

// We want to think about this as a staged grammar, like C++. In general, we
// need to retain various kinds of white space, line breaks, and comments
// until later stages where we can discard them.

namespace green
{
  // The character scanner transforms characters into tokens.
  struct CharacterScanner
  {
    CharacterScanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                     File const& F);

    token operator()();

    clang::SourceLocation getInputLocation()
    {
      return getSourceLocation(Input->data());
    }

    bool isDone() const
    {
      return First == Last;
    }

    char getLookahead() const
    {
      if (isDone())
        return 0;
      return *First;
    }

    char getLookahead(int N) const
    {
      if (N >= Last - First)
        return 0;
      return First[N];
    }

    bool nextCharacterIs(char C) const
    {
      return getLookahead() == C;
    }

    bool nextCharacterIsNot(char C) const
    {
      return !nextCharacterIs(C);
    }

    bool nthCharacterIs(int N, char C) const
    {
      return getLookahead(N) == C;
    }

    bool nthCharacterIsNot(int N, char C) const
    {
      return !nthCharacterIs(N, C);
    }

    char consume()
    {
      char C = *First;
      ++First;
      ++Column;
      return C;
    }

    void consume(int N)
    {
      assert(N <= Last - First);
      First += N;
      Column += N;
    }

    char match(char C)
    {
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

    char require(char C)
    {
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

    token makeToken(token_kind K, char const* F, char const* L);
    token makeToken(token_kind K, char const* S, std::size_t N);

    token matchEof();
    token matchSpace();
    token matchNewline();
    token matchLineComment();
    token matchBlockComment();
    token matchToken(token_kind K);
    token matchWord();
    token matchNumber();
    token matchDecimalNumber();
    token matchDecimalFraction();
    token matchDecimalExponent();
    token matchHexadecimalNumber();

    void matchDecimalDigitSeq();
    void matchDecimalDigitSeqOpt();
    void matchHexadecimalDigitSeq();

    token matchCharacter();
    token matchHexadecimalCharacter();
    token matchUnicodeCharacter();
    token matchString();

    void matchEscapeSequence();

    clang::SourceLocation getSourceLocation(char const* Loc);

    clang::DiagnosticsEngine& getDiagnostics() {
      return Diags;
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
        : Scanner(CS), Prev(CS.Start)
      {
        Scanner.Start = Scanner.First;
      }

      ~StartingPosition()
      {
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

    /// Used to generate source locations.
    clang::SourceManager &SM;

    /// Used for diagnostics.
    clang::DiagnosticsEngine &Diags;
  };


  /// Removes empty lines and comments from a translation unit.
  struct LineScanner
  {
    LineScanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                 File const& F)
      : Scanner(SM, Diags, F)
    { }

    token operator()();

    clang::DiagnosticsEngine& getDiagnostics() {
      return Scanner.getDiagnostics();
    }

    char char_lookahead(int n) {
      if (n)
        return Scanner.getLookahead(n);
      return Scanner.getLookahead();
    }

    /// The underlying scanner.
    CharacterScanner Scanner;
  };


  /// Combines newline and whitespace to create indents, dedents, and
  /// separators. Note that there are no newlines in the translation unit
  /// returned from this scanner.
  struct BlockScanner
  {
    BlockScanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                 File const& F)
      : Scanner(SM, Diags, F), Prefix()
    { }

    token operator()();

    token combine(token const& nl, token const& sp);
    token separate(token const& nl);
    token indent(token const& nl);
    token dedent(token const& nl);

    /// The current level of indentation. If the indentation stack is empty,
    /// return an empty token.
    token currentIndentation() const
    {
      if (Prefix.empty())
        return {};
      return Prefix.top();
    }

    /// Push a new indentation level.
    void pushIndentation(token const& Tok)
    {
      Prefix.push(Tok);
    }

    /// Pops the indentation level, returning the new level.
    token popIndentation()
    {
      Prefix.pop();
      return currentIndentation();
    }

    clang::DiagnosticsEngine& getDiagnostics() {
      return Scanner.getDiagnostics();
    }

    char char_lookahead(int n) {
      return Scanner.char_lookahead(n);
    }

    /// The underlying scanner.
    LineScanner Scanner;

    /// A buffered lookahead token.
    token Lookahead;

    /// The indentation stack.
    std::stack<token> Prefix;
  };


  // Lexer

  /// The lexer is ultimately responsible for producing tokens from
  /// input source. This is defined by a stack of scanners, each of
  /// which applies a phase of translation.
  struct Lexer
  {
    Lexer(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
          File const& F)
      : Scanner(SM, Diags, F)
    { }

    token operator()()
    {
      return Scanner();
    }

    char char_lookahead(int n = 0) {
      return Scanner.char_lookahead(n);
    }

    BlockScanner Scanner;
  };

} // namespace green

#endif

