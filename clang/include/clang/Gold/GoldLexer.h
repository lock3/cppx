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
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"

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
  class SyntaxContext;

  /// Transforms characters into tokens.
  struct CharacterScanner
  {
    CharacterScanner(clang::SourceManager &SM, File const &F,
                     SyntaxContext &Ctx, clang::Preprocessor &PP);

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
    Token makeToken(TokenKind K, char const* F, char const* L,
                    llvm::SmallVectorImpl<llvm::StringRef> &Suffixes);
    Token makeFusedToken(const char *S);
    Token makeFusedToken2(const char *S);
    Token makeFusedToken(Token Base, Token **Data, unsigned Count);

    Token matchEof();
    Token matchSpace();
    Token matchNewline();
    Token matchLineComment();
    Token matchBlockComment();
    Token matchToken(TokenKind K);
    Token matchWord();
    Token matchFusionArg(Token Base);
    Token matchNumber();
    Token matchDecimalNumber();
    Token matchDecimalFraction();
    Token matchDecimalExponent();
    Token matchHexadecimalNumber();
    Token matchBinaryNumber();

    void matchDecimalDigitSeq();
    void matchDecimalDigitSeqOpt();
    void matchHexadecimalDigitSeq();
    void matchBinaryDigitSeq();
    void matchLiteralSuffixSeq(llvm::SmallVectorImpl<llvm::StringRef> &Sufs);

    Token matchCharacter();
    Token matchHexadecimalCharacter();
    Token matchUnicodeCharacter();
    Token matchString();

    void matchEscapeSequence();

    // Handling textual mode, where we lex like we are inside of HTML (Kinda).
    // Token matchInTextMode();
    Token matchText();
    Token matchEscapedCharInTextMode();


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

    /// True when we are lexing a fused token.
    bool Fused = false;

    /// The beginning and end of the argument of a fused token,
    /// for example, `int` in `conversion"int"`.
    char const *FusionStart = nullptr;
    char const *FusionEnd = nullptr;

    clang::IdentifierInfo *ConversionII;

    /// The base location in the input file. Used to compute source locations
    /// as tokens are matched.
    clang::SourceLocation FileLoc;

    /// Provides context for generating source locations and emitting
    /// diagnostics.
    clang::SourceManager &SM;

    /// Maps identifiers to keyword tokens.
    llvm::StringMap<TokenKind> Keywords;

    /// The syntax context (for memory management).
    SyntaxContext &Ctx;

    /// The clang pre-processor, nothing is actually run through this
    /// but this is what's used to keep track of comments for tools such as
    /// llvm-lit.
    clang::Preprocessor &PP;

    /// This slightly changes the lexing mode by building a text node rather then
    /// a group of identifiers. This means we will include whitespace.
    /// This will read all tokens upto the first occurrence of the following
    /// tokens = \, {, }, #, <, >, &, ~
    /// This will also handle a special case for >= because that MIGHT be the
    /// end of the current attribute.
    // bool TextTokenMode = false;
  };


  /// Lexer
  /// The lexer is ultimately responsible for producing tokens from
  /// input source. This is defined by a stack of scanners, each of
  /// which applies a phase of translation.
  struct Lexer {
    Lexer(clang::SourceManager &SM, File const& F, SyntaxContext &Ctx,
          clang::Preprocessor &PP)
      : Scanner(SM, F, Ctx, PP)
    { }

    Token operator()() {
      return Scanner();
    }

    CharacterScanner Scanner;
  };

} // namespace gold

#endif

