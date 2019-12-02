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
  struct character_scanner
  {
    character_scanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                      File const& F);

    token operator()();

    clang::SourceLocation input_location()
    {
      // return make_location(input, line, column);
      return getSourceLocation(input->data());
    }

    bool done() const
    {
      return first == last;
    }

    char lookahead() const
    {
      if (done())
        return 0;
      return *first;
    }

    char lookahead(int n) const
    {
      if (n >= last - first)
        return 0;
      return first[n];
    }

    bool next_character_is(char c) const
    {
      return lookahead() == c;
    }

    bool next_character_is_not(char c) const
    {
      return !next_character_is(c);
    }

    bool nth_character_is(int n, char c) const
    {
      return lookahead(n) == c;
    }

    bool nth_character_is_not(int n, char c) const
    {
      return !nth_character_is(n, c);
    }

    char consume()
    {
      char c = *first;
      ++first;
      ++column;
      return c;
    }

    void consume(int n)
    {
      assert(n <= last - first);
      first += n;
      column += n;
    }

    char match(char c)
    {
      if (next_character_is(c))
        return consume();
      return 0;
    }

    template<typename F>
    char match_if(F pred) {
      if (pred(lookahead()))
        return consume();
      return 0;
    }

    char require(char c)
    {
      assert(next_character_is(c));
      return consume();
    }

    template <typename F>
    char require_if(F pred) {
      assert(pred(lookahead()));
      return consume();
    }

    void skip_until(char c);

    void consume_space();
    void consume_block_comment();
    void consume_line_comment();

    token make_token(token_kind k, char const* first, char const* last);
    token make_token(token_kind k, char const* str, std::size_t len);

    token match_eof();
    token match_space();
    token match_newline();

    token match_line_comment();
    token match_block_comment();

    token match_token(token_kind k);

    token match_word();

    token match_number();
    token match_decimal_number();
    token match_decimal_fraction();
    token match_decimal_exponent();
    token match_hexadecimal_number();

    void match_decimal_digit_seq();
    void match_decimal_digit_seq_opt();
    void match_hexadecimal_digit_seq();

    token match_character();
    token match_hexadecimal_character();
    token match_unicode_character();
    token match_string();
    void match_escape_sequence();

    /// The source file being lexed.
    File const* input;

    /// The current character of the input text.
    char const* first;

    /// Past the end of the last character of the input text.
    char const* last;

    /// The start of the current token.
    char const* start;

    /// The current line number (1-based).
    int line;

    // The current column (1-based).
    int column;

    /// Used to manage the starting position of a token. This class will
    /// also cache a previous position, allowing for nested token lexing.
    struct starting_position
    {
      starting_position(character_scanner& s)
        : scanner(s), prev(s.start)
      {
        scanner.start = scanner.first;
      }

      ~starting_position()
      {
        scanner.start = prev;
      }

      /// The scanner.
      character_scanner& scanner;

      /// The previous character.
      char const* prev;
    };

    clang::SourceLocation FileLoc;

    // Create a clang::SourceLocation for a token
    clang::SourceLocation getSourceLocation(char const* Loc);

    clang::SourceManager &SM;
    clang::DiagnosticsEngine &Diags;
  };


  /// Removes empty lines and comments from a translation unit.
  struct line_scanner
  {
    line_scanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                 File const& F)
      : scan(SM, Diags, F), SM(SM), Diags(Diags)
    { }

    token operator()();

    char char_lookahead(int n) {
      if (n)
        return scan.lookahead(n);
      return scan.lookahead();
    }

    /// The underlying scanner.
    character_scanner scan;

    clang::SourceManager &SM;
    clang::DiagnosticsEngine &Diags;
  };


  /// Combines newline and whitespace to create indents, dedents, and
  /// separators. Note that there are no newlines in the translation unit
  /// returned from this scanner.
  struct block_scanner
  {
    block_scanner(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
                  File const& F)
      : scan(SM, Diags, F), prefix(), SM(SM), Diags(Diags)
    { }

    token operator()();

    token combine(token const& nl, token const& sp);
    token separate(token const& nl);
    token indent(token const& nl);
    token dedent(token const& nl);

    /// The current level of indentation. If the indentation stack is empty,
    /// return an empty token.
    token indentation() const
    {
      if (prefix.empty())
        return {};
      return prefix.top();
    }

    /// Push a new indentation level.
    void push(token const& tok)
    {
      prefix.push(tok);
    }

    /// Pops the indentation level, returning the new level.
    token pop()
    {
      prefix.pop();
      return indentation();
    }

    char char_lookahead(int n) {
      return scan.char_lookahead(n);
    }

    /// The underlying scanner.
    line_scanner scan;

    /// A buffered lookahead token.
    token lookahead;

    /// The indentation stack.
    std::stack<token> prefix;

    clang::SourceManager &SM;
    clang::DiagnosticsEngine &Diags;
  };


  // Lexer

  /// The lexer owns the scanner stack.
  struct lexer
  {
    lexer(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
          File const& F)
      : scan(SM, Diags, F), SM(SM), Diags(Diags)
    { }

    token operator()()
    {
      return scan();
    }

    char char_lookahead(int n = 0) {
      return scan.char_lookahead(n);
    }

    block_scanner scan;

    clang::SourceManager &SM;
    clang::DiagnosticsEngine &Diags;
  };

} // namespace green

#endif

