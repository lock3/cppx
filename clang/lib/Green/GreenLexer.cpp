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

namespace green
{
  namespace
  {
    bool is_space(char c)
    {
      return c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f';
    }

    bool is_newline(char c)
    {
      return c == '\n';
    }

    bool is_identifier_start(char c)
    {
      return std::isalpha(c) || c == '_';
    }

    bool is_identifier_rest(char c)
    {
      return std::isalpha(c) || std::isdigit(c) || c == '_';
    }

    auto is_decimal_digit = [](char c) -> bool
    {
      return std::isdigit(c);
    };

    auto is_hexadecimal_digit = [](char c) -> bool
    {
      return std::isxdigit(c);
    };

    /// The byte order mark.
    char bom[] = {'\xef', '\xbe', '\xbb'};

  } // namespace

  character_scanner::character_scanner(clang::SourceManager &SM,
                                       clang::DiagnosticsEngine &Diags,
                                       File const& F)
    : input(&F),
      first(F.data()),
      last(first + F.size()),
      line(1),
      column(1),
      SM(SM),
      Diags(Diags)
  {
    // Bypass the byte order mark.
    if (std::equal(bom, bom + 3, first, last))
      first += 3;
  }

  token character_scanner::operator()()
  {
    while (!done())
    {
      // Establish this point as the start of the current token.
      starting_position pos(*this);

      switch (lookahead()) {
      case ' ':
      case '\t':
        return match_space();

      case '\n':
        // FIXME: Handle the optional \r\n.
        return match_newline();

      case '#':
        return match_line_comment();

      case '(':
        return match_token(tok_left_paren);
      case ')':
        return match_token(tok_right_paren);
      case '[':
        return match_token(tok_left_bracket);
      case ']':
        return match_token(tok_right_bracket);
      case '{':
        return match_token(tok_left_brace);
      case '}':
        return match_token(tok_right_brace);
      case ':':
        return match_token(tok_colon);
      case ';':
        return match_token(tok_semicolon);
      case ',':
        return match_token(tok_comma);

      case '.':
        if (is_decimal_digit(lookahead(1)))
          return match_decimal_number();
        return match_token(tok_dot);

      case '?':
        return match_token(tok_question);
      case '+':
        if (lookahead(1) == '=')
          return match_token(tok_plus_equal);
        return match_token(tok_plus);

      case '-':
        if (lookahead(1) == '=')
          return match_token(tok_minus_equal);
        if (lookahead(1) == '>')
          return match_token(tok_minus_greater);
        return match_token(tok_minus);

      case '*':
        if (lookahead(1) == '=')
          return match_token(tok_star_equal);
        return match_token(tok_star);

      case '/':
        if (lookahead(1) == '=')
          return match_token(tok_slash_equal);
        return match_token(tok_slash);

      case '%':
        if (lookahead(1) == '=')
          return match_token(tok_percent_equal);
        return match_token(tok_percent);

      case '&':
        if (lookahead(1) == '&')
          return match_token(tok_ampersand_ampersand);
        return match_token(tok_ampersand);

      case '|':
        if (lookahead(1) == '|')
          return match_token(tok_bar_bar);
        return match_token(tok_bar);

      case '<':
        if (lookahead(1) == '#')
          return match_block_comment();
        else if (lookahead(1) == '=')
          return match_token(tok_less_equal);
        else if (lookahead(1) == '>')
          return match_token(tok_less_greater);
        return match_token(tok_less);

      case '>':
        if (lookahead(1) == '=')
          return match_token(tok_greater_equal);
        return match_token(tok_greater);

      case '~':
        return match_token(tok_tilde);

      case '=':
        if (lookahead(1) == '=')
          return match_token(tok_equal_equal);
        else if (lookahead(1) == '>')
          return match_token(tok_equal_greater);
        return match_token(tok_equal);

      case '!':
        if (lookahead(1) == '=')
          return match_token(tok_bang_equal);
        return match_token(tok_bang);

      case '\'':
        return match_character();
      case '"':
        return match_string();

      case '0':
        if (nth_character_is(1, 'x') || nth_character_is(1, 'X'))
          return match_hexadecimal_number();
        if (nth_character_is(1, 'c'))
          return match_hexadecimal_character();
        if (nth_character_is(1, 'u'))
          return match_unicode_character();
        LLVM_FALLTHROUGH;

      default:
        if (is_identifier_start(lookahead()))
          return match_word();
        if (is_decimal_digit(lookahead()))
          return match_number();

        Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
        // error(input_location(), "invalid character {}", std::to_string(last - first));
        consume();
        continue;
      }
    }

    return match_eof();
  }

  token character_scanner::make_token(token_kind k, char const* first, char const* last)
  {
    return make_token(k, first, last - first);
  }

  token character_scanner::make_token(token_kind k, char const* str, std::size_t len)
  {
    std::size_t col = column - len;
    // location loc = make_location(input, line, col, len);
    clang::SourceLocation Loc = getSourceLocation(str);
    symbol sym = get_symbol(str, len);
    token tok(k, Loc, sym);

    // Update line flags.
    if (col == 1)
      tok.flags |= tf_starts_line;

    return tok;
  }

  token character_scanner::match_eof()
  {
    // TODO: replace with getendoffile
    return token(getSourceLocation(first));
  }

  token character_scanner::match_space()
  {
    consume();
    while (is_space(lookahead()))
      consume();
    return make_token(tok_space, start, first);
  }

  token character_scanner::match_newline()
  {
    assert(is_newline(lookahead()));

    // FIXME: Handle the \r\n case.
    consume();
    token tok = make_token(tok_newline, start, 1);

    // Update the line and reset the column.
    ++line;
    column = 1;

    return tok;
  }

  token character_scanner::match_line_comment()
  {
    assert(next_character_is('#'));
    consume();
    while (!done() && !is_newline(lookahead()))
      consume();
    return make_token(tok_line_comment, start, first);
  }

  token character_scanner::match_block_comment()
  {
    // source_position begin_pos(line, column);
    auto BeginLoc = getSourceLocation(first);
    consume(2); // '<#'
    while (!done() && next_character_is_not('#') && nth_character_is_not(1, '>'))
    {
      char c = lookahead();
      consume();
      if (is_newline(c))
      {
        ++line;
        column = 1;
      }
    }
    if (done())
    {
        Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
      // error(input_location(), "unterminated block comment");
      // note(make_location(input, begin_pos, 2), "beginning here");
      return match_eof();
    }
    consume(2); // '#>'
    // source_position end_pos(line, column);
    auto EndLoc = getSourceLocation(first);

    // Build the block comment
    // location loc = make_location(input, begin_pos, end_pos);
    symbol sym = get_symbol(start, first);
    return token(tok_block_comment, BeginLoc, EndLoc, sym);
  }

  token character_scanner::match_token(token_kind k)
  {
    std::size_t len = token_length(k);
    consume(len);
    return make_token(k, start, len);
  }

  token character_scanner::match_word()
  {
    assert(is_identifier_start(lookahead()));
    consume();
    while (is_identifier_rest(lookahead()))
      consume();

    return make_token(tok_identifier, start, first);
  }

  token character_scanner::match_number()
  {
    // FIXME: The specification also allows for hex ASCII (ish?) and
    // Unicode hex literals.
    if (lookahead(0) == '0')
    {
      if (lookahead(1) == 'x')
        return match_hexadecimal_number();
    }
    return match_decimal_number();
  }

  auto is_decimal_exponent = [](char c) -> bool
  {
    return c == 'e' || c == 'E';
  };

  auto is_sign = [](char c) -> bool 
  {
    return c == '+' || c == '-';
  };

  token character_scanner::match_decimal_number()
  {
    if (next_character_is('.'))
    {
      // Matches '. decimal-digit-seq ...'
      return match_decimal_fraction();
    }
    else
    {
      // Matches 'decimal-digit-seq [. decimal-digit-seq] ...]'
      match_decimal_digit_seq();
      if (next_character_is('.'))
      {
        if (is_decimal_digit(lookahead()))
          return match_decimal_fraction();

        if (is_decimal_exponent(lookahead()))
          return match_decimal_exponent();

        consume();
        return make_token(tok_decimal_float, start, first);
      }
    }

    // Matches 'decimal-digit-seq (e|E) ...'
    if (is_decimal_exponent(lookahead()))
      return match_decimal_exponent();

    return make_token(tok_decimal_integer, start, first);
  }

  token character_scanner::match_decimal_fraction()
  {
    require('.');
    match_decimal_digit_seq();
    if (is_decimal_exponent(lookahead()))
      return match_decimal_exponent();
    return make_token(tok_decimal_float, start, first);
  }

  token character_scanner::match_decimal_exponent()
  {
    require_if(is_decimal_exponent);
    match_if(is_sign);
    // FIXME: There could be an error here.
    match_decimal_digit_seq();
    return make_token(tok_decimal_float, start, first);
  }

  token character_scanner::match_hexadecimal_number()
  {
    consume(2); // Matches '0x'.

    // FIXME: Match hex floats?

    if (!is_hexadecimal_digit(lookahead()))
    {
      Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
      // error(input_location(), "invalid hexadecimal number");
      return {};
    }

    match_hexadecimal_digit_seq();

    return make_token(tok_hexadecimal_integer, start, first);
  }

  token character_scanner::match_character()
  {
    assert(next_character_is('\''));

    consume(); // '\''
    while (!done() && next_character_is_not('\''))
    {
      // Diagnose newlines, but continue lexing the token.
      if (is_newline(lookahead()))
      {
        // error (input_location(), "newline in character literal");
        consume();
      }

      if (next_character_is('\\'))
      {
        match_escape_sequence();
        continue;
      }

      // FIXME: Match the '{0cXX}' and '{0uXXXX}' cases. These are
      // interesting because the nested codes are other tokens. We could
      // leave these in place to be lexed later, or attach them to the
      // token in some interesting way. See comments in match_string also.

      consume();
    }
    if (done())
    {
        Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
      // error(input_location(), "unterminated character literal");
      // location loc = make_location(input, line, first - start);
      // note(loc, "beginning here");
      return match_eof();
    }
    consume(); // '\''

    return make_token(tok_character, start, first);
  }

  token character_scanner::match_string()
  {
    assert(next_character_is('"'));

    consume(); // '"'
    while (!done() && next_character_is_not('"'))
    {
      // Diagnose newlines, but continue lexing the token.
      if (is_newline(lookahead()))
      {
        // error (input_location(), "newline in string literal");
        consume();
      }

      if (next_character_is('\\'))
      {
        match_escape_sequence();
        continue;
      }

      // FIXME: Match nested tokens in '{ ... '}'. We're going to have to
      // do something pretty interesting for string tokens (i.e., storing
      // interpolation ranges in a side buffer somewhere so we don't copy
      // dynamic objects).

      consume();
    }
    if (done())
    {
      Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
      // error(input_location(), "unterminated string literal");
      // note(make_location(input, line, first - start), "beginning here");
      return match_eof();
    }
    consume(); // '"'

    return make_token(tok_string, start, first);
  }

  void character_scanner::match_escape_sequence()
  {
    consume(); // '\\'
    switch (lookahead ())
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
      // error (input_location(), "invalid escape character '{}'",
      //        std::to_string(lookahead()));
      consume();
      break;
    }
  }

  token character_scanner::match_hexadecimal_character()
  {
    // sorry(input_location(), "hexadecimal characters not supported");
    return {};
  }

  token character_scanner::match_unicode_character()
  {
    // sorry(input_location(), "unicode characters not supported");
    return {};
  }

  void character_scanner::match_decimal_digit_seq()
  {
    // FIXME: Allow digit separators?

    if (!match_if(is_decimal_digit))
    {
      Diags.Report(input_location(), clang::diag::err_bad_string_encoding);
      // error(input_location(), "invalid number");s
      return;
    }

    while (match_if(is_decimal_digit))
      ;
  }

  void character_scanner::match_decimal_digit_seq_opt()
  {
    if (is_decimal_digit(lookahead()))
      match_decimal_digit_seq();
  }

  void character_scanner::match_hexadecimal_digit_seq()
  {
    // FIXME: Allow digit separators?
    require_if(is_hexadecimal_digit);
    while (match_if(is_hexadecimal_digit))
      consume();
  }

  // Line scanner

  token line_scanner::operator()()
  {
    token tok;
    bool starts_line = false;
    while (true)
    {
      tok = scan();

      // Space at the beginning of a line cannot be discarded here.
      if (tok.is_space() && tok.starts_line())
        break;

      // Propagate a previous line-start flag to this next token.
      if (starts_line)
      {
        tok.flags |= tf_starts_line;
        starts_line = false;
      }

      // Empty lines are discarded.
      if (tok.is_newline() && tok.starts_line())
        continue;

      // Errors, space, and comments are discardable. If a token starts a
      // line, the next token will become the new start of line.
      if (tok.is_invalid() || tok.is_space() || tok.is_comment())
      {
        starts_line = tok.starts_line();
        continue;
      }

      // All other tokens are retained.
      break;
    };

    return tok;
  }

  clang::SourceLocation
  character_scanner::getSourceLocation(char const* Loc) {
    unsigned CharNo = Loc - input->data();

    if (FileLoc.isFileID())
      return FileLoc.getLocWithOffset(CharNo);
    assert(false && "Getting location from non-file");
  }

  // Block scanner

  token block_scanner::operator()()
  {
    // Get the next token, possibly one that we've buffered.
    token tok;
    if (lookahead)
      tok = std::exchange(lookahead, {});
    else
      tok = scan();

    // Check for a newline followed by indentation.
    if (tok.is_newline())
    {
      token next = scan();
      if (next.is_space())
      {
        tok = combine(tok, next);
      }
      else
      {
        // At the top-level a newline followed by a token implies the
        // presence of a terminator. For example:
        //
        //    x = 1 <newline>
        //    y = 2 <newline>
        //
        // We want to replace the first newline token with a separator.
        // However, we have to preserve the token we just found.
        //
        // Note that the second newline is followed by eof, so we'd
        // probably want to do the same.
        tok = combine(tok, {});

        // Buffer the next token for the next read.
        lookahead = next;
      }
    }

    assert(!tok.is_newline());
    return tok;
  }

  token block_scanner::separate(token const& nl)
  {
    return token(tok_separator, nl.loc, nl.sym);
  }

  token block_scanner::indent(token const& nl)
  {
    return token(tok_indent, nl.loc, nl.sym);
  }

  token block_scanner::dedent(token const& nl)
  {
    return token(tok_dedent, nl.loc, nl.sym);
  }

  static symbol
  get_symbol(token const& a)
  {
    return a.is_invalid() ? symbol() : a.sym;
  }

  /// True if `a` and `b` have the same spellings.
  static bool equal_spelling(token const& a, token const& b)
  {
    return get_symbol(a) == get_symbol(b);
  }

  /// True if `sym` starts with `pre`.
  static bool has_prefix(symbol sym, symbol pre)
  {
    return sym.str->compare(*(pre.str)) > 0;
  }

  /// True if the lexeme of `tok` has the lexeme of `pre` has a prefix.
  static bool has_prefix(token const& tok, token const& pre)
  {
    return has_prefix(get_symbol(tok), get_symbol(pre));
  }

  token block_scanner::combine(token const& nl, token const& ind)
  {
    token prev = indentation();

    // If the indentations are the same, this is a line separator. Note
    // that we replace the current prefix for diagnostics purposes.
    if (equal_spelling(ind, prev))
    {
      if (!prefix.empty())
        prefix.top() = ind;
      return separate(nl);
    }

    // If the new indentation has the previous as a prefix (i.e., new is
    // longer), then indent by pushing the new indentation.
    if (has_prefix(ind, prev))
    {
      push(ind);
      return indent(nl);
    }

    // If the previous indentation has new as a prefix (i.e, the previous
    // is longer), then dedent by popping the previous indentation and
    // ensuring that the new matches the newly popped stack. Also, update
    // the most recent indentation level for diagnostic purposes.
    if (has_prefix(prev, ind))
    {
      prev = pop();
      if (!equal_spelling(prev, ind))
      {
        Diags.Report(ind.loc, clang::diag::err_bad_string_encoding);
        // error(ind.loc, "indentation does not match previous after dedent");
        if (prev)
          ;// note(prev.loc, "indentation here");
      }
      else if (!prefix.empty())
        prefix.top() = ind;

      return dedent(nl);
    }

    Diags.Report(ind.loc, clang::diag::err_bad_string_encoding);
    // error(ind.loc, "indentation does not match previous");
    if (prev)
      ;// note(prev.loc, "indentation here");

    // Return a line separator just in case.
    return separate(nl);
  }

} // namespace green
