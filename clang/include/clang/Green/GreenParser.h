//===- GreenParser.h - Green Language Parser ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenParser interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENPARSER_H
#define CLANG_GREEN_GREENPARSER_H

#include "clang/Green/GreenLexer.h"

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"

namespace clang {

class DiagnosticsEngine;

} // namespace clang


namespace green
{
  struct Syntax;

  // FIXME: CHANGE THIS BACK
  struct parser;

  /// A pair of tokens.
  using token_pair = std::pair<token, token>;

  /// The parser transforms sequences of tokens into uninterpreted syntax
  /// trees.
  ///
  /// \todo Lift common parsing functions into a parameterized base class?
  struct parser
  {
    parser(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags,
           file const& f);

    bool eof()
    {
      return !la;
    }

    token const& peek()
    {
      return la;
    }

    token_kind lookahead() const
    {
      return la.kind();
    }

    clang::SourceLocation input_location() const
    {
      return la.loc;
    }

    bool next_token_is(token_kind k)
    {
      return lookahead() == k;
    }

    bool next_token_is(char const* id)
    {
      return next_token_is(tok_identifier) && peek().has_spelling(id);
    }

    bool next_token_is_not(token_kind k)
    {
      return !next_token_is(k);
    }

    bool next_token_is_not(char const* id)
    {
      return !next_token_is(id);
    }

    token consume()
    {
      token tok = la;
      la = lex();
      return tok;
    }

    token match(token_kind k)
    {
      if (next_token_is(k))
        return consume();
      return {};
    }

    token match(char const* id)
    {
      if (next_token_is(id))
        return consume();
      return {};
    }

    template<typename Fn>
    token match_if(Fn pred) {
      if (pred(lookahead()))
        return consume();
      return {};
    }

    template<typename Fn>
    token match_if(Fn pred, parser &p) {
      if (pred(p))
        return consume();
      return {};
    }

    token expect(token_kind k);

    token expect(char const* id)
    {
      if (next_token_is(id))
        return consume();
      return {};
    }

    token require(token_kind k)
    {
      assert(next_token_is(k));
      return consume();
    }

    Syntax* parse_file();

    Syntax* parse_array();
    Syntax* parse_list();

    Syntax* parse_expr();
    Syntax* parse_def();
    Syntax* parse_or();
    Syntax* parse_and();
    Syntax* parse_cmp();
    Syntax* parse_to();
    Syntax* parse_add();
    Syntax* parse_mul();

    Syntax* parse_macro();
    Syntax* parse_if();
    Syntax* parse_while();
    Syntax* parse_for();

    Syntax* parse_pre();
    Syntax* parse_post();
    Syntax* parse_call(Syntax* fn);
    Syntax* parse_elem(Syntax* map);
    Syntax* parse_dot(Syntax* obj);

    Syntax* parse_primary();
    Syntax* parse_id();
    Syntax* parse_paren();

    Syntax* parse_of();
    Syntax* parse_imm();

    Syntax* parse_block();
    Syntax* parse_braced_array();
    Syntax* parse_nested_array();

    Syntax* parse_pre_attr();
    Syntax* parse_doc_attr();

    Syntax* parse_catch();

    // Primary expressions
    Syntax* parse_reserved();
    Syntax* parse_key();
    Syntax* parse_word();
    Syntax* parse_char();
    Syntax* parse_string();
    Syntax* parse_num();

    // Semantic actions

    Syntax* on_atom(token const& tok);
    Syntax* on_array(std::vector<Syntax*> const& vec);
    Syntax* on_list(std::vector<Syntax*> const& vec);
    Syntax* on_binary(token const& tok, Syntax* e1, Syntax* e2);
    Syntax* on_unary(token const& tok, Syntax* e1);
    Syntax* on_call(token_pair const& toks, Syntax* e1, Syntax* e2);
    Syntax* on_elem(token_pair const& toks, Syntax* e1, Syntax* e2);
    Syntax* on_macro(Syntax* e1, Syntax* e2);
    Syntax* on_if(token const& tok, Syntax* e1, Syntax* e2, Syntax* e3);
    Syntax* on_else(token const& tok, Syntax* e1);
    Syntax* on_loop(token const& tok, Syntax* e1, Syntax* e2);

    /// The lexer.
    lexer lex;

    /// The lookahead token.
    token la;

    clang::DiagnosticsEngine &Diags;

    /// True if we are parsing a function parameter list.
    bool ParsingParams = false;
  };

} // namespace green

#endif
