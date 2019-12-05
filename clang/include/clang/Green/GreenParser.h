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

  /// A pair of tokens.
  using TokenPair = std::pair<Token, Token>;

  /// The parser transforms sequences of tokens into uninterpreted syntax
  /// trees.
  ///
  /// \todo Lift common parsing functions into a parameterized base class?
  struct Parser
  {
    Parser(clang::SourceManager &SM, File const& F);

    Token const& peekToken() {
      return Lookahead;
    }

    TokenKind getLookahead() const {
      return Lookahead.getKind();
    }

    clang::SourceLocation getInputLocation() const {
      return Lookahead.getLocation();
    }

    bool atEndOfFile() {
      return Lookahead.isEndOfFile();
    }

    bool nextTokenIs(TokenKind K) {
      return getLookahead() == K;
    }

    bool nextTokenIs(char const* Id) {
      return nextTokenIs(tok::Identifier) && peekToken().hasSpelling(Id);
    }

    bool nextTokenIsNot(TokenKind K) {
      return !nextTokenIs(K);
    }

    bool nextTokenIsNot(char const* Id) {
      return !nextTokenIs(Id);
    }

    Token consumeToken() {
      Token Tok = Lookahead;
      Lookahead = Lex();
      return Tok;
    }

    Token matchToken(TokenKind K)
    {
      if (nextTokenIs(K))
        return consumeToken();
      return {};
    }

    Token matchToken(char const* Id)
    {
      if (nextTokenIs(Id))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchTokenIf(Predicate Pred) {
      if (Pred(getLookahead()))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchTokenIf(Predicate Pred, Parser &P) {
      if (Pred(P))
        return consumeToken();
      return {};
    }

    Token expectToken(TokenKind K);

    Token expectToken(char const* Id);

    Token requireToken(TokenKind k)
    {
      assert(nextTokenIs(k));
      return consumeToken();
    }

    Syntax* parseFile();

    Syntax* parseArray();
    Syntax* parseList();

    Syntax* parseExpr();
    Syntax* parseDef();
    Syntax* parseOr();
    Syntax* parseAnd();
    Syntax* parseCmp();
    Syntax* parseTo();
    Syntax* parseAdd();
    Syntax* parseMul();

    Syntax* parseMacro();
    Syntax* parseIf();
    Syntax* parseWhile();
    Syntax* parseFor();

    Syntax* parsePre();
    Syntax* parsePost();
    Syntax* parseCall(Syntax* fn);
    Syntax* parseElem(Syntax* map);
    Syntax* parseDot(Syntax* obj);

    Syntax* parsePrimary();
    Syntax* parseId();
    Syntax* parseParen();

    Syntax* parseOf();
    Syntax* parseImm();

    Syntax* parseBlock();
    Syntax* parseBracedArray();
    Syntax* parseNestedArray();

    Syntax* parsePreAttr();
    Syntax* parseDocAttr();

    Syntax* parseCatch();

    // Primary expressions
    Syntax* parseReserved();
    Syntax* parseKey();
    Syntax* parseWord();
    Syntax* parseChar();
    Syntax* parseString();
    Syntax* parseNum();

    // Semantic actions

    Syntax* onAtom(Token const& tok);
    Syntax* onArray(std::vector<Syntax*> const& vec);
    Syntax* onList(std::vector<Syntax*> const& vec);
    Syntax* onBinary(Token const& tok, Syntax* e1, Syntax* e2);
    Syntax* onUnary(Token const& tok, Syntax* e1);
    Syntax* onCall(TokenPair const& toks, Syntax* e1, Syntax* e2);
    Syntax* onElem(TokenPair const& toks, Syntax* e1, Syntax* e2);
    Syntax* onMacro(Syntax* e1, Syntax* e2);
    Syntax* onIf(Token const& tok, Syntax* e1, Syntax* e2, Syntax* e3);
    Syntax* onElse(Token const& tok, Syntax* e1);
    Syntax* onLoop(Token const& tok, Syntax* e1, Syntax* e2);

    /// The lexer.
    Lexer Lex;

    /// The lookahead token.
    Token Lookahead;

    /// Diagnostics.
    clang::DiagnosticsEngine &Diags;

    /// True if we are parsing a function parameter list.
    bool ParsingParams = false;
  };

} // namespace green

#endif
