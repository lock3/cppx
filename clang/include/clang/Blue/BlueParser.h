//===- BlueParser.h - Blue Language Parser ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Blue Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEPARSER_H
#define CLANG_BLUE_BLUEPARSER_H

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"

#include "clang/Blue/BlueLexer.h"

namespace clang {

class DiagnosticsEngine;

} // namespace clang

namespace blue
{
  struct Syntax;

  /// The parser transforms sequences of tokens into uninterpreted syntax
  /// trees.
  ///
  /// \todo Lift common parsing functions into a parameterized base class?
  struct Parser {
    Parser(clang::SourceManager &SM, File const& F);

    void fetchToken() {
      Toks.push_back(Lex());
    }

    Token const& peekToken() const {
      return Toks.front();
    }

    Token const& peekToken(std::size_t N) {
      // The token is in the lookahead buffer.
      if (N < Toks.size())
        return Toks[N];

      // Load tokens until we reach the nth token.
      N = N - Toks.size() + 1;
      while (N != 0) {
        fetchToken();
        --N;
      }

      return Toks.back();
    }

    TokenKind getLookahead() const {
      return peekToken().getKind();
    }

    TokenKind getLookahead(std::size_t N) {
      return peekToken(N).getKind();
    }

    clang::SourceLocation getInputLocation() const {
      return peekToken().getLocation();
    }

    bool atEndOfFile() {
      return peekToken().isEndOfFile();
    }

    bool nextTokenIs(TokenKind K) const {
      return getLookahead() == K;
    }

    bool nextTokenIs(char const* Id) const {
      return nextTokenIs(tok::Identifier) && peekToken().hasSpelling(Id);
    }

    bool nthTokenIs(std::size_t N, TokenKind K) {
      return getLookahead(N) == K;
    }

    bool nthTokenIs(std::size_t N, char const* Id) {
      const Token &Tok = peekToken(N);
      return Tok.hasKind(tok::Identifier) && Tok.hasSpelling(Id);
    }

    bool nextTokenIsNot(TokenKind K) {
      return !nextTokenIs(K);
    }

    bool nextTokenIsNot(char const* Id) {
      return !nextTokenIs(Id);
    }

    bool nthTokenIsNot(std::size_t N, TokenKind K) {
      return !nthTokenIs(N, K);
    }

    bool nthTokenIsNot(std::size_t N, char const* Id) {
      return !nthTokenIs(N, Id);
    }

    Token consumeToken() {
      // Take the front token.
      Token Tok = Toks.front();
      Toks.pop_front();

      // Refresh the queue.
      if (Toks.empty())
        fetchToken();

      return Tok;
    }

    Token matchToken(TokenKind K) {
      if (nextTokenIs(K))
        return consumeToken();
      return {};
    }

    Token matchToken(char const* Id) {
      if (nextTokenIs(Id))
        return consumeToken();
      return {};
    }

    Token matchNthToken(std::size_t N, TokenKind K) {
      if (nthTokenIs(N, K))
        return consumeToken();
      return {};
    }

    Token matchNthToken(std::size_t N, char const* Id) {
      if (nthTokenIs(N, Id))
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
    Token matchNthTokenIf(std::size_t N, Predicate Pred) {
      if (Pred(getLookahead()))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchTokens(Predicate Pred, Parser &P) {
      if (Pred(P))
        return consumeToken();
      return {};
    }

    Token expectToken(TokenKind K);

    Token expectToken(char const* Id);

    Token requireToken(TokenKind k) {
      assert(nextTokenIs(k));
      return consumeToken();
    }

    // Syntax productions

    // Top-level
    Syntax *parseTranslationUnit();

    // Statements
    Syntax *parseStatementSeq();
    Syntax *parseStatement();
    Syntax *parseBlockStatement();
    Syntax *parseIfStatement();
    Syntax *parseWhileStatement();
    Syntax *parseForStatement();
    Syntax *parseBreakStatement();
    Syntax *parseContinueStatement();
    Syntax *parseReturnStatement();
    Syntax *parseDeclarationStatement();
    Syntax *parseExpressionStatement();

    // Declarations
    Syntax *parseDeclaration();

    // Expressions
    Syntax *parseExpression();
    Syntax *parseExpressionList();
    Syntax *parseAssignmentExpression();
    Syntax *parseLogicalOrExpression();
    Syntax *parseLogicalAndExpression();
    Syntax *parseEqualityExpression();
    Syntax *parseRelationalExpression();
    Syntax *parseShiftExpression();
    Syntax *parseAdditiveExpression();
    Syntax *parseMultiplicativeExpression();
    Syntax *parseConversionExpression();
    Syntax *parsePrefixExpression();
    Syntax *parsePostfixExpression();
    Syntax *parseCallExpression(Syntax *e);
    Syntax *parseIndexExpression(Syntax *e);
    Syntax *parseAccessExpression(Syntax *e);
    Syntax *parseApplicationExpression(Syntax *e);
    Syntax *parsePrimaryExpression();
    Syntax *parseIdExpression();
    Syntax *parseParenExpression();
    Syntax *parseBracketExpression();

    // Semantic actions
    Syntax *onLiteral(const Token &Tok);
    Syntax *onIdentifier(const Token &Tok);
    Syntax *onUnaryOperator(const Token &Op, Syntax *Arg);
    Syntax *onBinaryOperator(const Token &Op, Syntax *LHS, Syntax *RHS);

    /// The lexer.
    Lexer Lex;

    /// Lookahead tokens.
    std::deque<Token> Toks;

    /// Diagnostics.
    clang::DiagnosticsEngine &Diags;
  };

} // namespace blue

#endif
