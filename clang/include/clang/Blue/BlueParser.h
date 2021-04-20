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
#include "clang/Blue/BlueSyntax.h"
#include "clang/Blue/BlueTokens.h"

namespace clang {

class DiagnosticsEngine;

} // namespace clang

namespace blue
{
  class Syntax;
  using SyntaxSeq = llvm::SmallVectorImpl<Syntax *>;

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
    template<typename Callable>
    bool nthTokenConformsTo(std::size_t N, Callable CB) {
      return CB(getLookahead(N));
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
      PreviousToken = Tok;
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
    Syntax *parseFile();

    // Statements
    Syntax *parseBlockStatement();
    Syntax *parseDeclarationStatement();
    Syntax *parseExpressionStatement();

    // Declarations
    Syntax *parseDeclaration();

    // Signatures and initializers
    Syntax *parseEqualInitializer();
    Syntax *parseBraceInitializer();

    // Parameters
    Syntax *parseParameterGroup();
    Syntax *parseParameterList();
    Syntax *parseParameter();

    // Expressions
    Syntax *parseExpression();
    Syntax *parseAssignmentExpression();
    Syntax *parseImplicationExpression();
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
    Syntax *parseMemberExpression(Syntax *E);
    Syntax *parseCallExpression(Syntax *E);
    Syntax *parseIndexExpression(Syntax *E);
    Syntax *parseBraceExpression(Syntax *E);
    Syntax *parseApplicationExpression(Syntax *E);
    Syntax *parsePointerExpression(Syntax *E);
    Syntax *parsePointerExpression();
    Syntax *parsePrimaryExpression();
    Syntax *parseBuiltinCompilerOp();
    Syntax *parseIdExpression();
    Syntax *parseTupleExpression();
    Syntax *parseArrayExpression();
    Syntax *parseBlockExpression();

    // Semantic actions
    Syntax *onLiteral(const Token &Tok);
    Syntax *onIdentifier(const Token &Tok);
    Syntax *onUnary(const Token &Op, Syntax *Arg);
    Syntax *onPostfixUnary(const Token &Op, Syntax *Arg);
    Syntax *onBinary(const Token &Op, Syntax *LHS, Syntax *RHS);
    Syntax *onList(TokenKind K, llvm::SmallVectorImpl<Syntax *> &SS);
    Syntax *onTuple(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS);
    Syntax *onArray(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS);
    Syntax *onBlock(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS);
    Syntax *onDef(const Token &Tok, Syntax *Sig, Syntax *Init);
    Syntax *onControl(const Token &Tok, Syntax *Sig, Syntax *Block);
    Syntax *onTop(llvm::SmallVectorImpl<Syntax *> &SS);
    Syntax* onError(char const* Msg);

    // BLUEL3 PARSING
    Syntax *parseDeclSequence(SyntaxSeq &SS);
    Syntax *parseDefinition();
    Syntax *parseConstraint();
    Syntax *parsePattern();
    Syntax *parsePatternList();
    Syntax *parseDescriptor();
    Syntax *parseDeclaratorList();
    Syntax *parseDeclarator();
    Syntax *parseMappingDescriptor();
    Syntax *parseLeaveExpression();
    Syntax *parseControlExpression();
    Syntax *parseConditionalExpression();
    Syntax *parseStatementSeq();
    Syntax *parseStatement();
    Syntax *parseExpressionList();
    Syntax *parseBlock();
    Syntax *parseMatchExpression();
    Syntax *parseCaseList();
    Syntax *parseCase();
    Syntax *parseLoopExpression();
    Syntax *parseForExpression();
    Syntax *parseWhileExpression();
    Syntax *parseDoExpression();
    Syntax *parseRangeForExpression();
    Syntax *parseTraditionalForExpression();
    Syntax *parseLetExpression();
    Syntax *parseLambdaExpression();
    Syntax *parseCapture();
    

    Syntax *parseTemplateConstructor();
    Syntax *parseArrayConstructor();
    Syntax *parseFunctionConstructor();

    // Generic Parse Utilites
    enum class Enclosure
    {
      Parens,
      Brackets,
      Braces,
    };

    struct EnclosingTokens
    {
      tok::TokenKind Open;
      tok::TokenKind Close;
    };

    EnclosingTokens EnclosingToks[3] = {
      { tok::LeftParen,   tok::RightParen   },
      { tok::LeftBracket, tok::RightBracket },
      { tok::LeftBrace,   tok::RightBrace   },
    };

    tok::TokenKind openToken(Enclosure E) {
      return EnclosingToks[(int)E].Open;
    }

    tok::TokenKind closeToken(Enclosure E) {
      return EnclosingToks[(int)E].Close;
    }

    /// Parse a list enclosed by the tokens of E. Note that a list
    /// is comprised of groups, so that's allowed.
    template<Enclosure E, typename F>
    Syntax* parseEnclosed(F fn)
    {
      Token Open = requireToken(openToken(E));
      Syntax *T = nullptr;
      if (nextTokenIsNot(closeToken(E)))
        T = (this->*fn)();
      Token Close = expectToken(closeToken(E));
      return new EnclosureSyntax(Open, Close, T);
    }

    template<typename F>
    Syntax* parseParenEnclosed(F fn)
    {
      return parseEnclosed<Enclosure::Parens>(fn);
    }

    template<typename F>
    Syntax* parseBracketEnclosed(F fn)
    {
      return parseEnclosed<Enclosure::Brackets>(fn);
    }

    template<typename F>
    Syntax* parseBraceEnclosed(F fn)
    {
      return parseEnclosed<Enclosure::Braces>(fn);
    }

    /// A helper function for parsing items in a list or sequence.
    /// Accumulates the result in `ss`.
    template<typename P, typename F>
    static Syntax* parseItem(P &parser, F fn, SyntaxSeq &ss)
    {
      // TODO: If we represent syntax errors explicitly, then
      // the parser will always return a non-null pointer.
      Syntax *S = (parser.*fn)();
      // if (S)
      ss.push_back(S);
      return S;
    }

    /// The lexer.
    Lexer Lex;

    /// Lookahead tokens.
    std::deque<Token> Toks;

    /// Diagnostics.
    clang::DiagnosticsEngine &Diags;

    Token PreviousToken;

    // How deep into nested {}'s are we?
    unsigned BraceDepth = 0;

    // How deep into nested {}'s were we when we started tracking?
    unsigned BeginningBraceDepth = 0;

    bool ParsingControl = false;
    bool InBody = false;
  };

  struct BooleanRAII {
    BooleanRAII(bool &Boolean, bool Val)
      : Boolean(Boolean)
      {
        SavedVal = Boolean;
        Boolean = Val;
      }

    ~BooleanRAII() {
      Boolean = SavedVal;
    }

  private:
    bool &Boolean;
    bool SavedVal;
  };

} // namespace blue

#endif
