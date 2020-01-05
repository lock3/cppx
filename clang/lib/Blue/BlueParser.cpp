//===- BlueParser.cpp - Blue Language Parser Implementation ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Blue parser.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Blue/BlueParser.h"

#include <iostream>

namespace blue {

namespace {

namespace enc {
enum Kind
{
  Parens,
  Braces,
  Brackets,
};
} // namespace enc

using EnclosureKind = enc::Kind;

TokenKind OpenTokens[]
{
  tok::LeftParen,
  tok::LeftBrace,
  tok::LeftBracket,
};

TokenKind CloseTokens[]
{
  tok::RightParen,
  tok::RightBrace,
  tok::RightBracket,
};

/// A class to help match enclosing tokens.
template<EnclosureKind K>
struct EnclosingTokens
{
  EnclosingTokens(Parser& P)
    : P(P)
  { }

  bool expectOpen() {
    Open = P.expectToken(OpenTokens[K]);
    return (bool)Open;
  }

  bool expectClose() {
    Close = P.expectToken(CloseTokens[K]);
    if (!Close) {
      // FIXME: Emit a diagnostic.
      // note(Open.loc, "matching '{}'' here", spelling(Open.kind()));
    }
    return (bool)Close;
  }

  Parser& P;
  Token Open;
  Token Close;
};

struct EnclosingParens : EnclosingTokens<enc::Parens> {
  using EnclosingTokens<enc::Parens>::EnclosingTokens;
};

struct EnclosingBraces : EnclosingTokens<enc::Braces> {
  using EnclosingTokens<enc::Braces>::EnclosingTokens;
};

struct EnclosingBrackets : EnclosingTokens<enc::Brackets> {
  using EnclosingTokens<enc::Brackets>::EnclosingTokens;
};

// TODO: Handle errors more gracefully?
template<typename Enclosing, typename Parse>
Syntax *parseEnclosed(Parser& P, Parse Fn) {
  Enclosing Tokens(P);
  if (!Tokens.expectOpen())
    return nullptr;
  Syntax *S = Fn();
  if (!Tokens.expectClose())
    return nullptr;
  return S;
}

template<typename Parse>
Syntax *parseParenEnclosed(Parser& P, Parse Fn) {
  return parseEnclosed<EnclosingParens>(P, Fn);
}

template<typename Parse>
Syntax *parseBraceEnclosed(Parser& P, Parse Fn) {
  return parseEnclosed<EnclosingBraces>(P, Fn);
}

template<typename Parse>
Syntax *parseBracketEnclosed(Parser& P, Parse Fn) {
  return parseEnclosed<EnclosingBrackets>(P, Fn);
}

} // namespace

Parser::Parser(clang::SourceManager &SM, File const& F)
  : Lex(SM, F), Diags(SM.getDiagnostics()) {
  fetchToken();
}

Token Parser::expectToken(TokenKind K) {
  if (nextTokenIs(K))
    return consumeToken();
  char const* Spelling = getSpelling(K);
  Diags.Report(getInputLocation(), clang::diag::err_expected) << Spelling;
  return {};
}

Token Parser::expectToken(char const* Id) {
  if (nextTokenIs(Id))
    return consumeToken();
  Diags.Report(getInputLocation(), clang::diag::err_expected) << Id;
  return {};
}

// Syntax productions

Syntax *Parser::parseTranslationUnit() {
  if (!atEndOfFile())
    return parseStatementSeq();
  return nullptr;
}

Syntax *Parser::parseStatementSeq() {
  parseStatement();
  while (!atEndOfFile() && nextTokenIsNot(tok::RightBrace))
    parseStatement();
  return nullptr;
}

// True if the next tokens would start a declaration.
static bool startsDeclaration(Parser& P) {
  // The common case: 'x : ...' or 'x , ...'. Note that the comma currently
  // implies that we'll see a ':' eventually.
  if (P.nextTokenIs(tok::Identifier))
    if (P.nthTokenIs(1, tok::Colon) || P.nthTokenIs(1, tok::Comma))
      return true;

  // The uncommon case: ': type ...'. This is an unnamed declaration.
  if (P.nextTokenIs(tok::Colon))
    return true;

  return false;
}

Syntax *Parser::parseStatement() {
    switch (getLookahead()) {
    case tok::LeftBrace:
      return parseBlockStatement();
    case tok::IfKeyword:
      return parseIfStatement();
    case tok::WhileKeyword:
      return parseWhileStatement();
    case tok::ForKeyword:
      return parseForStatement();
    case tok::BreakKeyword:
      return parseBreakStatement();
    case tok::ContinueKeyword:
      return parseContinueStatement();
    case tok::ReturnKeyword:
      return parseReturnStatement();
    default:
      break;
    }

    if (startsDeclaration(*this))
      return parseDeclarationStatement();

    return parseExpressionStatement();
}

Syntax *Parser::parseBlockStatement() {
  return parseBraceEnclosed(*this, [this]() -> Syntax * {
    if (nextTokenIsNot(tok::RightBrace))
      return parseStatementSeq();
    return nullptr;
  });
  return nullptr;
}

Syntax *Parser::parseIfStatement() {
  requireToken(tok::IfKeyword);
  parseParenEnclosed(*this, [this]() -> Syntax * {
    return parseExpression();
  });
  parseStatement();

  if (matchToken(tok::ElseKeyword))
    parseStatement();

  return nullptr;
}

Syntax *Parser::parseWhileStatement() {
  requireToken(tok::WhileKeyword);
  parseParenEnclosed(*this, [this]() -> Syntax * {
    return parseExpression();
  });
  parseStatement();

  return nullptr;
}

Syntax *Parser::parseForStatement() {
  assert(false && "Not implemented");
  return nullptr;
}

Syntax *Parser::parseBreakStatement() {
  requireToken(tok::BreakKeyword);
  matchToken(tok::Semicolon);
  return nullptr;
}

Syntax *Parser::parseContinueStatement() {
  requireToken(tok::ContinueKeyword);
  matchToken(tok::Semicolon);
  return nullptr;
}

Syntax *Parser::parseReturnStatement() {
  requireToken(tok::ReturnKeyword);
  if (nextTokenIsNot(tok::Semicolon))
    parseExpression();
  matchToken(tok::Semicolon);
  return nullptr;
}

Syntax *Parser::parseDeclarationStatement() {
  return parseDeclaration();
}

Syntax *Parser::parseExpressionStatement() {
  parseExpression();
  matchToken(tok::Semicolon);
  return nullptr;
}

Syntax *Parser::parseDeclaration() {
  return nullptr;
}

Syntax *Parser::parseExpression() {
  return parseAssignmentExpression();
}

Syntax *Parser::parseExpressionList() {
  parseExpression();
  while (matchToken(tok::Comma))
    parseExpression();
  return nullptr;
}

Syntax *Parser::parseAssignmentExpression() {
  Syntax *LHS = parseLogicalOrExpression();
  // FIXME: Support compound assignment operators.
  if (Token Op = matchToken(tok::Equal)) {
    Syntax *RHS = parseAssignmentExpression();
    return onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

Syntax *Parser::parseLogicalOrExpression() {
  Syntax *LHS = parseLogicalAndExpression();
  while (Token Op = matchToken(tok::BarBar)) {
    Syntax *RHS = parseLogicalAndExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

Syntax *Parser::parseLogicalAndExpression() {
  Syntax *LHS = parseEqualityExpression();
  while (Token Op = matchToken(tok::AmpersandAmpersand)) {
    Syntax *RHS = parseEqualityExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isEqualityOperator(TokenKind K) {
  return K == tok::EqualEqual || K == tok::BangEqual;
}

Syntax *Parser::parseEqualityExpression() {
  Syntax *LHS = parseRelationalExpression();
  while (Token Op = matchTokenIf(isEqualityOperator)) {
    Syntax *RHS = parseRelationalExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isRelationalOperator(TokenKind K) {
  return K == tok::Less ||
         K == tok::Greater ||
         K == tok::LessEqual ||
         K == tok::GreaterEqual;
}

Syntax *Parser::parseRelationalExpression() {
  Syntax *LHS = parseShiftExpression();
  while (Token Op = matchTokenIf(isRelationalOperator)) {
    Syntax *RHS = parseShiftExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isShiftOperator(TokenKind K) {
  return K == tok::LessLess || K == tok::GreaterGreater;
}

Syntax *Parser::parseShiftExpression() {
  Syntax *LHS = parseAdditiveExpression();
  while (Token Op = matchTokenIf(isShiftOperator)) {
    Syntax *RHS = parseAdditiveExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isAdditiveOperator(TokenKind K) {
  return K == tok::Plus || K == tok::Minus;
}

Syntax *Parser::parseAdditiveExpression() {
  Syntax *LHS = parseMultiplicativeExpression();
  while (Token Op = matchTokenIf(isAdditiveOperator)) {
    Syntax *RHS = parseMultiplicativeExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isMultiplicativeOperator(TokenKind K) {
  return K == tok::Star || K == tok::Slash || K == tok::Percent;
}

Syntax *Parser::parseMultiplicativeExpression() {
  Syntax *LHS = parseConversionExpression();
  while (Token Op = matchTokenIf(isAdditiveOperator)) {
    Syntax *RHS = parseConversionExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

// FIXME: What other conversion-related expressions do we have here?
Syntax *Parser::parseConversionExpression() {
  Syntax *LHS = parsePrefixExpression();
  while (Token Op = matchToken(tok::Colon)) {
    Syntax *RHS = parsePrefixExpression();
    LHS = onBinaryOperator(Op, LHS, RHS);
  }
  return LHS;
}

static bool isPrefixOperator(TokenKind K) {
  switch (K) {
  case tok::Plus:
  case tok::Minus:
  case tok::Caret:
    return true;
  default:
    return false;
  }
}

Syntax *Parser::parsePrefixExpression() {
  if (Token Op = matchTokenIf(isPrefixOperator)) {
    Syntax *Arg = parsePrefixExpression();
    return onUnaryOperator(Op, Arg);
  }
  return parsePostfixExpression();
}

Syntax *Parser::parsePostfixExpression() {
  Syntax *E = parsePrimaryExpression();
  while (true) {
    switch (getLookahead()) {
    case tok::Comma:       // Ends an expression in a list.
    case tok::Semicolon:   // Ends an expression statement.
    case tok::LeftParen:   // Ends a condition or call.
    case tok::LeftBracket: // Ends an index.
      return E;

    case tok::Dot:
      E = parseAccessExpression(E);
      break;

    default:
      E = parseApplicationExpression(E);
    }
  }
}

Syntax *Parser::parseCallExpression(Syntax *LHS) {
  Syntax *Args = parseParenEnclosed(*this, [this]() {
    if (nextTokenIsNot(tok::RightParen))
      parseExpressionList();
    return nullptr;
  });

  // FIXME: Build the call expression.
  return nullptr;
}

Syntax *Parser::parseIndexExpression(Syntax *LHS) {
  Syntax *Args = parseBracketEnclosed(*this, [this]() {
    if (nextTokenIsNot(tok::RightBracket))
      parseExpressionList();
    return nullptr;
  });

  // FIXME: Build the index expression.
  return nullptr;
}

Syntax *Parser::parseAccessExpression(Syntax *LHS) {
  requireToken(tok::Dot);
  parseIdExpression();

  // FIXME: Build the member expression.
  return nullptr;
}

Syntax *Parser::parseApplicationExpression(Syntax *LHS) {
  Syntax *RHS = parsePrimaryExpression();

  // FIXME: Build the application.
  return nullptr;
}

Syntax *Parser::parsePrimaryExpression() {
  switch (getLookahead()) {
  case tok::BinaryInteger:
  case tok::DecimalInteger:
  case tok::HexadecimalInteger:
  case tok::DecimalFloat:
  case tok::HexadecimalFloat:
  case tok::Character:
  case tok::String:
    return onLiteral(consumeToken());

  case tok::Identifier:
    return parseIdExpression();

  case tok::LeftParen:
    return parseParenExpression();

  case tok::LeftBracket:
    return parseBracketExpression();

  default:
    // FIXME: Obviously, this should be an error.
    assert(false && "Not a primary expression");
    break;
  }
  return nullptr;
}

Syntax *Parser::parseIdExpression() {
  Token Id = requireToken(tok::Identifier);
  return onIdentifier(Id);
}

Syntax *Parser::parseParenExpression() {
  Syntax *Args = parseParenEnclosed(*this, [this]() -> Syntax * {
    if (nextTokenIsNot(tok::RightParen))
      return parseExpressionList();
    return nullptr;
  });

  // FIXME: Build a paren list.
  return nullptr;
}

Syntax *Parser::parseBracketExpression() {
  Syntax *Args = parseParenEnclosed(*this, [this]() -> Syntax * {
    if (nextTokenIsNot(tok::RightParen))
      return parseExpressionList();
    return nullptr;
  });

  // FIXME: Build a bracket list.
  return nullptr;
}

Syntax *Parser::onUnaryOperator(const Token &Tok, Syntax *Arg) {
  return nullptr;
}

Syntax *Parser::onBinaryOperator(const Token &Tok, Syntax *LHS, Syntax *RHS) {
  return nullptr;
}

Syntax *Parser::onLiteral(const Token &Tok) {
  return nullptr;
}

Syntax *Parser::onIdentifier(const Token &Tok) {
  return nullptr;
}

} // namespace blue
