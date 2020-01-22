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
#include "clang/Blue/BlueSyntax.h"

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
    Open = P.requireToken(OpenTokens[K]);
    return true;
  }

  bool expectClose() {
    Close = P.expectToken(CloseTokens[K]);
    if (!Close) {
      // FIXME: Emit a diagnostic.
    }
    return (bool)Close;
  }

  TokenPair getEnclosingTokens() const {
    return {Open, Close};
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

// FIXME: Return error nodes on failed expectations.
template<typename Enclosing, typename Parse>
Syntax *parseEnclosed(Parser &P, Parse Fn) {
  Enclosing Tokens(P);
  if (!Tokens.expectOpen())
    return nullptr;
  Syntax *S = Fn();
  if (!Tokens.expectClose())
    return nullptr;
  return S;
}

template<typename Parse>
Syntax *parseParenEnclosed(Parser &P, Parse Fn) {
  return parseEnclosed<EnclosingParens>(P, Fn);
}

template<typename Parse>
Syntax *parseBracketEnclosed(Parser &P, Parse Fn) {
  return parseEnclosed<EnclosingBrackets>(P, Fn);
}

template<typename Parse>
Syntax *parseBraceEnclosed(Parser &P, Parse Fn) {
  return parseEnclosed<EnclosingBraces>(P, Fn);
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

Syntax *Parser::parseFile() {
  llvm::SmallVector<Syntax *, 16> SS;
  if (!atEndOfFile())
    parseStatementSeq(SS);
  return onTop(SS);
}

template<typename Parse, typename Sequence>
static Syntax *parseIntoVector(Sequence &Seq, Parse Fn) {
  Syntax *S = Fn();
  if (!S || S->isError())
    return S;
  Seq.push_back(S);
  return S;
}

void Parser::parseStatementSeq(llvm::SmallVectorImpl<Syntax *> &SS) {
  parseIntoVector(SS, [this]() { return parseStatement(); });
  while (!atEndOfFile() && nextTokenIsNot(tok::RightBrace))
    parseIntoVector(SS, [this]() { return parseStatement(); });
}

// True if the next tokens would start a declaration. That is, we would
// match the tokens 'identifier :'. No other sequence of tokens matches
// a declarations.
static bool startsDeclaration(Parser& P) {
  if (P.nextTokenIs(tok::Identifier))
    if (P.nthTokenIs(1, tok::Colon))
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

// FIXME: Return errors for missed expectations.
Syntax *Parser::parseBlockStatement() {
  EnclosingBraces Braces(*this);
  if (!Braces.expectOpen())
    return nullptr;

  llvm::SmallVector<Syntax *, 4> SS;
  if (nextTokenIsNot(tok::RightBrace))
    parseStatementSeq(SS);

  if (!Braces.expectClose())
    return nullptr;
  return onSeq(Braces.getEnclosingTokens(), SS);
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

/// Parse a declaration statement:
///
///   declaration-statement:
///     declaration
Syntax *Parser::parseDeclarationStatement() {
  return parseDeclaration();
}

/// Parse an expression-statement:
///
///   expression-statement:
///     expression ;
Syntax *Parser::parseExpressionStatement() {
  Syntax* e = parseExpression();
  matchToken(tok::Semicolon);
  return e;
}

/// Parse a declaration, which has one of the following forms:
///
///   declaration:
///     identifier : declarator ;
///     identifier : equal-initializer
///     identifier : declarator initializer
///
/// TODO: Support a multi-declarator syntax.
Syntax *Parser::parseDeclaration() {
  Token Id = requireToken(tok::Identifier);
  expectToken(tok::Colon);

  // Match 'identifier := initializer'
  if (nextTokenIs(tok::Equal))
  {
    Syntax *Init = parseEqualInitializer();
    return onDef(Id, nullptr, Init);
  }

  // Match 'identifier : declarator ...'.
  Syntax * Decl = parseDeclarator();

  // Match 'identifier : declarator ;'.
  if (matchToken(tok::Semicolon))
    return onDef(Id, Decl, nullptr);

  // Match 'identifier : declarator initializer'.
  Syntax *Init = parseInitializer();
  return onDef(Id, Decl, Init);
}

/// Parse a declarator.
///
///   declarator:
///     postfix-expression
Syntax *Parser::parseDeclarator() {
  return parsePostfixExpression();
}

/// Parse an initializer.
///
///   initializer:
///     equal-initializer
///     brace-initializer
Syntax *Parser::parseInitializer() {
  if (nextTokenIs(tok::Equal))
    return parseEqualInitializer();
  if (nextTokenIs(tok::LeftBrace))
    return parseBraceInitializer();

  // TODO: Recover more gracefully.
  Syntax *Err = onError("expected initializer");
  consumeToken();
  return Err;
}

/// Parse an equal-initializer.
///
///   equal-initializer:
///     = expression-statement
Syntax *Parser::parseEqualInitializer() {
  requireToken(tok::Equal);
  return parseExpressionStatement();
}

/// Parse an equal-initializer.
///
///   brace-initializer:
///     block-statement
Syntax *Parser::parseBraceInitializer() {
  return parseBlockStatement();
}

/// Parse an expression.
///
///   expression:
///     assignment-expression
Syntax *Parser::parseExpression() {
  return parseAssignmentExpression();
}

/// Parse a parameter group.
///
///   parameter-group:
///     parameter-list
///     parameter-group ; parameter-list
void Parser::parseParameterGroup(llvm::SmallVectorImpl<Syntax *> &SS) {
  auto Parse = [this]() { return parseParameterList(); };
  parseIntoVector(SS, Parse);
  while (matchToken(tok::Semicolon))
    parseIntoVector(SS, Parse);
}

/// Parse an parameter list.
///
///   parameter-list:
///     parameter
///     parameter-list , parameter
Syntax *Parser::parseParameterList()
{
  llvm::SmallVector<Syntax *, 4> SS;
  parseParameterList(SS);
  return onList(tok::Comma, SS);
}

void Parser::parseParameterList(llvm::SmallVectorImpl<Syntax *> &SS) {
  auto Parse = [this]() { return parseParameter(); };
  parseIntoVector(SS, Parse);
  while (matchToken(tok::Comma))
    parseIntoVector(SS, Parse);
}

/// Parse a formal or actual parameter.
///
///   parameter:
///     formal-parameter
///     actual-parameter
Syntax *Parser::parseParameter() {
  if (startsDeclaration(*this))
    return parseFormalParameter();
  return parseActualParameter();
}

/// Parse a formal parameter (i.e., parameter).
///
///   formal-parameter:
///     identifier : declarator
///     identifier : = expression
///     identifier : declarator = expression
Syntax *Parser::parseFormalParameter() {
  Token Id = requireToken(tok::Identifier);
  matchToken(tok::Colon);

  // Match 'identifier : = expression'
  if (matchToken(tok::Equal)) {
    Syntax *Def = parseExpression();
    return onDef(Id, nullptr, Def);
  }

  // Match 'identifier : declarator ...'.
  Syntax *Decl = parseDeclarator();

  // Match 'identifier : declarator = expression'.
  Syntax *Def = nullptr;
  if (matchToken(tok::Equal))
    Def = parseExpression();

  return onDef(Id, Decl, Def);
}

/// Parse an actual parameter (i.e., argument).
///
///   actual-parameter (i.e., argument):
///     expression
Syntax *Parser::parseActualParameter() {
  return parseExpression();
}

Syntax *Parser::parseAssignmentExpression() {
  Syntax *LHS = parseLogicalOrExpression();
  // FIXME: Support compound assignment operators.
  if (Token Op = matchToken(tok::Equal)) {
    Syntax *RHS = parseAssignmentExpression();
    return onBinary(Op, LHS, RHS);
  }
  return LHS;
}

Syntax *Parser::parseLogicalOrExpression() {
  Syntax *LHS = parseLogicalAndExpression();
  while (Token Op = matchToken(tok::BarBar)) {
    Syntax *RHS = parseLogicalAndExpression();
    LHS = onBinary(Op, LHS, RHS);
  }
  return LHS;
}

Syntax *Parser::parseLogicalAndExpression() {
  Syntax *LHS = parseEqualityExpression();
  while (Token Op = matchToken(tok::AmpersandAmpersand)) {
    Syntax *RHS = parseEqualityExpression();
    LHS = onBinary(Op, LHS, RHS);
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
    LHS = onBinary(Op, LHS, RHS);
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
    LHS = onBinary(Op, LHS, RHS);
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
    LHS = onBinary(Op, LHS, RHS);
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
    LHS = onBinary(Op, LHS, RHS);
  }
  return LHS;
}

static bool isMultiplicativeOperator(TokenKind K) {
  return K == tok::Star || K == tok::Slash || K == tok::Percent;
}

Syntax *Parser::parseMultiplicativeExpression() {
  Syntax *LHS = parseConversionExpression();
  while (Token Op = matchTokenIf(isMultiplicativeOperator)) {
    Syntax *RHS = parseConversionExpression();
    LHS = onBinary(Op, LHS, RHS);
  }
  return LHS;
}

// FIXME: What other conversion-related expressions do we have here?
Syntax *Parser::parseConversionExpression() {
  Syntax *LHS = parsePrefixExpression();
  while (Token Op = matchToken(tok::Colon)) {
    Syntax *RHS = parsePrefixExpression();
    LHS = onBinary(Op, LHS, RHS);
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
    return onUnary(Op, Arg);
  }
  return parsePostfixExpression();
}

/// Parse a postfix expression.
///
///   postfix-expression:
///     primary-expression
///     postfix-expression . identifier
///     postfix-expression paren-expression
///     postfix-expression brace-expression
///     postfix-expression identifier
Syntax *Parser::parsePostfixExpression() {
  Syntax *E = parsePrimaryExpression();
  while (true) {
    switch (getLookahead()) {
    case tok::Dot:
      E = parseMemberExpression(E);
      break;

    case tok::LeftParen:
      E = parseCallExpression(E);
      break;

    case tok::LeftBrace:
      E = parseIndexExpression(E);
      break;

    case tok::Identifier:
      E = parseApplicationExpression(E);
      break;

    default:
      return E;
    }
  }
}

Syntax *Parser::parseMemberExpression(Syntax *LHS) {
  Token Op = requireToken(tok::Dot);
  Syntax *RHS = parseIdExpression();
  return  onBinary(Op, LHS, RHS);
}

Syntax *Parser::parseCallExpression(Syntax *LHS) {
  Syntax *RHS = parseParenExpression();
  return onBinary(Token(), LHS, RHS);
}

Syntax *Parser::parseIndexExpression(Syntax *LHS) {
  Syntax *RHS = parseBracketExpression();
  return onBinary(Token(), LHS, RHS);
}

Syntax *Parser::parseApplicationExpression(Syntax *LHS) {
  Syntax *RHS = parsePrimaryExpression();
  return onBinary(Token(), LHS, RHS);
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

  default: {
    // TODO: Add "but got..." to the error.
    Syntax *Err = onError("expected primary-expression");
    consumeToken();
    return Err;
  }
  }
  return nullptr;
}

Syntax *Parser::parseIdExpression() {
  Token Id = requireToken(tok::Identifier);
  return onIdentifier(Id);
}

// FIXME: Return errors as needed.
Syntax *Parser::parseParenExpression() {
  EnclosingParens Parens(*this);
  if (!Parens.expectOpen())
    return nullptr;

  llvm::SmallVector<Syntax *, 4> SS;
  if (nextTokenIsNot(tok::RightParen))
    parseParameterGroup(SS);

  if (!Parens.expectClose())
    return nullptr;
  return onTuple(Parens.getEnclosingTokens(), SS);
}

Syntax *Parser::parseBracketExpression() {
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return nullptr;

  llvm::SmallVector<Syntax *, 4> SS;
  if (nextTokenIsNot(tok::RightBracket))
    parseParameterGroup(SS);

  if (!Brackets.expectClose())
    return nullptr;
  return onArray(Brackets.getEnclosingTokens(), SS);
}

// Semantic actions

// FIXME: Allocate monotonically.
static llvm::ArrayRef<Syntax *> makeArray(llvm::SmallVectorImpl<Syntax *> &SS) {
  Syntax **Array = new Syntax *[SS.size()];
  std::copy(SS.begin(), SS.end(), Array);
  return llvm::ArrayRef<Syntax *>(Array, SS.size());
}

Syntax *Parser::onLiteral(const Token &Tok) {
  return new LiteralSyntax(Tok);
}

Syntax *Parser::onIdentifier(const Token &Tok) {
  return new IdentifierSyntax(Tok);
}

Syntax *Parser::onUnary(const Token &Op, Syntax *Arg) {
  return new UnarySyntax(Op, Arg);
}

Syntax *Parser::onBinary(const Token &Op, Syntax *LHS, Syntax *RHS) {
  return new BinarySyntax(Op, LHS, RHS);
}

Syntax *Parser::onList(TokenKind K, llvm::SmallVectorImpl<Syntax *> &SS) {
  return new ListSyntax(K, makeArray(SS));
}

static Syntax *FlattenGroup(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS) {
  // Replace empty groups with empty lists.
  if (SS.empty())
    return new ListSyntax(Enc, tok::Comma, llvm::None);

  // Replace singleton groups with their first list.
  if (SS.size() == 1)
    return SS.front();

  // Return a new group.
  return new ListSyntax(Enc, tok::Semicolon, makeArray(SS));
}

Syntax *Parser::onTuple(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS) {
  return FlattenGroup(Enc, SS);
}

Syntax *Parser::onArray(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS) {
  return FlattenGroup(Enc, SS);
}

Syntax *Parser::onSeq(const TokenPair &Enc, llvm::SmallVectorImpl<Syntax *> &SS) {
  return new SeqSyntax(Enc, makeArray(SS));
}

Syntax *Parser::onDef(const Token &Tok, Syntax *Sig, Syntax *Init) {
  return new DefSyntax(Tok, Sig, Init);
}

Syntax *Parser::onTop(llvm::SmallVectorImpl<Syntax *> &SS) {
  return new TopSyntax(makeArray(SS));
}

Syntax *Parser::onError(char const* Msg) {
  // FIXME: Use Clang diagnostics.
  llvm::errs() << "error: " << Msg << '\n';

  // FIXME: Maybe make this a singleton?
  return new ErrorSyntax();
}

} // namespace blue
