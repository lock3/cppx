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
  return onBlock(SS);
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

/// Parse a declaration, which has one of the following forms:
///
/// declaration:
///   identifier : signature ;
///   identifier : = definition
///   identifier : signature = definition
///
/// definition:
///   = expression-statement
///   block-statement
///
/// TODO: Support a multi-declarator syntax.
Syntax *Parser::parseDeclaration() {
  Token Id = requireToken(tok::Identifier);
  expectToken(tok::Colon);

  Syntax *Sig = nullptr;
  if (nextTokenIsNot(tok::Equal))
    Sig = parseSignature();

  Syntax* Init;
  if (matchToken(tok::Semicolon))
    Init = nullptr;
  else if (matchToken(tok::Equal))
    Init = parseExpressionStatement();
  else if (nextTokenIs(tok::LeftBrace))
    Init = parseBlockStatement();
  else
    Init = onError("expected definition");

  return onDef(Id, Sig, Init);
}

Syntax *Parser::parseSignature() {
  return parsePostfixExpression();
}

Syntax *Parser::parseExpression() {
  return parseAssignmentExpression();
}

/// Parse an argument list of the form:
///
///   argument-array:
///     argument-list
///     argument-array ; argument-list
///
///   argument-list:
///     argument
///     argument-list , argument
///
///   argument:
///     parameter
///     parameter-list
///     expression
///
///   parameter:
///     identifier ':' signature
///     identifier ':' signature = expression
///
///   parameter-list:
///     identifier-list ':' signature
///
///   identifier-list:
///     identifier
///     identifier-list ',' identifier
///
/// There is an ambiguity in arguments that can be resolved semantically. An
/// argument-list comprised of only identifiers except that the last term is a
/// parameter with no default argument, then that is a parameter-list.
///
/// FIXME: Parse argument arrays. I'm not sure how we want to represent these
/// syntactically. We probably jut want a tuple whose individual elements may
/// be parameter-lists. Note that we want the following to be semantically
/// equivalent:
///
///   f(x:int, y:bool, z:char)
///   f(x:int, y:bool; z:char)
///
/// There's no reason to have multiple representations for these declarations.
/// We also probably want these to be equivalent:
///
///   f(x, y : int)
///   f(x:int, y:int)
///
/// because their types are equivalent. Note that this makes us less compatible
/// with C++, since equivalence is defined in terms of syntax, not semantics.
///
/// FIXME: Parse parameter lists. This may require a tentative parse.
void Parser::parseArgumentList(llvm::SmallVectorImpl<Syntax *> &SS) {
  parseIntoVector(SS, [this]() { return parseExpression(); });
  while (matchToken(tok::Comma))
    parseIntoVector(SS, [this]() { return parseExpression(); });
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

Syntax *Parser::parsePostfixExpression() {
  Syntax *E = parsePrimaryExpression();
  while (true) {
    switch (getLookahead()) {
    case tok::Comma:       // Ends an expression in a list.
    case tok::Semicolon:   // Ends an expression statement.
    case tok::LeftParen:   // Ends a condition or call.
    case tok::LeftBracket: // Ends an index or subscript.
      return E;

    case tok::Dot:
      E = parseMemberExpression(E);
      break;

    default:
      E = parseApplicationExpression(E);
    }
  }
}

Syntax *Parser::parseMemberExpression(Syntax *LHS) {
  requireToken(tok::Dot);
  parseIdExpression();

  // FIXME: Build the member expression.
  return nullptr;
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

// FIXME: Return errors as needed.
Syntax *Parser::parseParenExpression() {
  EnclosingParens Parens(*this);
  if (!Parens.expectOpen())
    return nullptr;

  llvm::SmallVector<Syntax *, 4> SS;
  if (nextTokenIsNot(tok::RightParen))
    parseArgumentList(SS);

  if (!Parens.expectClose())
    return nullptr;
  return onTuple(SS);
}

Syntax *Parser::parseBracketExpression() {
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return nullptr;

  llvm::SmallVector<Syntax *, 4> SS;
  if (nextTokenIsNot(tok::RightBracket))
    parseArgumentList(SS);

  if (!Brackets.expectClose())
    return nullptr;
  return onArray(SS);
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

Syntax *Parser::onTuple(llvm::SmallVectorImpl<Syntax *> &SS) {
  return new TupleSyntax(makeArray(SS));
}

Syntax *Parser::onArray(llvm::SmallVectorImpl<Syntax *> &SS) {
  return new ArraySyntax(makeArray(SS));
}

Syntax *Parser::onBlock(llvm::SmallVectorImpl<Syntax *> &SS) {
  return new BlockSyntax(makeArray(SS));
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
