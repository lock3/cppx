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
#include "llvm/ADT/StringSwitch.h"

#include "clang/Blue/BlueParser.h"
#include "clang/Blue/BlueSyntax.h"

#include <iostream>

namespace blue {

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

// FIXME: obviously figure something better out here
static Syntax **AllocateSeq(SyntaxSeq &SS) {
  Syntax **Ret = new Syntax *[SS.size()];
  std::copy(SS.begin(), SS.end(), Ret);
  return Ret;
}

// Syntax productions

Syntax *Parser::parseFile() {
  Syntax *Decls = nullptr;

  llvm::SmallVector<Syntax *, 16> SS;
  if (!atEndOfFile())
    Decls = parseDeclSequence(SS);

  return new FileSyntax(Decls);
}

Syntax *Parser::parseDeclSequence(SyntaxSeq &SS) {
  while (!atEndOfFile())
    parseItem(*this, &Parser::parseDeclaration, SS);
  return new SequenceSyntax(AllocateSeq(SS), SS.size());
}

template<typename Parse, typename Sequence>
static Syntax *parseIntoVector(Sequence &Seq, Parse Fn) {
  Syntax *S = Fn();
  if (!S /*|| S->isError()*/)
    return S;
  Seq.push_back(S);
  return S;
}

static inline bool atClosedBrace(Parser &P, unsigned BeginningBraceDepth) {
  return P.BraceDepth == BeginningBraceDepth &&
    P.PreviousToken.hasKind(tok::RightBrace);
}

static inline bool atClosingBrace(Parser &P, unsigned BeginningBraceDepth) {
  return P.BraceDepth == BeginningBraceDepth &&
    P.nextTokenIs(tok::RightBrace);
}


/// Parse a block-statement.
///
///   block-statement:
///     block-expression
Syntax *Parser::parseBlockStatement() {
  return parseBraceEnclosed(&Parser::parseStatementSeq);
}

/// Returns true if the tokens at `La` start a definition.
static bool startsDefinition(Parser &P, std::size_t La)
{
  // Check for unnamed definitions.
  if (P.nthTokenIs(La, tok::Colon))
    return true;

  // Check for definitions of the form `x:` and `x,y,z:`
  if (P.nthTokenIs(La, tok::Identifier)) {
    ++La;

    // Match a single declarator.
    if (P.nthTokenIs(La, tok::Colon))
      return true;

    // Match multiple declarators. Basically, search through a comma-separated
    // list of identifiers and stop when we reach anything else. Note that
    // finding anying the like `x, 0` means we can short-circuit at the
    // 0. There's no way this can be a definition.
    while (P.nthTokenIs(La, tok::Comma)) {
      ++La;
      if (P.nthTokenIs(La, tok::Identifier))
        ++La;
      else
        return false;
    }

    return P.nthTokenIs(La, tok::Colon);
  }

  return false;
}

/// Returns true if `p` starts a parameter declaration.
static bool startsDefinition(Parser &P)
{
  return startsDefinition(P, 0);
}

namespace
{
  struct DescriptorClause
  {
    Syntax *Type = {};
    Syntax *Cons = {};
  };

  // Parse a descriptor clause.
  //
  //   descriptor-clause:
  //     : descriptor constraint?
  //     : constraint?
  //
  //   constraint:
  //     is pattern
  DescriptorClause parseDescriptorClause(Parser &P)
  {
    DescriptorClause DC;
    P.expectToken(tok::Colon);
    if (P.nextTokenIs(tok::IsKeyword)) {
      DC.Cons = P.parseConstraint();
    } else if(P.nextTokenIsNot(tok::Equal)) {
      DC.Type = P.parseDescriptor();
      if (P.nextTokenIs(tok::IsKeyword))
        DC.Cons = P.parseConstraint();
    }
    return DC;
  }

  struct InitializerClause
  {
    Syntax *Init = {};
  };

  //   initializer-clause: 
  //     ; 
  //     = expression ; 
  //     = block-statement
  InitializerClause parseInitializerClause(Parser &P)
  {
    InitializerClause Clause;
    if (P.nextTokenIs(tok::Equal)) {

      P.expectToken(tok::Equal);

      if (P.nextTokenIs(tok::LeftBrace)) {
        Clause.Init = P.parseBlockStatement();
      } else {
        Clause.Init = P.parseExpression();
        P.expectToken(tok::Semicolon);
      }
    } else {
      P.expectToken(tok::Semicolon);
    }

    return Clause;
  }
} // namespace

/// Parse a constraint.
///
///   constraint-clause:
///     is pattern
///
/// TODO: We can have is constraints and where constraints. The difference
/// is that in an is constraint, the declared entity implicitly participates
/// in the expression, but not in a where constraint.
Syntax *Parser::parseConstraint()
{
  requireToken(tok::IsKeyword);
  return parsePattern();
}

/// Parse a type expression.
///
///   descriptor:
///     prefix-expression
///
/// A descriptor specifies part of the signature of a declaration, including
/// its template parameters, function parameters, array bounds, and type.
Syntax *Parser::parseDescriptor()
{
  return parsePrefixExpression();
}

/// Parse a declaration:
///
///   declaration:
///     definition
Syntax *Parser::parseDeclaration() {
  return parseDefinition();
}

/// Definition declaration:
///
///   definition-declaration:
///     declarator-list? type-clause initializer-clause 
///
///   type-clause: 
///     : type constraint? 
///     : constraint? 
///
///   type: 
///     prefix-expression 
///
///   constraint:
///     is pattern
///
///   initializer-clause: 
///     ; 
///     = expression ; 
///     = block-statement
///
///   block:
///     block-statement 
Syntax *Parser::parseDefinition() {
  // Parse the declarator-list.
  Syntax *Decl = nullptr;
  if (nextTokenIsNot(tok::Colon))
    Decl = parseDeclaratorList();

  DescriptorClause DC = parseDescriptorClause(*this);
  InitializerClause IC;
  if (nextTokenIsNot(tok::Semicolon))
      IC = parseInitializerClause(*this);
  else
    expectToken(tok::Semicolon);

  return new DeclarationSyntax(Decl, DC.Type, DC.Cons, IC.Init);
}

static inline bool isCloseEnclosure(tok::TokenKind K) {
  switch (K) {
  case tok::RightParen:
  case tok::RightBracket:
  case tok::RightBrace:
    return true;
  default:
    return false;
  }
}

/// Builds the declarator list.
static Syntax *makeDeclaratorList(SyntaxSeq &SS, Parser &P)
{
  // TODO: What if `SS` is empty? Recovery means skipping the entire
  // declaration, probably.
  assert(!SS.empty());

  // Collapse singleton lists into simple declarators.
  if (!isCloseEnclosure(P.getLookahead()) && SS.size() == 1)
    return SS[0];

  // if (P.InBody && P.getLookahead() == tok::RightBrace &&
  //     SS.size() == 1) {
  //   P.InBody = false;
  //   return SS[0];
  // }

  // FIXME: find a way to maintain the token kind?
  return new ListSyntax(AllocateSeq(SS), SS.size());
}

Syntax *Parser::parseStatementSeq() {
  llvm::SmallVector<Syntax *, 4> SS;
  do
    parseItem(*this, &Parser::parseStatement, SS);
  while (nextTokenIsNot(tok::RightBrace));

  if (SS.size() == 1 && isa<ListSyntax>(SS.front()))
    return SS.front();

  return new ListSyntax(AllocateSeq(SS), SS.size());
}

/// Parse a statement.
///
///   statement:
///     block-statement
///     declaration-statement
///     expression-statement
Syntax *Parser::parseStatement()
{
  if (nextTokenIs(tok::LeftBrace))
    return parseBlockStatement();

  if (startsDefinition(*this))
    return parseDeclarationStatement();

  return parseExpressionStatement();
}

/// Parse a declaration-statement.
///
///   declaration-statement:
///     declaration
///
/// Not all declarations are allowed in all scopes. However, we don't
/// really have a notion of scope attached to the parse, so we have to
/// filter semantically.
// Syntax *Parser::parseDeclarationStatement()
Syntax *Parser::parseDeclarationStatement()
{
  return parseDeclaration();
}

/// Parse an expression-statement.
///
///   expression-statement:
///     expression-list ;
Syntax *Parser::parseExpressionStatement()
{
  Syntax *E = parseExpressionList();
  if (!PreviousToken.hasKind(tok::RightBrace))
    expectToken(tok::Semicolon);
  return E;
}


/// Parse an expression-list.
///
///   expression-list:
///     expression
///     expression-list , expression
///
/// This always returns a list, even if there's a single element.
Syntax *Parser::parseExpressionList()
{
  llvm::SmallVector<Syntax *, 4> SS;
  do
    parseItem(*this, &Parser::parseExpression, SS);
  while (matchToken(tok::Comma));
  // return new ListSyntax(AllocateSeq(SS), SS.size());
  return makeDeclaratorList(SS, *this);
}

/// Parse a declarator-list.
///   declarator-list:
///     declarator
///     declartor-list , declarator
///
/// Technically, this allows the declaration of multiple functions having
/// the same return type, but we can semantically limit declarators to just
/// variables.
Syntax *Parser::parseDeclaratorList()
{
  llvm::SmallVector<Syntax *, 4> SS;
  do
    parseItem(*this, &Parser::parseDeclarator, SS);
  while (matchToken(tok::Comma));
  return makeDeclaratorList(SS, *this);
}

/// Parse a declarator.
///
///   declarator:
///     id-expression
///
/// This is a restriction on a more general grammar that allows function
/// and/or array-like declarators.
Syntax *Parser::parseDeclarator()
{
  return parseIdExpression();
}

/// Parse a mapping descriptor.
///
///   mapping-descriptor:
///     [ parameter-group ] prefix-expression
///     ( parameter-group ) prefix-expression
Syntax* Parser::parseMappingDescriptor()
{
  assert(nextTokenIs(tok::LeftParen) || nextTokenIs(tok::LeftBracket));
  return parsePrefixExpression();
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
///     leave-expression 
///     expression where ( parameter-group )
Syntax *Parser::parseExpression() {
  Syntax *E0 = parseLeaveExpression();
  while (Token Op = matchToken(tok::WhereKeyword)) {
    // Syntax* E1 = parseParenEnclosed(&Parser::parse_parameter_group);
    Syntax *E1 = nullptr;
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

/// Parse a leave-expression.
///
///   leave-expression:
///     control-expression
///     return control-expression
///     throw control-expression
Syntax *Parser::parseLeaveExpression()
{
  switch (getLookahead()) {
  case tok::ReturnKeyword:
  case tok::ThrowKeyword: {
    Token KW = consumeToken();
    Syntax *S = parseControlExpression();
    return new PrefixSyntax(KW, S);
  }
  default:
    break;
  }

  return parseControlExpression();
}

/// Parse a control-expression.
///
///   control-expression:
///     assignment-expression
///     conditional-expression
///     case-expression
///     loop-expression
///     lambda-expression
///     let-expression
Syntax *Parser::parseControlExpression()
{
  switch (getLookahead()) {
  case tok::IfKeyword:
    return parseConditionalExpression();
  case tok::CaseKeyword:
  case tok::SwitchKeyword:
    return parseMatchExpression();
  case tok::ForKeyword:
  case tok::WhileKeyword:
  case tok::DoKeyword:
    return parseLoopExpression();
  case tok::LambdaKeyword:
    // return parse_lambda_expression();
  case tok::LetKeyword:
    // return parse_let_expression();;
  default:
    break;
  }

  return parseAssignmentExpression();
}

static bool isOpenToken(tok::TokenKind K)
{
  switch (K) {
  case tok::LeftParen:
  case tok::LeftBracket:
  case tok::LeftBrace:
    return true;
  default:
    return false;
  }
}

static bool isCloseToken(tok::TokenKind K)
{
  switch (K) {
  case tok::RightParen:
  case tok::RightBracket:
  case tok::RightBrace:
    return true;
  default:
    return false;
  }
}

/// Find the matching offset of the current token.
static std::size_t findMatched(Parser &P,
                               tok::TokenKind Open, tok::TokenKind Close) {
  assert(P.getLookahead() == Open);
  std::size_t la = 0;
  std::size_t braces = 0;
  while (tok::TokenKind k = P.getLookahead(la)) {
    if (isOpenToken(k)) {
      ++braces;
    }
    else if (isCloseToken(k)) {
      --braces;
      if (braces == 0)
        break;
    }
    ++la;
  }
  return la;
}

/// Parse a conditional-expression.
///
///   conditional-expression:
///     if ( expression ) block-expression
///     if ( expression ) block-expression else block-expression
Syntax *Parser::parseConditionalExpression()
{
  Token Ctrl = requireToken(tok::IfKeyword);
  expectToken(tok::LeftParen);
  Syntax *S0 = parseExpression();
  expectToken(tok::RightParen);
  Syntax *S1 = parseBlockExpression();

  // Match the else part. Turn the body into an application.
  if (matchToken(tok::ElseKeyword)) {
    Syntax *S2 = parseBlockExpression();
    S1 = new PairSyntax(S1, S2);
  } else {
    S1 = new PairSyntax(S1, nullptr);
  }

  return new ControlSyntax(Ctrl, S0, S1);
}

/// Parse a match-expression.
///
///   match-expression:
///     case ( expression ) case-list
///     switch ( expression ) case-list
Syntax *Parser::parseMatchExpression()
{
  assert(nextTokenIs(tok::CaseKeyword) || nextTokenIs(tok::SwitchKeyword));
  Token Ctrl = consumeToken();
  expectToken(tok::LeftParen);
  Syntax *S0 = parseExpression();
  expectToken(tok::RightParen);
  Syntax *S1 = parseCaseList();
  return new ControlSyntax(Ctrl, S0, S1);
}

/// Parses a case-list.
///
///   case-list:
///     case
///     case-list else case
Syntax *Parser::parseCaseList()
{
  llvm::SmallVector<Syntax *, 4> CS;
  parseItem(*this, &Parser::parseCase, CS);
  while (matchToken(tok::ElseKeyword))
    parseItem(*this, &Parser::parseCase, CS);
  return new ListSyntax(AllocateSeq(CS), CS.size());
}

/// Parse a case in a match-expression:
///
///   case:
///     pattern-list? => block-expression
Syntax *Parser::parseCase()
{
  Syntax *S0 = nullptr;
  if (nextTokenIsNot(tok::EqualGreater))
    S0 = parsePatternList();
  Token Op = expectToken(tok::EqualGreater);
  Syntax *S1 = parseBlockExpression();
  return new InfixSyntax(Op, S0, S1);
}

/// Parse a pattern-list.
///
///   pattern-list:
///     pattern
///     pattern-list , pattern
Syntax *Parser::parsePatternList()
{
  llvm::SmallVector<Syntax *, 4> PS;
  parseItem(*this, &Parser::parsePattern, PS);
  while (matchToken(tok::Comma))
    parseItem(*this, &Parser::parsePattern, PS);
  return new ListSyntax(AllocateSeq(PS), PS.size());
}

/// Parse a pattern.
///
///   pattern:
///     prefix-expression
///
/// A pattern describes a set of types or values.
Syntax *Parser::parsePattern()
{
  return parsePrefixExpression();
}

/// Parse a loop expression.
///
///   loop-expression:
///     for ( declarator type-clause in expression ) block-expression
///     while ( expression ) block-expression
///     do block-expression while ( condition )
///     do block-expression
Syntax *Parser::parseLoopExpression()
{
  switch (getLookahead()) {
  case tok::ForKeyword:
    return parseForExpression();
  case tok::WhileKeyword:
    return parseWhileExpression();
  case tok::DoKeyword:
    return parseDoExpression();
  default:
    break;
  }
  assert(false);
}

/// Parse a for loop.
///
///   loop-expression:
///     for ( declarator descriptor-clause in expression ) block-expression
///
/// The declaration in the parameter list is similar to normal parameters
/// except that the `=` is replaced by `in`.
///
/// TODO: Can we generalize the syntax for multiple parameters? What would
/// it mean? There are a few options (zip vs. cross). Note that these
/// don't group like other parameters.
Syntax *Parser::parseForExpression()
{
  // FIXME: differentiate two types of for expressions.
  return parseTraditionalForExpression();
}

Syntax *Parser::parseRangeForExpression() {
  Token ctrl = requireToken(tok::ForKeyword);
  expectToken(tok::LeftParen);
  Syntax* id = parseDeclarator();
  DescriptorClause dc = parseDescriptorClause(*this);
  expectToken(tok::InKeyword);
  Syntax* init = parseExpression();
  Syntax* decl = new DeclarationSyntax(id, dc.Type, dc.Cons, init);
  expectToken(tok::RightParen);
  Syntax* body = parseBlockExpression();
  return new ControlSyntax(ctrl, decl, body);
}

Syntax *Parser::parseTraditionalForExpression() {
  Token Keyword = requireToken(tok::ForKeyword);
  expectToken(tok::LeftParen);

  Syntax *Decl = nullptr;
  if (!nextTokenIs(tok::Semicolon))
    Decl = parseDeclaration();
  if (nextTokenIs(tok::Semicolon))
    consumeToken();

  Syntax *Condition = nullptr;
  if (!nextTokenIs(tok::Semicolon))
    Condition = parseEqualityExpression();
  expectToken(tok::Semicolon);

  Syntax *Increment = nullptr;
  if (!nextTokenIs(tok::RightParen))
    Increment = parseAssignmentExpression();
  expectToken(tok::RightParen);

  Syntax *Head = new TripleSyntax(Decl, Condition, Increment);
  Syntax *Body = parseBlockExpression();
  return new ControlSyntax(Keyword, Head, Body);
}

///   loop-expression:
///     while ( expression ) block-expression
Syntax *Parser::parseWhileExpression()
{
  Token ctrl = requireToken(tok::WhileKeyword);
  expectToken(tok::LeftParen);
  Syntax* s0 = parseExpression();
  expectToken(tok::RightParen);
  Syntax* s1 = parseBlockExpression();
  return new ControlSyntax(ctrl, s0, s1);
}

/// Parse a do/do-while expression.
///
///   loop-expression:
///     do block-expression while ( condition )
///     do block-expression
///
/// Note that the "head" of the do expression appears after the body
/// of the loop (if it appears at all).
Syntax *Parser::parseDoExpression()
{
  Token ctrl = requireToken(tok::DoKeyword);
  Syntax* s1 = parseBlockExpression();
  Syntax* s0 = nullptr;
  if (matchToken(tok::WhileKeyword)) {
    expectToken(tok::LeftParen);
    s0 = parseExpression();
    expectToken(tok::RightParen);
  }
  return new ControlSyntax(ctrl, s0, s1);
}

/// Parse a block-expression.
///
///   block-expression:
///     expression
///     block
Syntax *Parser::parseBlockExpression()
{
  InBody = true;
  if (nextTokenIs(tok::LeftBrace))
    return parseBlock();
  return parseExpression();
}

/// Parse a block.
///
///   block:
///     block-statement
Syntax* Parser::parseBlock()
{
  return parseBlockStatement();
}

/// Returns a list defining the group.
static Syntax *makeParameterGroup(Parser &P, SyntaxSeq &SS)
{
  // This only happens when there's an error and we can't accumulate
  // a group. If we propagate errors, this shouldn't happen at all.
  if (SS.empty())
    return nullptr;

  // Don't allocate groups if there's only one present.
  if (SS.size() == 1)
    return SS[0];

  return new ListSyntax(AllocateSeq(SS), SS.size());
}

/// Parse an expression-group.
///
///   parameter-group:
///     parameter-list
///     parameter-group ; parameter-list
///
/// Groups are only created if multiple groups are present.
Syntax *Parser::parseParameterGroup()
{
  llvm::SmallVector<Syntax *, 4> SS;
  parseItem(*this, &Parser::parseParameterList, SS);
  while (matchToken(tok::Semicolon))
    parseItem(*this, &Parser::parseParameterList, SS);
  return makeParameterGroup(*this, SS);
}

// Returns a list for `SS`.
static Syntax *makeParameterList(Parser &P, SyntaxSeq &SS)
{
  // This only happens when an error occurred.
  if (SS.empty())
    return nullptr;

  return new ListSyntax(AllocateSeq(SS), SS.size());
}

/// Parse an parameter-list.
///
///   parameter-list:
///     parameter
///     parameter-list , parameter
///
/// This always returns a list, even if there's a single element.
Syntax *Parser::parseParameterList()
{
  llvm::SmallVector<Syntax *, 4> SS;
  parseItem(*this, &Parser::parseParameter, SS);
  while (matchToken(tok::Comma))
    parseItem(*this, &Parser::parseParameter, SS);
  return makeParameterList(*this, SS);
}

/// Parser a parameter:
///
///   parameter:
///     identifier : type
///     identifier : type = expression
///     identifeir : = expression
///     : type
///     : type = expression
///
/// TODO: Can parameters have introducers?
///
/// TODO: Can paramters be packs (yes, but what's the syntax?).
Syntax *Parser::parseParameter()
{
  // Match unnamed variants.
  if (matchToken(tok::Colon)) {
    Syntax *Type = parseDescriptor();
    Syntax *Init = nullptr;
    if (matchToken(tok::Equal))
      Init = parseExpression();
    return new DeclarationSyntax(nullptr, Type, nullptr, Init);
  }

  // Match the identifier...
  Syntax *Id = parseIdExpression();

  // ... And optional declarative information
  Syntax *Type = nullptr;
  Syntax *Init = nullptr;
  if (matchToken(tok::Colon)) {
    if (nextTokenIsNot(tok::Equal))
      Type = parseDescriptor();
    if (matchToken(tok::Equal))
      Init = parseExpression();
  }

  return new DeclarationSyntax(Id, Type, nullptr, Init);
}


static bool isAssignmentOp(tok::TokenKind K)
{
  return K == tok::PlusEqual || K == tok::MinusEqual
        || K == tok::StarEqual || K == tok::SlashEqual
        || K == tok::PercentEqual || K == tok::Equal;
}


Syntax *Parser::parseAssignmentExpression() {
  Syntax *E0 = parseImplicationExpression();
  while (Token Op = matchTokenIf(isAssignmentOp)) {
    Syntax *E1 = parseAssignmentExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }
  return E0;
}

/// Parse an implication.
///
///   implication-expression:
///     logical-or-expression
///     logical-or-expression -> implication-expression
Syntax *Parser::parseImplicationExpression()
{
  Syntax *E0 = parseLogicalOrExpression();
  if (Token Op = matchToken(tok::MinusGreater)) {
    Syntax *E1 = parseImplicationExpression();
    return new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

/// Parse a logical or.
///
///   logical-or-expression:
///     logical-and-expression
///     logical-or-expression or logical-and-expression
Syntax *Parser::parseLogicalOrExpression() {
  Syntax *E0 = parseLogicalAndExpression();
  while (Token Op = matchToken(tok::OrKeyword)) {
    Syntax *E1 = parseLogicalAndExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

/// Parse an logical and.
///
///   logical-and-expression:
///     equality-expression
///     logical-and-expression and equality-expression
Syntax *Parser::parseLogicalAndExpression() {
  Syntax *E0 = parseEqualityExpression();
  while (Token Op = matchToken(tok::AndKeyword)) {
    Syntax *E1 = parseEqualityExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isEqualityOperator(tok::TokenKind K)
{
  return K == tok::EqualEqual || K == tok::BangEqual;
}

/// Parse an equality comparison.
///
///   equality-expression:
///     relational-expression
///     equality-expression == relational-expression
///     equality-expression != relational-expression
Syntax *Parser::parseEqualityExpression() {
  Syntax *E0 = parseRelationalExpression();
  while (Token Op = matchTokenIf(isEqualityOperator)) {
    Syntax *E1 = parseRelationalExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isRelationalOperator(tok::TokenKind K)
{
  return K == tok::Less ||
    K == tok::Greater ||
    K == tok::LessEqual ||
    K == tok::GreaterEqual;
}

/// Parse a relational expression.
///
///   relational-expression:
///     additive-expression
///     relational-expression < additive-expression
///     relational-expression > additive-expression
///     relational-expression <= additive-expression
///     relational-expression >= additive-expression
Syntax *Parser::parseRelationalExpression() {
  Syntax* E0 = parseAdditiveExpression();
  while (Token Op = matchTokenIf(isRelationalOperator)) {
    Syntax* E1 = parseAdditiveExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isAdditiveOperator(tok::TokenKind K)
{
  return K == tok::Plus || K == tok::Minus;
}

/// Parse an additive expression.
///
///   additive-expression:
///     multiplicative-expression
///     additive-expression + multiplicative-expression
///     additive-expression - multiplicative-expression
Syntax *Parser::parseAdditiveExpression() {
  Syntax *E0 = parseMultiplicativeExpression();
  while (Token Op = matchTokenIf(isAdditiveOperator)) {
    Syntax *E1 = parseMultiplicativeExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isMultiplicativeOperator(tok::TokenKind K)
{
  return K == tok::Star ||
    K == tok::Slash ||
    K == tok::Percent;
}

/// Parse a multiplicative expression.
///
///   multiplicative-expression:
///     prefix-expression
///     multiplicative-expression * prefix-expression
///     multiplicative-expression / prefix-expression
///     multiplicative-expression % prefix-expression
Syntax *Parser::parseMultiplicativeExpression() {
  Syntax *E0 = parsePrefixExpression();
  while (Token Op = matchTokenIf(isMultiplicativeOperator)) {
    Syntax *E1 = parsePrefixExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

// An lparen starts a prefix operator if the first few tokens start a
// prefix operator, and the entire enclosure is not followed by something
// that is eithe a primary expression or other prefix operator.
static bool isPrefixOperator(Parser &P, tok::TokenKind Open,
                             tok::TokenKind Close) {
  std::size_t La = findMatched(P, Open, Close);
  switch (P.getLookahead(La + 1)) {
    // Primary expressions.
  case tok::TrueKeyword:
  case tok::FalseKeyword:
  case tok::IntegerKeyword:
  case tok::IntKeyword:
  case tok::BoolKeyword:
  case tok::TypeKeyword:
  case tok::VoidKeyword:
  case tok::Identifier:
  // Both prefix and primary.
  case tok::LeftParen:
  // Other prefix operators.
  case tok::LeftBracket:
  case tok::ConstKeyword:
  case tok::Caret:
  case tok::Plus:
  case tok::Minus:
  case tok::NotKeyword:
      return true;
  default:
      break;
  }

  return false;
}

/// Contains information about the lexical structure of a potential prefix
/// operator.
struct EnclosureCharacterization
{
  /// True if we have `()` or `[]`.
  bool isEmpty = false;

  /// True if the non-empty contents are `:t`, `x:t`, or `x0, ..., xn:t`.
  bool hasParameters = false;

  /// True if the token following the closing `)` or `]` starts a prefix
  /// or primary expression.
  bool isOperator = false;
};

// For a term like `@ parameter-group | expression-list @`, determine some
// essential properties.
EnclosureCharacterization characterizePrefixOp(Parser &p)
{
  assert(p.nextTokenIs(tok::LeftParen) || p.nextTokenIs(tok::LeftBracket));

  // Get the matching token kinds.
  tok::TokenKind open = p.getLookahead();
  tok::TokenKind close = open == tok::LeftParen
    ? tok::RightParen
    : tok::RightBracket;

  EnclosureCharacterization info;

  // Characterize the enclosure.
  if (p.nthTokenIs(1, close))
    info.isEmpty = true;
  else if (startsDefinition(p, 1))
    info.hasParameters = true;

  // Characterize the token after the enclosure.
  info.isOperator = isPrefixOperator(p, open, close);

  return info;
}

/// Parse a prefix-expression.
///
///   prefix-expression:
///     postfix-expression
///     [ expression-list? ] prefix-expression
///     [ parameter-group ] prefix-expression
///     ( parameter-group? ) prefix-expression
///     const prefix-expression
///     ^ prefix-expression
///     - prefix-expression
///     + prefix-expression
///     not prefix-expression
Syntax *Parser::parsePrefixExpression() {
  switch (getLookahead())
  {
  case tok::LeftBracket: {
    auto info = characterizePrefixOp(*this);
    if (!info.isOperator)
      break;
    if (!info.isEmpty && info.hasParameters)
      return parseTemplateConstructor();
    else
      return parseArrayConstructor();
  }

  case tok::LeftParen: {
    auto info = characterizePrefixOp(*this);
    if (!info.isOperator)
      break;
    return parseFunctionConstructor();
  }
  case tok::PlusPlus:
  case tok::MinusMinus:
  case tok::ConstKeyword:
  case tok::Caret:
  case tok::Plus:
  case tok::Minus:
  case tok::NotKeyword: {
    Token op = consumeToken();
    Syntax *e = parsePrefixExpression();
    return new PrefixSyntax(op, e);
  }

  default:
    break;
  }

  return parsePostfixExpression();
}

/// Parse a template type constructor.
///
///   prefix-expression:
///     [ parameter-group ] prefix-expression
Syntax* Parser::parseTemplateConstructor()
{
  Syntax *Op = parseBracketEnclosed(&Parser::parseParameterGroup);
  Syntax *Type = parsePrefixExpression();
  return new TemplateSyntax(Op, Type);
}

/// Parse an array type constructor.
///
///   prefix-expression:
///     [ expression-list? ] prefix-expression
Syntax* Parser::parseArrayConstructor()
{
  Syntax *Op = parseBracketEnclosed(&Parser::parseExpressionList);
  Syntax *Type = parsePrefixExpression();
  return new ArraySyntax(Op, Type);
}

/// Parse a function type constructor.
///
///   prefix-expression:
///     ( parameter-group? ) prefix-expression
Syntax* Parser::parseFunctionConstructor()
{
  Syntax *Op = parseParenEnclosed(&Parser::parseParameterGroup);
  Syntax *Type = parsePrefixExpression();
  return new FunctionSyntax(Op, Type);
}

/// Parse a postfix-expression.
///
///   postfix-expression:
///     primary-expression
///     postfix-expression ( expression-list? )
///     postfix-expression [ expression-list? ]
///     postfix-expression . id-expression
///     postfix-expression ^
Syntax *Parser::parsePostfixExpression() {
  Syntax *E0 = parsePrimaryExpression();
  while (true)
  {
    if (nextTokenIs(tok::LeftParen)) {
      Syntax *Args = parseParenEnclosed(&Parser::parseExpressionList);
      E0 = new CallSyntax(E0, Args);
    }
    else if (nextTokenIs(tok::LeftBracket)) {
      Syntax *Args = parseBracketEnclosed(&Parser::parseExpressionList);
      E0 = new CallSyntax(E0, Args);
    }
    else if (Token Dot = matchToken(tok::Dot)) {
      Syntax *Member = parseIdExpression();
      E0 = new InfixSyntax(Dot, E0, Member);
    }
    else if (Token Op = matchToken(tok::Caret)) {
      E0 = new PostfixSyntax(Op, E0);
    }
    else if (Token Inc = matchToken(tok::PlusPlus)) {
      E0 = new PostfixSyntax(Inc, E0);
    }
    else if (Token Dec = matchToken(tok::MinusMinus)) {
      E0 = new PostfixSyntax(Dec, E0);
    } else
      break;
  }

  return E0;
}

Syntax *Parser::parseMemberExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parseCallExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parseIndexExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parseBraceExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parseApplicationExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parsePointerExpression(Syntax *E) {
  return nullptr;
}

Syntax *Parser::parsePointerExpression() {
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

    // Value literals
  case tok::TrueKeyword:
  case tok::FalseKeyword:
  case tok::NullKeyword:
    // Type literals
  case tok::VoidKeyword:
  case tok::FloatKeyword:
  case tok::IntKeyword:
  case tok::BoolKeyword:
  case tok::TypeKeyword:
    // Class keyword
  case tok::ClassKeyword:
    // Built in type functions
  case tok::IntegerKeyword:
  case tok::RealKeyword:
  case tok::CharacterKeyword:
  // Bitwise function Keywords
  case tok::BitAndKeyword:
  case tok::BitOrKeyword:
  case tok::BitXOrKeyword:
  case tok::BitShlKeyword:
  case tok::BitShrKeyword:
  case tok::BitNotKeyword:
    // Control primitives
  case tok::ContinueKeyword:
  case tok::BreakKeyword: {
    Token Value = consumeToken();
    return new LiteralSyntax(Value);
  }

  case tok::Identifier:
    return parseIdExpression();

  case tok::LeftParen: {
    auto Info = characterizePrefixOp(*this);
    if (!Info.isEmpty && Info.hasParameters)
      return parseParenEnclosed(&Parser::parseParameterGroup);
    return parseParenEnclosed(&Parser::parseExpressionList);
  }

  case tok::LeftBracket:
    return parseBracketEnclosed(&Parser::parseParameterGroup);

  default:
    break;
  }

  // FIXME: Return an error tree. Also, how can we recover from this?
  // It might depend on what we're parsing (declarator, type, initialzer,
  // etc.). To do that, we'd have to maintain a stack of recovery strategies
  // that we can use to skip tokens.
  // diagnose_expected("primary-expression");
  Diags.Report(getInputLocation(), clang::diag::err_expected) <<
    getSpelling(getLookahead());
  consumeToken();
  return nullptr;
}

Syntax *Parser::parseIdExpression() {
  Token Id = expectToken(tok::Identifier);
  if (Id)
    return new IdentifierSyntax(Id);
  return nullptr;
}

Syntax *Parser::parseTupleExpression() {
  return nullptr;
}

Syntax *Parser::parseArrayExpression() {
  return nullptr;
}

} // namespace blue
