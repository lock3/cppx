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
static bool startsDefinition(Parser &P);
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

// Reallocate and copy a declaration's array of specifiers.
static inline void copyParamSpecs(DeclarationSyntax *S,
                                  llvm::SmallVectorImpl<Token> &Toks) {
  if (Toks.empty())
    return;

  Token *Old = S->ParamSpecs;
  Token *New = new Token[S->NumParamSpecs + Toks.size()];
  if (Old)
    std::copy(Old, Old + S->NumParamSpecs, New);
  std::copy(Toks.begin(), Toks.end(), New + S->NumParamSpecs);
  S->NumParamSpecs += Toks.size();
  S->ParamSpecs = New;
  if (Old)
    delete[] Old;
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

// Syntax productions


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

static inline bool isParameterSpec(tok::TokenKind K);

static inline bool OverloadableOperator(tok::TokenKind K) {
  switch(K) {
    case tok::Equal:
    case tok::EqualEqual:
    case tok::BangEqual:
    case tok::Less:
    case tok::LessEqual:
    case tok::Greater:
    case tok::GreaterEqual:
    case tok::PlusPlus:
    case tok::MinusMinus:
    case tok::Plus:
    case tok::Minus:
    case tok::Star:
    case tok::Slash:
    case tok::Percent:
    case tok::Bang:
    case tok::AmpersandAmpersand:
    case tok::BarBar:
    case tok::PlusEqual:
    case tok::MinusEqual:
    case tok::StarEqual:
    case tok::SlashEqual:
    case tok::PercentEqual:
      return true;
    default:
      return false;
  }

}
/// Returns true if the tokens at `La` start a definition.
static bool startsDefinition(Parser &P, std::size_t La)
{
  if (La == 0) {
    if (P.nthTokenIs(La, tok::PublicKeyword)
        || P.nthTokenIs(La, tok::PrivateKeyword)
        || P.nthTokenIs(La, tok::ProtectedKeyword)) {
      return true;
    }

    if (P.nthTokenIs(La, tok::UsingKeyword))
      return true;

    // Check for bindings.
    if (P.nthTokenIs(La, tok::LeftParen)) {
      ++La;
      while (P.nthTokenIsNot(La, tok::RightParen)) {
        if (P.nthTokenIs(La, tok::EndOfFile))
          return false;

        ++La;
      }

      ++La;
    }
  }

  // Check for unnamed definitions.
  if (P.nthTokenIs(La, tok::Colon))
    return true;
  // If we have a parameter specifier then we are a function.
  if (isParameterSpec(P.peekToken(La).getKind()))
    return true;

  // Check for definitions of the form `x:` and `x,y,z:`
  if (P.nthTokenIs(La, tok::Identifier)
      || (P.nthTokenIs(La, tok::OperatorKeyword)
          && P.nthTokenConformsTo(La+1, OverloadableOperator))
      ) {
    if (P.nthTokenIs(La, tok::OperatorKeyword)) {
      La += 1;
    }

    if (P.peekToken(La).getSpelling() == "this" && La != 0)
      return true;
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
      else if(P.nthTokenIs(La, tok::OperatorKeyword)
              && P.nthTokenConformsTo(La+1, OverloadableOperator))
        La += 2;
      else
        return false;
    }

    return P.nthTokenIs(La, tok::Colon);
  }

  return false;
}

/// Returns true if `p` starts a parameter declaration.
bool startsDefinition(Parser &P)
{
  return startsDefinition(P, 0);
}

static inline bool isRepetitionKeyword(tok::TokenKind K) {
  switch (K) {
  case tok::ForKeyword:
  case tok::WhileKeyword:
  case tok::DoKeyword:
    return true;
  default:
    return false;
  }
}

static inline bool isAnonymousLambda(tok::TokenKind L1, tok::TokenKind L2) {
  return L1 == tok::Colon && L2 == tok::LeftParen;
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
    if(P.nextTokenIsNot(tok::Equal)) {
      DC.Type = P.parsePrefixExpression();
      if (P.nextTokenIs(tok::IsKeyword)) {
        P.consumeToken();
        DC.Cons = P.parsePrefixExpression();
      }
    }

    return DC;
  }

  struct InitializerClause
  {
    Syntax *Init = {};
  };

  inline bool isComputationIntro(Parser &P) {
    if (P.nextTokenIs(tok::LeftBrace))
      return true;
    if (P.nextTokenIs(tok::IfKeyword))
      return true;
    if (P.nextTokenIs(tok::SwitchKeyword))
      return true;
    if (isRepetitionKeyword(P.getLookahead()))
      return true;
    if (P.nextTokenIs(tok::LambdaKeyword))
      return true;
    if (isAnonymousLambda(P.getLookahead(), P.getLookahead(1)))
      return true;
    if (P.nextTokenIs(tok::LetKeyword))
      return true;

    return false;
  }

  //   initializer-clause: 
  //     ; 
  //     = expression ; 
  //     = block-statement
  InitializerClause parseInitializerClause(Parser &P, bool Parameter = false)
  {
    InitializerClause Clause;
    if (P.nextTokenIs(tok::Equal)) {

      P.expectToken(tok::Equal);

      if (P.nextTokenIs(tok::LeftBrace)) {
        Clause.Init = P.parseBlockExpression();
      } else if (P.nextTokenIs(tok::DefaultKeyword)
                 || P.nextTokenIs(tok::DeleteKeyword)) {
        Clause.Init = new LiteralSyntax(P.consumeToken());
        P.expectToken(tok::Semicolon);
      } else {
        bool RequiresSemicolon = !isComputationIntro(P) && !Parameter;
        Clause.Init = P.parseComputationExpression();
        if (RequiresSemicolon)
          P.expectToken(tok::Semicolon);
      }
    } else {
      // FIXME: if the next token is `{`, consider hinting to include `=`
      P.expectToken(tok::Semicolon);
    }

    return Clause;
  }
} // namespace

// Returns a list for `SS`.
static Syntax *makeParameterList(Parser &P, SyntaxSeq &SS)
{
  // This only happens when an error occurred.
  if (SS.empty())
    return nullptr;

  return new ListSyntax(AllocateSeq(SS), SS.size());
}


Syntax *Parser::parseBinding() {
  Token Id = expectToken(tok::Identifier);
  Syntax *Decl = new IdentifierSyntax(Id);

  if (Token Op = matchToken(tok::IsKeyword)) {
    Syntax *Cons = parsePrefixExpression();
    return new InfixSyntax(Op, Decl, Cons);
  }

  return Decl;
}

Syntax *Parser::parseBindingList() {
  llvm::SmallVector<Syntax *, 4> SS;
  parseItem(*this, &Parser::parseBinding, SS);
  while (matchToken(tok::Comma))
    parseItem(*this, &Parser::parseBinding, SS);
  return makeParameterList(*this, SS);
}

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

Syntax *Parser::parseFile() {
  Syntax *Decls = nullptr;

  llvm::SmallVector<Syntax *, 16> SS;
  if (!atEndOfFile())
    Decls = doParseFileDeclSeq(SS);

  return new FileSyntax(Decls);
}

Syntax *Parser::parseCppCodeBlock() {

  Token Extern;
  Token CppLiteral;

  Extern = requireToken(tok::ExternKeyword);
  if (!Extern)
    return nullptr;

  CppLiteral = requireToken(tok::String);
  if (!CppLiteral)
    return nullptr;

  std::string CppLiteralCheck = CppLiteral.getSpelling();
  if (CppLiteralCheck != "\"C++\"" && CppLiteralCheck != "\"c++\"") {
    Diags.Report(CppLiteral.getLocation(), clang::diag::err_blue_parsing_2)
          << "invalid extern \"C++\" block, expected extern \"c++\" "
             "or extern \"C++\" instead got "
          << CppLiteralCheck;
    return nullptr;
  }
  unsigned Depth = 0;
  auto OpenTokenId = openToken(Enclosure::Braces);
  auto CloseTokenId = closeToken(Enclosure::Braces);
  Token EnclosureOpen = requireToken(OpenTokenId);
  if (!EnclosureOpen)
    return nullptr;

  llvm::SmallVector<Token, 64> Tokens;
  while(true) {
    if (atEndOfFile()) {
      Diags.Report(getInputLocation(), clang::diag::err_blue_parsing)
            << "unexpected end of file";
      return nullptr;
    }

    if (Depth == 0)
      if (nextTokenIs(CloseTokenId))
        break;

    // Keeping track of the current depth.
    if (nextTokenIs(OpenTokenId)) {
      ++Depth;
    } else if (nextTokenIs(CloseTokenId)) {
      --Depth;
    }

    Tokens.emplace_back(consumeToken());
  }

  Token EnclosureClose = requireToken(CloseTokenId);
  if (!EnclosureClose)
    return nullptr;
  Token *Toks = nullptr;
  if (Tokens.size()) {
   Toks = new Token[Tokens.size()];
   unsigned Idx = 0;

   // Copying the vector of tokens into the array of tokens.
   for (auto T : Tokens) {
     Toks[Idx] = T;
     ++Idx;
   }
  }
  // Creating the TokenListSyntax first by attempting to create the array.
  Syntax *TokList = new TokenListSyntax(Toks, Tokens.size());
  Syntax *Enclosure = new EnclosureSyntax(EnclosureOpen, EnclosureClose, TokList);
  return new CppCodeBlockSyntax(Extern, CppLiteral, Enclosure);
}

Syntax *Parser::parseTopLevelDeclaration() {
  if (nextTokenIs(tok::ExternKeyword))
    return parseCppCodeBlock();
  return parseDeclaration();
}

Syntax *Parser::doParseFileDeclSeq(SyntaxSeq &SS) {
  while (!atEndOfFile())
    parseItem(*this, &Parser::parseTopLevelDeclaration, SS);
  return new SequenceSyntax(AllocateSeq(SS), SS.size());
}

Syntax *Parser::parseDeclSequence(SyntaxSeq &SS) {
  while (!atEndOfFile())
    parseItem(*this, &Parser::parseDeclaration, SS);
  return new SequenceSyntax(AllocateSeq(SS), SS.size());
}

/// Parse a declaration:
///
///   declaration:
///     definition-declaration
///     using-declaration
Syntax *Parser::parseDeclaration() {
  Token Intro;
  Syntax *Pars = nullptr;
  if (nextTokenIs(tok::UsingKeyword)) {
    Pars = parsePrefixExpression();
    expectToken(tok::Semicolon);
    return Pars;
  }

  Pars = parseDefinition();
  if (!Pars)
    return nullptr;

  DeclarationSyntax *Decl = cast<DeclarationSyntax>(Pars);
  return Decl;
}

/// Definition declaration:
///
///   decl-specifier-seq_{opt} identifier descriptor-clause initializer-clause
///
///   decl-specifier-seq_{opt} identifier-list descriptor-clause ;
///
///   ( binding-list ) : = expression ;
Syntax *Parser::parseDefinition() {
  // Parse the declarator-list.
  // Checking for unary declaration syntax.
  Token AccessSpecifier;
  if (nextTokenIs(tok::PublicKeyword)
      || nextTokenIs(tok::PrivateKeyword)
      || nextTokenIs(tok::ProtectedKeyword)) {
    AccessSpecifier = consumeToken();
  }

  if (nextTokenIs(tok::LeftParen)) {
    consumeToken();
    Syntax *Bindings = parseBindingList();
    expectToken(tok::RightParen);
    expectToken(tok::Colon);
    expectToken(tok::Equal);
    Syntax *Init = parseExpression();
    expectToken(tok::Semicolon);
    return new DeclarationSyntax(Bindings, nullptr, nullptr, Init, AccessSpecifier);
  }

  Syntax *Decl = nullptr;
  if (nextTokenIsNot(tok::Colon))
    Decl = parseDeclarator();
  DescriptorClause DC = parseDescriptorClause(*this);
  InitializerClause IC;
  if (nextTokenIsNot(tok::Semicolon))
      IC = parseInitializerClause(*this);
  else
    expectToken(tok::Semicolon);

  return new DeclarationSyntax(Decl, DC.Type, DC.Cons, IC.Init, AccessSpecifier);
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

/// Parse an expression.
///
///   expression:
///     leave-expression 
///     expression where ( parameter-group )
Syntax *Parser::parseExpression() {
  if (startsDefinition(*this))
    return parseDeclaration();

  return parseControlExpression();
  // Syntax *E0 = parseLeaveExpression();
  // while (Token Op = matchToken(tok::WhereKeyword)) {
  //   // Syntax* E1 = parseParenEnclosed(&Parser::parse_parameter_group);
  //   Syntax *E1 = nullptr;
  //   E0 = new InfixSyntax(Op, E0, E1);
  // }

  // return E0;
}

Syntax *Parser::parseControlExpression() {
  switch (getLookahead()) {
  case tok::ReturnKeyword:
  case tok::ThrowKeyword: {
    Token KW = consumeToken();
    Syntax *S = parseComputationExpression();
    expectToken(tok::Semicolon);
    return new PrefixSyntax(KW, S);
  }
  default:
    break;
  }

  return parseComputationExpression();
}

/// computation-expression:
///         assignment-expression
///         conditional-expression
///         selection-expression
///         repetition-expression
///         lambda-expression
///         let-expression
///         block-expression
Syntax *Parser::parseComputationExpression() {
  if (nextTokenIs(tok::LeftBrace))
    return parseBlockExpression();
  if (nextTokenIs(tok::IfKeyword))
    return parseConditionalExpression();
  if (nextTokenIs(tok::SwitchKeyword))
    return parseSelectionExpression();
  if (isRepetitionKeyword(getLookahead()))
    return parseRepetitionExpression();
  if (nextTokenIs(tok::LambdaKeyword))
    return parseLambdaExpression();
  if (isAnonymousLambda(getLookahead(), getLookahead(1)))
    return parseLambdaExpression();
  if (nextTokenIs(tok::LetKeyword))
    return parseLetExpression();
  // TODO: parse anonymous let parameter

  return parseAssignmentExpression();
}

Syntax *Parser::parseRepetitionExpression() {
  switch (getLookahead()) {
  case tok::ForKeyword:
    return parseForExpression();
  case tok::WhileKeyword:
    return parseWhileExpression();
  case tok::DoKeyword:
    return parseDoExpression();
  default:
    llvm_unreachable("invalid repetition expression");
  }
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
  Token Ctrl = requireToken(tok::ForKeyword);
  expectToken(tok::LeftParen);
  Syntax *Id = parseIdExpression();
  DescriptorClause DC = parseDescriptorClause(*this);
  expectToken(tok::InKeyword);
  Syntax *Init = parseExpression();
  Syntax *Decl = new DeclarationSyntax(Id, DC.Type, DC.Cons, Init);
  expectToken(tok::RightParen);
  Syntax *Body = parseBlockExpression();
  return new ControlSyntax(Ctrl, Decl, Body);
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
  Token Ctrl = requireToken(tok::DoKeyword);
  Syntax *S1 = parseBlockExpression();
  Syntax *S0 = nullptr;

  if (nextTokenIs(tok::Semicolon))
    consumeToken();

  if (matchToken(tok::WhileKeyword)) {
    expectToken(tok::LeftParen);
    S0 = parseComputationExpression();
    expectToken(tok::RightParen);
  } else {
    Diags.Report(getInputLocation(), clang::diag::err_expected) <<
      getSpelling(tok::WhileKeyword);
    // a do-while in our do-while parsing algorithm !
    do
      consumeToken();
    while (nextTokenIsNot(tok::Semicolon));
    expectToken(tok::Semicolon);
  }

  return new ControlSyntax(Ctrl, S0, S1);
}

/// Parse a let expression.
///
///   let-expression:
///     let ( parameter-group ) block-or-expression
Syntax *Parser::parseLetExpression()
{
  Token Ctrl = requireToken(tok::LetKeyword);
  Syntax *Head = parseParenEnclosed(&Parser::parseParameterGroup);
  Syntax *Body = parseBlockExpression();
  return new ControlSyntax(Ctrl, Head, Body);
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

// Returns true if the current sequnce of tokens a capture block.
static bool isCapture(Parser &P)
{
  assert(P.nextTokenIs(tok::LeftBrace));
  std::size_t La = findMatched(P, tok::LeftBrace, tok::RightBrace);
  switch (P.getLookahead(La + 1)) {
  case tok::LeftParen:
  case tok::LeftBracket:
  case tok::IsKeyword:
  case tok::EqualGreater:
    return true;
  default:
    break;
  }

  return false;
}

/// Parse a lambda-expression.
///
///   lambda-expression:
///     lambda capture? mapping-descriptor constraint? => block-expression
///     lambda block-expression
///
/// The capture, descriptor, and constraint comprise the head and are
/// stored in a triple.
Syntax *Parser::parseLambdaExpression()
{
  if (nextTokenIs(tok::LambdaKeyword))
    consumeToken();
  Token Ctrl = expectToken(tok::Colon);

  Syntax *Cap = nullptr;
  if (nextTokenIs(tok::LeftBrace))
  {
    if (!isCapture(*this)) {
      Syntax *Body = parseBlockExpression();
      return new ControlSyntax(Ctrl, nullptr, Body);
    }

    Cap = parseCapture();
  }

  Syntax *Desc = nullptr;
  if (nextTokenIs(tok::LeftParen)) {
    consumeToken();
    Desc = parseParameterList();
    expectToken(tok::RightParen);
  }

  Syntax *TrailingReturn = nullptr;
  if (nextTokenIs(tok::MinusGreater)) {
    consumeToken();
    TrailingReturn = parsePrefixExpression();
  }

  Syntax *Cons = nullptr;
  if (nextTokenIs(tok::IsKeyword))
    Cons = parseConstraint();

  expectToken(tok::EqualGreater);
  Syntax *Body = parseBlockExpression();

  Syntax *Head = new QuadrupleSyntax(Cap, Desc, Cons, TrailingReturn);
  return new ControlSyntax(Ctrl, Head, Body);
}

/// Parse a lambda capture.
///
///   capture:
///     block-statement
Syntax* Parser::parseCapture()
{
  return parseBlockExpression();
}

Syntax *Parser::parseSelectionExpression() {
  assert(nextTokenIs(tok::CaseKeyword) || nextTokenIs(tok::SwitchKeyword));
  Token Ctrl = consumeToken();
  expectToken(tok::LeftParen);
  Syntax *S0 = parseComputationExpression();
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

/// Parse a conditional-expression.
///
///   conditional-expression:
///     if ( expression ) block-expression
///     if ( expression ) block-expression else block-expression
Syntax *Parser::parseConditionalExpression()
{
  Token Ctrl = requireToken(tok::IfKeyword);
  expectToken(tok::LeftParen);
  Syntax *S0 = parseComputationExpression();
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

Syntax *Parser::parseExpressionSeq() {
  llvm::SmallVector<Syntax *, 4> SS;
  do {
    if (atEndOfFile())
      break;

    parseItem(*this, &Parser::parseExpression, SS);
    // Non-declaration expressions will still have their semicolon unconsumed.
    if (nextTokenIs(tok::Semicolon))
      consumeToken();
  } while (nextTokenIsNot(tok::RightBrace));

  if (SS.size() == 1 && isa<ListSyntax>(SS.front()))
    return SS.front();

  return new ListSyntax(AllocateSeq(SS), SS.size());
}

static bool isAssignmentOp(tok::TokenKind K)
{
  return K == tok::PlusEqual || K == tok::MinusEqual
        || K == tok::StarEqual || K == tok::SlashEqual
        || K == tok::PercentEqual || K == tok::Equal
        || K == tok::LessLessEqual || K == tok::GreaterGreaterEqual;
}

Syntax *Parser::parseAssignmentExpression() {
  Syntax *E0 = parseLogicalOrExpression();
  while (Token Op = matchTokenIf(isAssignmentOp)) {
    Syntax *E1 = nullptr;
    if (nextTokenIs(tok::LambdaKeyword))
      E1 = parseControlExpression();
    else
      E1 = parseAssignmentExpression();

    E0 = new InfixSyntax(Op, E0, E1);
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

static bool isRelationalOperator(tok::TokenKind K) {
  return K == tok::Less ||
    K == tok::Greater ||
    K == tok::LessEqual ||
    K == tok::GreaterEqual;
}

/// Parse a relational expression.
///
///   relational-expression:
///     shift-expression
///     relational-expression < shift-expression
///     relational-expression > shift-expression
///     relational-expression <= shift-expression
///     relational-expression >= shift-expression
Syntax *Parser::parseRelationalExpression() {
  Syntax* E0 = parseShiftExpression();
  while (Token Op = matchTokenIf(isRelationalOperator)) {
    Syntax* E1 = parseShiftExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isShiftOperator(tok::TokenKind K) {
  return K == tok::LessLess || K == tok::GreaterGreater;
}

/// Parse a shift expression.
///
///   shift-expression:
///     additive-expression
///     shift-expression << additive-expression
///     shift-expression >> additive-expression
Syntax *Parser::parseShiftExpression() {
  Syntax* E0 = parseAdditiveExpression();
  while (Token Op = matchTokenIf(isShiftOperator)) {
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
///     cast-expression
///     multiplicative-expression * cast-expression
///     multiplicative-expression / cast-expression
///     multiplicative-expression % cast-expression
Syntax *Parser::parseMultiplicativeExpression() {
  Syntax *E0 = parseCastExpression();
  while (Token Op = matchTokenIf(isMultiplicativeOperator)) {
    Syntax *E1 = parseCastExpression();
    E0 = new InfixSyntax(Op, E0, E1);
  }

  return E0;
}

static bool isCastOperator(tok::TokenKind K)
{
  return K == tok::IsKeyword ||
    K == tok::AsKeyword;
}

/// Parse a cast expression.
///
/// cast-expression:
///         prefix-expression
///         prefix-expression is prefix-expression
///         prefix-expression as prefix-expression
Syntax *Parser::parseCastExpression() {
  Syntax *E0 = parsePrefixExpression();
  while (Token Op = matchTokenIf(isCastOperator)) {
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
  case tok::MinusGreater:
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
///     [ parameter-group ] -> prefix-expression
///     ( parameter-group? ) -> prefix-expression
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
    if (!info.isOperator) {
      break;
    }
    return parseFunctionConstructor();
  }
  case tok::UsingKeyword:{
    Token Op = consumeToken();
    Syntax *E = parseExpression();
    return new PrefixSyntax(Op, E);
  }
  case tok::PlusPlus:
  case tok::MinusMinus:
  case tok::ConstKeyword:
  case tok::Caret:
  case tok::Plus:
  case tok::Minus:
  case tok::NotKeyword: {
    Token Op = consumeToken();
    Syntax *E = parsePrefixExpression();
    return new PrefixSyntax(Op, E);
  }

  default:
    break;
  }

  return parsePostfixExpression();
}

/// Parse a postfix-expression.
///
///   postfix-expression:
///     primary-expression
///     postfix-expression ( expression-list? )
///     postfix-expression [ expression-list? ]
///     postfix-expression . id-expression
///     postfix-expression ^
///     postfix-expression is prefix-expression
///     postfix-expression as prefix-expression
Syntax *Parser::parsePostfixExpression() {
  Syntax *E0 = parsePrimaryExpression();
  while (true)
  {
    if (nextTokenIs(tok::LeftParen)) {
      Syntax *Args = parseParenEnclosed(&Parser::parseExpressionList);
      E0 = new CallSyntax(E0, Args);
    } else if (nextTokenIs(tok::LeftBracket)) {
      Syntax *Args = parseBracketEnclosed(&Parser::parseExpressionList);
      E0 = new CallSyntax(E0, Args);
    } else if (Token Dot = matchToken(tok::Dot)) {
      if (nextTokenIs(tok::LeftParen)) {
        Token LParen = matchToken(tok::LeftParen);
        Syntax *QualIdE = parsePrefixExpression();
        if (Token RParen = matchToken(tok::RightParen)) {
          if (nextTokenIs(tok::Dot)) {
            consumeToken();
          }
          Syntax *Member = parseIdExpression();
          E0 = new QualifiedMemberAccessSyntax(Dot, LParen, RParen, E0, QualIdE, Member);
        } else {
          Diags.Report(getInputLocation(), clang::diag::err_expected) <<
                       getSpelling(tok::RightParen);
          // Continue parsing the expression.
        }
      } else {
        Syntax *Member = parseIdExpression();
        E0 = new InfixSyntax(Dot, E0, Member);
      }
    } else if (Token Op = matchToken(tok::Caret)) {
      E0 = new PostfixSyntax(Op, E0);
    } else if (Token Inc = matchToken(tok::PlusPlus)) {
      E0 = new PostfixSyntax(Inc, E0);
    } else if (Token Dec = matchToken(tok::MinusMinus)) {
      E0 = new PostfixSyntax(Dec, E0);
    } else if (Token Is = matchToken(tok::IsKeyword)) {
      E0 = new PostfixSyntax(Is, E0);
    } else if (Token As = matchToken(tok::AsKeyword)) {
      E0 = new PostfixSyntax(As, E0);
    } else {
      break;
    }
  }

  return E0;
}

/// Parse a primary expression.
///
/// primary-expression:
///         literal
///         identifier
///         ( expression-list )
/// expression-list:
///         expression
///         expression-list , expression
Syntax *Parser::parsePrimaryExpression() {
  switch (getLookahead()) {
  case tok::Dot:{

    Token DotTok = consumeToken();
    // Syntax *E = parsePrefixExpression();
    if (Token Id = expectToken(tok::Identifier)) {
      Syntax *IdSyntax = new IdentifierSyntax(Id);
      return new PrefixSyntax(DotTok, IdSyntax);
    }
    break;
  }
  case tok::DecltypeKeyword:
  case tok::SizeOfKeyword:
  case tok::AlignOfKeyword:
  case tok::NoExceptKeyword:
  case tok::TypeidKeyword: {
    return parseBuiltinCompilerOp();
  }
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
  case tok::NamespaceKeyword:
  case tok::InplaceNewKeyword:
  case tok::InplaceDeleteKeyword:
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
  unsigned DiagID =
    Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                          "unexpected token: %0");
  Diags.Report(getInputLocation(), DiagID) << getSpelling(getLookahead());
  consumeToken();
  return nullptr;
}

static inline bool isOperator(tok::TokenKind K) {
  return K >= tok::Question && K <= tok::MinusMinus;
}

Syntax *Parser::parseIdExpression() {
  if (nextTokenIs(tok::OperatorKeyword)) {
    Token KW = consumeToken();
    if (Token Op = matchTokenIf(isOperator)) {
      Token Fuse = Token(KW.getKind(), KW.getLocation(),
                         (void*)Op.getSymbol().data());
      return new IdentifierSyntax(Fuse);
    }

    return new IdentifierSyntax(KW);
  }

  Token Id = expectToken(tok::Identifier);
  if (Id) {
    return new IdentifierSyntax(Id);
  }

  return nullptr;
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
  do
    parseItem(*this, &Parser::parseParameterList, SS);
  while (matchToken(tok::Semicolon));

  return makeParameterGroup(*this, SS);
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

inline bool isParameterSpec(tok::TokenKind K) {
  switch (K) {
  case tok::OverrideKeyword:
  case tok::FinalKeyword:
  case tok::VirtualKeyword:
  case tok::InKeyword:
  case tok::InoutKeyword:
  case tok::OutKeyword:
  case tok::MoveKeyword:
  case tok::ForwardKeyword:
    return true;
  default:
    return false;
  }
}

/// Parser a parameter:
///
///   parameter:
///     parameter_specifier{opt} identifier : type
///     parameter_specifier{opt} identifier : type = expression
///     parameter_specifier{opt} identifier : = expression
///     parameter_specifier{opt} : type
///     parameter_specifier{opt} : type = expression
///
/// TODO: Can parameters have introducers?
///
/// TODO: Can paramters be packs (yes, but what's the syntax?).
Syntax *Parser::parseParameter()
{
  llvm::SmallVector<Token, 4> ParamSpecs;
  while(Token ParamSpec = matchTokenIf(isParameterSpec))
    ParamSpecs.push_back(ParamSpec);

  // Match unnamed variants.
  if (nextTokenIs(tok::Colon)) {
    DescriptorClause DC;
    InitializerClause IC;

    DC = parseDescriptorClause(*this);
    if (nextTokenIs(tok::Equal))
      IC = parseInitializerClause(*this, /*Parameter=*/true);
    return new DeclarationSyntax(nullptr, DC.Type, nullptr, IC.Init);
  }

  // Match the identifier...
  Syntax *Id = parseIdExpression();

  // ... And optional declarative information
  DescriptorClause DC;
  InitializerClause IC;
  if (nextTokenIs(tok::Colon)) {
    DC = parseDescriptorClause(*this);
    if (nextTokenIs(tok::Equal))
      IC = parseInitializerClause(*this, /*Parameter=*/true);
  }

  DeclarationSyntax *Ret = new DeclarationSyntax(Id, DC.Type, DC.Cons, IC.Init);
  copyParamSpecs(Ret, ParamSpecs);
  return Ret;
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
  return parseBraceEnclosed(&Parser::parseExpressionSeq);
}

/// Parse a template type constructor.
///
///   prefix-expression:
///     [ parameter-group ] prefix-expression
Syntax* Parser::parseTemplateConstructor()
{
  Syntax *Op = parseBracketEnclosed(&Parser::parseParameterGroup);
  Syntax *Type = nullptr;
  if (nextTokenIs(tok::MinusGreater)) {
    consumeToken();
    Type = parsePrefixExpression();
  }
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
///     ( parameter-group? ) -> prefix-expression
Syntax* Parser::parseFunctionConstructor()
{
  Syntax *Op = parseParenEnclosed(&Parser::parseParameterGroup);
  Syntax *Type = nullptr;
  if (nextTokenIs(tok::MinusGreater)) {
    consumeToken();
    Type = parsePrefixExpression();
  }
  return new FunctionSyntax(Op, Type);
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

static bool isBuiltinCompilerOperator(tok::TokenKind K) {
  switch(K) {
    case tok::DecltypeKeyword:
    case tok::SizeOfKeyword:
    case tok::AlignOfKeyword:
    case tok::NoExceptKeyword:
    case tok::TypeidKeyword:
      return true;
    default:
      return false;
  }
}

Syntax *Parser::parseBuiltinCompilerOp() {
  assert(nthTokenConformsTo(0, isBuiltinCompilerOperator));
  Token Op = matchTokenIf(isBuiltinCompilerOperator);
  assert(Op && "Invalid call to parseBuiltinCompilerOp");
  Syntax *Args = parseParenEnclosed(&Parser::parseExpressionList);
  return new BuiltinCompilerOpSyntax(Op, Args);
}

Syntax *Parser::parseTupleExpression() {
  return nullptr;
}

Syntax *Parser::parseArrayExpression() {
  return nullptr;
}

/// Parse an equal-initializer.
///
///   equal-initializer:
///     = block-expression
Syntax *Parser::parseEqualInitializer() {
  requireToken(tok::Equal);
  return parseBlockExpression();
}

/// Parse an equal-initializer.
///
///   brace-initializer:
///     block-expression
Syntax *Parser::parseBraceInitializer() {
  return parseBlockExpression();
}

/// Parse a declarator.
///
///   declarator:
///     id-expression
///
/// This is a restriction on a more general grammar that allows function
/// and/or array-like declarators.
Syntax *Parser::parseDeclarator() {
  Syntax *Ret = parseIdExpression();

  // Consume and build the correct name specifier, this only applies for
  // namespace name declarations.
  // This declaration cannot have nested name qualififiers in the form of
  // x.(y)z
  while(nextTokenIs(tok::Dot)) {
    Token DotTok = matchToken(tok::Dot);
    Syntax *RHS = parseIdExpression();
    Ret = new InfixSyntax(DotTok, Ret, RHS);
  }

  return Ret;
}

} // namespace blue
