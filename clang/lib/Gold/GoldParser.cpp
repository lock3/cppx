//===- GoldParser.cpp - Gold Language Parser ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GoldParser interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Gold/GoldParser.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"

#include <iostream>

namespace gold {

namespace {

namespace enc {
enum Kind : unsigned
{
  Parens,
  Braces,
  Brackets,
  Tabs,
  Angles,
};
} // namespace enc

using EnclosureKind = enc::Kind;

TokenKind OpenTokens[]
{
  tok::LeftParen,
  tok::LeftBrace,
  tok::LeftBracket,
  tok::Indent,
  tok::Less,
};

TokenKind CloseTokens[]
{
  tok::RightParen,
  tok::RightBrace,
  tok::RightBracket,
  tok::Dedent,
  tok::Greater,
};

/// A class to help match enclosing tokens.
template<EnclosureKind K>
struct EnclosingTokens
{
  EnclosingTokens(Parser& p)
    : p(p)
  { }

  bool expectOpen()
  {
    open = p.expectToken(OpenTokens[K]);
    if ((bool)open)
      p.incrementEnclosureCount(K);
    return (bool)open;
  }

  bool expectClose()
  {
    // Allow end-of-file where dedents are expected.
    if (K == enc::Tabs)
    {
      if (p.atEndOfFile())
      {
        close = p.peekToken();
        p.decrementEnclosureCount(K);
        return true;
      }
    }

    close = p.expectToken(CloseTokens[K]);
    if (!close)
    {
      // FIXME: Emit a diagnostic.
      // note(open.loc, "matching '{}'' here", spelling(open.kind()));
    }
    p.decrementEnclosureCount(K);
    return (bool)close;
  }

  Parser& p;
  Token open;
  Token close;
};

struct EnclosingParens : EnclosingTokens<enc::Parens>
{
  using EnclosingTokens<enc::Parens>::EnclosingTokens;
};

struct EnclosingBraces : EnclosingTokens<enc::Braces>
{
  using EnclosingTokens<enc::Braces>::EnclosingTokens;
};

struct EnclosingBrackets : EnclosingTokens<enc::Brackets>
{
  using EnclosingTokens<enc::Brackets>::EnclosingTokens;
};

struct EnclosingTabs : EnclosingTokens<enc::Tabs>
{
  using EnclosingTokens<enc::Tabs>::EnclosingTokens;
};

struct EnclosingAngles : EnclosingTokens<enc::Angles>
{
  using EnclosingTokens<enc::Angles>::EnclosingTokens;
};

} // namespace

static Syntax *makeOperator(const SyntaxContext &Ctx,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op);
static Syntax *makeList(const SyntaxContext &Ctx,
                        std::initializer_list<Syntax *> List);
static Attribute *makeAttr(const SyntaxContext &Ctx, Syntax *Arg);

Parser::Parser(SyntaxContext &Context, clang::SourceManager &SM, File const& F)
  : Lex(SM, F), Diags(SM.getDiagnostics()), Context(Context)
{
  fetchToken();
}

Token Parser::expectToken(TokenKind K)
{
  if (nextTokenIs(K))
    return consumeToken();

  char const* Spelling = getSpelling(K);
  Diags.Report(getInputLocation(), clang::diag::err_expected) << Spelling;
  return {};
}

Token Parser::expectToken(char const* Id)
{
  if (nextTokenIs(Id))
    return consumeToken();

  Diags.Report(getInputLocation(), clang::diag::err_expected) << Id;
  return {};
}

// file:
//    array?
Syntax *Parser::parseFile()
{
  if (!atEndOfFile()) {
    llvm::SmallVector<Syntax *, 16> Vec;
    parseArray(BlockArray, Vec);
    return onFile(Vec);
  }
  return nullptr;
}

static void appendTerm(llvm::SmallVectorImpl<Syntax *>& vec, Syntax *s)
{
  if (s && s != Syntax::error)
    vec.push_back(s);
}

static Syntax **createArray(const SyntaxContext &Ctx,
                            const llvm::SmallVectorImpl<Syntax *> &Vec) {
  Syntax **Array = new (Ctx) Syntax *[Vec.size()];
  std::copy(Vec.begin(), Vec.end(), Array);
  return Array;
}

static Syntax **createArray(const SyntaxContext &Ctx,
                            std::initializer_list<Syntax *> List) {
  Syntax **Array = new (Ctx) Syntax *[List.size()];
  std::copy(List.begin(), List.end(), Array);
  return Array;
}

static bool isSeparator(TokenKind K) {
  return K == tok::Separator || K == tok::Semicolon;
}

// array:
//    list
//    array sequencer list
//
// sequencer:
//    ;
//    separator
//
// Note that the original expression allows arrays to be empty. I think
// it's better to make that optionality part of the lower-precedence rule
// that contains this one.
//
// FIXME: It seems like this could be refactored into a more C++-like
// statement sequence where each statement is terminated by a separator
// or semi-colon. The original specification is:
//
//    list  = lookahead(';') | expr {',' scan expr}
//    array = push lineprefix='' scan [list {(';'|ending) scan list}] pop
//
// The fact that ';' is allowed as part of a list implies the
// existence of an empty statement -- probably to support things like this:
//
//    if (expr)
//      ;
//
//
// and this:
//
//    ;;;
//
// In this case, we have a list (';') followed by a sequencer (';') followed
// by another list ';'.
//
// But it's not entirely obvious if that's true. Note that this also allows
// some weirdness like ';;;;<newline>'.
//
// I think it would be better to define an explicit stmt-level construct.
//
//    stmts = {stmt}
//    stmt = [expr] (';'|ending)
//
// Or something like that.
Syntax *Parser::parseArray(ArraySemantic S) {
  llvm::SmallVector<Syntax *, 8> Vec;
  parseArray(S, Vec);
  return onArray(S, Vec);
}

// Parse the array and populate the vector.
void Parser::parseArray(ArraySemantic S, llvm::SmallVectorImpl<Syntax *> &Vec) {
  Syntax *List = parseList(S);
  appendTerm(Vec, List);

  while (true)
  {
    // Obviously, stop at the end of the file.
    if (atEndOfFile())
      break;

    // We're about to exist a nested block ...
    if (nextTokenIs(tok::Dedent) || nextTokenIs(tok::RightBrace))
      break;

    // ... or a paren-enclosed array ...
    if (nextTokenIs(tok::RightParen))
      break;

    // ... or a bracket-enclosed array.
    if (nextTokenIs(tok::RightBracket))
      break;

    // This should be a list separator, but only if the last token
    // consumed was not a dedent (which implicitly acts as a separator).
    // For now, match this as optional.
    //
    // FIXME: Actually diagnose missing separators.
    matchTokenIf(isSeparator);

    // The end-of-file is often after the last separator.
    if (atEndOfFile())
      break;

    List = parseList(S);
    appendTerm(Vec, List);
  }
}

// list:
//    ;
//    expr
//    list , expr
//
// Note that a ';' is interpreted as an empty list, so we return nullptr,
// in that case. The nullptr will be ignored by appendTerm, so empty lists are
// not represented in the AST.
//
// TODO: Represent empty lists in the AST?
Syntax *Parser::parseList(ArraySemantic S)
{
  // FIXME: Is this semantically meaningful?
  if (matchToken(tok::Semicolon))
    return nullptr;

  llvm::SmallVector<Syntax *, 4> Vec;
  parseList(Vec);

  return onList(S, Vec);
}

// Parse the list and populate the vector.
void Parser::parseList(llvm::SmallVectorImpl<Syntax *> &Vec)
{
  Syntax *Expr = parseExpr();
  appendTerm(Vec, Expr);
  while (matchToken(tok::Comma)) {
    Expr = parseExpr();
    appendTerm(Vec, Expr);
  }
}

// expr:
//    def
//    def => block
//    def => expr
//    def where-clause
//
// where-clause:
//    block
//    def-list
//
// def-list:
//    def
//    def-list , def
Syntax *Parser::parseExpr()
{
  while (nextTokenIs(tok::LeftBracket))
    if (parsePreattr())
      return onError();

  Syntax *Def = parseDef();

  if (Token Op = matchToken(tok::EqualGreater)) {
    // Note that '=>' is the mapping operator (e.g., a => 5). I'm not at
    // all sure what this means semantically.
    //
    // Note that this is right associative.
    //
    // TODO: Why is this a sequence of "trailers" on definitions. We end
    // up allowing things like: 'x => e1 => e2 => { ... }'.
    Syntax *Val;
    if (nextTokenIs(tok::LeftBrace))
      Val = parseBracedArray();
    else if (nextTokenIs(tok::Indent))
      Val = parseNestedArray();
    else
      Val = parseExpr();

    // FIXME: Skip to the end of the list or stmt/line.
    if (Val == Syntax::error)
      return onError();

    return onBinary(Op, Def, Val);
  }

  if (matchToken("where"))
  {
    // FIXME: Try to match this:
    //
    //    scankey 'where' word (block | def {',' scan def} !',')}
    //
    // I'm not sure 'word' is supposed to match. Also note that there's
    // a potential ambiguity depending on how we interpret negation in
    // the grammar.
    //
    // Nominally, we want to support expressions of the form:
    //
    //    a where b, c, d
    //
    // and presumably b is some kind of boolean expression. Of course,
    // the grammar being recursive allows things like:
    //
    //    a where b where c where d
    //
    // which may or may not be equivalent to the above? Also, what happens
    // if any of the conditions are false?
    llvm_unreachable("where clauses not supported");
  }

  // FIXME: Support trailing docattrs.

  return Def;
}

bool Parser::parsePreattr() {
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return true;

  AttributeScope AttrScope(InAttribute);

  // Don't parse an attribute if the angles are empty.
  Syntax *Arg = !(nextTokenIs(tok::Greater) || nextTokenIs(tok::GreaterEqual))
    ? parseExpr() : nullptr;

  if (!Brackets.expectClose())
    return true;

  Attribute *Attr = makeAttr(Context, Arg);
  Preattributes.push_back(Attr);
  expectToken(tok::Separator);
  return false;
}

static bool isAssignmentOperator(TokenKind K) {
  switch (K) {
  default:
    return false;
  case tok::Equal:
  case tok::ColonEqual:
  case tok::PlusEqual:
  case tok::MinusEqual:
  case tok::StarEqual:
  case tok::SlashEqual:
    return true;
  }
}

// def:
//    or assignment-operator def
//    or ! braced-array
//    or ! nested-array
//
// The original specification was:
//
//    def = or { ('='|':='|'+='|'-='|'*='|'/='  ) scan def | ... }
//
// Where the ... is the ! production for function definitions. I'm not
// sure the repetition is desirable here because the it's already right
// recursive. If you have;
//
//    x = y = z
//
// that will naturally parse as
//
//    x = (y = z)
//
// And the repetition never matches. Note that this is likely true for all
// uses of right recursion in the grammar.
Syntax *Parser::parseDef() {
  Syntax *def = parseOr();

  if (Token op = matchTokenIf(isAssignmentOperator)) {
    Syntax *val = parseDef();
    return onBinary(op, def, val);
  }

  // FIXME: Is the only way to define a function to follow the declarator
  // with a '!'? It seems like that would work better as a suffix operator
  // on the declarator (it also leads naturally to factorials!).
  if (Token op = matchToken(tok::Bang)) {
    // FIXME: This should probably not be inside the loop. It allows
    // weirdness like this: 'f ! { ...} ! { ... } ! ...'. This would also
    // be interspersed with assignments: 'f ! { ... } = expr'
    Syntax *body;
    if (nextTokenIs(tok::LeftBrace)) {
      body = parseBracedArray();
    } else if (nextTokenIs(tok::Indent)) {
      body = parseNestedArray();
    } else {
      // FIXME: Skip to the end of the list or stmt/line.
      Diags.Report(getInputLocation(), clang::diag::err_expected) << "'{' or indent";
      return onError();
    }

    return onBinary(op, def, body);
  }

  return def;
}

static bool isOrOperator(Parser& P) {
  return P.nextTokenIs(tok::BarBar) || P.nextTokenIs("or");
}

// or:
//    and
//    or or-operator and
//
// or-operator:
//    ||
//    "or"
Syntax *Parser::parseOr() {
  Syntax *E1 = parseAnd();
  while (Token Op = matchTokens(isOrOperator, *this)) {
    Syntax *E2 = parseAnd();
    E1 = onBinary(Op, E1, E2);
  }
  return E1;
}

auto isAndOperator(Parser &P) {
  return P.nextTokenIs(tok::AmpersandAmpersand) || P.nextTokenIs("and");
}

// and:
//    cmp
//    and and-operator cmp
//
// and-operator:
//    &&
//    "and"
Syntax *Parser::parseAnd() {
  Syntax *E1 = parseCmp();
  while (Token Op = matchTokens(isAndOperator, *this)) {
    Syntax *E2 = parseCmp();
    E1 = onBinary(Op, E1, E2);
  }
  return E1;
}

static bool isLogicalUnaryOperator(Parser& P) {
  return P.nextTokenIs(tok::Ampersand)
      || P.nextTokenIs(tok::DotDot)
      || P.nextTokenIs(tok::Bang)
      || P.nextTokenIs("not");
}

static bool is_relational_operator(Parser& P) {
  switch (P.getLookahead()) {
  default:
    return false;
  case tok::EqualEqual:
  case tok::BangEqual:
  case tok::LessGreater:
  case tok::Less:
  case tok::LessEqual:
    return true;
  case tok::Greater:
  case tok::GreaterEqual:
    return P.GreaterThanIsOperator;
  }
}

/// cmp:
///    to
///    cmp relational-operator to
///    unary-operator cmp
///
/// FIXME: Is there any reason these don't have two levels of precedence?
Syntax *Parser::parseCmp() {
  if (Token op = matchTokens(isLogicalUnaryOperator, *this)) {
    Syntax *e1 = parseCmp();
    return onUnary(op, e1);
  }

  Syntax *e1 = parseTo();

  while (Token op = matchTokens(is_relational_operator, *this)) {
    Syntax *e2 = parseTo();
    e1 = onBinary(op, e1, e2);
  }

  return e1;
}

bool isToOperator(Parser& P) {
  return P.nextTokenIs(tok::Colon)
      || P.nextTokenIs(tok::MinusGreater)
      || P.nextTokenIs("in");
}

// to:
//    add
//    to to-operator add
//
// to-operator:
//    :
//    ->
//    in
//
// TODO: -> is at the wrong level of precedence and has the wrong
// associativity. Also, what's the behavior.
Syntax *Parser::parseTo() {
  Syntax *E1 = parseAdd();
  while (Token op = matchTokens(isToOperator, *this)) {
    Syntax *E2 = parseAdd();
    E1 = onBinary(op, E1, E2);
  }
  return E1;
}

bool isAddOperator(Parser& P) {
  return P.nextTokenIs(tok::Plus) || P.nextTokenIs(tok::Minus)
    || P.nextTokenIs(tok::DotDot);
}

/// add:
///   mul
///   add add-operator mul
///
/// add-operator:
///   +
///   -
///   ..
Syntax *Parser::parseAdd() {
  Syntax *E1 = parseMul();
  while (Token Op = matchTokens(isAddOperator, *this)) {
    Syntax *E2 = parseMul();
    E1 = onBinary(Op, E1, E2);
  }
  return E1;
}

static bool isMulOperator(Parser& P) {
  return P.nextTokenIs(tok::Star) || P.nextTokenIs(tok::Slash);
}

/// mul:
///   call
///   mul mul-operator call
Syntax *Parser::parseMul() {
  Syntax *E1 = parseMacro();
  while (Token Op = matchTokens(isMulOperator, *this)) {
    Syntax *E2 = parseMacro();
    E1 = onBinary(Op, E1, E2);
  }
  return E1;
}

/// macro:
///   post
///   post block
///   if ( list ) block
///   if ( list ) block else block
///   while ( list ) block
///   for ( list ) block
///   block
///
/// catch:
///   catch ( list ) block
///
/// \todo We can parse a macro as a list of (post block) chains if we
/// had an continuation token (e.g., 'else').
///
/// \todo Implement catch blocks. We should probably only allow these
/// on the 'block' production, and possibly explicitly allow a 'try'
/// statement. Note that catch-blocks are really a sequence of catches,
/// possibly followed by a finally.
///
/// \note Macros are essentially right associative in structure, so
/// that a naive post-order traversal will not produce the correct typing
/// or evaluation. They must be traversed pre-order (i.e., analyze the
/// outermost properties, followed by the innermost).
Syntax *Parser::parseMacro()
{
  if (nextTokenIs("if"))
    return parseIf();

  if (nextTokenIs("while"))
    return parseWhile();

  if (nextTokenIs("for"))
    return parseFor();

  // FIXME: What are we matching here?
  if (nextTokenIs(tok::LeftBrace) || nextTokenIs(tok::Colon))
    return parseBlock();

  // FIXME: Should this be parsePost?
  Syntax *e1 = parsePre();

  if (nextTokenIs(tok::LeftBrace))
  {
    Syntax *e2 = parseBlock();
    return onMacro(e1, e2);
  }
  if (nextTokenIs(tok::Colon) && nthTokenIs(1, tok::Indent)) {
      Syntax *e2 = parseBlock();
      return onMacro(e1, e2);
  }

  return e1;
}

Syntax *Parser::parseIf()
{
  Token if_tok = expectToken("if");
  Syntax *cond = nextTokenIs(tok::Colon) ? parseBlock() : parseParen();

  Syntax *then_block;
  if (matchToken("then"))
    then_block = nextTokenIs(tok::Colon) ? parseBlock() : parseExpr();
  else
    then_block = parseBlock();

  Syntax *else_macro;
  if (Token else_tok = matchToken("else"))
  {
    Syntax *else_block;
    if (nextTokenIs("if"))
      else_block = parseIf();
    else
      else_block = parseBlock();

    else_macro = onElse(else_tok, else_block);
  }
  else
  {
    else_macro = nullptr;
  }

  return onIf(if_tok, cond, then_block, else_macro);
}

Syntax *Parser::parseWhile()
{
  Token WhileTok = expectToken("while");
  if (nextTokenIs(tok::Colon))
    return parseBlockLoop(WhileTok);
  Syntax *Cond = parseParen();

  Syntax *Block = matchToken("do") ? parseExpr() : parseBlock();

  return onLoop(WhileTok, Cond, Block);
}

Syntax *Parser::parseFor()
{
  Token ForTok = expectToken("for");
  if (nextTokenIs(tok::Colon))
    return parseBlockLoop(ForTok);

  Syntax *Cond = parseParen();

  Syntax *Block = matchToken("do") ? parseExpr() : parseBlock();

  return onLoop(ForTok, Cond, Block);
}

Syntax *Parser::parseBlockLoop(Token KWTok)
{
  Syntax *CondBlock = parseBlock();

  expectToken("do");
  Syntax *Block = parseBlock();

  return onLoop(KWTok, CondBlock, Block);
}

auto is_unary_operator = [](TokenKind k) -> bool
{
  switch (k)
  {
  case tok::Question:
  case tok::Caret:
  case tok::Plus:
  case tok::Minus:
  case tok::Star:
    return true;
  default:
    return false;
  }
};

Syntax *Parser::parsePre()
{
  if (Token Op = matchTokenIf(is_unary_operator)) {
    Syntax *E = parsePre();
    return onUnary(Op, E);
  }

  if (nextTokenIs("const")) {
    Token Op = consumeToken();
    Syntax *E = parsePre();
    return onUnary(Op, E);
  }

  if (nextTokenIs("return") || nextTokenIs("returns")) {
    Token Op = consumeToken();
    Syntax *E = nullptr;
    if (getLookahead() != tok::Dedent && getLookahead() != tok::Separator) {
      E = parseExpr();
    }
    return onUnaryOrNull(Op, E);
  }

  if (nextTokenIs(tok::LeftBracket)) {
    return parseArrayPrefix();
  }

  if (nextTokenIs(tok::Dot)) {
    Token Operator = consumeToken();
    Syntax *Operand = parseExpr();
    return onUnary(Operator, Operand);
  }

  return parsePost();
}

static bool isNonAngleEnclosure(TokenKind K) {
  switch (K) {
  case tok::RightParen:
  case tok::RightBracket:
  case tok::RightBrace:
  case tok::Dedent:
  case tok::LeftParen:
  case tok::LeftBracket:
  case tok::LeftBrace:
  case tok::Indent:
    return true;
  default:
    return false;
  }
}

/// Whether or not this is a closing enclosure token, not counting angles.
static bool isNonAngleCloseEnclosure(TokenKind K) {
  switch (K) {
  case tok::RightParen:
  case tok::RightBracket:
  case tok::RightBrace:
  case tok::Dedent:
    return true;
  default:
    return false;
  }
}

/// Keep track of the depth of enclosure tokens when scanning for
/// attributes.
void Parser::trackEnclosureDepth(Token Enclosure) {
  std::size_t K = 0;
  switch (Enclosure.getKind()) {
  case tok::LeftParen:
  case tok::RightParen:
    break;
  case tok::LeftBracket:
  case tok::RightBracket:
    K = 1;
    break;
  case tok::LeftBrace:
  case tok::RightBrace:
    K = 2;
    break;
  case tok::Indent:
  case tok::Dedent:
    K = 3;
    break;
  default:
    llvm_unreachable("using non-enclosure as enclosure");
  }

  AngleBracketTracker::Loc EncLoc{Enclosure.getLocation(),
    {Angles.EnclosureCounts[0], Angles.EnclosureCounts[1],
        Angles.EnclosureCounts[2], Angles.EnclosureCounts[3]}};

  if (isNonAngleCloseEnclosure(Enclosure.getKind())) {
    if (Angles.isOpen() &&
        Angles.hasSameDepth(Angles.Angles.back(), EncLoc))
      Angles.Angles.pop_back();

    if (Angles.EnclosureCounts[K])
      --Angles.EnclosureCounts[K];
    if (!Angles.Enclosures.empty())
      Angles.Enclosures.pop_back();

    return;
  }

  ++Angles.EnclosureCounts[K];
  ++EncLoc.EnclosureCounts[K];
  Angles.Enclosures.push_back(EncLoc);
}

/// Scan through tokens starting from a '<' and determine whether or not this
/// is a comparison or attribute.
bool Parser::scanAngles(Syntax *Base) {
  std::size_t I = 0;

  // This came after a token that does not appear in base names.
  if (!PreviousToken.hasKind(tok::Identifier) &&
      !PreviousToken.hasKind(tok::Greater) &&
      !isNonAngleEnclosure(PreviousToken.getKind()))
    return false;

  AngleBracketTracker::Loc PotentialBaseLoc{Base->getLoc(),
                                            {Angles.EnclosureCounts[0],
                                             Angles.EnclosureCounts[1],
                                             Angles.EnclosureCounts[2],
                                             Angles.EnclosureCounts[3]}};

  while (true) {
    Token Current = peekToken(I++);

    // Quit at the end of the line, or end of file in the case of the
    // rare one-line program.
    if (Current.isNewline() || Current.isEndOfFile())
      return false;

    // Newlines might be recognized as separators rather than newline tokens.
    // FIXME: Move this to Token::isNewline()
    if (Current.hasKind(tok::Separator))
      if (*(Current.getSymbol().data()) == '\n')
        return false;

    // If the programmer has used semicolons instead of newlines, we need
    // to be sure the semicolon is actually ending the line; in other words,
    // at the same depth as the base name.
    if (Current.hasKind(tok::Semicolon)) {
      AngleBracketTracker::Loc SemiLoc{Current.getLocation(),
                                       {Angles.EnclosureCounts[0],
                                        Angles.EnclosureCounts[1],
                                        Angles.EnclosureCounts[2],
                                        Angles.EnclosureCounts[3]}};
      if (Angles.hasSameDepth(SemiLoc, PotentialBaseLoc))
        return false;
      // We reached a semicolon in some sort of nested list (or typo). We
      // already know we don't care about this token so just skip ahead.
      continue;
    }

    if (isNonAngleEnclosure(Current.getKind()))
      trackEnclosureDepth(Current);

    if (Current.hasKind(tok::Less))
      startPotentialAngleBracket(Current);

    if (Angles.isOpen() &&
        (Current.hasKind(tok::Greater) ||
         Current.hasKind(tok::GreaterEqual))) {
      finishPotentialAngleBracket(Current);

      if (!Angles.isOpen())
        return true;
    }
  }
}

/// postfix:
///   base
///   postfix ( array )
///   postfix [ array ]
///   postfix < expr >
///   postfix . identifier
///   postfix suffix-operator
///
/// suffix-operator:
///   ?
///   ^
///   @
Syntax *Parser::parsePost()
{
  Syntax *e = parsePrimary();
  while (true)
  {
    switch (getLookahead())
    {
    case tok::LeftParen:
      e = parseCall(e);
      break;

    case tok::LeftBracket:
      e = parseElem(e);
      break;

    case tok::Less: {
      if (scanAngles(e))
        e = parsePostAttr(e);
      else {
        Angles.clear();
        return e;
      }

      break;
    }
    case tok::Dot:
      e = parseDot(e);
      break;

    case tok::Question:
    case tok::Caret:
    case tok::At:
      llvm_unreachable("suffix operators not implemented");
      consumeToken();
      break;

    default:
      return e;
    }
  }

  // We should never reach this point.
  assert(false);
  return nullptr;
}

Syntax *Parser::parseCall(Syntax *Fn)
{
  EnclosingParens Parens(*this);
  if (!Parens.expectOpen())
    return onError();

  // Don't parse an array if the parens are empty.
  //
  // FIXME: Don't allow newlines in the parameter array?
  //
  // TODO: If this is an Syntax::error, should we skip to the next paren or
  // to the the nearest comma? separator? What?
  Syntax *Args = !nextTokenIs(tok::RightParen) ? parseArray(ArgArray)
    : onList(ArgArray, llvm::SmallVector<Syntax *, 0>());

  if (!Parens.expectClose())
    return onError();

  return onCall({Parens.open, Parens.close}, Fn, Args);
}

Syntax *Parser::parseElem(Syntax *Map)
{
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return onError();

  Syntax *Args = parseArray(ArgArray);

  if (!Brackets.expectClose())
    return onError();

  return onElem({Brackets.open, Brackets.close}, Map, Args);
}

Syntax *Parser::parseDot(Syntax *obj)
{
  Token op = expectToken(tok::Dot);

  // FIXME: This is somehow a qualified-id, except that I don't know
  // what that means.
  Syntax *sub = parseId();

  return onBinary(op, obj, sub);
}

// Parse a call to operator'[]' of the form [a]b
Syntax *Parser::parseArrayPrefix()
{
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return onError();

  Syntax *Arg = parseExpr();

  if (!Brackets.expectClose())
    return onError();

  Syntax *Map = parsePrimary();
  return new (Context)
    CallSyntax(makeOperator(Context, Arg->getLoc(), "[]"),
               makeList(Context, {Arg, Map}));
}

Syntax *Parser::parsePostAttr(Syntax *Pre) {
  EnclosingAngles Angles(*this);
  if (!Angles.expectOpen())
    return onError();

  GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, false);

  // Don't parse an attribute if the angles are empty.
  Syntax *Arg = !(nextTokenIs(tok::Greater) || nextTokenIs(tok::GreaterEqual))
    ? parseExpr() : nullptr;

  // In the case where the user ended the attribute list with `>=`, such as
  // in `x<private>=0`, we use this dirthack to split >= back into
  // separate tokens.
  if (nextTokenIs(tok::GreaterEqual)) {
    clang::SourceLocation Loc = Toks.front().getLocation();
    Toks.pop_front();
    Toks.emplace_front(tok::Equal, Loc, getSymbol("="));
    Toks.emplace_front(tok::Greater, Loc, getSymbol(">"));
  }

  if (!Angles.expectClose())
    return onError();

  Attribute *Attr = makeAttr(Context, Arg);

  Pre->addAttribute(Attr);

  this->Angles.clear();
  return Pre;
}

Syntax *Parser::parsePrimary() {
  switch (getLookahead()) {
  case tok::Identifier:
    return parseId();

  case tok::BinaryInteger:
  case tok::DecimalInteger:
  case tok::HexadecimalInteger:
  case tok::DecimalFloat:
  case tok::HexadecimalFloat:
  case tok::Character:
  case tok::String:
  case tok::ClassKeyword:
  case tok::NamespaceKeyword:
  case tok::TrueKeyword:
  case tok::FalseKeyword:
    return onAtom(consumeToken());

  case tok::LeftParen:
    return parseParen();

  case tok::VoidKeyword:
  case tok::BoolKeyword:
  case tok::CharKeyword:
  case tok::Wchar_tKeyword:
  case tok::Wint_tKeyword:
  case tok::Char8_tKeyword:
  case tok::Char16_tKeyword:
  case tok::Char32_tKeyword:
  case tok::SignedCharKeyword:
  case tok::ShortKeyword:
  case tok::IntKeyword:
  case tok::LongKeyword:
  case tok::LongLongKeyword:
  case tok::Int128_tKeyword:
  case tok::UnsignedCharKeyword:
  case tok::UnsignedShortKeyword:
  case tok::UnsignedKeyword:
  case tok::UnsignedLongKeyword:
  case tok::UnsignedLongLongKeyword:
  case tok::Uint128_tKeyword:
  case tok::FloatKeyword:
  case tok::DoubleKeyword:
  case tok::LongDoubleKeyword:
  case tok::Float128_tKeyword:
  case tok::TypeKeyword:
  case tok::ArgsKeyword:
    return onLiteral(consumeToken());

  default:
    break;
  }

  // Diagnose the error and consume the token so we don't see it again.
  Diags.Report(getInputLocation(), clang::diag::err_expected) << "primary-expression";
  consumeToken();
  return onError();
}

/// id:
///   identifier
Syntax *Parser::parseId() {
  Token id = expectToken(tok::Identifier);
  if (!id)
    return onError();
  return onAtom(id);
}

Syntax *Parser::parseParen() {
  EnclosingParens parens(*this);
  if (!parens.expectOpen())
    return onError();

  GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, true);

  // TODO: If this is an Syntax::error, should we skip to the next paren or
  // to the the nearest comma? separator? What?
  Syntax *seq = parseArray(ArgArray);

  if (!parens.expectClose())
    return onError();

  return seq;
}

// braced-array:
//    { array }
Syntax *Parser::parseBracedArray() {
  EnclosingBraces braces(*this);
  if (!braces.expectOpen())
    return onError();

  // FIXME: How do we recover from errors?
  Syntax *ret = parseArray(BlockArray);

  if (!braces.expectClose())
    return onError();

  return ret;
}

// nested-array:
//    indent array dedent
Syntax *Parser::parseNestedArray() {
  EnclosingTabs Tabs(*this);
  if (!Tabs.expectOpen())
    return onError();

  Syntax *ret = parseArray(BlockArray);

  if (!Tabs.expectClose())
    return onError();

  return ret;
}

/// block:
///   braced-array
///   : nested-array
Syntax *Parser::parseBlock() {
  if (nextTokenIs(tok::LeftBrace))
    return parseBracedArray();

  expectToken(tok::Colon);
  return parseNestedArray();
}

// Semantic actions

// Returns the identifier 'operator\'<op>\''.
static Syntax *makeOperator(const SyntaxContext &Ctx,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op)
{
  // FIXME: Make this a fused operator?
  std::string Name = "operator'" + std::string(Op) + "'";
  Symbol Sym = getSymbol(Name);
  Token Tok(tok::Identifier, Loc, Sym);
  return new (Ctx) AtomSyntax(Tok);
}

static Syntax *makeOperator(const SyntaxContext &Ctx, Token const& Tok) {
  return makeOperator(Ctx, Tok.getLocation(), Tok.getSpelling());
}

static Syntax *makeList(const SyntaxContext &Ctx,
                        std::initializer_list<Syntax *> List) {
  assert(std::all_of(List.begin(), List.end(), [](Syntax *s) { return s; }));
  return new (Ctx) ListSyntax(createArray(Ctx, List), List.size());
}

static Syntax *makeCall(const SyntaxContext &Ctx, const Token& Tok) {
  return new (Ctx) CallSyntax(makeOperator(Ctx, Tok), makeList(Ctx, {}));
}

static Syntax *makeCall(const SyntaxContext &Ctx, const Token& Tok,
                        Syntax *Args) {
  return new (Ctx) CallSyntax(makeOperator(Ctx, Tok), Args);
}

static Attribute *makeAttr(const SyntaxContext &Ctx, Syntax *Arg) {
  return new (Ctx) Attribute(Arg);
}

Syntax *Parser::onAtom(const Token& Tok) {
  Syntax *Ret = new (Context) AtomSyntax(Tok);
  if (!InAttribute)
    attachPreattrs(Ret);
  return Ret;
}

Syntax *Parser::onLiteral(const Token& Tok) {
  return new (Context) LiteralSyntax(Tok);
}

Syntax *Parser::onArray(ArraySemantic S,
                        llvm::SmallVectorImpl<Syntax *> const& Vec) {
  if (S == ArgArray) {
    // For arguments, a singleton array is replaced by its element.
    if (Vec.size() == 1)
      return Vec.front();
  }
  return new (Context)
    ArraySyntax(createArray(Context, Vec), Vec.size());
}

Syntax *Parser::onList(ArraySemantic S,
                       llvm::SmallVectorImpl<Syntax *> const& Vec) {
  if (S == BlockArray) {
    // Within a block, flatten empty and singleton lists. Note that empty
    // lists will not be added to an array.
    if (Vec.empty())
      return nullptr;
    if (Vec.size() == 1)
      return Vec.front();
  }
  return new (Context) ListSyntax(createArray(Context, Vec), Vec.size());
}

Syntax *Parser::onBinary(Token const& Tok, Syntax *e1, Syntax *e2) {
  return new (Context)
    CallSyntax(makeOperator(Context, Tok), makeList(Context, {e1, e2}));
}

Syntax *Parser::onUnaryOrNull(Token const& Tok, Syntax *e1) {
  if (e1) {
    return new (Context)
      CallSyntax(makeOperator(Context, Tok), makeList(Context, {e1}));
  }
  return new (Context)
    CallSyntax(makeOperator(Context, Tok), makeList(Context, { }));
}

Syntax *Parser::onUnary(Token const& Tok, Syntax *e1) {
  return new (Context)
    CallSyntax(makeOperator(Context, Tok), makeList(Context, {e1}));
}

Syntax *Parser::onCall(TokenPair const& Toks, Syntax *e1, Syntax *e2) {
  // FIXME: Toks is unused.
  return new (Context) CallSyntax(e1, e2);
}

Syntax *Parser::onCall(Syntax *e1, Syntax *e2) {
  return new (Context) CallSyntax(e1, e2);
}

Syntax *Parser::onElem(TokenPair const& tok, Syntax *e1, Syntax *e2) {
  return new (Context) ElemSyntax(e1, e2);
}

Syntax *Parser::onMacro(Syntax *e1, Syntax *e2) {
  return new (Context) MacroSyntax(e1, e2, nullptr);
}

Syntax *Parser::onElse(Token const& Tok, Syntax *e1) {
  return new (Context) MacroSyntax(makeCall(Context, Tok), e1, nullptr);
}

Syntax *Parser::onIf(Token const& Tok, Syntax *e1, Syntax *e2, Syntax *e3) {
  return new (Context) MacroSyntax(makeCall(Context, Tok, e1), e2, e3);
}

Syntax *Parser::onLoop(Token const& Tok, Syntax *e1, Syntax *e2) {
  return new (Context) MacroSyntax(makeCall(Context, Tok, e1), e2, nullptr);
}

Syntax *Parser::onFile(const llvm::SmallVectorImpl<Syntax*> &Vec) {
  return new (Context) FileSyntax(createArray(Context, Vec), Vec.size());
}

Syntax *Parser::onError() const {
  return Syntax::error;
}

void Parser::incrementEnclosureCount(unsigned Enclosure) {
  switch (Enclosure) {
  case enc::Parens:
    ++ParenCount;
    break;
  case enc::Braces:
    ++BraceCount;
    break;
  case enc::Brackets:
    ++BracketCount;
    break;
  case enc::Tabs:
    ++IndentCount;
    break;
  default:
    return;
  }
}

void Parser::decrementEnclosureCount(unsigned Enclosure) {
  LastTokWasClose = true;

  switch (Enclosure) {
  case enc::Parens:
    if (ParenCount)
      --ParenCount;
    break;
  case enc::Braces:
    if (BraceCount)
      --BraceCount;
    break;
  case enc::Brackets:
    if (BracketCount)
      --BracketCount;
    break;
  case enc::Tabs:
    if (IndentCount)
      --IndentCount;
    break;
  default:
    return;
  }
}

// Remember that we saw this in case we see a potentially-matching
// '>' token later on.
void Parser::startPotentialAngleBracket(const Token &OpToken) {
  assert(OpToken.hasKind(tok::Less) && "not at a potential angle bracket");
  Angles.Angles.push_back({OpToken.getLocation(),
                           {Angles.EnclosureCounts[0],
                            Angles.EnclosureCounts[1],
                            Angles.EnclosureCounts[2],
                            Angles.EnclosureCounts[3]}});
}

// After a '>' or '>=', we're no longer potentially in a construct that's
// intended to be treated as an attribute.
void Parser::finishPotentialAngleBracket(const Token &OpToken) {
  assert (OpToken.hasKind(tok::Greater) || OpToken.hasKind(tok::GreaterEqual) &&
          "invalid angle bracket close.");
  AngleBracketTracker::Loc CloseLoc{OpToken.getLocation(),
                                    {Angles.EnclosureCounts[0],
                                     Angles.EnclosureCounts[1],
                                     Angles.EnclosureCounts[2],
                                     Angles.EnclosureCounts[3]}};
  if (Angles.hasSameDepth(Angles.Angles.back(), CloseLoc))
    Angles.Angles.pop_back();
}

void Parser::attachPreattrs(Syntax *S) {
  if (!Preattributes.size())
    return;

  for (auto *Attr : Preattributes)
    S->addAttribute(Attr);

  Preattributes.clear();
}

} // namespace gold
