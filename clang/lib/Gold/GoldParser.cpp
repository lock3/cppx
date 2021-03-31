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

#include "clang/Gold/GoldParser.h"

#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticLex.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/TargetInfo.h"

#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"

#include <iostream>
#include <string>
#include <iterator>

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
  BlockComment,
  DocAttr,
  ClosingHTMLTag
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
  tok::LessHash,
  tok::LessBar,
  tok::LessSlash,
};

TokenKind CloseTokens[]
{
  tok::RightParen,
  tok::RightBrace,
  tok::RightBracket,
  tok::Dedent,
  tok::Greater,
  tok::HashGreater,
  tok::Greater,
  tok::Greater,
};

/// A class to help match enclosing tokens.
template<EnclosureKind K, bool NoFetchExpect = false>
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


template<EnclosureKind K>
struct EnclosingTokens<K, true>
{
  EnclosingTokens(Parser& p)
    : p(p)
  { }

  bool expectOpen()
  {
    open = p.expectTokenNoFetch(OpenTokens[K]);
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

    close = p.expectTokenNoFetch(CloseTokens[K]);
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

struct EnclosingBlockComment : EnclosingTokens<enc::BlockComment>
{
  using EnclosingTokens<enc::BlockComment>::EnclosingTokens;
};

struct EnclosingDocAttr : EnclosingTokens<enc::DocAttr>
{
  using EnclosingTokens<enc::DocAttr>::EnclosingTokens;
};

struct EnclosingHtmlEndingTag : EnclosingTokens<enc::ClosingHTMLTag> {
  using EnclosingTokens<enc::ClosingHTMLTag>::EnclosingTokens;
};


} // namespace

static Syntax *makeOperator(const SyntaxContext &Ctx,
                            Parser &P,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op);

static Syntax *makeOperator(const SyntaxContext &Ctx,
                            Parser &P,
                            TokenKind TK,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op);

static Syntax *makeList(const SyntaxContext &Ctx,
                        std::initializer_list<Syntax *> List);

static Attribute *makeAttr(const SyntaxContext &Ctx, Syntax *Arg);

static Syntax *onMarkup(clang::ASTContext &Context, MarkupStyle S, Syntax *Name,
                        Syntax *Content, Syntax *OtherBlock, Syntax *EndingTag);

Parser::Parser(SyntaxContext &Context, clang::SourceManager &SM, File const& F,
               clang::Preprocessor &PP)
  : Lex(SM, F, Context, PP),
    Diags(SM.getDiagnostics()), Context(Context)
{
  fetchToken();
}

Token Parser::blockScannerFetch() {
  Token Tok;
  if (Lookahead)
    Tok = std::exchange(Lookahead, {});
  else
    Tok = lineScannerFetch();

  // Check for a newline followed by indentation.
  if (Tok.isNewline()) {
    Token Next = lineScannerFetch();
    if (Next.isSpace()) {
      // A newline followed by space is either an indent or a dedent.
      // For example:
      //
      //    if (x < y): <newline>
      //      stuff <newline>
      //
      // Combine the tokens to create the appropriate level of indentation.
      Tok = combineSpace(Tok, Next);
    } else {
      // At the top-level a newline followed by a token implies the
      // presence of a separator or dedent. For example:
      //
      //    x = 1 <newline>
      //    y = 2 <newline>
      //
      // For dedents, we might have this:
      //
      //    if (x < y): <newline>
      //      stuff <newline>
      //    more_stuff
      //
      // We want to replace the first newline token with a separator.
      // However, we have to preserve the token we just found.
      //
      // Note that the second newline is followed by eof, so we'd
      // probably want to do the same.
      Tok = combineSpace(Tok, {});

      // Buffer the next token for the next read.
      Lookahead = Next;
    }
  } else if (!Dedents.empty()) {
    Lookahead = Tok;
    Tok = combineSpace({}, {});
  }

  assert(!Tok.isNewline());
  return Tok;
}

// Line scanner

static llvm::StringMap<bool> InfixKeywords {
  {"where", true},
  {"otherwise", true},
  {"returns", true},
  {"until", true},
  {"using", true},
  {"catch", true}
};

static bool isInfix(Token Op, bool GTIO) {
  switch (Op.getKind()) {
  case tok::Plus:
  case tok::Minus:
  case tok::Star:
  case tok::Slash:
  case tok::Percent:
  case tok::Ampersand:
  case tok::Bar:
  case tok::Less:
  case tok::Equal:
  case tok::EqualEqual:
  case tok::LessEqual:
  case tok::GreaterEqual:
  case tok::AmpersandAmpersand:
  case tok::BarBar:
  case tok::ColonEqual:
  case tok::PlusEqual:
  case tok::MinusEqual:
  case tok::StarEqual:
  case tok::SlashEqual:
  case tok::PercentEqual:
  case tok::MinusGreater:
  case tok::EqualGreater:
  case tok::LeftParen:
  case tok::LeftBracket:
  case tok::Comma:
  case tok::CatchKeyword:
    return true;
  case tok::Identifier: {
    auto It = InfixKeywords.find(Op.getSymbol().data());
    if (It == InfixKeywords.end())
      return false;
    return true;
  }

  case tok::Greater:
    return GTIO;

  default:
    return false;
  }
}

// true when a token is not a space or comment
static inline bool isSignificant(const Token &Tok) {
  return !Tok.hasKind(tok::Invalid) || Tok.isSpace() ||
    Tok.isNewline() || Tok.isComment();
}



Token Parser::lineScannerFetch() {
  Token Tok;
  bool StartsLine = false;

  // These two booleans, as well as the temporary SkipNewlineAfterComment,
  // keep track of the state of comments.
  // Comments can end with a newline that may or may not be significant.
  // For example:
  // \code
  //   <# comment #>               <--- newline is insignficant
  //   main() : int! <# comment #> <--- newline is a separator
  //     return 0
  // \endcode
  // The newline after a comment is signficant when the last token
  // before the current SEQUENCE of comments was not whitespace or
  // invalid.
  bool PreviousWasComment = false;
  bool LastWasSignificant = false;
  while (true) {
    Tok = Lex();

    // If the last token wasn't a comment, check if it was signficant.
    // If it was, then we'll see if the previous token before the comment
    // sequence was signficant. If it was NOT, then the next newline we see
    // is NOT a separator.
    bool SkipNewlineAfterComment = false;
    if (!PreviousWasComment)
      LastWasSignificant = isSignificant(Current);
    else if (!LastWasSignificant)
      SkipNewlineAfterComment = true;

    if (!Tok.isSpace() && !Tok.isNewline()) {
      Current = Tok;
      if (Tok.isComment())
        PreviousWasComment = true;
    }

    // Space at the beginning of a line cannot be discarded here.
    if (Tok.isSpace() && Tok.isAtStartOfLine() &&
        !isInfix(Current, GreaterThanIsOperator))
      break;

    // Propagate a previous line-start flag to this next token.
    if (StartsLine) {
      Tok.Flags |= TF_StartsLine;
      StartsLine = false;
    }

    // Empty lines are discarded.
    if (Tok.isNewline() && Tok.isAtStartOfLine())
      continue;
    if (Tok.isNewline() && SkipNewlineAfterComment)
      continue;

    // Errors, space, and comments are discardable. If a token starts a
    // line, the next token will become the new start of line.
    if (AllowEmitNonIndentSpaces) {
      if (Tok.isSpace()) {
        return Tok;
      }
    }
    if (Tok.isInvalid() || Tok.isSpace() || Tok.isComment()) {
      StartsLine = Tok.isAtStartOfLine();
      continue;
    }

    // Discard space between a line-broken infix operator. e.g.)
    //
    // \code
    //   x = 2 + 2 *
    //       2 + 2
    // \endcode
    if ((Tok.isSpace() || Tok.isNewline()) &&
        isInfix(Current, GreaterThanIsOperator)) {
      continue;
    }

    // All other tokens are retained.
    break;
  }

  return Tok;
}

void Parser::fetchToken() {
  if (RawLexing) {
    Toks.push_back(Lex());
  } else {
    Toks.push_back(blockScannerFetch());
  }
}

Token Parser::expectTokenNoFetch(TokenKind K) {
  if (nextTokenIs(K))
    return consumeTokenNoFetch();
  char const* Spelling = getSpelling(K);
  Diags.Report(getInputLocation(), clang::diag::err_expected) << Spelling;
  return {};
}

Token Parser::expectTokenNoFetch(char const* Id) {
  if (nextTokenIs(Id))
    return consumeTokenNoFetch();
  Diags.Report(getInputLocation(), clang::diag::err_expected) << Id;
  return {};
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
//   list
//   indent array dedent catch_opt
//   array separator list
// separator:
//    ;
//    newline
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

  while (true) {


   // Obviously, stop at the end of the file.
    if (atEndOfFile())
      break;

    // We're about to exit a nested block ...
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
    while (!nextTokenIs(tok::Dedent) && matchTokenIf(isSeparator))
      ;

    // Check for an exit once again, as a semicolon might be followed
    // by a dedent.
    if (nextTokenIs(tok::Dedent) || nextTokenIs(tok::RightBrace))
      break;

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
  if (getLookahead() == tok::LessHash) {

    // Basically if the current expression is a seperator then ignore it.
    if (getLookahead() == tok::Separator) {
      consumeToken();
    }
    if (atEndOfFile()){
      return nullptr;
    }
  }
  // FIXME: Is this semantically meaningful?
  if (matchToken(tok::Semicolon))
    return nullptr;

  llvm::SmallVector<Syntax *, 4> Vec;
  parseList(Vec);

  return onList(S, Vec);
}

// Parse the list and populate the vector.
void Parser::parseList(llvm::SmallVectorImpl<Syntax *> &Vec) {
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

  // Don't parse an attribute if the brackets are empty.
  Syntax *Arg = nextTokenIs(tok::RightBracket) ? nullptr : parseExpr();

  if (!Brackets.expectClose())
    return true;
  if (!Arg)
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
  case tok::PercentEqual:
    return true;
  }
}

bool isAssignmentOperatorFromParser(Parser &P) {
  return isAssignmentOperator(P.getLookahead());
}

// static Syntax *invalidBinary
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
  if (InsideKnownFoldExpr && !ParseFoldOp) {
    def = parseDefFold(def);
  } else if (InsideKnownFoldExpr && ParseFoldOp) {
    // Do nothing here.
  } else if (Token op = matchTokenIf(isAssignmentOperator)) {
    Syntax *val = parseDef();
    def = onBinary(op, def, val);
  } else {
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
      def = onBinary(op, def, body);
    }
  }
  return def;
}

Syntax *Parser::parseDefFold(Syntax *E1) {
  if (nextOperatorIsFold(isAssignmentOperatorFromParser)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parseOr());
    }
  }
  return E1;
}

static bool isOrOperator(Parser& P) {
  return P.nextTokenIs(tok::BarBar) || P.nextTokenIs("or");
}


// this handles a trailing ... for an expression, this may be disabled
// in some circumstances when we are parsing a fold expression.
Syntax *Parser::parseExpansion() {
  // Basically this sits just before the to operator and allows me to handle
  // the ... operator as the last thing with the lowest precedence of
  // all operators and it wraps the expression output.
  Syntax *E1 = parseAdd();
  if (getLookahead() == tok::Ellipsis)
    E1 = parseExpansionOperator(E1);

  return E1;
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
  if (InsideKnownFoldExpr && !ParseFoldOp)
    return parseOrFold(E1);
  else if (InsideKnownFoldExpr && ParseFoldOp)
    return E1;

  while (Token Op = matchTokens(isOrOperator, *this)) {
    Syntax *E2 = parseAnd();
    E1 = onBinary(Op, E1, E2);
  }

  return E1;
}

Syntax *Parser::parseOrFold(Syntax *E1) {
  if (nextOperatorIsFold(isOrOperator)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parseAnd());
    }
  }

  return E1;
}

static auto isAndOperator(Parser &P) {
  return P.nextTokenIs(tok::AmpersandAmpersand) || P.nextTokenIs("and");
}

// and:
//    cmp
//    and and-operator cmp
//
// and-operator:
//    ^
//    &
//    &&
//    "and"
Syntax *Parser::parseAnd() {
  Syntax *E1 = parseCmp();
  if (InsideKnownFoldExpr && !ParseFoldOp)
    E1 = parseAndFold(E1);
  else if (InsideKnownFoldExpr && ParseFoldOp)
    return E1;

  while (Token Op = matchTokens(isAndOperator, *this)) {
    Syntax *E2 = parseCmp();
    E1 = onBinary(Op, E1, E2);
  }

  return E1;
}

Syntax *Parser::parseAndFold(Syntax *E1) {
  if (nextOperatorIsFold(isAndOperator)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parseCmp());
    }
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
  case tok::Less:
    return P.getLookahead(1) != tok::Bar;
  case tok::EqualEqual:
  case tok::BangEqual:
  case tok::LessGreater:
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

  Syntax *E1 = parseTo();
  if (InsideKnownFoldExpr && !ParseFoldOp)
    E1 = parseCmpFold(E1);
  else if (InsideKnownFoldExpr && ParseFoldOp)
    return E1;

  while (Token Op = matchTokens(is_relational_operator, *this)) {
    Syntax *E2 = parseTo();
    E1 = onBinary(Op, E1, E2);
  }

  return E1;
}

Syntax *Parser::parseCmpFold(Syntax *E1) {
  if (nextOperatorIsFold(is_relational_operator)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parseTo());
    }
  }

  return E1;
}

bool isToOperator(Parser& P) {
  return P.nextTokenIs(tok::Colon)
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
  Syntax *E1 = parseExpansion();
  while (Token op = matchTokens(isToOperator, *this)) {

    Syntax *E2 = parseExpansion();
    E1 = onBinary(op, E1, E2);
  }
  return E1;
}

bool isAddOperator(Parser& P) {
  return P.nextTokenIs(tok::Plus) || P.nextTokenIs(tok::Minus)
    || P.nextTokenIs(tok::DotDot) || P.nextTokenIs(tok::MinusGreater);
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
  if (InsideKnownFoldExpr && !ParseFoldOp)
    E1 = parseAddFold(E1);
  else if (InsideKnownFoldExpr && ParseFoldOp)
    return E1;

  while (Token Op = matchTokens(isAddOperator, *this)) {
    Syntax *E2 = parseMul();
    E1 = onBinary(Op, E1, E2);
  }

  return E1;
}

Syntax *Parser::parseAddFold(Syntax *E1) {
  if (nextOperatorIsFold(isAddOperator)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parseMul());
    }
  }
  return E1;
}

static bool isMulOperator(Parser& P) {
  return P.nextTokenIs(tok::Star) || P.nextTokenIs(tok::Slash)
         || P.nextTokenIs(tok::Percent);
}

static Syntax *makeCall(const SyntaxContext &Ctx, Parser &P, const Token& Tok);
static Syntax *makeCall(const SyntaxContext &Ctx, Parser &P,
                        const Token& Tok, Syntax *Args);

/// mul:
///   pre
///   pre mul-operator pre
Syntax *Parser::parseMul() {
  Syntax *E1 = parsePre();
  if (InsideKnownFoldExpr && !ParseFoldOp) {
    E1 = parseMulFold(E1);
  } else if (InsideKnownFoldExpr && ParseFoldOp) {
    return E1;
  }
  while (Token Op = matchTokens(isMulOperator, *this)) {
    Syntax *E2 = parsePre();
    E1 = onBinary(Op, E1, E2);
  }
  return E1;
}

Syntax *Parser::parseMulFold(Syntax *E1) {
  if (nextOperatorIsFold(isMulOperator)) {
    if (nextTokensMatchBinaryFoldOp()) {
      Token Op = consumeToken();
      Token Ellipsis = consumeToken();
      Op = consumeToken();
      return onBinaryFoldExpr(Op, Ellipsis, E1, parsePre());
    }
  }
  return E1;
}

static bool isFoldableOperator(const Token &T);

bool Parser::nextTokensMatchBinaryFoldOp(TokenKind TK) {
  return nextTokenIs(TK) && nthTokenIs(1, tok::Ellipsis) &&
         nthTokenIs(2, TK);
}

bool Parser::nextTokensMatchBinaryFoldOp() {
  Token NextToken = peekToken();
  return isFoldableOperator(NextToken) && nthTokenIs(1, tok::Ellipsis) &&
         nthTokenIs(2, NextToken.getKind());
}

static bool isUnaryFoldOperator(Parser &P) {
  auto Tok = P.peekToken();
  switch(Tok.getKind()) {
    case tok::Identifier:
      return (Tok.getSpelling() == "or" || Tok.getSpelling() == "and");
    case tok::Comma:
    case tok::AmpersandAmpersand:
    case tok::BarBar:
      return true;
    default:
      return false;
  }
}

Syntax *Parser::parseFoldExpr(FoldKind FK) {
  EnclosingParens parens(*this);
  if (!parens.expectOpen())
    return onError();
  FoldSubExprRAII FoldRAII(*this, true);
  GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, true);
  Syntax *E = nullptr;
  switch(FK) {
    case FK_Unary_Left:{
      // Need to consume tokens.
      Token EllipsisToken = matchToken(tok::Ellipsis);
      Token OperatorToken = matchTokens(isUnaryFoldOperator, *this);
      if (!OperatorToken) {
        Token InvalidOpTok = consumeToken();
        Diags.Report(InvalidOpTok.getLocation(), clang::diag::err_invalid_fold)
            << /*left*/2 << InvalidOpTok.getSpelling() << /*operator*/0;
        return onError();
      }
      // In the event we are a LHS fold expression, we skip all of the
      // other expression stuff and skip right to parsePrimary, because none of
      // the other operators are able to be used as part of the expression.
      Syntax *FoldOperand = parsePre();
      E = onUnaryFoldExpr(FD_Left, OperatorToken, EllipsisToken, FoldOperand);
    }
    break;
    case FK_Unary_Right:{
      // Parse primary expression.
      Syntax *FoldOperand = parsePre();

      // Need to consume tokens.
      Token OperatorToken = matchTokens(isUnaryFoldOperator, *this);
      Token EllipsisToken = matchToken(tok::Ellipsis);

      if (!OperatorToken) {
        Token InvalidOpTok = consumeToken();
        Diags.Report(InvalidOpTok.getLocation(), clang::diag::err_invalid_fold)
            << /*right*/1 << InvalidOpTok.getSpelling() << /*operator*/0;
        return onError();
      }

      // Parsing the suffix version of the operator.
      E = onUnaryFoldExpr(FD_Right, OperatorToken, EllipsisToken, FoldOperand);
    }
    break;
    case FK_Binary:{
      E = parseExpr();
    }
    break;
    default:
      llvm_unreachable("Invalid fold expression kind");
  }

  Syntax *Seq = onList(ArgArray, llvm::SmallVector<Syntax *, 1>({E}));
  if (!parens.expectClose())
    return onError();

  return Seq;
}

void Parser::parseComment(const char *Caller) {
  // if (Caller) {
  //   llvm::outs() << "Called parseComment from " << Caller << " ";
  // }
  // llvm::outs() << "current token display name = "
  //              << getDisplayName(getLookahead())<< " \n";
  while(true) {
    switch(getLookahead()) {
      case tok::Hash:
        parseLineComment();
        return;
      case tok::LessHash:
        llvm::outs() << "Parsing block comment\n";
        parseBlockComment();
        continue;
      default:
        return;
    }
  }
}

void Parser::parseLineComment() {
  llvm_unreachable("Line comment not implemented yet!");
}

void Parser::parseWhitespaceOrBlockComment() {
  while(true) {
    switch(getLookahead()) {
      case tok::Space:
        consumeToken();
        continue;
      case tok::LessHash:
        parseBlockComment();
        continue;
      default:
        return;
    }
  }
}

void Parser::parseBlockComment() {
  EnclosingBlockComment BlockComment(*this);
  if (!BlockComment.expectOpen())
    return;
  bool ContinueParsing = true;
  while(ContinueParsing) {
    switch(getLookahead()) {
      case tok::EndOfFile:{

        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unexpected end of file");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return;
      }
      case tok::HashGreater:
        ContinueParsing = false;
        break;
      case tok::LessHash:
        parseBlockComment();
        break;
      default:
        consumeToken();
        continue;
    }
  }
  BlockComment.expectClose();
}

Syntax *Parser::parseIf()
{
  Token if_tok = expectToken("if");

  llvm::SmallVector<Attribute *, 4> Attrs;
  if (!Preattributes.empty()) {
    std::copy(Preattributes.begin(), Preattributes.end(),
              std::back_inserter(Attrs));
    Preattributes.clear();
  }

  // FIXME: only allow attributes here for `if:` style syntax.
  while (nextTokenIs(tok::Less) || nextTokenIs(tok::LessBar))
    Attrs.push_back(parsePostAttr());

  Syntax *cond = nextTokenIs(tok::Colon) ? parseBlock() : parseParen();

  while (nextTokenIs(tok::Less) || nextTokenIs(tok::LessBar))
    Attrs.push_back(parsePostAttr());

  Syntax *then_block;
  if (matchToken("then")) {
    then_block = nextTokenIs(tok::Colon) ? parseBlock() : parseExpr();
  } else {
    matchToken("do");
    then_block = parseBlock();
  }

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

  // Abuse the preattribute vector to attach these to the operator'if' atom.
  std::copy_if(Attrs.begin(), Attrs.end(), std::back_inserter(Preattributes),
               [](Attribute *Attr) -> bool { return Attr; });
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

Syntax *Parser::parseNew() {
  Token Tok = expectToken(tok::NewKeyword);

  Syntax *PlacementArgs = nextTokenIs(tok::LeftParen) ? parseParen() : nullptr;
  // Syntax *Call = makeCall(Context, *this, Tok, PlacementArgs);
  Syntax *Call = new (Context) CallSyntax(makeOperator(Context, *this,
                                                       tok::NewKeyword,
                                                       Tok.getLocation(),
                                                       Tok.getSpelling()),
                                          PlacementArgs);
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return onError();

  Syntax *TypeExpr = parsePre();

  if (!Brackets.expectClose())
    return onError();

  if (nextTokenIs(tok::LeftParen)) {
    Syntax *CtorArgs = parseParen();
    TypeExpr = onCall(TypeExpr, CtorArgs);
    // This allows for universal initialization syntax.
  } else if (nextTokenIs(tok::LeftBrace)) {
    Syntax *e2 = parseBlock();
    TypeExpr = onMacro(TypeExpr, e2);
  }
  return onMacro(Call, TypeExpr);
}

Syntax *Parser::parseDelete() {
  Token DeleteToken = expectToken(tok::DeleteKeyword);
  bool ArrayDelete = false;
  if (nextTokenIs(tok::LeftBracket) && nthTokenIs(1, tok::RightBracket)) {
    ArrayDelete = true;
    consumeToken();
    consumeToken();
  }
  Syntax *Arg = parsePre();
  Syntax *Seq = onList(ArgArray, llvm::SmallVector<Syntax *, 1>({Arg}));
  Syntax *Name;
  if (ArrayDelete) {
    Name = makeOperator(Context, *this, tok::ArrayDelete,
                        DeleteToken.getLocation(), "delete[]");
  } else {
    Name = makeOperator(Context, *this, tok::DeleteKeyword,
                        DeleteToken.getLocation(), DeleteToken.getSpelling());
  }
  return onCall(Name, Seq);
}

Syntax *Parser::parseLambda() {
  Token Tok = expectToken(tok::LambdaKeyword);
  Syntax *Capture = nullptr, *Parms = nullptr, *Block = nullptr;

  llvm::SmallVector<Attribute *, 4> Attrs;
  while (nextTokenIs(tok::Less) || nextTokenIs(tok::LessBar))
    Attrs.push_back(parsePostAttr());

  Syntax *Templ = nullptr;
  if (nextTokenIs(tok::LeftBracket))
    Templ = parseElem(onAtom(Tok));

  if (nextTokenIs(tok::LeftBrace)) {
    BooleanRAII LCS(LambdaCaptureScope, true);
    Capture = parseBlock();
  } else if (nextTokenIs(tok::Colon)) {
    BooleanRAII LCS(LambdaCaptureScope, true);
    Capture = parseNestedArray();
  }

  // Note if we parsed a capture default and reset the parser's variable.
  bool Default = LambdaCaptureDefault;
  LambdaCaptureDefault = false;

  Parms = nextTokenIs(tok::RightParen) ?
    onList(ArgArray, llvm::SmallVector<Syntax *, 1>()) : parseParen();
  if (isa<ErrorSyntax>(Parms))
    return onError();

  if (nextTokenIs(tok::LeftBrace))
    Block = parseBlock();
  else if (nextTokenIs(tok::Colon))
    Block = parseNestedArray();
  else if (nextTokenIs(tok::EqualGreater)) {
    consumeToken();
    Block = parseExpr();
  }

  Syntax *Call = onCall(Templ ? Templ : onAtom(Tok), Parms);
  std::for_each(Attrs.begin(), Attrs.end(),
                [Call](Attribute *A) { Call->addAttribute(A); });
  return onLambdaMacro(Call, Block, Capture, Default);
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

static inline bool isEndOfThrow(TokenKind K){
  return K == tok::Separator || K == tok::Semicolon || K == tok::Dedent;
}

// True when the lookahead is ends a line, block, or file. Used for void
// return expressions.
static inline bool isEnd(TokenKind Lookahead) {
  switch (Lookahead) {
  case tok::Dedent:
  case tok::Separator:
  case tok::EndOfFile:
    return true;
  default:
    return false;
  }
}

Syntax *Parser::parseThrow() {
  Token ThrowKW = consumeToken();
  if (InAttribute) {
    return onAtom(ThrowKW);
  }
  Token NextTok = peekToken(0);
  if (!isEndOfThrow(NextTok.getKind())) {
    Syntax *E = parsePre();
    return onUnary(ThrowKW, E);
  }
  return makeCall(Context, *this, ThrowKW);
}

// pre:
//   macro
//   ? pre
//   ^ pre
//   + pre
//   - pre
//   * pre
//   . pre
//   [ expr ] pre
//   ( expr ) pre
//   const pre
//   return pre
//   returns pre
Syntax *Parser::parsePre() {
  // Nothing to parse for a fused operator.
  if (PreviousToken.isFused() && PreviousToken.FusionInfo.Base == tok::Operator)
    return FusionToks.clear(), nullptr;

  if (Token Op = matchTokenIf(is_unary_operator)) {
    Syntax *E = parsePre();
    return onUnary(Op, E);
  }

  if (!InAttribute && (nextTokenIs(tok::ConstKeyword)
                       || nextTokenIs(tok::RefKeyword)
                       || nextTokenIs(tok::RValueRefKeyword))) {
    Token Op = consumeToken();
    Syntax *E = parsePre();
    return onUnary(Op, E);
  }

  if (nextTokenIs("return") || nextTokenIs("returns")) {
    Token Op = consumeToken();
    Syntax *E = nullptr;
    if (!isEnd(getLookahead()))
      E = parseExpr();

    return onUnaryOrNull(Op, E);
  }
  // This might not be right, there is a chance that this could be a pre-attribute
  // or something like that, this would be to support constexpr if statements.
  if (nextTokenIs(tok::LeftBracket))
    return parseArrayPrefix();

  if (nextTokenIs(tok::LeftParen))
    if (scanNNSPrefix())
      return parseNNSPrefix();

  if (nextTokenIs(tok::Dot)) {
    Token Operator = consumeToken();
    Syntax *Operand = parseExpr();
    return onUnary(Operator, Operand);
  }

  return parseMacro();
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

  if (nextTokenIs(tok::NewKeyword))
    return parseNew();

  if (nextTokenIs(tok::LambdaKeyword))
    return parseLambda();

  if (nextTokenIs(tok::DeleteKeyword)) {
    TokenKind TokAfterDelete = getLookahead(1);
    // If the next character after delete isn't some kind of separator, eof,
    // or dedent, then this is the delete operator, and not the delete kw used
    // for deleting functions.
    if (!(isSeparator(TokAfterDelete)
        || TokAfterDelete == tok::EndOfFile
        || TokAfterDelete == tok::Dedent)) {
      return parseDelete();
    }
  }

  Syntax *e1 = parsePost();

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
static void trackEnclosureDepth(Token Enclosure,
                                Parser::AngleBracketTracker &Track) {
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

  using Tracker_t = Parser::AngleBracketTracker;
  Tracker_t::Loc EncLoc{Enclosure.getLocation(),
    {Track.EnclosureCounts[0], Track.EnclosureCounts[1],
        Track.EnclosureCounts[2], Track.EnclosureCounts[3]}};

  if (isNonAngleCloseEnclosure(Enclosure.getKind())) {
    if (Track.isOpen() && (Track.Angles.back() == EncLoc))
      Track.Angles.pop_back();

    if (Track.EnclosureCounts[K])
      --Track.EnclosureCounts[K];
    if (!Track.Enclosures.empty())
      Track.Enclosures.pop_back();
    return;
  }

  ++Track.EnclosureCounts[K];
  ++EncLoc.EnclosureCounts[K];
  Track.Enclosures.push_back(EncLoc);
}

/// Scan through tokens starting from a '<' and determine whether or not this
/// is a comparison or attribute.
bool Parser::scanAngles(Syntax *Base) {
  // auto PreviousStart = Lex.Scanner.Start;
  // std::size_t StartTokenDequeSize = Toks.size();
  std::size_t I = 0;

  // This came after a token that does not appear in base names.
  if (!PreviousToken.hasKind(tok::Identifier) &&
      !PreviousToken.hasKind(tok::Greater) &&
      !isNonAngleEnclosure(PreviousToken.getKind()))
    // There is no need to reset here because we didn't do anything.
    return false;

  AngleBracketTracker::Loc PotentialBaseLoc{Base->getLoc(),
                                            {Angles.EnclosureCounts[0],
                                             Angles.EnclosureCounts[1],
                                             Angles.EnclosureCounts[2],
                                             Angles.EnclosureCounts[3]}};
  // auto ResetLexer = [&]() {
  //   Toks.resize(StartTokenDequeSize);
  //   Lex.Scanner.Start = PreviousStart;
  // };
  while (true) {
    Token Current = peekToken(I++);

    // Quit at the end of the line, or end of file in the case of the
    // rare one-line program.
    if (Current.isNewline() || Current.isEndOfFile())
      return false;

    // Newlines might be recognized as separators rather than newline tokens.
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
      if (SemiLoc == PotentialBaseLoc)
        return false;

      // We reached a semicolon in some sort of nested list (or typo). We
      // already know we don't care about this token so just skip ahead.
      continue;
    }

    if (isNonAngleEnclosure(Current.getKind()))
      trackEnclosureDepth(Current, Angles);

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

// Returns true if T is an operator that appears in a fold expression.
bool isFoldableOperator(const Token &T) {
  if (T.isFused())
    return false;
  switch(T.getKind()) {
    case tok::Comma:
    case tok::Plus:
    case tok::Minus:
    case tok::Star:
    case tok::Slash:
    case tok::Percent:
    case tok::Equal:
    case tok::Less:
    case tok::Greater:
    case tok::PlusEqual:
    case tok::MinusEqual:
    case tok::StarEqual:
    case tok::SlashEqual:
    case tok::PercentEqual:
    case tok::EqualEqual:
    case tok::LessGreater:
    case tok::LessEqual:
    case tok::GreaterEqual:
    case tok::AmpersandAmpersand:
    case tok::BarBar:
      return true;
    default:
      return false;
  }
}

Parser::FoldKind Parser::scanForFoldExpr() {
  // We can abuse the nesting tracker of the AngleBracketTracker, since
  // attributes never appear in folds.
  AngleBracketTracker::Loc StartLoc{PreviousToken.getLocation(),
                                    {Folds.EnclosureCounts[0],
                                     Folds.EnclosureCounts[1],
                                     Folds.EnclosureCounts[2],
                                     Folds.EnclosureCounts[3]}};
  // Checking for simple Unary left fold
  // This is also the only case where the ellipsis can preceed the operator.
  if (peekToken(1).hasKind(tok::Ellipsis))
    return FK_Unary_Left;

  unsigned I = 1;
  std::size_t ScanningDepth = 0;
  while (true) {
    Token Current = peekToken(I++);
    if (Current.hasKind(tok::LeftParen)) {
      ++ScanningDepth;
      continue;
    }

    if (ScanningDepth) {
      // Simply ignore sub-expressions because they will be scanned later on.
      // all we are interested in is the current expression.
      if (Current.hasKind(tok::RightParen)) {
        --ScanningDepth;
        continue;
      }

      if (Current.isEndOfFile())
        return FK_None;
    } else {
      if (Current.hasKind(tok::LeftParen)) {
        ++ScanningDepth;
        continue;
      }

      // Quit at the end of the line, or end of file in the case of the
      // rare one-line program.
      if (Current.hasKind(tok::RightParen) || Current.isNewline()
          || Current.isEndOfFile())
        return FK_None;

      // Newlines might be recognized as separators rather than newline tokens.
      if (Current.hasKind(tok::Separator))
        if (*(Current.getSymbol().data()) == '\n')
          return FK_None;

      // If the programmer has used semicolons instead of newlines, we need
      // to be sure the semicolon is actually ending the line; in other words,
      // at a shallower depth than the starting `(`.
      if (Current.hasKind(tok::Semicolon)) {
        AngleBracketTracker::Loc SemiLoc{Current.getLocation(),
                                        {Folds.EnclosureCounts[0],
                                          Folds.EnclosureCounts[1],
                                          Folds.EnclosureCounts[2],
                                          Folds.EnclosureCounts[3]}};
        if (SemiLoc < StartLoc)
          return FK_None;

        // We reached a semicolon in some sort of nested list (or typo). We
        // already know we don't care about this token so just skip ahead.
        continue;
      }

      if (isNonAngleEnclosure(Current.getKind()))
        trackEnclosureDepth(Current, Folds);

      if (isFoldableOperator(Current)) {
        Token NextPeekTok = peekToken(I);
        if (NextPeekTok.hasKind(tok::Ellipsis)) {
          Token PossibleBinaryOp = peekToken(I + 1);
          // Minimally we are a unary right fold expression.
          if (PossibleBinaryOp.getKind() == Current.getKind())
            return FK_Binary;
          else
            return FK_Unary_Right;
        }
      }
    }
  }

  return FK_None;
}

/// postfix:
///   base
///   postfix ( array )
///   postfix [ array ]
///   postfix < expr >
///   postfix . identifier
///   postfix . ( expr ) identifier
///   postfix .* ( expr )
///   postfix of-macro
///   postfix suffix-operator
///
/// suffix-operator:
///   ?
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
    case tok::LessBar:
      e = parsePostAttr(e);
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

    case tok::DotCaret:
      e = parseDotCaret(e);
      break;

    case tok::Question:
      llvm_unreachable("suffix operators not implemented");
      consumeToken();
      break;

    case tok::OfKeyword:
      e = parseMacro();
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

  Syntax *Args = !nextTokenIs(tok::RightBracket) ? parseArray(ArgArray)
    : onList(ArgArray, llvm::SmallVector<Syntax *, 0>());

  if (!Brackets.expectClose())
    return onError();

  return onElem({Brackets.open, Brackets.close}, Map, Args);
}

Syntax *Parser::parseDot(Syntax *Obj) {
  Token Op = expectToken(tok::Dot);

  // FIXME: this is a qualified-id, which is not clearly defined.
  // Perhaps our disambiguating operator'()' is enough to meet the criteria?
  // Syntax *Sub = nextTokenIs(tok::LeftParen) ? parsePre() : parseId();
  Syntax *Sub = nullptr;
  if (nextTokenIs(tok::LeftParen) && scanNNSPrefix()) {
    Syntax *LHS = parseParen();
    Syntax *RHS = parseId();
    Sub = new (Context) CallSyntax(
      makeOperator(Context, *this, LHS->getLoc(), "()"),
      makeList(Context, {LHS, RHS}));
  } else if (nextTokenIs(tok::LeftParen)) {
    Sub = parsePre();
  } else {
    Sub = parseId();
  }
  return onBinary(Op, Obj, Sub);
}

Syntax *Parser::parseDotCaret(Syntax *Obj) {
  Token Op = expectToken(tok::DotCaret);

  Syntax *Sub = nextTokenIs(tok::LeftParen) ? parsePre() : parseId();
  return onBinary(Op, Obj, Sub);
}

Syntax *Parser::parseExpansionOperator(Syntax *Obj) {
  assert(Obj && "Invalid object");
  Token EllipsisTok = consumeToken();
  assert(EllipsisTok.getKind() == tok::Ellipsis && "Invalid token");
  return new (Context)
    CallSyntax(onAtom(EllipsisTok), makeList(Context, {Obj}));
}

// Parse a call to operator'[]' of the form [a]b
Syntax *Parser::parseArrayPrefix()
{
  EnclosingBrackets Brackets(*this);
  if (!Brackets.expectOpen())
    return onError();
  ArraySemantic S{};
  Syntax *Arg = parseList(S);
  if (!Brackets.expectClose())
    return onError();
  Syntax *Map = parsePre();
  return new (Context)
    CallSyntax(makeOperator(Context, *this, Arg->getLoc(), "[]"),
               makeList(Context, {Arg, Map}));
}

// Parse a call to operator'()' of the form (a)b
Syntax *Parser::parseNNSPrefix()
{
  EnclosingParens Parens(*this);
  if (!Parens.expectOpen())
    return onError();
  Syntax *Arg = parseExpr();
  if (!Parens.expectClose())
    return onError();
  Syntax *Map = parsePre();
  return new (Context)
    CallSyntax(makeOperator(Context, *this, Arg->getLoc(), "()"),
               makeList(Context, {Arg, Map}));
}

/// Use lookahead to determine if this could be an NNS prefix of the form (a)b,
/// essentially meaning there is an identifier following a balanced rparen.
bool Parser::scanNNSPrefix() {
  assert(getLookahead() == tok::LeftParen && "Invalid NNS scan");

  std::size_t I = 0;
  std::size_t ParenDepth = 1;
  Token Current = peekToken(I++);

  while (ParenDepth) {
    Current = peekToken(I++);

    if (Current.hasKind(tok::LeftParen))
      ++ParenDepth;

    if (Current.hasKind(tok::RightParen))
      if (ParenDepth)
        --ParenDepth;

    if (Current.isNewline() ||
        Current.hasKind(tok::Dedent) ||
        Current.hasKind(tok::Indent) ||
        Current.isEndOfFile())
      return false;
  }

  Current = peekToken(I++);
  if (Current.hasKind(tok::Identifier))
    return true;

  return false;
}

Attribute *Parser::parsePostAttr() {
  if (getLookahead() == tok::LessBar) {
    auto *DocAttr = parseDocAttr();
    return makeAttr(Context, DocAttr);
  }
  EnclosingAngles Angles(*this);
  if (!Angles.expectOpen())
    return nullptr;

  GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, false);
  AttributeScope AttrScope(InAttribute);

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
    return makeAttr(Context, onError());

  return makeAttr(Context, Arg);
}

Syntax *Parser::parsePostAttr(Syntax *Pre) {
  Attribute *Attr = parsePostAttr();
  if (!Attr)
    return onError();

  Pre->addAttribute(Attr);

  this->Angles.clear();
  return Pre;
}


Syntax *Parser::parseDocAttr() {
  EnclosingDocAttr DocAttrTracking(*this);
  llvm::SmallVector<Syntax *, 32> DocAttrParts;
  {
    // BooleanRAII CharLexing(Lex.Scanner.LexSingleCharacter, true);
    BooleanRAII InDocAttrTracking(InsideDocAttr, true);
    BooleanRAII RawLexingTracking(RawLexing, true);
    BooleanRAII StrLexing(Lex.Scanner.LexingString, true);
    // Not sure if I need this yet.
    // GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, false);
    if (!DocAttrTracking.expectOpen())
      return onError();
    bool ContinueDocAttr = true;
    // assert(LessBarTok.hasKind(tok::LessBar) && "Invalid Document attribute.");
    while(ContinueDocAttr) {
      switch(getLookahead()) {
        case tok::Less:
          DocAttrParts.emplace_back(parseMarkupElement());
          break;
        case tok::Greater:
          ContinueDocAttr = false;
          break;
        case tok::LeftBrace:
          DocAttrParts.emplace_back(parseStrInterpolationExprBraces());
          break;

        case tok::Ampersand:{
          DocAttrParts.emplace_back(parseStrInterpolationExprAmpersand());
        }
          break;

        case tok::EndOfFile:
          ContinueDocAttr = false;
          break;

        case tok::LessBar:
          DocAttrParts.emplace_back(parseDocAttr());
          break;

        default:
          DocAttrParts.emplace_back(onText(consumeToken()));
          break;
      }
    }
  }

  // There are only 2 one is as an error where we reached the end of a file.
  // the other is that we reached a >, otherwise this function is greedy and
  // consumes everything. Including things inside of itself.
  if (getLookahead() == tok::EndOfFile) {
    Toks.emplace_back(Lex());
    unsigned DiagID =
      Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "unexpected end of file");
    Diags.Report(Toks.front().getLocation(), DiagID);
    return onError();
  }

  if (!DocAttrTracking.expectClose())
    return onError();

  return onDocAttr(DocAttrParts);
}

Syntax *Parser::parseTagName() {
  Syntax *e = parsePrimary();
  while (true)
  {
    switch (getLookahead())
    {
    case tok::LeftBrace:{
        Syntax *OtherAttributes = parseBracedArray();
        e = onMarkup(Context.CxxAST, MS_InName, e, nullptr, OtherAttributes, nullptr);
      }
      break;
    case tok::LeftParen:
      e = parseCall(e);
      break;

    case tok::LeftBracket:
      e = parseElem(e);
      break;
    case tok::LessBar:
      e = parsePostAttr(e);
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

    case tok::DotCaret:
      e = parseDotCaret(e);
      break;

    case tok::Question:
    // case tok::Caret:
    // case tok::At:
      llvm_unreachable("suffix operators not implemented");
      consumeToken();
      break;

    case tok::OfKeyword:
      e = parseMacro();
      break;
    default:
      return e;
    }
  }

  // We should never reach this point.
  assert(false);
  return nullptr;
}

Syntax *Parser::parseMarkupElement() {
  EnclosingAngles Angles(*this);
  MarkupSyntax *MarkupTag = nullptr;
  MarkupStyle Style = MS_MarkdownStyle;
  Syntax *ContentBlock = nullptr;
  Syntax *EndingHTMLTag = nullptr;
  Syntax *Name = nullptr;
  {
    BooleanRAII RawLexingTracking(RawLexing, false);
    BooleanRAII StrLexing(Lex.Scanner.LexingString, false);
    if (!Angles.expectOpen())
      return onError();

    Name = parseTagName();

    switch(getLookahead()) {
      case tok::ColonGreater:
        Style = MS_MarkdownStyle;
        ContentBlock = parseIndentedContent();
        break;
      case tok::Colon:
        Style = MS_ContentInTag;
        ContentBlock = parseInTagContent();
        break;
      case tok::Comma:
        ContentBlock = parseMarkupWithComma();
        Style = MS_ContentInTag;
        break;
      case tok::Greater:
        {
          {
            Style = MS_HTMLStyle;
            BooleanRAII BodyParseTracking(RawLexing, true);
            BooleanRAII AllowEscapedChar(Lex.Scanner.LexingString, true);
            // Consuming the end of the initial tag.
            if (!Angles.expectClose())
              return onError();
            ContentBlock = parseHtmlBody();
          }
          EndingHTMLTag = parseEndingTag();
        }
        break;
      default:
        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unterminated tag");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return onError();
    }
  }

  // Making sure to restore the previous lexing context before continuing.
  if (Style != MS_HTMLStyle && Style != MS_MarkdownStyle
      && !Angles.expectClose())
    return onError();

  if ((MarkupTag = dyn_cast<MarkupSyntax>(Name))) {
    MarkupTag->setStyle(Style);
    MarkupTag->setBlock(ContentBlock);
    MarkupTag->setEndingTagName(EndingHTMLTag);
    return MarkupTag;
  }
  return onMarkup(Context.CxxAST, Style, Name, ContentBlock,
                  nullptr, EndingHTMLTag);
}


Syntax *Parser::parseHtmlBody() {
  llvm::SmallVector<Syntax *, 64> TagContents;
  while(true) {
    switch(getLookahead()) {
      case tok::Less:
        TagContents.emplace_back(parseMarkupElement());
        break;
      case tok::LessHash:
        parseComment();
        break;
      case tok::LessSlash:
        // Don't consume that means we have an ending tag at the same level
        // as we are.
        return onArray(ArraySemantic::BlockArray, TagContents);
      case tok::LeftBrace:
        TagContents.emplace_back(parseStrInterpolationExprBraces());
        break;
      case tok::Ampersand:
        TagContents.emplace_back(parseStrInterpolationExprAmpersand());
        break;
      case tok::EndOfFile:{
        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unexpected end of file");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return onError();
      }
      break;
      default:
        TagContents.emplace_back(onAtom(consumeToken()));
    }
  }
}

Syntax *Parser::parseEndingTag() {
  EnclosingHtmlEndingTag EndingTag(*this);
  if (!EndingTag.expectOpen())
    return onError();

  Syntax *Name = parseTagName();

  if (!EndingTag.expectClose())
    return onError();

  return Name;
}

Syntax *Parser::parseIndentedContent() {
  EnclosingTabs Tabs(*this);
  llvm::SmallVector<Syntax *, 64> TagContents;
  {

    BooleanRAII RawLexingTracking(RawLexing, false);
    BooleanRAII StrLexing(Lex.Scanner.LexingString, false);
    // Reading the :> token
    consumeToken();
    if (!Tabs.expectOpen())
      return onError();
    bool KeepConsuming = true;
    BooleanRAII AllowSpecialWhitespcae(AllowEmitNonIndentSpaces, true);
    while(KeepConsuming) {
      switch(getLookahead()) {
      case tok::Dedent:
        KeepConsuming = false;
        break;
      case tok::Less:
        TagContents.emplace_back(parseMarkupElement());
        break;
      case tok::LessHash:
        parseComment();
        break;
      case tok::LeftBrace:
        TagContents.emplace_back(parseStrInterpolationExprBraces());
        break;
      case tok::Ampersand:
        TagContents.emplace_back(parseStrInterpolationExprAmpersand());
        break;
      case tok::EndOfFile:{
        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unexpected end of file");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return onError();
      }
      break;
      default:
        TagContents.emplace_back(onAtom(consumeToken()));
      }
    }
  }

  if (!Tabs.expectClose())
    return onError();

  return onArray(ArraySemantic::BlockArray, TagContents);
}

Syntax *Parser::parseInTagContent() {
  llvm::SmallVector<Syntax *, 64> TagContents;
  while(true) {
    switch(getLookahead()) {
      case tok::Less:
        TagContents.emplace_back(parseMarkupElement());
        break;
      case tok::Greater:
        return onArray(ArraySemantic::BlockArray, TagContents);
      case tok::LessHash:
        parseComment();
        break;
      case tok::LessSlash:
        // Don't consume that means we have an ending tag at the same level
        // as we are.
        return onArray(ArraySemantic::BlockArray, TagContents);
      case tok::LeftBrace:
        TagContents.emplace_back(parseStrInterpolationExprBraces());
        break;
      case tok::Ampersand:
        TagContents.emplace_back(parseStrInterpolationExprAmpersand());
        break;
      case tok::EndOfFile:{
        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unexpected end of file");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return onError();
      }
      break;
      default:
        TagContents.emplace_back(onAtom(consumeToken()));
    }
  }
}

Syntax *Parser::parseMarkupWithComma() {
  llvm::SmallVector<Syntax *, 64> TagContents;
  while(true) {
    switch(getLookahead()) {
      case tok::Less:
        TagContents.emplace_back(parseMarkupElement());
        break;
      case tok::LessHash:
        parseComment();
        break;
      case tok::Greater:
        return onArray(ArraySemantic::BlockArray, TagContents);
      case tok::LeftBrace:
        TagContents.emplace_back(parseStrInterpolationExprBraces());
        break;
      case tok::Ampersand:
        TagContents.emplace_back(parseStrInterpolationExprAmpersand());
        break;
      case tok::EndOfFile:{
        unsigned DiagID =
          Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                "unexpected end of file");
        Diags.Report(Toks.front().getLocation(), DiagID);
        return onError();
      }
      break;
      default:
        TagContents.emplace_back(onAtom(consumeToken()));
    }
  }
}

Syntax *Parser::parseStrInterpolationExprBraces() {
  EnclosingBraces Braces(*this);
  Syntax *Body = nullptr;
  {
    if (!Braces.expectOpen())
      return onError();

    Body = parseArray(ArraySemantic::BlockArray);
  }
  if (!Braces.expectClose())
    return onError();

  return onStringInterpolation(Body);
}

Syntax *Parser::parseStrInterpolationExprAmpersand() {
  // Reading the ampersand.
  Syntax *E = nullptr;
  {
    BooleanRAII RawLexingTracking(RawLexing, false);
    BooleanRAII StrLexing(Lex.Scanner.LexingString, false);
    Token Ampersand = consumeToken();
    E = parseExpr();
    if (!E) {
      E = onError();
    }
  }

  if (getLookahead() == tok::EndOfFile) {
    unsigned DiagID =
      Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "unexpected end of file");
    Diags.Report(Toks.front().getLocation(), DiagID);
    E = onError();
  } else if (getLookahead() != tok::Semicolon) {
    Diags.Report(getInputLocation(), clang::diag::err_expected)
        << "';'";
  }
  return onStringInterpolation(E);
}


static inline bool isKeyword(TokenKind K) {
  return K >= tok::VoidKeyword && K <= tok::AnonymousKeyword;
}

Syntax *Parser::parsePrimary() {
  switch (getLookahead()) {
  case tok::Identifier:
    return parseId();

  case tok::ThrowKeyword:
    return parseThrow();

  case tok::SizeOfPack:
    return onAtom(consumeToken());
  case tok::RefKeyword:
  case tok::RValueRefKeyword:
  case tok::ConstKeyword:
    assert(InAttribute && "unary keyword should not be a primary expression "
           "outside of attributes");
    return onAtom(consumeToken());
  case tok::Less:
    return parseMarkupElement();
  case tok::LeftParen:{
    FoldKind ScanResult = scanForFoldExpr();
    if (ScanResult != FK_None)
      return parseFoldExpr(ScanResult);

    return parseParen();
  }

  case tok::Colon:
    // Report warning for missing _ before :
    Diags.Report(getInputLocation(),
                 clang::diag::warn_implict_anonymous_scope);
   LLVM_FALLTHROUGH;
  case tok::LeftBrace:
    return parsePrimaryBlock();
  case tok::AnonymousKeyword:{
    if (nthTokenIs(1, tok::LeftBrace) || nthTokenIs(1, tok::Colon)) {
      // Consume the anonymous token.
      consumeToken();
      return parsePrimaryBlock();
    }
    return onAtom(consumeToken());
  }

  case tok::TrueKeyword:
  case tok::FalseKeyword:
  case tok::NullKeyword:
  case tok::NullTKeyword:
  case tok::BinaryInteger:
  case tok::DecimalInteger:
  case tok::HexadecimalInteger:
  case tok::DecimalFloat:
  case tok::HexadecimalFloat:
  case tok::DecimalExponent:
  case tok::HexadecimalCharacter:
  case tok::UnicodeCharacter:
    return onLiteral(consumeToken());
  case tok::DoubleQuote:
    return parseString();
  case tok::SingleQuote:
    return parseCharacter();

  default:
    break;
  }

  if (isKeyword(getLookahead()))
    return onAtom(consumeToken());

  // Diagnose the error and consume the token so we don't see it again.
  Diags.Report(getInputLocation(), clang::diag::err_expected)
      << "primary-expression";
  consumeToken();
  return onError();
}

/// id:
///   identifier
Syntax *Parser::parseId() {
  Token Id = expectToken(tok::Identifier);
  if (!Id)
    return onError();

  if (Id.isFused()) {
    for (unsigned I = 0; I < Id.FusionInfo.NumTokens; ++I)
      FusionToks.push_back(*Id.FusionInfo.Tokens[I]);
    if (Id.FusionInfo.Base != tok::Operator)
      FusionToks.emplace_back(tok::EndOfFile, clang::SourceLocation(), Symbol());

    Syntax *Data = parsePre();
    if (Id.FusionInfo.Base != tok::Operator && !Data)
      return onError();
    else if (Id.FusionInfo.Base == tok::Operator && Data)
      return onError();

    if (Id.FusionInfo.Base != tok::Operator)
      expectToken(tok::EndOfFile);

    return onAtom(Id, Id.FusionInfo.Base, Data);
  }

  return onAtom(Id);
}

Syntax *Parser::parseParen() {
  FoldSubExprRAII FoldRAII(*this, false);
  EnclosingParens parens(*this);
  if (!parens.expectOpen())
    return onError();
  GreaterThanIsOperatorScope GTIOS(GreaterThanIsOperator, true);

  // TODO: If this is an Syntax::error, should we skip to the next paren or
  // to the the nearest comma? separator? What?
  Syntax *Seq = (!nextTokenIs(tok::RightParen)) ? parseArray(ArgArray) :
    onList(ArgArray, llvm::SmallVector<Syntax *, 1>());
  if (!parens.expectClose())
    return onError();

  return Seq;
}

// character:
//    ' length == 1 identifier '
Syntax *Parser::parseCharacter() {
  BooleanRAII RawLexingTracking(RawLexing, true);
  BooleanRAII CharLexing(Lex.Scanner.LexSingleCharacter, true);
  Token StartingSingleQuote = consumeToken();
  Token MatchedChar = consumeToken();
  Lex.Scanner.LexSingleCharacter = false;
  RawLexing = false;
  Token FinalQuote = expectToken(tok::SingleQuote);
  if (!FinalQuote)
    return onError();

  // Rebuilding character token, this is done to provide an identical
  // output to the original version compiler.
  std::string NewSymbol = "'" + MatchedChar.getSpelling() + "'";
  Symbol Sym = getSymbol(NewSymbol);
  Token Tok(tok::Character, StartingSingleQuote.getLocation(), Sym);
  return onLiteral(Tok);
}

Syntax *Parser::parseString() {
  BooleanRAII RawLexingTracking(RawLexing, true);
  BooleanRAII CharLexing(Lex.Scanner.LexingString, true);
  Token DQOpen = consumeToken();
  Token Current;
  std::string buffer = "\"";
  while(getLookahead() != tok::EndOfFile
        && getLookahead() != tok::DoubleQuote
        && getLookahead() != tok::Newline) {
    Current = consumeToken();
    buffer += Current.getSpelling();
  }
  buffer += "\"";
  RawLexing = false;
  Lex.Scanner.LexingString = false;
  if (getLookahead() == tok::EndOfFile
      || getLookahead() == tok::Newline) {
    unsigned DiagID =
      Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "unterminated string constant");
    Diags.Report(DQOpen.getLocation(), DiagID);
    return onError();
  }
  if (getLookahead() != tok::DoubleQuote) {
    unsigned DiagID =
      Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "unexpected end of string");
    Diags.Report(DQOpen.getLocation(), DiagID);
    return onError();
  }
  // Consuming the final double quotation
  Current = consumeToken();
  Symbol Sym = getSymbol(buffer);
  Token Tok(tok::String, DQOpen.getLocation(), Sym);
  return onLiteral(Tok);
}

// braced-array:
//    { array }
Syntax *Parser::parseBracedArray() {
  EnclosingBraces Braces(*this);
  if (!Braces.expectOpen())
    return onError();

  // If the block contains nothing but separators, create an empty array.
  for (std::size_t I = 0; getLookahead(I) == tok::Separator; ++I) {
    std::size_t J = I + 1;
    if (getLookahead(J) == tok::EndOfFile)
      break;
    if (getLookahead(J) == tok::RightBrace) {
      while (nextTokenIs(tok::Separator))
        consumeToken();
      goto RIGHT_BRACE;
    }
  }

  if (nextTokenIs(tok::RightBrace)) {
  RIGHT_BRACE:
    Braces.expectClose();
    llvm::SmallVector<Syntax *, 1> Vec;
    return onArray(BlockArray, Vec);
  }

  if (LambdaCaptureScope && nextTokenIs(tok::Equal)) {
    consumeToken();
    if (!nextTokenIs(tok::RightBrace))
      expectToken(tok::Comma);
    LambdaCaptureDefault = true;
  };

  // There could be any number of indents here.
  // They are not relevant.
  while (nextTokenIs(tok::Indent))
    consumeToken();

  Syntax *ret = nullptr;
  if (!nextTokenIs(tok::RightBrace)) {

    // FIXME: How do we recover from errors?
    ret = parseArray(BlockArray);
  } else {
    // Creating an empty block array
    llvm::SmallVector<Syntax *, 0> Vec;
    ret = onArray(BlockArray, Vec);
  }

  // Ignore dedents as well.
  while (nextTokenIs(tok::Dedent))
    consumeToken();

  if (!Braces.expectClose())
    return onError();

  return ret;
}


// nested-array:
//    indent array dedent
Syntax *Parser::parseNestedArray() {
  EnclosingTabs Tabs(*this);
  if (!Tabs.expectOpen())
    return onError();

  if (LambdaCaptureScope && nextTokenIs(tok::Equal)) {
    consumeToken();
    if (!nextTokenIs(tok::Dedent))
      expectToken(tok::Comma);
    LambdaCaptureDefault = true;
  }

  if (nextTokenIs(tok::Dedent)) {
    Tabs.expectClose();
    llvm::SmallVector<Syntax *, 1> Vec;
    return onArray(BlockArray, Vec);
  }

  Syntax *ret = parseArray(BlockArray);

  if (!Tabs.expectClose())
    return onError();

  return ret;
}

/// block:
///   braced-array  catch_opt
///   : nested-array  catch_opt
/// FIXME: allow catch blocks to be parsed here
Syntax *Parser::parseBlock() {
  if (nextTokenIs(tok::LeftBrace))
    return parseBracedArray();

  expectToken(tok::Colon);
  return parseNestedArray();
}

Syntax *Parser::parsePrimaryBlock() {
  Token AnonymousName(tok::AnonymousKeyword, getInputLocation(),
                      getSymbol("__BLOCK__"));
  Syntax *Name = onAtom(AnonymousName);
  Syntax *BlockExp = parseBlock();
  Syntax *BlockMacro = onMacro(Name, BlockExp);
  if (nextTokenIs(tok::CatchKeyword))
    return parseCatchSequence(BlockMacro);
  return BlockMacro;
}

/// catch:
/// catch ( list ) block
Syntax *Parser::parseCatch() {
  Token KW = expectToken(tok::CatchKeyword);

  EnclosingParens Parens(*this);
  if (!Parens.expectOpen())
    return onError();

  Syntax *Args = !nextTokenIs(tok::RightParen) ? parseList(ArgArray)
    : onList(ArgArray, llvm::SmallVector<Syntax *, 0>());

  if (!Parens.expectClose())
    return onError();
  Syntax *Block = parseBlock();
  auto *Ret = onCatch(KW, Args, Block);
  return Ret;
}


/// this stores the macro using the following sequence for the array body:
///   array
///     |-macro
///     |  |-call
///     |  |  |-Atom = __BLOCK__
///     |  |  `-List
///     |   `-Array # Body for the try catch block.
///     |-macro - catch 1
///     |-macro - catch 2
///     `-macro - catch N
///
Syntax *Parser::parseCatchSequence(Syntax *Contents) {
  const MacroSyntax *MS = cast<MacroSyntax>(Contents);
  const AtomSyntax *NameNode = cast<AtomSyntax>(MS->getCall());
  llvm::SmallVector<Syntax *, 16> TryCatchContents;
  TryCatchContents.emplace_back(Contents);
  Token TryBlockName(tok::TryBlock, NameNode->getLoc(), getSymbol("__TRY__"));
  Syntax *Name = onAtom(TryBlockName);
  // Syntax *TryCatchMacro = onCall(Name, onList(ArgArray,
  //                                     llvm::SmallVector<Syntax *, 0>()));
  // Reacing the sequence of catch statements and continue building up a
  // chain of them
  while(nextTokenIs(tok::CatchKeyword)) {
    Syntax *S = parseCatch();
    // I'm not sure how to recover from this.
    if (!S)
      return nullptr;
    TryCatchContents.emplace_back(S);
  }
  Syntax *BodyAndCatch = onArray(BlockArray, TryCatchContents);
  return onMacro(Name, BodyAndCatch);
}

// Semantic actions

// Returns the identifier 'operator\'<op>\''.
Syntax *makeOperator(const SyntaxContext &Ctx,
                            Parser &P,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op)
{
  // FIXME: Make this a fused operator?
  std::string Name = "operator'" + std::string(Op) + "'";
  Symbol Sym = getSymbol(Name);
  Token Tok(tok::Identifier, Loc, Sym);
  return P.onAtom(Tok);
}

Syntax *makeOperator(const SyntaxContext &Ctx,
                            Parser &P,
                            TokenKind TK,
                            clang::SourceLocation Loc,
                            llvm::StringRef Op)
{
  // FIXME: Make this a fused operator?
  std::string Name = "operator'" + std::string(Op) + "'";
  Symbol Sym = getSymbol(Name);
  Token Tok(TK, Loc, Sym);
  return P.onAtom(Tok);
}

static Syntax *makeOperator(const SyntaxContext &Ctx, Parser &P,
                            Token const& Tok) {
  return makeOperator(Ctx, P, Tok.getLocation(), Tok.getSpelling());
}

static Syntax *makeList(const SyntaxContext &Ctx,
                        std::initializer_list<Syntax *> List) {
  assert(std::all_of(List.begin(), List.end(), [](Syntax *s) { return s; }));
  return new (Ctx) ListSyntax(createArray(Ctx, List), List.size());
}

Syntax *makeCall(const SyntaxContext &Ctx, Parser &P, const Token& Tok) {
  return new (Ctx) CallSyntax(makeOperator(Ctx, P, Tok), makeList(Ctx, {}));
}

Syntax *makeCall(const SyntaxContext &Ctx, Parser &P,
                        const Token& Tok, Syntax *Args) {
  return new (Ctx) CallSyntax(makeOperator(Ctx, P, Tok), Args);
}

static Syntax *makeSimpleCall(const SyntaxContext &Ctx, Parser &P,
                              const Token& Tok, Syntax *Args) {
  return new (Ctx) CallSyntax(P.onAtom(Tok), Args);
}

static Attribute *makeAttr(const SyntaxContext &Ctx, Syntax *Arg) {
  return new (Ctx) Attribute(Arg);
}

Syntax *Parser::onAtom(const Token &Tok) {
  Syntax *Ret = new (Context) AtomSyntax(Tok);
  if (!InAttribute)
    attachPreattrs(Ret);
  return Ret;
}

Syntax *Parser::onAtom(const Token &Tok, const tok::FusionKind K,
                       Syntax *Data) {
  Syntax *Ret = new (Context) AtomSyntax(Tok, K, Data);
  if (!InAttribute)
    attachPreattrs(Ret);
  return Ret;
}

Syntax *Parser::onDocAttr(const llvm::SmallVectorImpl<Syntax*>& Vec) {
  return new (Context) DocAttrSyntax(createArray(Context, Vec), Vec.size());
}

Syntax *onMarkup(clang::ASTContext &Context, MarkupStyle S, Syntax *Name, Syntax *Content,
                         Syntax *OtherBlock, Syntax *EndingTag) {
  return new (Context) MarkupSyntax(S, Name, Content, OtherBlock, EndingTag);
}

Syntax *Parser::onStringInterpolation(Syntax *Expr) {
  return new (Context) StrInterpolationExprSyntax(Expr);
}

Syntax *Parser::onText(const Token &Tok) {
  return new (Context) TextSyntax(Tok);
}

Syntax *Parser::onUserDefinedLiteral(Syntax *Base, const Token &Lit) {
  // construct a `literal"Lit"` token.
  Token *T = new (Context) Token(tok::Identifier, Lit.getLocation(),
                                 getSymbol(Lit.getSpelling()));
  Token **Ts = new (Context) Token *[1];
  Ts[0] = T;
  Token Fuse(tok::Identifier, Lit.getLocation(), tok::Literal,
             Ts, 1, T->getSpelling());
  Syntax *Args = onList(ArgArray, llvm::SmallVector<Syntax *, 0>({Base}));
  return onCall(onAtom(Fuse, tok::Literal, onAtom(Lit)), Args);
}

static void parseSuffix(SyntaxContext &Context, clang::DiagnosticsEngine &Diags,
                        LiteralSyntax *Literal, llvm::StringRef Suffix) {
  const char *SuffixBegin = Suffix.begin();
  const char *SuffixEnd = Suffix.end();
  clang::SourceLocation Loc = Literal->getLoc();

  switch (std::tolower(*SuffixBegin)) {
  case 'u': {
    if (Literal->Suffix.IsUnsigned) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "signed" << "unsigned";
      return;
    }

    if (Literal->Suffix.IsUnsigned)
      return;

    ++SuffixBegin;

    int BitWidth = Context.CxxAST.getTargetInfo().getIntWidth();
    if (SuffixBegin != SuffixEnd) {
      std::size_t BitWidthSize = SuffixEnd - SuffixBegin;
      BitWidth = std::stoi({SuffixBegin, BitWidthSize});
    }

    if (BitWidth < 1 || BitWidth > 128) {
      Diags.Report(Loc, clang::diag::err_invalid_bitwidth_suffix) <<
        BitWidth << (BitWidth < 1);
      return;
    }

    Literal->Suffix.IsUnsigned = true;
    Literal->Suffix.BitWidth = BitWidth;
    return;
  }

  case 's': {
    if (Literal->Suffix.IsUnsigned) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "unsigned" << "signed";
      return;
    }

    if (Literal->Suffix.IsSigned)
      return;

    ++SuffixBegin;

    int BitWidth = Context.CxxAST.getTargetInfo().getIntWidth();
    if (SuffixBegin != SuffixEnd) {
      std::size_t BitWidthSize = SuffixEnd - SuffixBegin;
      BitWidth = std::stoi({SuffixBegin, BitWidthSize});
    }

    if (BitWidth < 1 || BitWidth > 128) {
      Diags.Report(Loc, clang::diag::err_invalid_bitwidth_suffix) <<
        BitWidth << (BitWidth < 1);
      return;
    }

    Literal->Suffix.IsSigned = true;
    Literal->Suffix.BitWidth = BitWidth;
    return;
  }

  case 'f':
    if (Literal->Suffix.IsDouble) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "double" << "float";
      return;
    }

    Literal->Suffix.IsFloat = true;
    return;

  case 'd':
    if (Literal->Suffix.IsFloat) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "float" << "double";
      return;
    }

    Literal->Suffix.IsDouble = true;
    return;

  case 'h':
    if (Literal->Suffix.IsFloat) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "float" << "half";
      return;
    }

    if (Literal->Suffix.IsDouble) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "double" << "half";
      return;
    }

    if (Literal->Suffix.IsQuarter) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "quarter" << "half";
      return;
    }

    Literal->Suffix.IsHalf = true;
    return;

  case 'q':
    if (Literal->Suffix.IsFloat) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "float" << "quarter";
      return;
    }

    if (Literal->Suffix.IsDouble) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "double" << "quarter";
      return;
    }

    if (Literal->Suffix.IsHalf) {
      Diags.Report(Loc, clang::diag::err_incompatible_suffix) <<
        "half" << "quarter";
      return;
    }

    Literal->Suffix.IsQuarter = true;
    return;

  default:
    Diags.Report(Loc, clang::diag::err_unknown_suffix) <<
      std::string(1, *SuffixBegin);
    return;
  }
}

Syntax *Parser::onLiteral(const Token& Tok) {
  LiteralSyntax *Literal = new (Context) LiteralSyntax(Tok);

  if (Tok.hasSuffix())
    for (auto Suffix : Tok.getSuffixes())
      parseSuffix(Context, Diags, Literal, Suffix);

  if (nextTokenIs(tok::Identifier) && peekToken().getSpelling()[0] == '_') {
    Token LitSuffix = consumeToken();
    return onUserDefinedLiteral(Literal, LitSuffix);
  }

  return Literal;
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
    CallSyntax(makeOperator(Context, *this, Tok), makeList(Context, {e1, e2}));
}

Syntax *Parser::onUnaryOrNull(Token const& Tok, Syntax *e1) {
  if (e1) {
    return new (Context)
      CallSyntax(makeOperator(Context, *this, Tok), makeList(Context, {e1}));
  }
  return new (Context)
    CallSyntax(makeOperator(Context, *this, Tok), makeList(Context, { }));
}

Syntax *Parser::onUnary(Token const& Tok, Syntax *e1) {
  return new (Context)
    CallSyntax(makeOperator(Context, *this, Tok), makeList(Context, {e1}));
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

Syntax *Parser::onMacro(Syntax *e1, Syntax *e2, Syntax *e3) {
  return new (Context) MacroSyntax(e1, e2, e3);
}

Syntax *Parser::onLambdaMacro(Syntax *e1, Syntax *e2, Syntax *e3, bool Default) {
  return new (Context) LambdaMacroSyntax(e1, e2, e3, Default);
}

Syntax *Parser::onCatch(const Token &Catch, Syntax *Args, Syntax *Block) {
  return new (Context) MacroSyntax(makeSimpleCall(Context, *this, Catch, Args),
                                   Block, nullptr);
}

Syntax *Parser::onElse(Token const& Tok, Syntax *e1) {
  return new (Context) MacroSyntax(makeCall(Context, *this, Tok), e1, nullptr);
}

Syntax *Parser::onIf(Token const& Tok, Syntax *e1, Syntax *e2, Syntax *e3) {
  return new (Context) MacroSyntax(makeCall(Context, *this, Tok, e1), e2, e3);
}

Syntax *Parser::onLoop(Token const& Tok, Syntax *e1, Syntax *e2) {
  return new (Context) MacroSyntax(makeCall(Context, *this, Tok, e1),
                                   e2, nullptr);
}

Syntax *Parser::onFile(const llvm::SmallVectorImpl<Syntax*> &Vec) {
  return new (Context) FileSyntax(createArray(Context, Vec), Vec.size());
}

Syntax *Parser::onUnaryFoldExpr(FoldDirection Dir, const Token &Operator,
                                const Token &Ellipsis, Syntax *E) {
  TokenKind TK;
  std::string OperatorText;
  std::string NormalizedSpelling;
  switch(Operator.getKind()) {
  case tok::AmpersandAmpersand:
    NormalizedSpelling = "&&";
    break;
  case tok::Comma:
    NormalizedSpelling = ",";
    break;
  case tok::Identifier:
    if (Operator.getSpelling() == "or") {
      NormalizedSpelling = "||";
    } else if (Operator.getSpelling() == "and") {
      NormalizedSpelling = "&&";
    } else {
      // FIXME: This may need a real error message.
      llvm_unreachable("Invalid unary fold operator?!");
    }
    break;
  case tok::BarBar:
    NormalizedSpelling = "||";
    break;
  default:
    llvm_unreachable("Invalid unary fold operator");
  }

  switch(Dir) {
  case FD_Left:
    TK = tok::UnaryLeftFold;
    OperatorText = "... " + NormalizedSpelling;
    break;
  case FD_Right:
    TK = tok::UnaryRightFold;
    OperatorText = NormalizedSpelling + " ...";
    break;
  }

  ParseFoldOp = true;
  Syntax *CallOp = makeOperator(Context, *this, TK, Operator.getLocation(),
                                OperatorText);
  return onCall(CallOp, onList(ArgArray, llvm::SmallVector<Syntax *, 1>({E})));
}

Syntax *Parser::onBinaryFoldExpr(const Token &Operator,
                                 const Token &Ellipsis,
                                 Syntax *LHS, Syntax *RHS) {
  std::string OperatorText;
  std::string NormalizedSpelling;
  switch(Operator.getKind()) {
    case tok::AmpersandAmpersand:
      NormalizedSpelling = "&&";
      break;
    case tok::Identifier:
      if (Operator.getSpelling() == "or") {
        NormalizedSpelling = "||";
      } else if (Operator.getSpelling() == "and") {
        NormalizedSpelling = "&&";
      } else {
        Diags.Report(getInputLocation(), clang::diag::err_invalid_fold)
          << /*binary*/0 << Operator.getSpelling() << /*operator*/0;
        NormalizedSpelling = Operator.getSpelling();
      }
      break;
    case tok::BarBar:
      NormalizedSpelling = "||";
      break;

    default:
      // Handling all other operator names.
      NormalizedSpelling = Operator.getSymbol().str();
  }
  OperatorText = NormalizedSpelling + " ... " + NormalizedSpelling;
  ParseFoldOp = true;
  Syntax *CallOp = makeOperator(Context, *this, tok::BinaryFold, Operator.getLocation(),
                                OperatorText);
  return onCall(CallOp, onList(ArgArray, llvm::SmallVector<Syntax *, 1>({LHS, RHS})));
}

Syntax *Parser::onInvalidRightUnaryFoldOperator() {
  // Always consume both operator and ellipsis
  Diags.Report(getInputLocation(), clang::diag::err_invalid_fold)
      << /*right*/1 << peekToken().getSpelling() << /*operator*/0;
  // Consume the operator and the ellipsis and return an error.
  consumeToken();
  consumeToken();
  return onError();


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
  assert ((OpToken.hasKind(tok::Greater) || OpToken.hasKind(tok::GreaterEqual)) &&
          "invalid angle bracket close.");
  AngleBracketTracker::Loc CloseLoc{OpToken.getLocation(),
                                    {Angles.EnclosureCounts[0],
                                     Angles.EnclosureCounts[1],
                                     Angles.EnclosureCounts[2],
                                     Angles.EnclosureCounts[3]}};
  if (Angles.Angles.back() == CloseLoc)
    Angles.Angles.pop_back();
}

void Parser::attachPreattrs(Syntax *S) {
  if (!Preattributes.size())
    return;

  for (auto *Attr : Preattributes)
    S->addAttribute(Attr);

  Preattributes.clear();
}


Token Parser::matchSeparator(Token const& Tok) {
  return Token(tok::Separator, Tok.getLocation(), Tok.getSymbol());
}

Token Parser::matchIndent(Token const& Tok) {
  return Token(tok::Indent, Tok.getLocation(), Tok.getSymbol());
}

Token Parser::matchDedent(Token const& Tok) {
  return Token(tok::Dedent, Tok.getLocation(), Tok.getSymbol());
}

static Symbol getSymbol(Token const& Tok) {
  return Tok.isInvalid() ? Symbol() : Tok.getSymbol();
}

/// True if `A` and `B` have the same spellings.
static bool equalSpelling(Token const& A, Token const& B) {
  return getSymbol(A) == getSymbol(B);
}

/// True if `sym` starts with `pre` (i.e., is lexicographically greater).
static bool startsWith(Symbol Sym, Symbol Pre) {
  return Sym.str().compare(Pre.str()) > 0;
}

/// True if the lexeme of `tok` has the lexeme of `pre` has a prefix.
static bool startsWith(Token const& Tok, Token const& Pre) {
  return startsWith(getSymbol(Tok), getSymbol(Pre));
}

Token Parser::combineSpace(Token const& Nl, Token const& NewIndent) {
  // Emit queued dedents.
  if (!Dedents.empty())
    return popDedent();

  Token PrevIndent = currentIndentation();

  // If the indentations are the same, this is a separator. Note
  // that we replace the current prefix for diagnostics purposes.
  if (equalSpelling(NewIndent, PrevIndent)) {
    if (!Indents.empty())
      Indents.back() = NewIndent;
    return matchSeparator(Nl);
  }

  // If the new indentation starts with the previous (i.e., new is longer),
  // then indent.
  if (startsWith(NewIndent, PrevIndent)) {
    pushIndentation(NewIndent);
    return matchIndent(NewIndent);
  }

  // The previous indentation starts with the new (i.e., previous is longer),
  // then dendent. Note that this can entail multiple dedents: one for each
  // level that does not have the same spelling.
  if (startsWith(PrevIndent, NewIndent)) {
    do {
      popIndentation();
      PrevIndent = currentIndentation();
      pushDedent(matchDedent(NewIndent));
    } while (!Indents.empty() && !equalSpelling(NewIndent, PrevIndent));

    // Update the location of the last indent for diagnostic purposes.
    if (!Indents.empty())
      Indents.back() = NewIndent;

    return popDedent();
  }

  // FIXME: Note the previous indentation depth. This is the reason we
  // overwrite the last entry in the stack: to get the nearest indentation
  // of the expected length.
  Diags.Report(NewIndent.getLocation(),
               clang::diag::err_invalid_indentation);

  // Return a line separator just in case.
  return matchSeparator(Nl);
}

} // namespace gold
