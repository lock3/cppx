#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Green/GreenParser.h"
#include "clang/Green/Syntax.h"
#include "clang/Green/SyntaxContext.h"

#include <iostream>

namespace green {

namespace {

namespace enc {
enum Kind
{
  Parens,
  Braces,
  Brackets,
  Tabs,
};
} // namespace enc

using EnclosureKind = enc::Kind;

TokenKind OpenTokens[]
{
  tok::LeftParen,
  tok::LeftBrace,
  tok::LeftBracket,
  tok::Indent,
};

TokenKind CloseTokens[]
{
  tok::RightParen,
  tok::RightBrace,
  tok::RightBracket,
  tok::Dedent,
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
        return true;
      }
    }

    close = p.expectToken(CloseTokens[K]);
    if (!close)
    {
      // FIXME: Emit a diagnostic.
      // note(open.loc, "matching '{}'' here", spelling(open.kind()));
    }
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

} // namespace

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
    parseArray(Vec);
    return onFile(Vec);
  }
  return nullptr;
}

static void Append(llvm::SmallVectorImpl<Syntax *>& vec, Syntax *s)
{
  if (s && s != Syntax::error)
    vec.push_back(s);
}

static Syntax **makeArray(const SyntaxContext &Ctx,
                          const llvm::SmallVectorImpl<Syntax *> &Vec) {
  Syntax **Array = new (Ctx) Syntax *[Vec.size()];
  std::copy(Vec.begin(), Vec.end(), Array);
  return Array;
}

static Syntax **makeArray(const SyntaxContext &Ctx,
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
Syntax *Parser::parseArray() {
  llvm::SmallVector<Syntax *, 8> Vec;
  parseArray(Vec);
  return onArray(Vec);
}

// Parse the array and populate the vector.
void Parser::parseArray(llvm::SmallVectorImpl<Syntax *> &Vec) {
  Syntax *List = parseList();
  Append(Vec, List);

  while (true)
  {
    // Obviously stop at the end of the file.
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

    List = parseList();
    Append(Vec, List);
  }
}

// list:
//    ;
//    expr
//    list , expr
//
// Note that a ';' is interpreted as an empty list, so we return nullptr,
// in that case. The nullptr will be ignored by Append, so empty lists are
// not represented in the AST.
//
// TODO: Represent empty lists in the AST?
Syntax *Parser::parseList()
{
  if (matchToken(tok::Semicolon))
    return nullptr;

  llvm::SmallVector<Syntax *, 4> Vec;
  parseList(Vec);

  return onList(Vec);
}

// Parse the list and populate the vector.
void Parser::parseList(llvm::SmallVectorImpl<Syntax *> &Vec)
{
  Append(Vec, parseExpr());
  while (matchToken(tok::Comma))
    Append(Vec, parseExpr());
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
  if (nextTokenIs(tok::LeftBracket)) {
    // FIXME: Match the 'pre-attr expr' production.
    llvm_unreachable("attributed expressions not supported");
  }

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
      return Syntax::error;

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
};

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
      Diags.Report(getInputLocation(), clang::diag::err_expected) << "expected '{{' or indent";
      return Syntax::error;
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
};

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
  case tok::Greater:
  case tok::LessEqual:
  case tok::GreaterEqual:
    return true;
  }
};

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
      || P.nextTokenIs(tok::DotDot)
      || P.nextTokenIs(tok::MinusGreater);
};

// to:
//    add
//    to to-operator add
//
// to-operator:
//    :
//    ..
//    ->
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
  return P.nextTokenIs(tok::Plus) || P.nextTokenIs(tok::Minus);
};

/// add:
///   mul
///   add add-operator mul
///
/// add-operator:
///   +
///   -
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
};

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
  Syntax *cond = parseParen();

  // FIXME: Allow an optional 'then' keyword?
  Syntax *then_block = parseBlock();

  Syntax *else_macro;
  if (Token else_tok = matchToken("else"))
  {
    Syntax *else_block = parseBlock();
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
  Token tok = expectToken("while");
  Syntax *cond = parseParen();

  // FIXME: Allow an optional 'do' keyword?
  Syntax *block = parseBlock();

  return onLoop(tok, cond, block);
}

Syntax *Parser::parseFor()
{
  Token tok = expectToken("for");
  Syntax *cond = parseParen();

  // FIXME: Allow an optional 'do' keyword?
  Syntax *block = parseBlock();

  return onLoop(tok, cond, block);
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
  if (Token op = matchTokenIf(is_unary_operator))
  {
    Syntax *e = parsePre();
    return onUnary(op, e);
  }

  return parsePost();
}

/// postfix:
///   base
///   postfix ( array )
///   postfix [ array ]
///   postfix < list >
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

    case tok::Less:
      llvm_unreachable("attributes not implemented");
      break;

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

Syntax *Parser::parseCall(Syntax *fn)
{
  EnclosingParens parens(*this);
  if (!parens.expectOpen())
    return Syntax::error;

  // Begin parsing function parameters.
  ParsingParams = true;

  // Don't parse an array if the parens are empty.
  //
  // FIXME: Don't allow newlines in the parameter array?
  //
  // TODO: If this is an Syntax::error, should we skip to the next paren or
  // to the the nearest comma? separator? What?
  Syntax *Args = nullptr;
  if (getLookahead() != tok::RightParen)
    Args = parseArray();
  else
    Args = onArray(llvm::SmallVector<Syntax *, 0>());

  // Finish parsing function parameters.
  ParsingParams = false;

  if (!parens.expectClose())
    return Syntax::error;

  return onCall({parens.open, parens.close}, fn, Args);
}

Syntax *Parser::parseElem(Syntax *map)
{
  EnclosingBrackets brackets(*this);
  if (!brackets.expectOpen())
    return Syntax::error;

  Syntax *args = parseArray();

  if (!brackets.expectClose())
    return Syntax::error;

  return onElem({brackets.open, brackets.close}, map, args);
}

Syntax *Parser::parseDot(Syntax *obj)
{
  Token op = expectToken(tok::Dot);

  // FIXME: This is somehow a qualified-id, except that I don't know
  // what that means.
  Syntax *sub = parseId();

  return onBinary(op, obj, sub);
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
    return onAtom(consumeToken());

  case tok::LeftParen:
    return parseParen();

  default:
    break;
  }

  // Diagnose the error, but consume the token so we don't see it again.
  Diags.Report(getInputLocation(), clang::diag::err_expected) << "primary-expression";
  consumeToken();
  return Syntax::error;
}

/// id:
///   identifier
Syntax *Parser::parseId() {
  Token id = expectToken(tok::Identifier);
  if (!id)
    return Syntax::error;
  return onAtom(id);
}

Syntax *Parser::parseParen() {
  EnclosingParens parens(*this);
  if (!parens.expectOpen())
    return Syntax::error;

  // TODO: If this is an Syntax::error, should we skip to the next paren or
  // to the the nearest comma? separator? What?
  Syntax *seq = parseArray();

  if (!parens.expectClose())
    return Syntax::error;

  return seq;
}

// braced-array:
//    { array }
Syntax *Parser::parseBracedArray() {
  EnclosingBraces braces(*this);
  if (!braces.expectOpen())
    return Syntax::error;

  // FIXME: How do we recover from errors?
  Syntax *ret = parseArray();

  if (!braces.expectClose())
    return Syntax::error;

  return ret;
}

// nested-array:
//    indent array dedent
Syntax *Parser::parseNestedArray() {
  EnclosingTabs Tabs(*this);
  if (!Tabs.expectOpen())
    return Syntax::error;

  Syntax *ret = parseArray();

  if (!Tabs.expectClose())
    return Syntax::error;

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
                            clang::SourceLocation Loc, llvm::StringRef Op)
{
  // FIXME: Make this a fused operator?
  std::string Name = "operator'" + std::string(Op) + "'";
  Symbol Sym = getSymbol(Name);
  Token Tok(tok::Identifier, Loc, Sym);
  return new (Ctx) AtomSyntax(Tok, clang::SourceLocation());
}

static Syntax *makeOperator(const SyntaxContext &Ctx, Token const& Tok) {
  return makeOperator(Ctx, Tok.getLocation(), Tok.getSpelling());
}

static Syntax *makeList(const SyntaxContext &Ctx,
                        std::initializer_list<Syntax *> List) {
  assert(std::all_of(List.begin(), List.end(), [](Syntax *s) { return s; }));
  return new (Ctx)
    ListSyntax(makeArray(Ctx, List), List.size(), clang::SourceLocation());
}

static Syntax *makeCall(const SyntaxContext &Ctx, const Token& Tok) {
  return new (Ctx)
    CallSyntax(makeOperator(Ctx, Tok), makeList(Ctx, {}),
               clang::SourceLocation());
}

static Syntax *makeCall(const SyntaxContext &Ctx, const Token& Tok,
                        Syntax *Args) {
  return new (Ctx)
    CallSyntax(makeOperator(Ctx, Tok), Args, clang::SourceLocation());
}

Syntax *Parser::onAtom(const Token& Tok) {
  return new (Context) AtomSyntax(Tok, clang::SourceLocation(), ParsingParams);
}

Syntax *Parser::onArray(llvm::SmallVectorImpl<Syntax *> const& Vec) {
  return new (Context)
    ArraySyntax(makeArray(Context, Vec), Vec.size(), clang::SourceLocation());
}

Syntax *Parser::onList(llvm::SmallVectorImpl<Syntax *> const& Vec) {
  // Flatten empty and singleton lists.
  if (Vec.empty())
    return nullptr;
  if (Vec.size() == 1)
    return Vec.front();

  return new (Context)
    ListSyntax(makeArray(Context, Vec), Vec.size(), clang::SourceLocation());
}

Syntax *Parser::onBinary(Token const& Tok, Syntax *e1, Syntax *e2) {
  return new (Context)
    CallSyntax(makeOperator(Context, Tok), makeList(Context, {e1, e2}),
               clang::SourceLocation());
}

Syntax *Parser::onUnary(Token const& Tok, Syntax *e1) {
  return new (Context)
    CallSyntax(makeOperator(Context, Tok), makeList(Context, {e1}),
               clang::SourceLocation());
}

Syntax *Parser::onCall(TokenPair const& Toks, Syntax *e1, Syntax *e2) {
  // FIXME: Toks is unused.
  return new (Context)
    CallSyntax(e1, e2, clang::SourceLocation(), ParsingParams);
}

Syntax *Parser::onElem(TokenPair const& tok, Syntax *e1, Syntax *e2) {
  return new (Context)
    ElemSyntax(e1, e2, clang::SourceLocation());
}

Syntax *Parser::onMacro(Syntax *e1, Syntax *e2) {
  return new (Context)
    MacroSyntax(e1, e2, nullptr, clang::SourceLocation());
}

Syntax *Parser::onElse(Token const& Tok, Syntax *e1) {
  return new (Context)
    MacroSyntax(makeCall(Context, Tok), e1, nullptr, clang::SourceLocation());
}

Syntax *Parser::onIf(Token const& Tok, Syntax *e1, Syntax *e2, Syntax *e3) {
  return new (Context)
    MacroSyntax(makeCall(Context, Tok, e1), e2, e3, clang::SourceLocation());
}

Syntax *Parser::onLoop(Token const& Tok, Syntax *e1, Syntax *e2) {
  return new (Context)
    MacroSyntax(makeCall(Context, Tok, e1), e2,
                nullptr, clang::SourceLocation());
}

Syntax *Parser::onFile(const llvm::SmallVectorImpl<Syntax*> &Vec) {
  return new (Context)
    FileSyntax(makeArray(Context, Vec), Vec.size(), clang::SourceLocation());
}

} // namespace green
