//===- BlueSyntax.h - Blue Concrete Syntax Tree ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Defines the concrete syntax tree for the Blue language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUESYNTAX_H
#define CLANG_BLUE_BLUESYNTAX_H

#include "clang/Blue/BlueTokens.h"

#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>
#include <array>

namespace blue {
  // Bring isa/cast/dyn_cast into scope.
  using clang::isa;
  using clang::cast;
  using clang::dyn_cast;
  using clang::cast_or_null;
  using clang::dyn_cast_or_null;

/// Represents the set of all syntactic constructs in the Blue language.
/// This is the base class of derived syntax nodes.
class Syntax {
public:
  // TODO: FIXME: This may have to handled using table gen because of how the
  // derived structures are used, and the fact that it allows for generation of
  // start and ending classes.
  enum KindType {
#define def_syntax(K, B) \
  K,
#include "clang/Blue/BlueSyntax.def"
  };

// Defining ranges for abstract types.
#define def_non_leaf_start(T, First)\
  static constexpr std::size_t T##Start = First;

#define def_non_leaf_end(T, Last)\
  static constexpr std::size_t T##End = Last + 1;
#include "clang/Blue/BlueSyntax.def"

protected:
  Syntax(KindType K) noexcept
    : Kind(K) { }

public:
  KindType getKind() const {
    return Kind;
  }

  /// The name of the syntax's node kind. Used for debugging.
  const char *getKindName() const;

  // Source location

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const;

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const;

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const;

  /// The location spanning the entire term.
  clang::SourceRange getRange() const {
    return {getBeginLocation(), getEndLocation()};
  }

  // Children
  using child_iterator = llvm::ArrayRef<Syntax *>::iterator;
  using const_child_iterator = llvm::ArrayRef<Syntax *>::const_iterator;
  using reverse_child_iterator = llvm::ArrayRef<Syntax *>::reverse_iterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;
  using reverse_child_range = llvm::iterator_range<reverse_child_iterator>;

  child_range children();
  const_child_range children() const;
  reverse_child_range reverseChildren() const;
  /// Emit debugging information about this node.
  void dump() const;
private:
  KindType Kind;
};

using Syntax_span = Syntax *;
using ConstSyntax_span = const Syntax *;
using Syntax_seq = llvm::SmallVector<Syntax *, 4>;

// Term structure
//
// The following classes provided basic structure for specific terms. The
// intent is to simplify some algorithms that can be largely defined in
// terms of the tree's structure, and not its interpretation.

/// A node with a fixed number of operands.
/// @NOTE: You can't dyn_cast to this because of the template, instead use one
/// of it's K indicating derived classes Unary, Binary, Ternary, and Quaternary.
template<std::size_t N>
struct KarySyntax : Syntax
{
  template<typename... Ts>
  KarySyntax(KindType k, Ts*... ts)
    : Syntax(k), m_terms{ts...}
  {
  }

  /// Returns the nth operand.
  Syntax* operand(std::size_t n) const
  {
    return m_terms[n];
  }

  /// Returns the operands.
  const Syntax *operands() const
  {
    return m_terms;
  }

  /// Returns the operands.
  Syntax *operands()
  {
    return m_terms;
  }

  child_range children() {
    return child_range(m_terms, m_terms + N);
  }

  const child_range children() const {
    return const_child_range(m_terms, m_terms + N);
  }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    return m_terms[0]->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return m_terms[0]->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return m_terms[N-1]->getLocation();
  }

  Syntax* m_terms[N];
};

/// Specialization for unary nodes.
template<>
struct KarySyntax<1> : Syntax
{
  KarySyntax(KindType k, Syntax* s)
    : Syntax(k), m_term(s)
  { }

  /// Returns the operand.
  Syntax* operand() const
  {
    return m_term;
  }

  /// Returns the operands.
  const Syntax *operands() const
  {
    return m_term;
  }

  /// Returns the operands.
  Syntax *operands()
  {
    return m_term;
  }

  child_range children() {
    return child_range(&m_term, &m_term + 1);
  }

  const child_range children() const {
    return const_child_range(&m_term, &m_term + 1);
  }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    return m_term->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return m_term->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return m_term->getLocation();
  }

  Syntax *m_term;
};

/// A unary expression has a single operand.
struct UnarySyntax : KarySyntax<1>
{
  UnarySyntax(KindType k, Syntax* s)
    : KarySyntax<1>(k, s)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() >= UnaryStart && S->getKind() < UnaryEnd;
  }
};

/// A binary expression with two operands.
struct BinarySyntax : KarySyntax<2>
{
  BinarySyntax(KindType k, Syntax* s0, Syntax* s1)
    : KarySyntax<2>(k, s0, s1)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() >= BinaryStart && S->getKind() < BinaryEnd;
  }
};

/// A ternary expression.
struct TernarySyntax : KarySyntax<3>
{
  TernarySyntax(KindType k, Syntax* s0, Syntax* s1, Syntax* s2)
    : KarySyntax<3>(k, s0, s1, s2)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() >= TernaryStart && S->getKind() < TernaryEnd;
  }
};

/// A quaternary expression.
struct QuaternarySyntax : KarySyntax<4>
{
  QuaternarySyntax(KindType k, Syntax* s0, Syntax* s1, Syntax* s2, Syntax* s3)
    : KarySyntax<4>(k, s0, s1, s2, s3)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() >= QuaternaryStart && S->getKind() < QuaternaryEnd;
  }
};

/// A term with an unspecified number of operands.
struct MultiarySyntax : Syntax
{
  MultiarySyntax(KindType k, Syntax **s, unsigned size)
    : Syntax(k), m_terms(s), num_terms(size)
  { }

  /// Returns the nth operand.
  Syntax* operand(std::size_t n) const
  {
    return m_terms[n];
  }

  /// Returns the operands.
  Syntax **operands()
  {
    return m_terms;
  }

  child_range children() {
    return child_range(m_terms, m_terms + num_terms);
  }

  const child_range children() const {
    return const_child_range(m_terms, m_terms + num_terms);
  }

  unsigned getNumChildren() const { return num_terms; }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    if (num_terms == 0) {
      return clang::SourceLocation();
    }
    return m_terms[0]->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    if (num_terms == 0) {
      return clang::SourceLocation();
    }
    return m_terms[0]->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    if (num_terms == 0)
      return clang::SourceLocation();

    // return the last term that exists
    for (unsigned i = num_terms - 1; num_terms > 0; --i)
      if (m_terms[i])
        return m_terms[i]->getLocation();
    // This removes a warning.
    return clang::SourceLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= MultiaryStart && S->getKind() < MultiaryEnd;
  }

  Syntax **m_terms;
  unsigned num_terms;
};

// Specific trees

/// Any tree represented by a single token.
struct AtomSyntax : Syntax
{
  AtomSyntax(KindType k, Token tok)
    : Syntax(k), m_tok(tok)
  { }

  /// Returns the token of the atom.
  Token token() const
  {
    return m_tok;
  }

  /// Returns the spelling of the atom.
  llvm::StringRef spelling() const
  {
    return m_tok.getSpelling();
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= AtomStart && S->getKind() < AtomEnd;
  }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    return m_tok.getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return m_tok.getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return m_tok.getLocation();
  }

  Token m_tok;
};

// Represents a suffix on a literal value, such as `64u64`
struct LiteralSuffix
{
  bool IsSigned = false;
  bool IsUnsigned = false;
  std::size_t BitWidth = 0;
  bool IsFloat = false;
  bool IsDouble = false;
  bool IsHalf = false;
  bool IsQuarter = false;
};

/// Represents literal values.
struct LiteralSyntax : AtomSyntax
{
  static constexpr KindType this_kind = Literal;

  LiteralSyntax(Token tok)
    : AtomSyntax(this_kind, tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }

  LiteralSuffix Suffix;
};

/// Represents user-defined names.
struct IdentifierSyntax : AtomSyntax
{
  static constexpr KindType this_kind = Identifier;

  IdentifierSyntax(Token tok)
    : AtomSyntax(this_kind, tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A sequence of delimited terms.
///
/// TODO: This doesn't store the delimiters. I'm not sure if that's
/// actually important.
struct ListSyntax : MultiarySyntax
{
  static constexpr KindType this_kind = List;

  ListSyntax(Syntax **s, unsigned size)
    : MultiarySyntax(this_kind, s, size)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A sequence of terms.
struct SequenceSyntax : MultiarySyntax
{
  static constexpr KindType this_kind = Sequence;

  SequenceSyntax(Syntax **s, unsigned size)
    : MultiarySyntax(this_kind, s, size)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A term enclosed by a pair of tokens.
struct EnclosureSyntax : UnarySyntax
{
  static constexpr KindType this_kind = Enclosure;

  EnclosureSyntax(Token o, Token c, Syntax* t)
    : UnarySyntax(this_kind, t), m_open(o), m_close(c)
  { }

  /// Returns the opening token.
  Token open() const
  {
    return m_open;
  }

  /// Returns the closing token.
  Token close() const
  {
    return m_close;
  }

  /// Returns the inner term.
  Syntax* term() const
  {
    return m_term;
  }

  bool isParenEnclosure() const {
    return m_open.hasKind(tok::LeftParen);
  }

  bool isBracketEnclosure() const {
    return m_open.hasKind(tok::LeftBracket);
  }

  bool isBraceEnclosure() const {
    return m_open.hasKind(tok::LeftBrace);
  }

  Token m_open;
  Token m_close;

  clang::SourceLocation getLocation() const {
    return m_open.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return m_close.getLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A pair of terms.
struct PairSyntax : BinarySyntax
{
  static constexpr KindType this_kind = Pair;

  PairSyntax(Syntax* s0, Syntax* s1)
    : BinarySyntax(this_kind, s0, s1)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A triple of terms.
struct TripleSyntax : TernarySyntax
{
  static constexpr KindType this_kind = Triple;

  TripleSyntax(Syntax* s0, Syntax* s1, Syntax* s2)
    : TernarySyntax(this_kind, s0, s1, s2)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// A unary prefix operator expression.
struct PrefixSyntax : UnarySyntax
{
  static constexpr KindType this_kind = Prefix;

  PrefixSyntax(Token tok, Syntax* s)
    : UnarySyntax(this_kind, s), m_op(tok)
  { }

  /// Returns the prefix operation (operator).
  Token operation() const
  {
    return m_op;
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }

  Token m_op;
};

/// Compound type constructors for arrays, templates, and functions.
struct ConstructorSyntax : BinarySyntax
{
  ConstructorSyntax(KindType k, Syntax* s, Syntax* r)
    : BinarySyntax(k, s, r)
  { }

  /// Returns the "constructor" of a constructor. Either an array bound
  /// or a parameter list.
  Syntax *constructor() const
  {
    return operand(0);
  }

  /// Returns the result type of the constructor.
  Syntax *result() const
  {
    return operand(1);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= ConstructorStart && S->getKind() < ConstructorEnd;
  }
};

/// Array type constructor.
struct ArraySyntax : ConstructorSyntax
{
  static constexpr KindType this_kind = Array;

  ArraySyntax(Syntax* s, Syntax* t)
    : ConstructorSyntax(this_kind, s, t)
  { }

  /// Returns the array bound.
  Syntax* bounds() const
  {
    return constructor();
  }

  using ConstructorSyntax::ConstructorSyntax;
  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// Mapping type constructors (templates and functions).
struct MappingSyntax : ConstructorSyntax
{
  MappingSyntax(KindType k, Syntax* p, Syntax* r)
    : ConstructorSyntax(k, p, r)
  { }

  /// Returns the parameters.
  Syntax *parameters() const
  {
    return constructor();
  }
  static bool classof(const Syntax *S) {
    return S->getKind() >= MappingStart && S->getKind() < MappingEnd;
  }
};

/// Function type constructor.
struct FunctionSyntax : MappingSyntax
{
  static constexpr KindType this_kind = Function;

  FunctionSyntax(Syntax* p, Syntax* r)
    : MappingSyntax(this_kind, p, r)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// Template type constructor.
struct TemplateSyntax : MappingSyntax
{
  static constexpr KindType this_kind = Template;

  TemplateSyntax(Syntax* p, Syntax* r)
    : MappingSyntax(this_kind, p, r)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// Unary postfix operators.
struct PostfixSyntax : UnarySyntax
{
  static constexpr KindType this_kind = Postfix;

  PostfixSyntax(Token tok, Syntax* s)
    : UnarySyntax(this_kind, s), m_op(tok)
  { }

  /// Returns the prefix operation (operator).
  Token operation() const
  {
    return m_op;
  }
  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }

  Token m_op;
};

/// Applies arguments to a function, template, or array.
struct ApplicationSyntax : BinarySyntax
{
  ApplicationSyntax(KindType k, Syntax* e, Syntax* a)
    : BinarySyntax(k, e, a)
  { }

  /// Returns term being applied to arguments.
  Syntax* applicant() const
  {
    return m_terms[0];
  }

  /// Returns the arguments of the call.
  Syntax* arguments() const
  {
    return m_terms[1];
  }
  static bool classof(const Syntax *S) {
    return S->getKind() >= ApplicationStart && S->getKind() < ApplicationEnd;
  }
};

/// Represents a function call.
struct CallSyntax : ApplicationSyntax
{
  static constexpr KindType this_kind = Call;

  CallSyntax(Syntax* s0, Syntax* s1)
    : ApplicationSyntax(this_kind, s0, s1)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// Represents indexing into a table.
struct IndexSyntax : ApplicationSyntax
{
  static constexpr KindType this_kind = Index;

  IndexSyntax(Syntax* s0, Syntax* s1)
    : ApplicationSyntax(this_kind, s0, s1)
  { }
  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// Infix binary operators.
struct InfixSyntax : BinarySyntax
{
  static constexpr KindType this_kind = Infix;

  InfixSyntax(Token t, Syntax* l, Syntax* r)
    : BinarySyntax(this_kind, l, r), m_op(t)
  { }

  /// Returns the infix operation (operator).
  Token operation() const
  {
    return m_op;
  }

  /// Returns the left-hand operand.
  Syntax* lhs() const
  {
    return m_terms[0];
  }

  /// Returns the right-hand operand.
  Syntax* rhs() const
  {
    return m_terms[1];
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
  Token m_op;
};

/// Control structures. Control structures are primarily defined by two
/// syntactic structures: head and body. The "head" part introduces variables,
/// defines conditons, etc. The "body" is the main sub-expression.
///
/// TODO: We might need to expand this for specific kinds of control
/// structures. Lambda expressions don't readily fit the pattern unless we
/// encode them cleverly using pairs.
struct ControlSyntax : BinarySyntax
{
  static constexpr KindType this_kind = Control;

  ControlSyntax(Token t, Syntax* c, Syntax* b)
    : BinarySyntax(this_kind, c, b), m_ctrl(t)
  { }

  /// Returns the infix operation (operator).
  Token control() const
  {
    return m_ctrl;
  }

  /// Returns the head of the control.
  Syntax* head() const
  {
    return m_terms[0];
  }

  /// Returns the body of the control.
  Syntax* body() const
  {
    return m_terms[1];
  }

  clang::SourceLocation getLocation() const {
    return control().getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    if (m_terms[1])
      return m_terms[1]->getLocation();
    if (m_terms[0])
      return m_terms[0]->getLocation();
    return getLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
  Token m_ctrl;
};


/// A definition or parameter. It has:
///
///   - a declarator (either identifier or list thereof)
///   - a type
///   - a constraint
///   - an initializer
struct DeclarationSyntax : QuaternarySyntax
{
  static constexpr KindType this_kind = Declaration;

  DeclarationSyntax(Syntax* d, Syntax* t, Syntax* c, Syntax* i)
    : QuaternarySyntax(this_kind, d, t, c, i)
  { }

  /// Returns the declarator.
  Syntax* declarator() const
  {
    return operand(0);
  }

  /// Returns the type.
  Syntax* type() const
  {
    return operand(1);
  }

  Syntax* constraint() const
  {
    return operand(2);
  }

  /// Returns the initializer.
  Syntax* initializer() const
  {
    return operand(3);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

/// The top-level container of terms.
struct FileSyntax : UnarySyntax
{
  static constexpr KindType this_kind = File;

  FileSyntax(Syntax* ds)
    : UnarySyntax(this_kind, ds)
  { }

  /// Returns the sequence of declarations in the file.
  Syntax* declarations() const
  {
    return operand();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == this_kind;
  }
};

} // namespace blue

#endif
