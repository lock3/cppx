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
    : Syntax(k), Terms{ts...}
  {
  }

  /// Returns the nth operand.
  Syntax *getOperand(std::size_t n) const
  {
    return Terms[n];
  }

  /// Returns the operands.
  const Syntax *getOperands() const
  {
    return Terms;
  }

  /// Returns the operands.
  Syntax *getOperands()
  {
    return Terms;
  }

  child_range children() {
    return child_range(Terms, Terms + N);
  }

  const child_range children() const {
    return const_child_range(Terms, Terms + N);
  }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    return Terms[0]->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return Terms[0]->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return Terms[N-1]->getLocation();
  }

  Syntax *Terms[N];
};

/// Specialization for unary nodes.
template<>
struct KarySyntax<1> : Syntax
{
  KarySyntax(KindType K, Syntax *S)
    : Syntax(K), Term(S)
  { }

  /// Returns the operand.
  Syntax *getOperand() const
  {
    return Term;
  }

  /// Returns the operands.
  const Syntax *getOperands() const
  {
    return Term;
  }

  /// Returns the operands.
  Syntax *getOperands()
  {
    return Term;
  }

  child_range children() {
    return child_range(&Term, &Term + 1);
  }

  const child_range children() const {
    return const_child_range(&Term, &Term + 1);
  }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    return Term->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return Term->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return Term->getLocation();
  }

  Syntax *Term;
};

/// A unary expression has a single operand.
struct UnarySyntax : KarySyntax<1>
{
  UnarySyntax(KindType K, Syntax *S)
    : KarySyntax<1>(K, S)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() >= UnaryStart && S->getKind() < UnaryEnd;
  }
};

/// A binary expression with two operands.
struct BinarySyntax : KarySyntax<2>
{
  BinarySyntax(KindType K, Syntax *S0, Syntax *S1)
    : KarySyntax<2>(K, S0, S1)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() >= BinaryStart && S->getKind() < BinaryEnd;
  }
};

/// A ternary expression.
struct TernarySyntax : KarySyntax<3>
{
  TernarySyntax(KindType K, Syntax *S0, Syntax *S1, Syntax *S2)
    : KarySyntax<3>(K, S0, S1, S2)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() >= TernaryStart && S->getKind() < TernaryEnd;
  }
};

/// A quaternary expression.
struct QuaternarySyntax : KarySyntax<4>
{
  QuaternarySyntax(KindType K, Syntax *S0, Syntax *S1, Syntax *S2, Syntax *S3)
    : KarySyntax<4>(K, S0, S1, S2, S3)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() >= QuaternaryStart && S->getKind() < QuaternaryEnd;
  }
};

/// A term with an unspecified number of operands.
struct MultiarySyntax : Syntax
{
  MultiarySyntax(KindType K, Syntax **S, unsigned Size)
    : Syntax(K), Terms(S), NumTerms(Size)
  { }

  /// Returns the nth operand.
  Syntax *getOperand(std::size_t N) const
  {
    return Terms[N];
  }

  /// Returns the operands.
  Syntax **getOperands()
  {
    return Terms;
  }

  child_range children() {
    return child_range(Terms, Terms + NumTerms);
  }

  const child_range children() const {
    return const_child_range(Terms, Terms + NumTerms);
  }

  unsigned getNumChildren() const { return NumTerms; }

  /// The location that best indicates the best position of the term.
  clang::SourceLocation getLocation() const {
    if (NumTerms == 0)
      return clang::SourceLocation();

    return Terms[0]->getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    if (NumTerms == 0)
      return clang::SourceLocation();

    return Terms[0]->getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    if (Terms == 0)
      return clang::SourceLocation();

    // return the last term that exists
    for (unsigned I = NumTerms - 1; NumTerms > 0; --I)
      if (Terms[I])
        return Terms[I]->getLocation();
    // This removes a warning.
    return clang::SourceLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= MultiaryStart && S->getKind() < MultiaryEnd;
  }

  Syntax **Terms;
  unsigned NumTerms;
};

// Specific trees

/// Any tree represented by a single token.
struct AtomSyntax : Syntax
{
  AtomSyntax(KindType K, Token Tok)
    : Syntax(K), Tok(Tok)
  { }

  /// Returns the token of the atom.
  Token getToken() const
  {
    return Tok;
  }

  /// Returns the spelling of the atom.
  std::string getSpelling() const
  {
    return Tok.getSpelling();
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
    return Tok.getLocation();
  }

  /// The location of the beginning of the term.
  clang::SourceLocation getBeginLocation() const {
    return Tok.getLocation();
  }

  /// The location of the end of the term.
  clang::SourceLocation getEndLocation() const {
    return Tok.getLocation();
  }

  Token Tok;
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
  static constexpr KindType Kind = Literal;

  LiteralSyntax(Token tok)
    : AtomSyntax(Kind, tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }

  LiteralSuffix Suffix;
};

/// Represents user-defined names.
struct IdentifierSyntax : AtomSyntax
{
  static constexpr KindType Kind = Identifier;

  IdentifierSyntax(Token tok)
    : AtomSyntax(Kind, tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A sequence of delimited terms.
///
/// TODO: This doesn't store the delimiters. I'm not sure if that's
/// actually important.
struct ListSyntax : MultiarySyntax
{
  static constexpr KindType Kind = List;

  ListSyntax(Syntax **S, unsigned Size)
    : MultiarySyntax(Kind, S, Size)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A sequence of terms.
struct SequenceSyntax : MultiarySyntax
{
  static constexpr KindType Kind = Sequence;

  SequenceSyntax(Syntax **S, unsigned Size)
    : MultiarySyntax(Kind, S, Size)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A term enclosed by a pair of tokens.
struct EnclosureSyntax : UnarySyntax
{
  static constexpr KindType Kind = Enclosure;

  EnclosureSyntax(Token O, Token C, Syntax *T)
    : UnarySyntax(Kind, T), Open(O), Close(C)
  { }

  /// Returns the opening token.
  Token getOpen() const
  {
    return Open;
  }

  /// Returns the closing token.
  Token getClose() const
  {
    return Close;
  }

  /// Returns the inner term.
  Syntax *getTerm() const
  {
    return Term;
  }

  bool isParenEnclosure() const {
    return Open.hasKind(tok::LeftParen);
  }

  bool isBracketEnclosure() const {
    return Open.hasKind(tok::LeftBracket);
  }

  bool isBraceEnclosure() const {
    return Open.hasKind(tok::LeftBrace);
  }

  Token Open;
  Token Close;

  clang::SourceLocation getLocation() const {
    return Open.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Close.getLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A pair of terms.
struct PairSyntax : BinarySyntax
{
  static constexpr KindType Kind = Pair;

  PairSyntax(Syntax *S0, Syntax *S1)
    : BinarySyntax(Kind, S0, S1)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A triple of terms.
struct TripleSyntax : TernarySyntax
{
  static constexpr KindType Kind = Triple;

  TripleSyntax(Syntax *S0, Syntax *S1, Syntax *S2)
    : TernarySyntax(Kind, S0, S1, S2)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A quadruple of terms.
struct QuadrupleSyntax : QuaternarySyntax
{
  static constexpr KindType Kind = Quadruple;

  QuadrupleSyntax(Syntax *S0, Syntax *S1, Syntax *S2, Syntax *S3)
    : QuaternarySyntax(Kind, S0, S1, S2, S3)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// A unary prefix operator expression.
struct PrefixSyntax : UnarySyntax
{
  static constexpr KindType Kind = Prefix;

  PrefixSyntax(Token Tok, Syntax *S)
    : UnarySyntax(Kind, S), Op(Tok)
  { }

  /// Returns the prefix operation (operator).
  Token getOperation() const
  {
    return Op;
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }

  Token Op;
};

/// Compound type constructors for arrays, templates, and functions.
struct ConstructorSyntax : BinarySyntax
{
  ConstructorSyntax(KindType K, Syntax *S, Syntax *R)
    : BinarySyntax(K, S, R)
  { }

  /// Returns the "constructor" of a constructor. Either an array bound
  /// or a parameter list.
  Syntax *getConstructor() const
  {
    return getOperand(0);
  }

  /// Returns the result type of the constructor.
  Syntax *getResult() const
  {
    return getOperand(1);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= ConstructorStart && S->getKind() < ConstructorEnd;
  }
};

/// Array type constructor.
struct ArraySyntax : ConstructorSyntax
{
  static constexpr KindType Kind = Array;

  ArraySyntax(Syntax *S, Syntax *T)
    : ConstructorSyntax(Kind, S, T)
  { }

  /// Returns the array bound.
  Syntax *getBounds() const
  {
    return getConstructor();
  }

  using ConstructorSyntax::ConstructorSyntax;
  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// Mapping type constructors (templates and functions).
struct MappingSyntax : ConstructorSyntax
{
  MappingSyntax(KindType K, Syntax *P, Syntax *R)
    : ConstructorSyntax(K, P, R)
  { }

  /// Returns the parameters.
  Syntax *getParameters() const
  {
    return getConstructor();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= MappingStart && S->getKind() < MappingEnd;
  }
};

/// Function type constructor.
struct FunctionSyntax : MappingSyntax
{
  static constexpr KindType Kind = Function;

  FunctionSyntax(Syntax *P, Syntax *R)
    : MappingSyntax(Kind, P, R)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// Template type constructor.
struct TemplateSyntax : MappingSyntax
{
  static constexpr KindType Kind = Template;

  TemplateSyntax(Syntax *P, Syntax *R)
    : MappingSyntax(Kind, P, R)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// Unary postfix operators.
struct PostfixSyntax : UnarySyntax
{
  static constexpr KindType Kind = Postfix;

  PostfixSyntax(Token Tok, Syntax *S)
    : UnarySyntax(Kind, S), Op(Tok)
  { }

  /// Returns the prefix operation (operator).
  Token getOperation() const
  {
    return Op;
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }

  Token Op;
};

/// Applies arguments to a function, template, or array.
struct ApplicationSyntax : BinarySyntax
{
  ApplicationSyntax(KindType K, Syntax *E, Syntax *A)
    : BinarySyntax(K, E, A)
  { }

  /// Returns term being applied to arguments.
  Syntax *getApplicant() const
  {
    return Terms[0];
  }

  /// Returns the arguments of the call.
  Syntax *getArguments() const
  {
    return Terms[1];
  }

  static bool classof(const Syntax *S) {
    return S->getKind() >= ApplicationStart && S->getKind() < ApplicationEnd;
  }
};

/// Represents a function call.
struct CallSyntax : ApplicationSyntax
{
  static constexpr KindType Kind = Call;

  CallSyntax(Syntax *S0, Syntax *S1)
    : ApplicationSyntax(Kind, S0, S1)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// Represents indexing into a table.
struct IndexSyntax : ApplicationSyntax
{
  static constexpr KindType Kind = Index;

  IndexSyntax(Syntax *S0, Syntax *S1)
    : ApplicationSyntax(Kind, S0, S1)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

/// Infix binary operators.
struct InfixSyntax : BinarySyntax
{
  static constexpr KindType Kind = Infix;

  InfixSyntax(Token T, Syntax *L, Syntax *R)
    : BinarySyntax(Kind, L, R), Op(T)
  { }

  /// Returns the infix operation (operator).
  Token getOperation() const
  {
    return Op;
  }

  /// Returns the left-hand operand.
  Syntax *getLhs() const
  {
    return Terms[0];
  }

  /// Returns the right-hand operand.
  Syntax *getRhs() const
  {
    return Terms[1];
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
  Token Op;
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
  static constexpr KindType Kind = Control;

  ControlSyntax(Token T, Syntax *C, Syntax *B)
    : BinarySyntax(Kind, C, B), Ctrl(T)
  { }

  /// Returns the infix operation (operator).
  Token getControl() const
  {
    return Ctrl;
  }

  /// Returns the head of the control.
  Syntax *getHead() const
  {
    return Terms[0];
  }

  /// Returns the body of the control.
  Syntax *getBody() const
  {
    return Terms[1];
  }

  clang::SourceLocation getLocation() const {
    return getControl().getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    if (Terms[1])
      return Terms[1]->getLocation();
    if (Terms[0])
      return Terms[0]->getLocation();
    return getLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }

private:
  Token Ctrl;
};


/// A definition or parameter. It has:
///
///   - a declarator (either identifier or list thereof)
///   - a type
///   - a constraint
///   - an initializer
struct DeclarationSyntax : QuaternarySyntax
{
  // The introducer-keyword that started this declaration, if any.
  enum IntroducerKind : unsigned {
    // A parameter or ill-formed declaration
    Unknown,

    // A variable declaration
    Variable,

    // A function declaration
    Function,

    // A declaration with a type as its value.
    Type,

    // A declaration of a base class.
    Super,

    // A namespace declaration
    Namespace,
  };

  static constexpr KindType Kind = Declaration;

  DeclarationSyntax(Syntax *D, Syntax *T, Syntax *C, Syntax *I,
                    IntroducerKind IdK = Unknown)
    : QuaternarySyntax(Kind, D, T, C, I),
    IntroKind(IdK)
  { }

  /// This attempts to return the first valid source location from a declaration
  /// for reporting an error. This takes into account that we may not have an
  /// identifier name available, and we would instead need to use the type
  /// or initializer in order to get a valid meaningful location.
  clang::SourceLocation getErrorLocation() const;

  /// Returns the declarator.
  Syntax *getDeclarator() const
  {
    return getOperand(0);
  }

  bool declaratorIsThis() const;

  /// Returns the type.
  Syntax *getType() const
  {
    return getOperand(1);
  }

  Syntax *getConstraint() const
  {
    return getOperand(2);
  }

  /// Returns the initializer.
  Syntax *getInitializer() const
  {
    return getOperand(3);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }

  Token getParamPassingSpecifier() const;

  Token *ParamSpecs = nullptr;
  unsigned NumParamSpecs = 0;
  IntroducerKind IntroKind = Unknown;
};

/// The top-level container of terms.
struct FileSyntax : UnarySyntax
{
  static constexpr KindType Kind = File;

  FileSyntax(Syntax *Ds)
    : UnarySyntax(Kind, Ds)
  { }

  /// Returns the sequence of declarations in the file.
  Syntax *getDeclarations() const
  {
    return getOperand();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Kind;
  }
};

} // namespace blue

#endif
