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
  enum KindType {
#define def_syntax(K) \
  K,
#include "clang/Blue/BlueSyntax.def"
  };

protected:
  Syntax(KindType K) noexcept
    : Kind(K) { }

public:
  KindType getKind() const {
    return Kind;
  }

  bool isError() const {
    return getKind() == Error;
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
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;

  child_range children();
  const_child_range children() const;

  /// Emit debugging information about this node.
  void dump() const;

private:
  KindType Kind;
};

/// Supports the implementation of leaf nodes in the grammar.
class LeafSyntax : public Syntax {
protected:
  LeafSyntax(KindType K)
    : Syntax(K)
  {}

public:
  clang::SourceLocation getLocation() const {
    return {};
  }

  clang::SourceLocation getBeginLocation() const {
    return {};
  }

  clang::SourceLocation getEndLocation() const {
    return {};
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

/// Represents an error.
class ErrorSyntax : public LeafSyntax {
public:
  ErrorSyntax()
    : LeafSyntax(Error)
  {}

  static bool classof(const Syntax *S) {
    return S->getKind() == Error;
  }
};

/// Supports the implementation of leaf nodes defined by a single token. This is
/// a base class and is used to share the implementation between literals and
/// identifiers.
class AtomSyntax : public LeafSyntax {
protected:
  AtomSyntax(KindType K, Token Tok)
    : LeafSyntax(K), Tok(Tok)
  {}

public:
  clang::SourceLocation getLocation() const {
    return Tok.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return Tok.getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Tok.getLocation();
  }

  const Token& getToken() const {
    return Tok;
  }

  llvm::StringRef getSpelling() const {
    return Tok.getSpelling();
  }

private:
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
class LiteralSyntax : public AtomSyntax {
public:
  LiteralSyntax(const Token &Tok)
    : AtomSyntax(Literal, Tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Literal;
  }
  LiteralSuffix Suffix;
};

/// Represents identifiers.
class IdentifierSyntax : public AtomSyntax {
public:
  IdentifierSyntax(const Token &Tok)
    : AtomSyntax(Identifier, Tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Identifier;
  }
};

/// Supports the implementation of variadic (interior) nodes of dynamic size
/// (e.g., lists and and sequences).
class VectorSyntax : public Syntax
{
protected:
  VectorSyntax(KindType K, llvm::ArrayRef<Syntax *> A)
    : Syntax(K), Elems(A)
  { }

public:
  bool hasChildren() const {
    return !Elems.empty();
  }

  std::size_t getNumChildren() const {
    return Elems.size();
  }

  Syntax* getChild(std::size_t N) {
    return Elems[N];
  }

  const Syntax* getChild(std::size_t N) const {
    return Elems[N];
  }

  child_range children() {
    return child_range(Elems.data(), Elems.data() + Elems.size());
  }

  const child_range children() const {
    return child_range(Elems.data(), Elems.data() + Elems.size());
  }

protected:
  llvm::ArrayRef<Syntax *> Elems;
};

/// Represents a possibly enclosed, token-separated list of terms. Note that
/// the separator token is symbolic.
class ListSyntax : public VectorSyntax {
public:
  /// Construct an unenclosed list of terms.
  ListSyntax(TokenKind K, llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(List, A), Enc(), Sep(K)
  { }

  /// Construct an enclosed list of terms.
  ListSyntax(const TokenPair Enc, TokenKind K, llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(List, A), Enc(Enc), Sep(K)
  { }

  const TokenPair &getEnclosingTokens() const {
    return Enc;
  }

  bool isUnenclosedList() {
    return Enc.first.isInvalid();
  }

  bool isParenList() const {
    return Enc.first.hasKind(tok::LeftParen);
  }

  bool isBracketList() const {
    return Enc.first.hasKind(tok::LeftBracket);
  }

  TokenKind getSeparatorKind() {
    return Sep;
  }

  bool isCommaSeparated() const {
    return Sep == tok::Comma;
  }

  bool isSemicolonSeparated() const {
    return Sep == tok::Semicolon;
  }

  clang::SourceLocation getLocation() const {
    if (Enc.first.isValid())
      return Enc.first.getLocation();
    if (Elems.empty())
      return {};
    return Elems.front()->getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    if (Enc.first.isValid())
      return Enc.second.getLocation();
    if (Elems.empty())
      return {};
    return Elems.back()->getLocation();
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == List;
  }

private:
  TokenPair Enc;
  TokenKind Sep;
};

/// Represents an enclosed sequence of terms.
///
/// TODO: TopSyntax is an unclosed sequence. Maybe we should merge those
/// nodes, or are there sufficiently distinct things in Top that would
/// make it inadvisable. Probably.
class SeqSyntax : public VectorSyntax {
public:
  SeqSyntax(const TokenPair &Enc, llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Seq, A), Enc(Enc)
  { }

  const TokenPair &getEnclosingTokens() const {
    return Enc;
  }

  bool isUnenclosedList() {
    return Enc.first.isInvalid();
  }

  bool isBraceList() const {
    return Enc.first.hasKind(tok::LeftBrace);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Seq;
  }

  clang::SourceLocation getLocation() const {
    return Enc.first.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Enc.second.getLocation();
  }

private:
  TokenPair Enc;
};

/// Represents unary operators.
class UnarySyntax : public Syntax {
public:
  UnarySyntax(const Token &Op, Syntax *Arg)
    : Syntax(Unary), Op(Op), Arg(Arg)
  { }

  const Token &getOperator() const {
    return Op;
  }

  llvm::StringRef getOperatorSpelling() const {
    return Op.getSpelling();
  }

  Syntax *getOperand() {
    return Arg;
  }

  const Syntax *getOperand() const {
    return Arg;
  }

  clang::SourceLocation getLocation() const {
    return Op.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Arg->getEndLocation();
  }

  child_range children() {
    return child_range(&Arg, &Arg + 1);
  }

  const child_range children() const {
    return child_range(&Arg, &Arg + 1);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Unary;
  }

private:
  Token Op;
  Syntax *Arg;
};

/// Represents binary operators. If the operator token is unspecified, this
/// represents the application of one term to another.
class BinarySyntax : public Syntax {
public:
  BinarySyntax(Syntax *LHS, Syntax *RHS)
    : Syntax(Binary), Op(), Args{LHS, RHS} { }

  BinarySyntax(const Token &Op, Syntax *LHS, Syntax *RHS)
    : Syntax(Binary), Op(Op), Args{LHS, RHS} { }

  const Token &getOperator() const {
    return Op;
  }

  llvm::StringRef getOperatorSpelling() const {
    return Op.getSpelling();
  }

  bool isApplication() const {
    return Op.isInvalid();
  }

  Syntax *getLeftOperand() {
    return Args[0];
  }

  const Syntax *getLeftOperand() const {
    return Args[0];
  }

  Syntax *getRightOperand() {
    return Args[1];
  }

  const Syntax *getRightOperand() const {
    return Args[1];
  }

  clang::SourceLocation getLocation() const {
    return Op.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return Args[0]->getBeginLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Args[1]->getEndLocation();
  }

  child_range children() {
    return child_range(Args, Args + 2);
  }

  const child_range children() const {
    return child_range(Args, Args + 2);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Binary;
  }

private:
  Token Op;
  Syntax *Args[2];
};

/// A declaration with a definition.
class DefSyntax : public Syntax {
public:
  DefSyntax(Token Id, Syntax *Sig, Syntax *Init)
    : Syntax(Def), Id(Id), Args{Sig, Init} {}

  const Token& getIdentifier() const {
    return Id;
  }

  llvm::StringRef getIdentifierSpelling() const {
    return Id.getSpelling();
  }

  bool hasDeclarator() const
  {
    return getDeclarator();
  }

  const Syntax *getDeclarator() const {
    return Args[0];
  }

  Syntax *getDeclarator() {
    return Args[0];
  }

  bool hasInitializer() const
  {
    return getInitializer();
  }

  const Syntax *getInitializer() const {
    return Args[1];
  }

  Syntax *getInitializer() {
    return Args[1];
  }

  /// The location of a declaration is that of its identifier.
  clang::SourceLocation getLocation() const {
    return Id.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Args[1]->getEndLocation();
  }

  child_range children() {
    return child_range(Args, Args + 2);
  }

  const child_range children() const {
    return child_range(Args, Args + 2);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Def;
  }

private:
  Token Id;
  Syntax *Args[2];
};

/// Represents a control structure
class ControlSyntax : public Syntax {
public:
  ControlSyntax(Token KW, Syntax *Sig, Syntax *Block)
    : Syntax(Control), Keyword(KW), Args{Sig, Block} {}

  const Token& getKeyword() const {
    return Keyword;
  }

  Syntax *getSignature() {
    return Args[0];
  }

  const Syntax *getSignature() const {
    return Args[0];
  }

  Syntax *getBlock() {
    return Args[1];
  }

  const Syntax *getBlock() const {
    return Args[1];
  }

  /// The location of a declaration is that of its keyword.
  clang::SourceLocation getLocation() const {
    return Keyword.getLocation();
  }

  clang::SourceLocation getBeginLocation() const {
    return getLocation();
  }

  clang::SourceLocation getEndLocation() const {
    return Args[1]->getEndLocation();
  }


  child_range children() {
    return child_range(Args, Args + 2);
  }

  const child_range children() const {
    return child_range(Args, Args + 2);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Control;
  }

private:
  Token Keyword;
  Syntax *Args[2];
};

/// Represents the top-level sequence of statements.
class TopSyntax : public VectorSyntax {
public:
  TopSyntax(llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Top, A) {}

  clang::SourceLocation getLocation() const {
    return {};
  }

  clang::SourceLocation getBeginLocation() const {
    return {};
  }

  clang::SourceLocation getEndLocation() const {
    return {};
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == Top;
  }
};

} // namespace blue

#endif
