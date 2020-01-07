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

#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Blue/BlueTokens.h"

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

  const char *getKindName() const;

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
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

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
  const Token& getToken() const {
    return Tok;
  }

  llvm::StringRef getSpelling() const {
    return Tok.getSpelling();
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

private:
  Token Tok;
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

private:
  llvm::ArrayRef<Syntax *> Elems;
};

/// Represents a paren-enclosed list of expressions.
///
/// TODO: Save enclosing tokens.
class TupleSyntax : public VectorSyntax {
public:
  TupleSyntax(llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Tuple, A)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Tuple;
  }
};

/// Represents a brace-enclosed list of expressions.
///
/// TODO: Save enclosing tokens.
class ArraySyntax : public VectorSyntax {
public:
  ArraySyntax(llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Array, A)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Array;
  }
};

/// Represents a brace-enclosed list of statements.
///
/// TODO: Save enclosing tokens.
class BlockSyntax : public VectorSyntax {
public:
  BlockSyntax(llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Block, A)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Block;
  }
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

  Syntax *getOperand() {
    return Arg;
  }

  const Syntax *getOperand() const {
    return Arg;
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
    : Syntax(Binary), Op(), LHS(LHS), RHS(RHS) { }

  BinarySyntax(const Token &Op, Syntax *LHS, Syntax *RHS)
    : Syntax(Binary), Op(Op), LHS(LHS), RHS(RHS) { }

  const Token &getOperator() const {
    return Op;
  }

  bool isApplication() const {
    return Op.isInvalid();
  }

  Syntax *getLeftOperand() {
    return LHS;
  }

  const Syntax *getLeftOperand() const {
    return LHS;
  }

  Syntax *getRightOperand() {
    return RHS;
  }

  const Syntax *getRightOperand() const {
    return RHS;
  }

private:
  Token Op;
  Syntax *LHS;
  Syntax *RHS;
};

/// Represents the top-level sequence of statements.
class TopSyntax : public VectorSyntax {
public:
  TopSyntax(llvm::ArrayRef<Syntax *> A)
    : VectorSyntax(Block, A)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == Top;
  }
};

} // namespace blue

#endif
