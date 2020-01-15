//===- GoldSyntax.h - Classes for representing Gold syntax constructs -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GoldSyntax interface and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_SYNTAX_H
#define CLANG_GOLD_SYNTAX_H

#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntaxIterator.h"
#include "clang/Gold/GoldTokens.h"

#include <vector>
#include <array>

namespace gold
{
  // Bring isa/cast/dyn_cast into scope.
  using clang::isa;
  using clang::cast;
  using clang::dyn_cast;
  using clang::dyn_cast_or_null;

struct Syntax {

  enum SyntaxKind {
#define def_syntax(K) \
  SK_ ## K,
#include "GoldSyntax.def"
  };

  Syntax() = delete;
  Syntax(SyntaxKind SK) noexcept :
    Kind(SK) {}

  static Syntax *error;

  bool IsError() const {
    return getKind() == SK_Error;
  }

  SyntaxKind getKind() const { return Kind; }
  const char *getSyntaxKindName() const;
  void dump() const;

  using child_iterator = SyntaxIterator;
  using const_child_iterator = ConstSyntaxIterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;

  child_range children();

  const_child_range children() const {
    auto Children = const_cast<Syntax *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

  clang::SourceLocation getLoc() const;

private:
  SyntaxKind Kind;
};

struct ErrorSyntax : Syntax {
  ErrorSyntax()
    : Syntax(SK_Error)
  {}

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Error;
  }
};

/// Any term represented by a single token (e.g., literals, identifiers).
struct AtomSyntax : Syntax {
  AtomSyntax(Token Tok)
    : Syntax(SK_Atom), Tok(Tok)
  {}

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

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Atom;
  }

  clang::SourceLocation getTokenLoc() const {
    return Tok.Loc;
  }

  /// The token for the atom.
  Token Tok;
};

/// An arbitrary, but known, length sequence of terms (e.g., the arguments
/// of a call expression).
template<typename T>
struct VectorNode
{
  using value_type = T;

  VectorNode(T **ts, unsigned NumElems)
    : Elems(ts), NumElems(NumElems)
  { }

  bool hasChildren() const {
    return NumElems != 0;
  }

  std::size_t getNumChildren() const {
    return NumElems;
  }

  Syntax* getChild(std::size_t N) {
    assert(N < NumElems);
    return Elems[N];
  }

  const Syntax* getChild(std::size_t N) const {
    assert(N < NumElems);
    return Elems[N];
  }

  T **Elems;
  unsigned NumElems;
};

/// A comma-separated list of terms.
///
/// \todo These are separated by either commas, semicolons, pr separators,
/// and there's (possibly) a semantic difference.
struct ListSyntax : Syntax, VectorNode<Syntax> {
  ListSyntax(Syntax **Ts, unsigned NumElems)
    : Syntax(SK_List), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(Elems, Elems + NumElems);
  }

  const child_range children() const {
    return const_child_range(Elems, Elems + NumElems);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_List;
  }
};

struct ArraySyntax : Syntax, VectorNode<Syntax> {
  ArraySyntax(Syntax **Ts, unsigned NumElems)
    : Syntax(SK_Array), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(Elems, Elems + NumElems);
  }

  const child_range children() const {
    return const_child_range(Elems, Elems + NumElems);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Array;
  }
};

/// A call to a function.
struct CallSyntax : Syntax {
  CallSyntax(Syntax *Fn, Syntax *Args)
    : Syntax(SK_Call)
  {
    Elems[0] = Fn;
    Elems[1] = Args;
  }

  const Syntax *getCallee() const {
    return Elems[0];
  }

  Syntax *getCallee() {
    return Elems[0];
  }

  std::size_t getNumArguments() const;

  const Syntax *getArguments() const {
    return Elems[1];
  }

  Syntax *getArguments() {
    return Elems[1];
  }

  const Syntax *getArgument(std::size_t N) const {
    return const_cast<CallSyntax*>(this)->getArgument(N);
  }

  Syntax *getArgument(std::size_t N);

  child_range children() {
    return child_range(Elems.data(), Elems.data() + 2);
  }

  const child_range children() const {
    auto Children = const_cast<CallSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Call;
  }

  clang::SourceLocation getCalleeLoc() const {
    return getCallee()->getLoc();
  }

  std::array<Syntax *, 2> Elems;
};

/// A lookup in a dictionary.
struct ElemSyntax : Syntax {
  ElemSyntax(Syntax *Map, Syntax *Sel)
    : Syntax(SK_Elem) {
    Elems[0] = Map;
    Elems[1] = Sel;
  }

  const Syntax *getObject() const {
    return Elems[0];
  }

  Syntax *getObject() {
    return Elems[0];
  }

  const Syntax *getArguments() const {
    return Elems[1];
  }

  Syntax *getArguments() {
    return Elems[1];
  }

  child_range children() {
    return child_range(Elems.data(), Elems.data() + 2);
  }

  const child_range children() const {
    auto Children = const_cast<ElemSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Elem;
  }

  clang::SourceLocation getObjectLoc() const {
    return getObject()->getLoc();
  }

  std::array<Syntax *, 2> Elems;
};

/// A labeled block of code (e.g., a loop).
struct MacroSyntax : Syntax {
  MacroSyntax(Syntax *Call, Syntax *Block, Syntax *Next)
    : Syntax(SK_Macro)
  {
    Elems[0] = Call;
    Elems[1] = Block;
    Elems[2] = Next;
  }

  const Syntax *getCall() const {
    return Elems[0];
  }

  Syntax *getCall() {
    return Elems[0];
  }

  const Syntax *getBlock() const {
    return Elems[1];
  }

  Syntax *getBlock() {
    return Elems[1];
  }

  const Syntax *getNext() const {
    return Elems[2];
  }

  Syntax *getNext() {
    return Elems[2];
  }

  child_range children() {
    return child_range(Elems.data(), Elems.data() + 3);
  }

  const child_range children() const {
    auto Children = const_cast<MacroSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Macro;
  }

  clang::SourceLocation getCallLoc() const {
    return getCall()->getLoc();
  }

  clang::SourceLocation getBlockLoc() const {
    return getBlock()->getLoc();
  }

  clang::SourceLocation getNextLoc() const {
    return getNext()->getLoc();
  }

  std::array<Syntax *, 3> Elems;
};

struct FileSyntax : Syntax, VectorNode<Syntax> {
  FileSyntax(Syntax **Ts, unsigned NumElems)
    : Syntax(SK_File), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(Elems, Elems + NumElems);
  }

  const child_range children() const {
    return const_child_range(Elems, Elems + NumElems);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_File;
  }
};

} // namespace gold

#endif
