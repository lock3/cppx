//===- GreenSyntax.h - Green Language Lexer -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenSyntax interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_SYNTAX_H
#define CLANG_GREEN_SYNTAX_H

#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Green/GreenScope.h"
#include "clang/Green/SyntaxIterator.h"
#include "clang/Green/Tokens.h"

#include <vector>
#include <array>

namespace green
{

struct Syntax {

  enum SyntaxKind {
#define def_syntax(K) \
  SK_ ## K,
#include "Syntax.def"
  };

  Syntax() = delete;
  Syntax(SyntaxKind SK, clang::SourceLocation Loc, bool IsParam = false) noexcept :
    Loc(Loc), Kind(SK), IsParam(IsParam) {}

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

  /// Whether or not this node is a function parameter.
  bool isParam() const { return IsParam; }

  clang::SourceLocation Loc;

private:
  SyntaxKind Kind;

  /// Whether or not this node is a function parameter.
  bool IsParam;
};

struct ErrorSyntax : Syntax {
  ErrorSyntax()
    : Syntax(SK_Error, clang::SourceLocation())
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
  AtomSyntax(Token Tok, clang::SourceLocation Loc, bool IsParam = false)
    : Syntax(SK_Atom, Loc, IsParam), Tok(Tok)
  {}

  Token Tok;

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Atom;
  }
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

  auto begin()
  {
    return Elems[0];
  }

  auto end()
  {
    return Elems[NumElems];
  }

  auto begin() const
  {
    return Elems[0];
  }

  auto end() const
  {
    return Elems[NumElems];
  }

  T **Elems;
  unsigned NumElems;
};

/// A comma-separated list of terms.
///
/// \todo These are separated by either commas, semicolons, pr separators,
/// and there's (possibly) a semantic difference.
struct ListSyntax : Syntax, VectorNode<Syntax> {
  ListSyntax(Syntax **Ts, unsigned NumElems, clang::SourceLocation Loc)
    : Syntax(SK_List, Loc), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(&Elems[0], &Elems[NumElems]);
  }

  const child_range children() const {
    return const_child_range(&Elems[0], &Elems[NumElems]);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_List;
  }
};

struct ArraySyntax : Syntax, VectorNode<Syntax> {
  ArraySyntax(Syntax **Ts, unsigned NumElems, clang::SourceLocation Loc)
    : Syntax(SK_Array, Loc), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(&Elems[0], &Elems[NumElems]);
  }

  const child_range children() const {
    return const_child_range(&Elems[0], &Elems[NumElems]);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Array;
  }
};

/// A call to a function.
struct CallSyntax : Syntax {
  CallSyntax(Syntax *Fn, Syntax *Args, clang::SourceLocation Loc, bool IsParam = false)
    : Syntax(SK_Call, Loc, IsParam)
  {
    Elems[0] = Fn;
    Elems[1] = Args;
  }

  const Syntax *Callee() const {
    return Elems[0];
  }

  Syntax *Callee() {
    return Elems[0];
  }

  const Syntax *Args() const {
    return Elems[1];
  }

  Syntax *Args() {
    return Elems[1];
  }

  std::array<Syntax *, 2> Elems;

  child_range children() {
    return child_range(&Elems[0], &Elems[2]);
  }

  const child_range children() const {
    auto Children = const_cast<CallSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Call;
  }
};

/// A lookup in a dictionary.
struct ElemSyntax : Syntax {
  ElemSyntax(Syntax *Map, Syntax *Sel, clang::SourceLocation Loc)
    : Syntax(SK_Elem, Loc) {
    Elems[0] = Map;
    Elems[1] = Sel;
  }

  const Syntax *Map() const {
    return Elems[0];
  }

  Syntax *Map() {
    return Elems[0];
  }

  const Syntax *Args() const {
    return Elems[1];
  }

  Syntax *Args() {
    return Elems[1];
  }


  std::array<Syntax *, 2> Elems;

  child_range children() {
    return child_range(&Elems[0], &Elems[2]);
  }

  const child_range children() const {
    auto Children = const_cast<ElemSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Elem;
  }
};

/// A labeled block of code (e.g., a loop).
struct MacroSyntax : Syntax {
  MacroSyntax(Syntax *Call, Syntax *Block, Syntax *Next, clang::SourceLocation Loc)
    : Syntax(SK_Macro, Loc)
  {
    Elems[0] = Call;
    Elems[1] = Block;
    Elems[2] = Next;
  }

  const Syntax *Call() const {
    return Elems[0];
  }

  Syntax *Call() {
    return Elems[0];
  }

  const Syntax *Block() const {
    return Elems[1];
  }

  Syntax *Block() {
    return Elems[1];
  }

  const Syntax *End() const {
    return Elems[2];
  }

  Syntax *End() {
    return Elems[2];
  }

  child_range children() {
    return child_range(&Elems[0], &Elems[3]);
  }

  const child_range children() const {
    auto Children = const_cast<MacroSyntax *>(this)->children();
    return const_child_range(Children);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Macro;
  }

  std::array<Syntax *, 3> Elems;
};

struct FileSyntax : Syntax, VectorNode<Syntax> {
  FileSyntax(Syntax **Ts, unsigned NumElems, clang::SourceLocation Loc)
    : Syntax(SK_File, Loc), VectorNode(Ts, NumElems)
  {}

  child_range children() {
    return child_range(&Elems[0], &Elems[NumElems]);
  }

  const child_range children() const {
    return const_child_range(&Elems[0], &Elems[NumElems]);
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_File;
  }
};

} // namespace green

#endif
