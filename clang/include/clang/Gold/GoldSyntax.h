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

namespace gold {

// Bring isa/cast/dyn_cast into scope.
using clang::isa;
using clang::cast;
using clang::cast_or_null;
using clang::dyn_cast;
using clang::dyn_cast_or_null;

struct Attribute;

struct Syntax
{

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

  using AttrVec = llvm::SmallVector<Attribute *, 4>;
  AttrVec const &getAttributes() const { return Attributes; }
  void addAttribute(Attribute *Attr);
  void updateAttributes(const AttrVec &Attrs) {
    Attributes.insert(Attributes.end(), Attrs.begin(), Attrs.end());
  }
  void updateAttributes(AttrVec &&Attrs) {
    if (Attributes.empty()) {
      Attributes = Attrs;
      return;
    }
    updateAttributes(Attrs);
  }

protected:
  SyntaxKind Kind;
  AttrVec Attributes;
};

struct ErrorSyntax : Syntax
{
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
struct AtomSyntax : Syntax
{
  AtomSyntax(Token Tok)
    : Syntax(SK_Atom), Tok(Tok)
  {}

  AtomSyntax(Token Tok, tok::FusionKind Base, Syntax *Data)
    : Syntax(SK_Atom), Tok(Tok), FusionInfo({Base, Data})
  {}

protected:
  AtomSyntax(SyntaxKind K, Token Tok)
    : Syntax(K), Tok(Tok)
  {}

public:
  const Token& getToken() const {
    return Tok;
  }

  bool hasToken(tok::TokenKind K) const {
    return getToken().hasKind(K);
  }

  std::string getSpelling() const {
    return Tok.getSpelling();
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }

  const child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Atom || S->getKind() == SK_Literal;
  }

  clang::SourceLocation getTokenLoc() const {
    return Tok.Loc;
  }

  bool isFused() const {
    return Tok.isFused();
  }

  /// The token for the atom.
  Token Tok;

  tok::FusionKind getFusionBase() const {
    assert(isFused());
    return FusionInfo.Base;
  }

  Syntax *getFusionArg() const {
    assert(isFused());
    return FusionInfo.Data;
  }

private:
  struct {
    tok::FusionKind Base;
    Syntax *Data;
  } FusionInfo;
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
  LiteralSyntax(Token Tok)
    : AtomSyntax(SK_Literal, Tok)
  { }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Literal;
  }

  LiteralSuffix Suffix;
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
struct ListSyntax : Syntax, VectorNode<Syntax>
{
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

struct ArraySyntax : Syntax, VectorNode<Syntax>
{
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
struct CallSyntax : Syntax
{
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
struct ElemSyntax : Syntax
{
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

  Syntax *getArgument(std::size_t N);

  const Syntax *getArgument(std::size_t N) const {
    return const_cast<ElemSyntax*>(this)->getArgument(N);
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
struct MacroSyntax : Syntax
{
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
    return S->getKind() == SK_Macro || S->getKind() == SK_LambdaMacro;
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


// FIXME: this can be deleted once we infer lambda captures
struct LambdaMacroSyntax : MacroSyntax {
  LambdaMacroSyntax(Syntax *Call, Syntax *Block, Syntax *Next, bool Default)
    : MacroSyntax(Call, Block, Next), HasDefault(Default)
  {
    Kind = SK_LambdaMacro;
  }

  bool HasDefault = false;

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_LambdaMacro;
  }
};

struct FileSyntax : Syntax, VectorNode<Syntax>
{
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

struct Attribute
{
  Attribute(Syntax *Arg)
    : Arg(Arg)
  {}

  const Syntax *getArg() const {
    return Arg;
  }

private:
  Syntax *Arg;
};

} // namespace gold

#endif
