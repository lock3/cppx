//===- Syntax.h - Classes for representing syntaxes -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Syntax class hierarchy.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_SYNTAX_H
#define CLANG_GREEN_SYNTAX_H

#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"

#include "clang/GreenAST/SyntaxIterator.h"
#include "clang/GreenParse/SymbolTable.h"

#include <memory>
#include <string>
#include <vector>

namespace usyntax {

// Syntax types.
struct Syntax {
  clang::SourceLocation Loc;

  enum SyntaxKind {
    SK_ConstInt,
    SK_ConstString,
    SK_ConstPath,
    SK_Ident,
    SK_Call,
    SK_Attr,
    SK_Macro,
    SK_Escape,
  };

public:
  Syntax() = delete;
  Syntax(SyntaxKind SK, clang::SourceLocation Loc) noexcept :
    Loc(Loc), Kind(SK) {}
  virtual ~Syntax() {}

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

private:
  const SyntaxKind Kind;
};

struct SyntaxConstInt : Syntax {
  int64_t value;

  SyntaxConstInt(clang::SourceLocation Loc, int64_t _value) noexcept
    : Syntax(SK_ConstInt, Loc), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstInt;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

};

struct SyntaxConstString : Syntax {
  std::string value;

  SyntaxConstString(clang::SourceLocation Loc, const std::string &_value) noexcept
    : Syntax(SK_ConstString, Loc), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstString;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

struct SyntaxConstPath : Syntax {
  std::string value;

  SyntaxConstPath(clang::SourceLocation Loc, const std::string &_value) noexcept
    : Syntax(SK_ConstPath, Loc), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstPath;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

struct SyntaxIdent : Syntax {
  Syntax *qualifier;
  std::string name;

  SyntaxIdent(clang::SourceLocation Loc, Syntax *_qualifier,
               const std::string &_name) noexcept
    : Syntax(SK_Ident, Loc), qualifier(_qualifier), name(_name) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Ident;
  }

  child_range children() {
    return child_range(&qualifier, &qualifier + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxIdent *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxCall : Syntax {
  bool may_fail;

  // TODO: Make this a trailing object?
  Syntax *call_function;
  std::vector<Syntax *> call_parameters;

  SyntaxCall(
    clang::SourceLocation Loc, bool _may_fail,
    Syntax *_call_function,
    const std::vector<Syntax *> &_call_parameters) noexcept
    : Syntax(SK_Call, Loc), may_fail(_may_fail),
      call_function(_call_function),
      call_parameters(_call_parameters)
    {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Call;
  }

  child_range children() {
    return child_range(&call_function, &call_function + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxCall *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxAttr : Syntax {
private:
  enum {BASE, ATTR, END};

  Syntax *SubSyntaxes[END];
public:
  SyntaxAttr(clang::SourceLocation Loc, Syntax *_base,
             Syntax *_attr) noexcept
    : Syntax(SK_Attr, Loc) {
    SubSyntaxes[BASE] = _base;
    SubSyntaxes[ATTR] = _attr;
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Attr;
  }

  child_range children() {
    return child_range(&SubSyntaxes[0], &SubSyntaxes[0] + END);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxAttr *>(this)->children();
    return const_child_range(Children);
  }

  const Syntax *getBase() const {
    return SubSyntaxes[BASE];
  }

  const Syntax *getAttr() const {
    return SubSyntaxes[ATTR];
  }
};

struct Clause {
  usyntax::res_t keyword;
  std::vector<Syntax *> attrs, body;
};

struct SyntaxMacro : Syntax {
  Syntax *macro;
  std::vector<Clause> clauses;

  SyntaxMacro(clang::SourceLocation Loc, Syntax *_macro,
               const std::vector<Clause> &_clauses)
    : Syntax(SK_Macro, Loc), macro(_macro), clauses(_clauses) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Macro;
  }

  child_range children() {
    return child_range(&macro, &macro + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxMacro *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxEscape : Syntax {
  Syntax *escaped;

  SyntaxEscape(clang::SourceLocation Loc,
                Syntax *_escaped) noexcept
    : Syntax(SK_Escape, Loc), escaped(_escaped) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Escape;
  }

  child_range children() {
    return child_range(&escaped, &escaped + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxEscape *>(this)->children();
    return const_child_range(Children);
  }
};

} // namespace usyntax

#endif
