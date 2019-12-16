//===- GreenScope.h - Simple scope used in Green parsing ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenScope interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENSCOPE_H
#define CLANG_GREEN_GREENSCOPE_H

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/iterator_range.h"

namespace green {

struct Syntax;

/// A declaration is stores information about the declaration of an
/// identifier. It binds together the declaring operator, the declarator,
/// the definition, and the some corresponding C++ declaration.
struct Declaration {
  Declaration(clang::IdentifierInfo *Id,
              const Syntax *Op,
              const Syntax *Decl,
              const Syntax *Init)
    : Id(Id), Op(Op), Decl(Decl), Init(Init)
  { }

  /// The identifier for the declaration.
  const clang::IdentifierInfo *Id = nullptr;

  /// The top-level operator that forms the declaration or definition.
  const Syntax *Op = nullptr;

  /// The declarator (form of declaration).
  const Syntax *Decl = nullptr;

  /// The initializer or definition.
  const Syntax *Init = nullptr;

  /// The corresponding C++ declaration.
  clang::Decl* Cxx = nullptr;
};


class GreenScope {
  /// The parent/enclosing scope of this scope.
  GreenScope *Parent;

  /// The syntactic term associated with this scope.
  const Syntax *CST;

  /// The C++ AST tree associated with this term.
  using TermType = llvm::PointerUnion3<clang::Decl *, clang::Expr *, clang::Stmt *>;
  TermType AST;

  /// The mapping of declarations to its construction.
  ///
  /// FIXME: For overloading a single identifier can refer to a set of
  /// declarations. We'll need to adjust this in order to make it work.
  using IdMapType = llvm::DenseMap<clang::IdentifierInfo const*, Declaration *>;
  IdMapType IdMap;

  /// The mapping of original syntax to its construction.
  using DeclMapType = llvm::DenseMap<const Syntax *, Declaration *>;
  DeclMapType DeclMap;

  // /// Declarations declared in this scope.
  // using DeclSetTy = llvm::SmallPtrSet<clang::Decl *, 32>;
  // DeclSetTy DeclsInScope;

  unsigned Depth;

public:
  GreenScope(const Syntax *S, TermType A, GreenScope *P)
    : Parent(P), CST(S), AST(A) {
    Depth = Parent ? Parent->getDepth() + 1 : 0;
  }

  /// The parent of this scope.
  GreenScope *getParent() const {
    return Parent;
  }

  /// The depth of the scope.
  unsigned getDepth() const {
    return Depth;
  }

  /// The original, concrete term associated with the scope.
  const Syntax *getConcreteTerm() const {
    return CST;
  }

  /// True if the scope corresponds to a declaration.
  bool isDeclarationScope() const {
    return AST.is<clang::Decl *>();
  }

  /// True if the scope corresponds to a statement.
  bool isStatementScope() const {
    return AST.is<clang::Stmt *>();
  }

  /// Returns the associated declaration.
  clang::Decl *getDeclaration() const {
    assert(isDeclarationScope());
    return AST.get<clang::Decl *>();
  }

  /// Returns the associated statement.
  clang::Stmt *getStatement() const {
    assert(isStatementScope());
    return AST.get<clang::Stmt *>();
  }

  /// Adds a declaration to this scope.
  void addDecl(Declaration *D) {
    assert(DeclMap.count(D->Op) == 0);
    DeclMap.try_emplace(D->Op, D);

    // FIXME: If D is overloaded, then we need to add this to the declaration
    // set instead of just forcing it into place.
    IdMap.try_emplace(D->Id, D);
  }

  /// Finds a declaration with the given name in this scope.
  ///
  /// FIXME: This could return an overload set.
  Declaration *findDecl(const clang::IdentifierInfo *Id) const {
    auto Iter = IdMap.find(Id);
    if (Iter == IdMap.end())
      return nullptr;
    return Iter->second;
  }

  /// Finds the declaration corresponding to the given syntax or null if
  /// the syntax does not form a declaration.
  Declaration *findDecl(const Syntax *S) const {
    auto Iter = DeclMap.find(S);
    if (Iter == DeclMap.end())
      return nullptr;
    return Iter->second;
  }
};

} // namespace green

#endif
