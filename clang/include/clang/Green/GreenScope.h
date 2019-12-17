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
/// the definition, and the corresponding C++ declaration.
struct Declaration {
  /// The (binary) operator that introduces the definition. This is null
  /// for the top-level file declaration.
  Syntax *Operator = nullptr;

  /// The declarator (form of declaration).
  Syntax *Declarator = nullptr;

  /// The definition.
  Syntax *Definition = nullptr;

  /// The corresponding C++ declaration.
  clang::Decl* Cxx = nullptr;
};

/// Stores information about declarations and the scope they were stored in.
/// Associated with a C++ AST term (declaration, expression, or statement).
class GreenScope {
  /// The parent/enclosing scope of this scope.
  GreenScope *Parent;

  /// The syntactic term associated with this scope.
  const Syntax *CST;

  // The C++ AST tree associated with this term.
  using Term = llvm::PointerUnion3<clang::Decl *, clang::Expr *, clang::Stmt *>;
  Term AST;

  /// Declarations declared in this scope.
  using DeclSetTy = llvm::SmallPtrSet<clang::Decl *, 32>;
  DeclSetTy DeclsInScope;

  unsigned Depth;
public:
  GreenScope(const Syntax *S, Term A, GreenScope *P)
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

  using decl_range = llvm::iterator_range<DeclSetTy::iterator>;

  decl_range decls() const {
    return decl_range(DeclsInScope.begin(), DeclsInScope.end());
  }

  bool decl_empty() const { return DeclsInScope.empty(); }

  void addDecl(clang::Decl *D) {
    DeclsInScope.insert(D);
  }

  void removeDecl(clang::Decl *D) {
    DeclsInScope.erase(D);
  }
};

} // namespace green

#endif
