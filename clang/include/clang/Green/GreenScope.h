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
    assert(AST.is<clang::Decl *>() &&
           "Adding Decl into non-Decl-associated scope.");
    DeclsInScope.insert(D);
  }

  void removeDecl(clang::Decl *D) {
    assert(AST.is<clang::Decl *>() &&
           "Removing Decl from non-Decl-associated scope.");
    DeclsInScope.erase(D);
  }
};

} // namespace green

#endif
