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

using AssociationUnion = llvm::PointerUnion3<clang::Decl *, clang::Expr *,
  clang::Stmt *>;

class GreenScope {
  // The parent/enclosing scope of this scope.
  GreenScope *Parent;

  using DeclSetTy = llvm::SmallPtrSet<clang::Decl *, 32>;
  DeclSetTy DeclsInScope;

  unsigned Depth;
public:
  GreenScope *getParent() const { return Parent; }
  unsigned getDepth() const { return Depth; }

  GreenScope(AssociationUnion Association, GreenScope *Parent)
    : Parent(Parent), Association(Association) {
    Depth = Parent ? Parent->getDepth() + 1 : 0;
  }

  using decl_range = llvm::iterator_range<DeclSetTy::iterator>;
  decl_range decls() const {
    return decl_range(DeclsInScope.begin(), DeclsInScope.end());
  }
  bool decl_empty() const { return DeclsInScope.empty(); }

  void addDecl(clang::Decl *D) {
    assert(Association.is<clang::Decl *>() &&
           "Adding Decl into non-Decl-associated scope.");
    DeclsInScope.insert(D);
  }

  void removeDecl(clang::Decl *D) {
    assert(Association.is<clang::Decl *>() &&
           "Removing Decl from non-Decl-associated scope.");
    DeclsInScope.erase(D);
  }

  // The clang entity that this scope is associated with.
  AssociationUnion Association;
};

} // namespace green

#endif
