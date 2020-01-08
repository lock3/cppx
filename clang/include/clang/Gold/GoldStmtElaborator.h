//=== GoldStmtElaborator.h - Elaboration for Gold Stmts -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the StmtElaborator interface, which creates
//  clang::Stmt nodes out of Gold "statements".
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_STMTELABORATOR_H
#define CLANG_GOLD_STMTELABORATOR_H

#include "llvm/ADT/SmallVector.h"

#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSyntax.h"

namespace clang {

class ASTContext;
class Stmt;

} // namespace clang

namespace gold {

// Builds a clang::Stmt out of a gold::Syntax node.
class StmtElaborator {

  clang::ASTContext &CxxAST;

  Sema &SemaRef;

  ExprElaborator ExprElab;
public:
  StmtElaborator(clang::ASTContext &CxxAST, Sema &SemaRef);

  clang::Stmt *elaborateStmt(const Syntax *S);
  clang::Stmt *elaborateCall(const CallSyntax *S);
  clang::Stmt *elaborateAtom(const AtomSyntax *S);
  clang::Stmt *elaborateMacro(const MacroSyntax *S);

  // Build a clang::CompoundStmt out of a block of gold::Syntax nodes (e.g.,
  // an ArraySyntax or ListSyntax).
  clang::Stmt *elaborateBlock(const Syntax *S);
  void elaborateBlockForArray(const ArraySyntax *S,
                              llvm::SmallVectorImpl<clang::Stmt *> &Results);
  void elaborateBlockForList(const ListSyntax *S,
                             llvm::SmallVectorImpl<clang::Stmt *> &Results);


private:
  clang::Stmt *elaborateIfStmt(const MacroSyntax *S);
  clang::Stmt *elaborateElseStmt(const MacroSyntax *S);

};

} // namespace gold

#endif
