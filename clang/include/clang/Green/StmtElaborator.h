//=== StmtElaborator.h - Elaboration for Green Statemtns ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the StmtElaborator interface, which creates
//  clang::Stmt nodes out of Green "statements".
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_STMTELABORATOR_H
#define CLANG_GREEN_STMTELABORATOR_H

#include "llvm/ADT/SmallVector.h"

#include "clang/Green/ExprElaborator.h"
#include "clang/Green/Syntax.h"

namespace clang {

class ASTContext;
class Stmt;

} // namespace clang

namespace green {

// Builds a clang::Stmt out of a green::Syntax node.
class StmtElaborator {

  clang::ASTContext &CxxAST;

  GreenSema &SemaRef;

  ExprElaborator ExprElab;
public:
  StmtElaborator(clang::ASTContext &CxxAST, GreenSema &SemaRef);

  clang::Stmt *elaborateStmt(const Syntax *S);
  clang::Stmt *elaborateCall(const CallSyntax *S);
  clang::Stmt *elaborateAtom(const AtomSyntax *S);
  clang::Stmt *elaborateMacro(const MacroSyntax *S);

  // Build a clang::CompoundStmt out of a block of green::Syntax nodes (e.g.,
  // an ArraySyntax or ListSyntax).
  clang::Stmt *elaborateBlock(const Syntax *S);
  void elaborateBlockForArray(const ArraySyntax *S,
                              llvm::SmallVectorImpl<clang::Stmt *> &Results);
  void elaborateBlockForList(const ListSyntax *S,
                             llvm::SmallVectorImpl<clang::Stmt *> &Results);


};

} // namespace green

#endif
