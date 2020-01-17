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
class DiagnosticsEngine;
class Stmt;

} // namespace clang

namespace gold {

class SyntaxContext;

// Represents the kinds of C++ statements in the standard.
// Each enumerator is annotated with its section in the standard.
enum StatementKind {
  // Does not match any standard C++ statement.
  SK_Unknown,

  // [8.3] A C++ expression-statement.
  SK_Expression,

  // [8.4] A C++ compound-statement.
  SK_Compound,

  // [8.5] A C++ selection-statement.
  // if
  SK_Selection,

  // [8.6] A C++ iteration-statement.
  // for
  SK_Iteration,

  // [8.7] A C++ jump-statement.
  // break, continue, return
  SK_Jump,

  // [8.8] A C++ declaration-statement.
  SK_Declaration,

  // [8.9] A C++ try-block
  SK_Try,
};

struct Statement {
  Statement(StatementKind K)
    : Kind(K)
    {}

  StatementKind Kind;

  union {
    // if this is equivalent ot a C++ expression-statement,
    // the C++ expression that this represents.
    clang::Expr *SubExpr;

  } Data;
};

// Builds a clang::Stmt out of a gold::Syntax node.
class StmtElaborator {
  SyntaxContext &Context;

  clang::ASTContext &CxxAST;

  Sema &SemaRef;

  clang::DiagnosticsEngine &Diags;
public:
  StmtElaborator(SyntaxContext &Context, Sema &SemaRef);

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

  // Identify the C++ class of statement that this represents.
  void identifyStmt(const Syntax *S);


private:
  clang::Stmt *elaborateIfStmt(const MacroSyntax *S);
  clang::Stmt *elaborateElseStmt(const MacroSyntax *S);

};

} // namespace gold

#endif
