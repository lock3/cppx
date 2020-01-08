//=== GoldStmtElaborator.cpp - Elaboration for Gold Stmts -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the StmtElaborator interface, which creates
//  clang::Stmt nodes out of Gold "statements".
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldStmtElaborator.h"

namespace gold {

StmtElaborator::StmtElaborator(clang::ASTContext &CxxAST, Sema &SemaRef)
  : CxxAST(CxxAST), SemaRef(SemaRef),
    ExprElab(CxxAST, SemaRef)
{
}

clang::Stmt *
StmtElaborator::elaborateStmt(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S));
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if (isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));

  return nullptr;
}

clang::Stmt *
StmtElaborator::elaborateAtom(const AtomSyntax *S) {
  ExprElaborator ExEl(CxxAST, SemaRef);
  ExprElaborator::Expression Expression = ExEl.elaborateExpr(S);

  if (Expression.is<clang::TypeSourceInfo *>()) {
    llvm::errs() << "Expected expression.\n";
    return nullptr;
  }

  return Expression.get<clang::Expr *>();
}

static clang::Stmt *
createDeclStmt(clang::ASTContext &CxxAST, Sema &SemaRef,
               const CallSyntax *S) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

  // TODO: can this be something other than a name?
  const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);

  clang::Sema &ClangSema = SemaRef.getCxxSema();
  clang::IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
  clang::DeclarationNameInfo DNI(II, S->Loc);
  clang::LookupResult R(ClangSema, DNI, clang::Sema::LookupAnyName);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  if (R.empty()) {
    Elaborator DeclElab(SemaRef.getContext(), SemaRef);
    clang::Decl *Declaration = DeclElab.elaborateDeclSyntax(S);

    clang::StmtResult Res =
      ClangSema.ActOnDeclStmt(ClangSema.ConvertDeclToDeclGroup(Declaration),
                              S->Loc, S->Loc);
    if (!Res.isInvalid())
      return Res.get();
  } else {
    // FIXME: if the name already exists in this scope, it's an obvious error.
    // However, if the name exists in an outer scope, should we shadow it?
    // We'll need lookup to understand the difference.
    llvm::errs() << "This name already exists... what should I do?\n";
  }

  return nullptr;
}

clang::Stmt *
StmtElaborator::elaborateCall(const CallSyntax *S) {
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  clang::IdentifierInfo *Spelling = &CxxAST.Idents.get(Callee->Tok.getSpelling());

  // A typed declaration.
  // FIXME : what about 'x = 3:int'
  if (Spelling == SemaRef.OperatorColonII) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

    // TODO: can this be something other than a name?
    const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);
    clang::Sema &ClangSema = SemaRef.getCxxSema();
    clang::IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
    clang::DeclarationNameInfo DNI(II, S->Loc);
    clang::LookupResult R(ClangSema, DNI, clang::Sema::LookupAnyName);
    SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());


    if (R.empty())
      return createDeclStmt(CxxAST, SemaRef, S);
  }

  ExprElaborator ExprElab(CxxAST, SemaRef);
  ExprElaborator::Expression Expression = ExprElab.elaborateCall(S);

  if (Expression.is<clang::TypeSourceInfo *>()) {
    llvm::errs() << "Expected expression.\n";
    return nullptr;
  }

  return Expression.get<clang::Expr *>();
}

clang::Stmt *StmtElaborator::elaborateIfStmt(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  clang::Expr *ConditionExpr;
  ExprElaborator ExEl(CxxAST, SemaRef);
  if (const ArraySyntax *BlockCond = dyn_cast<ArraySyntax>(Call->getArguments())) {
    ExprElaborator::Expression Expression = ExEl.elaborateBlockCondition(BlockCond);

    if (Expression.is<clang::TypeSourceInfo *>()) {
      llvm::errs() << "Expected expression.\n";
      return nullptr;
    }

    ConditionExpr = Expression.get<clang::Expr *>();
  } else if (const ListSyntax *Args = dyn_cast<ListSyntax>(Call->getArguments())) {
    ExprElaborator::Expression Expression = ExEl.elaborateExpr(Args->getChild(0));

    if (Expression.is<clang::TypeSourceInfo *>()) {
      llvm::errs() << "Expected expression.\n";
      return nullptr;
    }

    ConditionExpr = Expression.get<clang::Expr *>();
  } else {
    return nullptr;
  }

  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr, S->Loc,
                                        ConditionExpr,
                                        clang::Sema::ConditionKind::Boolean);

  SemaRef.enterScope(SK_Block, S);

  clang::Stmt *Then;
  if (isa<ArraySyntax>(S->getBlock()) || isa<ListSyntax>(S->getBlock()))
    Then = elaborateBlock(S->getBlock());
  else
    // This could be a single expression in the case of `if(cond) then expr`
    Then = elaborateStmt(S->getBlock());

  SemaRef.leaveScope(S);

  clang::Stmt *Else = nullptr;
  clang::SourceLocation ElseLoc;
  if (S->getNext()) {
    Else = elaborateMacro(cast<MacroSyntax>(S->getNext()));
    ElseLoc = S->getNext()->Loc;
  }

  clang::StmtResult If = SemaRef.getCxxSema().ActOnIfStmt(
    S->Loc, /*Constexpr=*/false, /*InitStmt=*/nullptr,
    Condition, Then, ElseLoc, Else);

  if (If.isInvalid())
    return nullptr;

  return If.get();
}

clang::Stmt *StmtElaborator::elaborateElseStmt(const MacroSyntax *S) {
  // The else block might be an if statement (i.e., else if)
  if (isa<MacroSyntax>(S->getBlock()))
    return elaborateMacro(cast<MacroSyntax>(S->getBlock()));

  SemaRef.enterScope(SK_Block, S);

  // Otherwise, it's just a normal else block.
  clang::Stmt *Else = elaborateBlock(S->getBlock());

  SemaRef.leaveScope(S);
  return Else;
}

clang::Stmt *
StmtElaborator::elaborateMacro(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  const AtomSyntax *MacroCallee = cast<AtomSyntax>(Call->getCallee());
  clang::IdentifierInfo *CallName = &CxxAST.Idents.get(MacroCallee->Tok.getSpelling());

  if (CallName == SemaRef.OperatorIfII)
    return elaborateIfStmt(S);
  else if (CallName == SemaRef.OperatorElseII)
    return elaborateElseStmt(S);

  llvm::errs() << "Unsupported macro.\n";
  return nullptr;
}

clang::Stmt *
StmtElaborator::elaborateBlock(const Syntax *S) {
  if (isa<ErrorSyntax>(S))
    return nullptr;

  assert((isa<ArraySyntax>(S) || isa<ListSyntax>(S)) &&
         "Cannot create block out of non-list syntax.");

  llvm::SmallVector<clang::Stmt *, 16> Results;

  if (isa<ArraySyntax>(S))
    elaborateBlockForArray(cast<ArraySyntax>(S), Results);
  if (isa<ListSyntax>(S))
    elaborateBlockForList(cast<ListSyntax>(S), Results);

  clang::CompoundStmt *Block =
    clang::CompoundStmt::Create(CxxAST, Results, S->Loc, S->Loc);

  return Block;
}

void
StmtElaborator::elaborateBlockForArray(const ArraySyntax *S,
                                llvm::SmallVectorImpl<clang::Stmt *> &Results) {
  for (const Syntax *Child : S->children()) {
    if (isa<ListSyntax>(Child)) {
      elaborateBlockForList(cast<ListSyntax>(Child), Results);
      continue;
    } else {
      clang::Stmt *NewStmt = elaborateStmt(Child);
      if (!NewStmt) {
        continue;
      }
      Results.push_back(NewStmt);
    }

    // TODO: implement other syntaxes.
    continue;
  }
}

void
StmtElaborator::elaborateBlockForList(const ListSyntax *S,
                                llvm::SmallVectorImpl<clang::Stmt *> &Results) {
  for (const Syntax *Child : S->children()) {
    clang::Stmt *NewStmt = elaborateStmt(Child);
    if (!NewStmt)
      continue;
    Results.push_back(NewStmt);
  }
}

} // namespace gold
