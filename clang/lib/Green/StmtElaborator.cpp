//=== StmtElaborator.cpp - Elaboration for Green Statemtns ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the StmtElaborator interface, which creates
//  clang::Stmt nodes out of Green "statements".
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/StmtElaborator.h"

namespace green {

using namespace clang;

StmtElaborator::StmtElaborator(ASTContext &CxxAST, GreenSema &SemaRef)
  : CxxAST(CxxAST), SemaRef(SemaRef),
    ExprElab(CxxAST, SemaRef)
{
}

Stmt *
StmtElaborator::elaborateStmt(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S));
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if (isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));

  return nullptr;
}

Stmt *
StmtElaborator::elaborateAtom(const AtomSyntax *S) {
  ExprElaborator ExEl(CxxAST, SemaRef);
  return ExEl.elaborateExpr(S);
}

static Stmt *
createDeclStmt(ASTContext &CxxAST, GreenSema &SemaRef,
               const CallSyntax *S) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

  // TODO: can this be something other than a name?
  const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);

  clang::Sema &ClangSema = SemaRef.getCxxSema();
  IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
  DeclarationNameInfo DNI(II, S->Loc);
  LookupResult R(ClangSema, DNI, Sema::LookupAnyName);
  SemaRef.LookupName(R, SemaRef.getCurrentScope());

  if (R.empty()) {
    Elaborator DeclElab(SemaRef.getContext(), SemaRef);
    Decl *Declaration = DeclElab.elaborateDeclSyntax(S);

    StmtResult Res =
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

Stmt *
StmtElaborator::elaborateCall(const CallSyntax *S) {
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  IdentifierInfo *Spelling = &CxxAST.Idents.get(Callee->Tok.getSpelling());

  // A typed declaration.
  // FIXME : what about 'x = 3:int'
  if (Spelling == SemaRef.OperatorColonII) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

    // TODO: can this be something other than a name?
    const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);
    clang::Sema &ClangSema = SemaRef.getCxxSema();
    IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
    DeclarationNameInfo DNI(II, S->Loc);
    LookupResult R(ClangSema, DNI, Sema::LookupAnyName);
    SemaRef.LookupName(R, SemaRef.getCurrentScope());


    if (R.empty())
      return createDeclStmt(CxxAST, SemaRef, S);
    else {
      ExprElaborator ExprElab(CxxAST, SemaRef);
      return ExprElab.elaborateCall(S);
    }
  }

  ExprElaborator ExprElab(CxxAST, SemaRef);
  return ExprElab.elaborateCall(S);
}

Stmt *StmtElaborator::elaborateIfStmt(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  Expr *ConditionExpr;
  ExprElaborator ExEl(CxxAST, SemaRef);
  if (const ArraySyntax *BlockCond = dyn_cast<ArraySyntax>(Call->getArguments()))
    ConditionExpr = ExEl.elaborateBlockCondition(BlockCond);
  else if (const ListSyntax *Args = dyn_cast<ListSyntax>(Call->getArguments()))
    ConditionExpr = ExEl.elaborateExpr(Args->getChild(0));
  else
    return nullptr;

  Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr, S->Loc,
                                        ConditionExpr,
                                        Sema::ConditionKind::Boolean);

  SemaRef.enterScope(SK_Block, S);

  Stmt *Then;
  if (isa<ArraySyntax>(S->getBlock()) || isa<ListSyntax>(S->getBlock()))
    Then = elaborateBlock(S->getBlock());
  else
    // This could be a single expression in the case of `if(cond) then expr`
    Then = elaborateStmt(S->getBlock());

  SemaRef.leaveScope(S);

  Stmt *Else = nullptr;
  SourceLocation ElseLoc;
  if (S->getNext()) {
    Else = elaborateMacro(cast<MacroSyntax>(S->getNext()));
    ElseLoc = S->getNext()->Loc;
  }

  StmtResult If = SemaRef.getCxxSema().ActOnIfStmt(
    S->Loc, /*Constexpr=*/false, /*InitStmt=*/nullptr,
    Condition, Then, ElseLoc, Else);

  if (If.isInvalid())
    return nullptr;

  return If.get();
}

Stmt *StmtElaborator::elaborateElseStmt(const MacroSyntax *S) {
  // The else block might be an if statement (i.e., else if)
  if (isa<MacroSyntax>(S->getBlock()))
    return elaborateMacro(cast<MacroSyntax>(S->getBlock()));

  SemaRef.enterScope(SK_Block, S);

  // Otherwise, it's just a normal else block.
  Stmt *Else = elaborateBlock(S->getBlock());

  SemaRef.leaveScope(S);
  return Else;
}

Stmt *
StmtElaborator::elaborateMacro(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  const AtomSyntax *MacroCallee = cast<AtomSyntax>(Call->getCallee());
  IdentifierInfo *CallName = &CxxAST.Idents.get(MacroCallee->Tok.getSpelling());

  if (CallName == SemaRef.OperatorIfII)
    return elaborateIfStmt(S);
  else if (CallName == SemaRef.OperatorElseII)
    return elaborateElseStmt(S);

  llvm::errs() << "Unsupported macro.\n";
  return nullptr;
}

Stmt *
StmtElaborator::elaborateBlock(const Syntax *S) {
  if (isa<ErrorSyntax>(S))
    return nullptr;

  assert((isa<ArraySyntax>(S) || isa<ListSyntax>(S)) &&
         "Cannot create block out of non-list syntax.");

  llvm::SmallVector<Stmt *, 16> Results;

  if (isa<ArraySyntax>(S))
    elaborateBlockForArray(cast<ArraySyntax>(S), Results);
  if (isa<ListSyntax>(S))
    elaborateBlockForList(cast<ListSyntax>(S), Results);

  CompoundStmt *Block =
    CompoundStmt::Create(CxxAST, Results, S->Loc, S->Loc);

  return Block;
}

void
StmtElaborator::elaborateBlockForArray(const ArraySyntax *S,
                                       llvm::SmallVectorImpl<Stmt *> &Results) {
  for (const Syntax *Child : S->children()) {
    if (isa<ListSyntax>(Child)) {
      elaborateBlockForList(cast<ListSyntax>(Child), Results);
      continue;
    } else {
      Stmt *NewStmt = elaborateStmt(Child);
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
                                      llvm::SmallVectorImpl<Stmt *> &Results) {
  for (const Syntax *Child : S->children()) {
    Stmt *NewStmt = elaborateStmt(Child);
    if (!NewStmt)
      continue;
    Results.push_back(NewStmt);
  }
}

} // namespace green
