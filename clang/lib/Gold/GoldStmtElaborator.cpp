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
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"

namespace gold {

using clang::cast_or_null;

StmtElaborator::StmtElaborator(SyntaxContext &Context, Sema &SemaRef)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
    Diags(CxxAST.getSourceManager().getDiagnostics())
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
  ExprElaborator ExEl(Context, SemaRef);
  ExprElaborator::Expression Expression = ExEl.elaborateExpr(S);

  if (Expression.is<clang::TypeSourceInfo *>()) {
    Diags.Report(S->getTokenLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  return Expression.get<clang::Expr *>();
}

static clang::Stmt *
createDeclStmt(clang::ASTContext &CxxAST, Sema &SemaRef,
               const CallSyntax *S) {
  // FIXME: elaborate this expression, it might not be a name.
  
  const AtomSyntax *Name = cast<AtomSyntax>(S->getArgument(0));

  // TODO: elaborate the name if we need to.
  // ExprElaborator LHSElab(CxxAST, SemaRef);
  // ExprElaborator::Expression NameExpr = LHSElab.elaborateExpr(S->getArgument(0));

  clang::Sema &ClangSema = SemaRef.getCxxSema();
  clang::IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
  clang::DeclarationNameInfo DNI(II, Name->getLoc());
  clang::LookupResult R(ClangSema, DNI, clang::Sema::LookupAnyName);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  if (R.empty()) {
    Elaborator DeclElab(SemaRef.getContext(), SemaRef);
    clang::Decl *Declaration = DeclElab.elaborateDeclSyntax(S);

    clang::StmtResult Res =
      ClangSema.ActOnDeclStmt(ClangSema.ConvertDeclToDeclGroup(Declaration),
                              Name->getLoc(), S->getLoc());
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

static clang::Stmt *
createDeclStmt(clang::ASTContext &CxxAST, Sema &SemaRef,
               clang::Decl *D, clang::SourceLocation StartLoc,
               clang::SourceLocation EndLoc) {
  clang::Sema &CxxSema = SemaRef.getCxxSema();
   clang::StmtResult Res =
      CxxSema.ActOnDeclStmt(CxxSema.ConvertDeclToDeclGroup(D), StartLoc, EndLoc);
   return !Res.isInvalid() ? Res.get() : nullptr;
}

namespace {
  /// Helper class that marks all of the declarations referenced by
  /// potentially-evaluated subexpressions as "referenced".
  /// Effectively a less complicated version of the EvaluatedExprMarker
  /// found in Sema/SemaExpr.cpp
  struct ExprMarker : public clang::EvaluatedExprVisitor<ExprMarker> {
    clang::ASTContext &CxxAST;
    Sema &SemaRef;
    typedef EvaluatedExprVisitor<ExprMarker> Inherited;

    ExprMarker(clang::ASTContext &CxxAST, Sema &SemaRef)
      : Inherited(CxxAST), CxxAST(CxxAST), SemaRef(SemaRef)
      {}

    void VisitDeclRefExpr(clang::DeclRefExpr *E) {
      // FIXME: references to virtual methods may cause problems here.
      SemaRef.getCxxSema().MarkAnyDeclReferenced(E->getBeginLoc(),
                                                 E->getDecl(),
                                                 /*OdrUsed=*/false);
      // E->getDecl()->markUsed(CxxAST);
    }
  };
} // anonymous namespace

static clang::Stmt *
elaborateDefaultCall(SyntaxContext &Context, Sema &SemaRef, const CallSyntax *S) {
  ExprElaborator ExprElab(Context, SemaRef);
  ExprElaborator::Expression Expression = ExprElab.elaborateCall(S);

  if (Expression.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  return Expression.get<clang::Expr *>();
}

clang::Stmt *
StmtElaborator::elaborateCall(const CallSyntax *S) {
  if (isa<ElemSyntax>(S->getCallee()))
    return elaborateDefaultCall(Context, SemaRef, S);

  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  FusedOpKind OpKind = getFusedOpKind(SemaRef, Callee->getSpelling());

  // If we're seeing a declaration-like syntax for the first time in
  // block scope, create a Declaration for it.
  if (SemaRef.getCurrentScope()->isBlockScope()) {
    if (OpKind == FOK_Colon || OpKind == FOK_Equals) {
      Elaborator E(Context, SemaRef);
      clang::Decl *N = E.elaborateDeclSyntax(S);
      if (N)
        return createDeclStmt(CxxAST, SemaRef, N, S->getLoc(),
                              S->getArgument(0)->getLoc());
    }
  }

  // Otherwise, this is just a regular statement-expression, so
  // try and elaborate it as such.
  switch (OpKind) {
  case FOK_Colon: {
    // FIXME: fully elaborate the name expression.
    const AtomSyntax *Name = cast<AtomSyntax>(S->getArgument(0));
    clang::Sema &ClangSema = SemaRef.getCxxSema();
    clang::IdentifierInfo *II = &CxxAST.Idents.get(Name->Tok.getSpelling());
    clang::DeclarationNameInfo DNI(II, S->getLoc());
    clang::LookupResult R(ClangSema, DNI, clang::Sema::LookupAnyName);
    SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());


    if (R.empty())
      return createDeclStmt(CxxAST, SemaRef, S);
    break;
  }

  case FOK_Equals: {
    ExprElaborator LHSElab(Context, SemaRef);
    ExprElaborator::Expression NameExpr =
      LHSElab.elaborateExpr(S->getArgument(0));

    if (NameExpr.is<clang::TypeSourceInfo *>()) {
      Diags.Report(S->getArgument(0)->getLoc(), clang::diag::err_expected_expression);
      return nullptr;
    }

    ExprElaborator RHSElab(Context, SemaRef);
    ExprElaborator::Expression InitExpr =
      RHSElab.elaborateExpr(S->getArgument(1));

    // We didn't create an expression for this, so build a decl.
    if (NameExpr.isNull()) {
      auto *DS = cast_or_null<clang::DeclStmt>(createDeclStmt(CxxAST, SemaRef, S));

      if (!DS)
        return nullptr;

      // TODO: support type aliases.
      assert(!InitExpr.is<clang::TypeSourceInfo *>() && "Aliases not supported yet.");

      for (clang::Decl *D : DS->decls())
        SemaRef.getCxxSema().AddInitializerToDecl(
          D, InitExpr.get<clang::Expr *>(), /*DirectInit=*/true);

      return DS;
    }

    clang::ExprResult Assignment =
      SemaRef.getCxxSema().ActOnBinOp(SemaRef.getCxxSema().getCurScope(),
                                      S->getLoc(), clang::tok::equal,
                                      NameExpr.get<clang::Expr *>(),
                                      InitExpr.get<clang::Expr *>());
    if (Assignment.isInvalid()) {
      llvm::errs() << "Unexpected assignment.\n";
      return nullptr;
    }

    // We can readily assume anything here is getting used.
    ExprMarker(CxxAST, SemaRef).Visit(NameExpr.get<clang::Expr *>());
    ExprMarker(CxxAST, SemaRef).Visit(InitExpr.get<clang::Expr *>());
    return Assignment.get();
  }

  case FOK_Return: {
    ExprElaborator::Expression RetVal =
      ExprElaborator(Context, SemaRef).elaborateExpr(S->getArgument(0));

    if (RetVal.isNull())
      return nullptr;

    if (RetVal.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                           clang::diag::err_expected_lparen_after_type);
      return nullptr;
    }

    ExprMarker(CxxAST, SemaRef).Visit(RetVal.get<clang::Expr *>());
    clang::StmtResult ReturnResult = SemaRef.getCxxSema().
      BuildReturnStmt(S->getCallee()->getLoc(), RetVal.get<clang::Expr *>());
    return ReturnResult.get();
  }

  default:
    break; // Silence warning.
  }

  // If all else fails, just see if we can elaborate any expression.
  return elaborateDefaultCall(Context, SemaRef, S);
  // ExprElaborator ExprElab(Context, SemaRef);
  // ExprElaborator::Expression Expression = ExprElab.elaborateCall(S);

  // if (Expression.is<clang::TypeSourceInfo *>()) {
  //   Diags.Report(S->getLoc(), clang::diag::err_expected_expression);
  //   return nullptr;
  // }

  // return Expression.get<clang::Expr *>();
}

clang::Stmt *StmtElaborator::elaborateIfStmt(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  clang::Expr *ConditionExpr;
  ExprElaborator ExEl(Context, SemaRef);
  if (const ArraySyntax *BlockCond = dyn_cast<ArraySyntax>(Call->getArguments())) {
    ExprElaborator::Expression Expression = ExEl.elaborateBlockCondition(BlockCond);

    if (Expression.is<clang::TypeSourceInfo *>()) {
      Diags.Report(BlockCond->getLoc(), clang::diag::err_expected_expression);
      return nullptr;
    }

    ConditionExpr = Expression.get<clang::Expr *>();
  } else if (const ListSyntax *Args = dyn_cast<ListSyntax>(Call->getArguments())) {
    ExprElaborator::Expression Expression = ExEl.elaborateExpr(Args->getChild(0));

    if (Expression.is<clang::TypeSourceInfo *>()) {
      Diags.Report(Args->getChild(0)->getLoc(), clang::diag::err_expected_expression);
      return nullptr;
    }

    ConditionExpr = Expression.get<clang::Expr *>();
  } else {
    return nullptr;
  }

  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr,
                                        S->getLoc(), ConditionExpr,
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
    ElseLoc = S->getNext()->getLoc();
  }

  clang::StmtResult If = SemaRef.getCxxSema().ActOnIfStmt(
    S->getLoc(), /*Constexpr=*/false, /*InitStmt=*/nullptr,
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

  const AtomSyntax *Callee = cast<AtomSyntax>(Call->getCallee());
  FusedOpKind OpKind = getFusedOpKind(SemaRef, Callee->getSpelling());

  switch (OpKind) {
  case FOK_If:
    return elaborateIfStmt(S);
  case FOK_Else:
    return elaborateElseStmt(S);
  default:
    llvm::errs() << "Unsupported macro.\n";
    return nullptr;
  }
}

clang::Stmt *
StmtElaborator::elaborateBlock(const Syntax *S) {
  if (isa<ErrorSyntax>(S))
    return nullptr;

  SemaRef.enterScope(SK_Block, S);

  llvm::SmallVector<clang::Stmt *, 16> Results;
  clang::SourceLocation StartLoc, EndLoc;
  if (auto *Array = dyn_cast<ArraySyntax>(S)) {
    elaborateBlockForArray(cast<ArraySyntax>(S), Results);

    StartLoc = Array->getChild(0)->getLoc();
    EndLoc = Array->getChild(Array->getNumChildren() - 1)->getLoc();
  } else if (auto *List = dyn_cast<ListSyntax>(S)) {
    elaborateBlockForList(cast<ListSyntax>(S), Results);

    StartLoc = List->getChild(0)->getLoc();
    EndLoc = List->getChild(List->getNumChildren() - 1)->getLoc();
  } else {
    // This is some sort of one-line block like `f(x : int) = x * x`
    // FIXME: Make sure this only creates return stmts in a function context.
    ExprElaborator::Expression ReturnVal =
      ExprElaborator(Context, SemaRef).elaborateExpr(S);

    if (ReturnVal.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_expected_lparen_after_type);
    }

    if (ReturnVal.is<clang::Expr *>()) {
      ExprMarker(CxxAST, SemaRef).Visit(ReturnVal.get<clang::Expr *>());

      clang::StmtResult ReturnRes = SemaRef.getCxxSema().
        BuildReturnStmt(S->getLoc(), ReturnVal.get<clang::Expr *>());
      if (!ReturnRes.isInvalid()) 
        Results.push_back(ReturnRes.get());
    }

    StartLoc = EndLoc = S->getLoc();
  }

  clang::CompoundStmt *Block =
    clang::CompoundStmt::Create(CxxAST, Results, StartLoc, EndLoc);

  SemaRef.leaveScope(S);
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
