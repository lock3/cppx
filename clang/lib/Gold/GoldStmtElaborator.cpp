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
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldExprMarker.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"

namespace gold {

using clang::cast_or_null;
using clang::dyn_cast;
using clang::cast;

StmtElaborator::StmtElaborator(SyntaxContext &Context, Sema &SemaRef)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
    Diags(CxxAST.getSourceManager().getDiagnostics())
{
}

clang::Stmt *
StmtElaborator::elaborateStmt(const Syntax *S) {
  assert(S && "invalid statement elaboration");
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S));
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if (isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));

  // If the statement kind is unknown then simply punt to the expression
  // elaborator.
  ExprElaborator Elab(Context, SemaRef);
  auto Res = Elab.elaborateExpr(S);
  if (!Res)
    return nullptr;
  auto CheckExpr = SemaRef.getCxxSema().CheckPlaceholderExpr(Res);
  if (CheckExpr.isInvalid())
    return nullptr;
  auto ExprStmt =
    SemaRef.getCxxSema().ActOnExprStmt(CheckExpr.get(), /*discardedValue*/true);
  if (ExprStmt.isInvalid())
    return nullptr;

  return ExprStmt.get();
}

clang::Stmt *
StmtElaborator::elaborateAtom(const AtomSyntax *S) {
  Token T = S->Tok;
  // Check if we have a statement-only identifier.
  switch (T.getKind()) {
  case tok::ContinueKeyword: {
    auto StmtResult = SemaRef.getCxxSema().ActOnContinueStmt(S->getLoc(),
        SemaRef.getCurClangScope());
    return StmtResult.get();
  }

  case tok::BreakKeyword: {
    auto StmtResult = SemaRef.getCxxSema().ActOnBreakStmt(S->getLoc(),
        SemaRef.getCurClangScope());
    return StmtResult.get();
  }

  default: {
    ExprElaborator ExEl(Context, SemaRef);
    clang::Expr *Res = ExEl.elaborateExpr(S);
    auto CheckExpr = SemaRef.getCxxSema().CheckPlaceholderExpr(Res);
    if (CheckExpr.isInvalid())
      return nullptr;
    return CheckExpr.get();
  }
  } // switch (T.getKind())
}

static clang::Stmt *
createDeclStmt(clang::ASTContext &CxxAST, Sema &SemaRef,
               const CallSyntax *S) {
  clang::Sema &ClangSema = SemaRef.getCxxSema();

  Elaborator DeclElab(SemaRef.getContext(), SemaRef);
  clang::Decl *Declaration = DeclElab.elaborateDeclSyntax(S);

  clang::StmtResult Res =
    ClangSema.ActOnDeclStmt(ClangSema.ConvertDeclToDeclGroup(Declaration),
                            S->getCallee()->getLoc(), S->getLoc());
  if (!Res.isInvalid())
    return Res.get();

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

static clang::Stmt *
elaborateDefaultCall(SyntaxContext &Context, Sema &SemaRef, const CallSyntax *S) {
  ExprElaborator ExprElab(Context, SemaRef);
  auto *Stmt = ExprElab.elaborateCall(S);
  if (!Stmt)
    return nullptr;

  if (auto *PartialExpr = dyn_cast<clang::CppxPartialEvalExpr>(Stmt)) {
    Stmt = PartialExpr->forceCompleteExpr();
    if (!Stmt)
      return nullptr;
  }
  clang::ExprResult Res(Stmt);
  auto StmtResult = SemaRef.getCxxSema().ActOnExprStmt(Res,
                                                      /*discardedValue*/true);
  return StmtResult.get();
}

clang::Stmt *
StmtElaborator::elaborateCall(const CallSyntax *S) {
  if (!isa<AtomSyntax>(S->getCallee())) {
    return elaborateDefaultCall(Context, SemaRef, S);
  }

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
  case FOK_Throw:
    return elaborateThrowStmt(S);
  case FOK_Colon: {
    return createDeclStmt(CxxAST, SemaRef, S);
  }

  case FOK_Equals: {
    ExprElaborator LHSElab(Context, SemaRef);
    clang::Expr *NameExpr = LHSElab.elaborateExpr(S->getArgument(0));
    ExprElaborator RHSElab(Context, SemaRef);
    clang::Expr *InitExpr = RHSElab.elaborateExpr(S->getArgument(1));

    // We didn't create an expression for this, so build a decl.
    if (!NameExpr) {
      auto *DS = cast_or_null<clang::DeclStmt>(createDeclStmt(CxxAST, SemaRef, S));

      if (!DS)
        return nullptr;

      for (clang::Decl *D : DS->decls())
        SemaRef.getCxxSema().AddInitializerToDecl(D, InitExpr, /*DirectInit=*/true);

      return DS;
    }

    if (!InitExpr)
      return nullptr;

    clang::ExprResult BinOpResult =
      SemaRef.getCxxSema().ActOnBinOp(SemaRef.getCxxSema().getCurScope(),
                                      S->getLoc(), clang::tok::equal,
                                      NameExpr, InitExpr);
    if (BinOpResult.isInvalid())
      return nullptr;
    auto CompletedBinOpRes = SemaRef.getCxxSema().ActOnExprStmt(BinOpResult);
    // We can readily assume anything here is getting used.
    ExprMarker(CxxAST, SemaRef).Visit(NameExpr);
    ExprMarker(CxxAST, SemaRef).Visit(InitExpr);
    return CompletedBinOpRes.get();
  }

  case FOK_Return: {
    clang::StmtResult ReturnResult;
    if (S->getNumArguments()) {
      clang::Expr *RetVal =
        ExprElaborator(Context, SemaRef).elaborateExpr(S->getArgument(0));
      if (!RetVal)
        return nullptr;
      ExprMarker(CxxAST, SemaRef).Visit(RetVal);
      ReturnResult = SemaRef.getCxxSema().ActOnReturnStmt(
                  S->getCallee()->getLoc(), RetVal, SemaRef.getCurClangScope());
    } else {
      ReturnResult = SemaRef.getCxxSema().ActOnReturnStmt(
                 S->getCallee()->getLoc(), nullptr, SemaRef.getCurClangScope());
    }
    return ReturnResult.get();
  }

  default:
    break; // Silence warning.
  }

  // If all else fails, just see if we can elaborate any expression.
  return elaborateDefaultCall(Context, SemaRef, S);
}

static bool isConstExprIf(Sema &SemaRef, Attributes &Attrs, bool &IsConstExpr) {
  return locateValidAttribute(Attrs,
    // OnAttr
    [&](const Syntax *Attr) -> bool{
      std::string ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        if (ActualName == "constexpr") {
          IsConstExpr = true;
          return true;
        }
        return false;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "constexpr") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          IsConstExpr = true;
          return true;
        }
        return false;
      }
      return false;
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Attr)) {
        if (Atom->getSpelling() == "constexpr") {
          return true;
        }
      }
      return false;
    },
    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr) {
      return SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                                  clang::diag::err_conflicting_attributes)
                                  << FirstAttr->getLoc()
                                  << DuplicateAttr->getLoc();
    });
}

clang::Stmt *StmtElaborator::elaborateIfStmt(const MacroSyntax *S) {
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());
  clang::Expr *ConditionExpr;
  ExprElaborator ExEl(Context, SemaRef);
  bool IsConstExprIfStmt = false;
  Attributes Attrs;
  // Gathering attributes from if statement.
  if (const auto *Call = dyn_cast<CallSyntax>(S->getCall())) {
    if (const auto *IfAtom = dyn_cast<AtomSyntax>(Call->getCallee())) {
      for (const Attribute *Attr : IfAtom->getAttributes()) {
        Attrs.emplace_back(Attr->getArg());
      }
      if (isConstExprIf(SemaRef, Attrs, IsConstExprIfStmt)) {
        return nullptr;
      }
    }
  }
  if (const ArraySyntax *BlockCond
                                = dyn_cast<ArraySyntax>(Call->getArguments())) {
    ConditionExpr = ExEl.elaborateBlockCondition(BlockCond, IsConstExprIfStmt);

  } else if (const ListSyntax *Args
                                 = dyn_cast<ListSyntax>(Call->getArguments())) {
    if (IsConstExprIfStmt)
      ConditionExpr = ExEl.elaborateExpectedConstantExpr(Args->getChild(0));
    else
      ConditionExpr = ExEl.elaborateExpr(Args->getChild(0));
  } else {
    return nullptr;
  }

  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr,
                                        S->getLoc(), ConditionExpr,
                                        IsConstExprIfStmt ?
                                        clang::Sema::ConditionKind::ConstexprIf:
                                        clang::Sema::ConditionKind::Boolean);


  clang::Stmt *Then;
  if (isa<ErrorSyntax>(S->getBlock())) {
    SemaRef.Diags.Report(S->getBlock()->getLoc(), clang::diag::err_in_ast);
    return nullptr;
  }
  SemaRef.enterScope(SK_Block, S);
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
    S->getLoc(), /*Constexpr=*/false, ConditionExpr->getExprLoc(),
    /*InitStmt=*/nullptr, Condition, ElseLoc, Then, ElseLoc, Else);

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

// Elaborates an array macro as a statement, just creates and discards the array
// FIXME: this does not currently emit properly.
clang::Stmt *StmtElaborator::elaborateArrayMacroStmt(const MacroSyntax *S) {
  ExprElaborator InitListElab(Context, SemaRef);
  clang::Expr *InitListExpr = InitListElab.elaborateExpr(S);

  if (!InitListExpr) {
    Diags.Report(S->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::InitListExpr *InitList = dyn_cast<clang::InitListExpr>(InitListExpr);
  if (!InitList)
    return nullptr;

  if (!InitList->inits().size())
    return nullptr;

  clang::QualType ElementType = InitList->getInit(0)->getType();
  auto Size = llvm::APSInt::getUnsigned(InitList->getNumInits());
  clang::QualType ArrayType =
    Context.CxxAST.getConstantArrayType(ElementType, Size,
                                        /*SizeExpr=*/nullptr,
                                        clang::ArrayType::Normal, 0);
  auto *TSI = Context.CxxAST.CreateTypeSourceInfo(ArrayType);

  clang::DeclContext *DC =
    clang::Decl::castToDeclContext(SemaRef.getCurrentDecl()->Cxx);



  clang::DeclarationName Name(&Context.CxxAST.Idents.get("bep"));
  clang::VarDecl *ArrayDecl =
    clang::VarDecl::Create(Context.CxxAST, DC, S->getLoc(), S->getLoc(), Name,
                           TSI->getType(), TSI, clang::SC_Auto);
  ArrayDecl->setInit(InitList);

  clang::DeclStmt *DS = new (Context.CxxAST) clang::DeclStmt(
    SemaRef.getCxxSema().ConvertDeclToDeclGroup(ArrayDecl).get(),
    S->getLoc(), S->getLoc());
  return DS;
}

static void handleUsingBlockList(SyntaxContext &Ctx, Sema &SemaRef,
                                 const ListSyntax *List,
                                 clang::SourceLocation UsingLoc,
                                 llvm::SmallVectorImpl<clang::Stmt *> &Res) {
  for (const Syntax *Item : List->children()) {
    clang::Decl *D = handleUsing(Ctx, SemaRef, Item, UsingLoc);
    if (!D)
      continue;
    clang::SourceLocation Loc = Item->getLoc();
    clang::DeclStmt *DS = new (Ctx.CxxAST) clang::DeclStmt(
      SemaRef.getCxxSema().ConvertDeclToDeclGroup(D).get(), UsingLoc, Loc);
    Res.push_back(DS);
  }
}

static void handleUsingBlock(SyntaxContext &Ctx, Sema &SemaRef,
                             const ArraySyntax *Block,
                             clang::SourceLocation UsingLoc,
                             llvm::SmallVectorImpl<clang::Stmt *> &Res) {
  for (const Syntax *Item : Block->children()) {
    if (const ArraySyntax *II = dyn_cast<ArraySyntax>(Item))
      handleUsingBlock(Ctx, SemaRef, II, UsingLoc, Res);
    else if (const ListSyntax *II = dyn_cast<ListSyntax>(Item))
      handleUsingBlockList(Ctx, SemaRef, II, UsingLoc, Res);
    else {
      clang::Decl *D = handleUsing(Ctx, SemaRef, Item, UsingLoc);
      if (!D)
        continue;
      clang::SourceLocation Loc = Item->getLoc();
      clang::DeclStmt *DS = new (Ctx.CxxAST) clang::DeclStmt(
        SemaRef.getCxxSema().ConvertDeclToDeclGroup(D).get(), UsingLoc, Loc);
      Res.push_back(DS);
    }
  }
}

clang::Stmt *StmtElaborator::elaborateUsingMacroStmt(const MacroSyntax *S) {
  if (!isa<ArraySyntax>(S->getBlock()))
    return nullptr;
  const ArraySyntax *Block = cast<ArraySyntax>(S->getBlock());


  clang::SourceLocation Loc = S->getCall()->getLoc();
  llvm::SmallVector<clang::Stmt *, 4> Res;
  // We might have any amount of nesting of lists within the macro block.
  handleUsingBlock(Context, SemaRef, Block, Loc, Res);
  return clang::CompoundStmt::Create(CxxAST, Res, Loc, Loc, /*Vector*/true);
}

// An auto-deduced operator'in' call does not appear to be a declaration to our
// elaborator, so we will have to fabricate something here.
static clang::DeclStmt *
createForRangeLoopVarDecl(SyntaxContext &Ctx, Sema &SemaRef,
                          const CallSyntax *S) {
  clang::ASTContext &CxxAST = Ctx.CxxAST;
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::TypeSourceInfo *TInfo =
    CxxAST.CreateTypeSourceInfo(CxxAST.getAutoDeductType());
  clang::SourceLocation Loc = S->getArgument(0)->getLoc();

  std::string Name = cast<AtomSyntax>(S->getArgument(0))->getSpelling();
  clang::IdentifierInfo *Id = &CxxAST.Idents.get(Name);
  clang::VarDecl *VD =
    clang::VarDecl::Create(CxxAST, Owner, Loc, Loc, Id, TInfo->getType(),
                           TInfo, clang::SC_Auto);

  Elaborator(Ctx, SemaRef).identifyDecl(S);
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  SemaRef.setDeclForDeclaration(D, VD);

  clang::DeclStmt *DS = new (CxxAST) clang::DeclStmt(
    SemaRef.getCxxSema().ConvertDeclToDeclGroup(VD).get(), Loc, Loc);
  return DS;
}

clang::Stmt *StmtElaborator::handleRangeBasedFor(const ListSyntax *S,
                                                 clang::SourceLocation ForLoc) {
  assert(isa<CallSyntax>(S->getChild(0)) && "Invalid argument in for range");
  const CallSyntax *InCall = cast<CallSyntax>(S->getChild(0));
  assert(cast<AtomSyntax>(InCall->getCallee())->getSpelling() == "operator'in'"
         && "For range statement must have a top-level operator'in' call");

  clang::Stmt *LoopVar = isa<AtomSyntax>(InCall->getArgument(0)) ?
    createForRangeLoopVarDecl(Context, SemaRef, InCall) :
    StmtElaborator(Context, SemaRef).elaborateStmt(InCall->getArgument(0));

  if (!LoopVar)
    return nullptr;

  clang::Expr *RangeExpr =
    ExprElaborator(Context, SemaRef).elaborateExpr(InCall->getArgument(1));
  if (!RangeExpr) {
    SemaRef.Diags.Report(InCall->getArgument(1)->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  ExprMarker(Context.CxxAST, SemaRef).Visit(RangeExpr);

  clang::StmtResult Res = SemaRef.getCxxSema().ActOnCXXForRangeStmt(
    SemaRef.getCxxSema().getCurScope(), ForLoc, clang::SourceLocation(),
    /*InitStmt=*/nullptr, LoopVar, InCall->getCallee()->getLoc(),
    RangeExpr, ForLoc, clang::Sema::BFRK_Build);

  if (Res.isInvalid())
    return nullptr;

  return Res.get();
}

/// Handle the syntax `for (__loopvar in a..b)` where a and b are
/// comparable and incrementable types. This maps directly to the
/// C++ syntax `for (auto __loopvar = a; __loopvar < b; ++__loopvar)`
clang::Stmt *StmtElaborator::handleCartesianFor(const ListSyntax *S,
                                                const Syntax *BodyCST,
                                               clang::SourceLocation ForLoc) {
  assert(isa<CallSyntax>(S->getChild(0)) && "Invalid argument in for range");
  const CallSyntax *InCall = cast<CallSyntax>(S->getChild(0));
  assert(cast<AtomSyntax>(InCall->getCallee())->getSpelling() == "operator'in'"
         && "For statement must have a top-level operator'in' call");
  assert(isa<CallSyntax>(InCall->getArgument(1))
         && "Cartesian for statement without operator'..'");
  const CallSyntax *ToCall = cast<CallSyntax>(InCall->getArgument(1));

  clang::Stmt *LoopVar = isa<AtomSyntax>(InCall->getArgument(0)) ?
    createForRangeLoopVarDecl(Context, SemaRef, InCall) :
    StmtElaborator(Context, SemaRef).elaborateStmt(InCall->getArgument(0));

  if (!LoopVar)
    return nullptr;

  clang::Expr *Args[2] = { nullptr, nullptr };

  // Fabricate `auto __LoopVar = a`
  clang::Expr *First =
    ExprElaborator(Context, SemaRef).elaborateExpr(ToCall->getArgument(0));
  if (!First) {
    SemaRef.Diags.Report(ToCall->getArgument(0)->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  Args[0] = First;
  clang::Sema &ClangSema = SemaRef.getCxxSema();

  clang::DeclStmt *LoopVarDS = cast<clang::DeclStmt>(LoopVar);
  clang::VarDecl *LoopVarDecl = cast<clang::VarDecl>(LoopVarDS->getSingleDecl());
  ClangSema.AddInitializerToDecl(LoopVarDecl, Args[0], /*DI=*/false);

  // Fabricate `__LoopVar <= b`
  clang::Expr *Second =
    ExprElaborator(Context, SemaRef).elaborateExpr(ToCall->getArgument(1));
  if (!Second) {
    SemaRef.Diags.Report(ToCall->getArgument(0)->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  Args[1] = Second;

  clang::DeclRefExpr *LoopVarDRE =
    clang::DeclRefExpr::Create(Context.CxxAST,
                               clang::NestedNameSpecifierLoc(),
                               clang::SourceLocation(), LoopVarDecl,
                               /*Capture=*/false, LoopVarDecl->getBeginLoc(),
                               LoopVarDecl->getType(), clang::VK_LValue);
  clang::ExprResult LessThan =
    ClangSema.BuildBinOp(ClangSema.getCurScope(), ToCall->getLoc(),
                         clang::BO_LE, LoopVarDRE, Args[1]);
  if (LessThan.isInvalid())
    return nullptr;
  clang::Sema::ConditionResult Condition =
    ClangSema.ActOnCondition(ClangSema.getCurScope(), ToCall->getLoc(),
                           LessThan.get(), clang::Sema::ConditionKind::Boolean);

  // Fabricate `++__LoopVar`, if possible.
  clang::Expr *IncrementExpr = nullptr;

  clang::ExprResult PlusPlus =
    ClangSema.BuildUnaryOp(ClangSema.getCurScope(), ToCall->getLoc(),
                           clang::UO_PreInc, LoopVarDRE);
  if (!PlusPlus.isInvalid())
    IncrementExpr = PlusPlus.get();

  // We could not create a pre-increment expression, so try to
  // create a `+= 1` expression.
  if (!IncrementExpr) {
    clang::IntegerLiteral *One =
     clang::IntegerLiteral::Create(Context.CxxAST, llvm::APSInt::getUnsigned(1),
                                Context.CxxAST.getSizeType(), ToCall->getLoc());
    clang::ExprResult PlusEqual =
      ClangSema.BuildBinOp(ClangSema.getCurScope(), ToCall->getLoc(),
                           clang::BO_AddAssign, LoopVarDRE, One);
    if (PlusEqual.isInvalid()) {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "Cannot increment cartesian-for type.");
      Diags.Report(ToCall->getLoc(), DiagID);
      return nullptr;
    }

    IncrementExpr = PlusEqual.get();
  }

  // Mark the LoopVar used
  ExprMarker(Context.CxxAST, SemaRef).Visit(LoopVarDRE);

  // Elaborate the body.
  clang::Stmt *Body = StmtElaborator(Context, SemaRef).elaborateBlock(BodyCST);

  clang::StmtResult ForStmt =
    ClangSema.ActOnForStmt(ForLoc, InCall->getArgument(0)->getLoc(),
                           LoopVarDS, Condition,
                           ClangSema.MakeFullExpr(IncrementExpr),
                           ToCall->getArgument(1)->getLoc(),
                           Body);
  if (ForStmt.isInvalid())
    return nullptr;
  return ForStmt.get();
}

// Check a block-for MacroSyntax's arguments to ensure that only one or zero
// ranges are being iterated over. Returns true on error.
// \param SemaRef - reference to the Sema object
// \param Args - the argument array of the for-block MacroSyntax
// \param ResultRange - points to the operator'in' call in the block,
//                      if there is one.
static bool checkBlockForArgs(Sema &SemaRef, const ArraySyntax *Args,
                              const Syntax *&ResultRange) {
  ResultRange = nullptr;
  for (const Syntax *Arg : Args->children()) {
    if (const CallSyntax *Call = dyn_cast<CallSyntax>(Arg)) {
      if (getFusedOpKind(SemaRef, Call) == FOK_In) {
        if (!ResultRange) {
          ResultRange = Arg;
          return false;
        } else {
          unsigned ErrID =
            SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                          "for macro may only have one range");
          unsigned NoteID =
            SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Note,
                                          "previous range is here");
          SemaRef.Diags.Report(Arg->getLoc(), ErrID);
          SemaRef.Diags.Report(ResultRange->getLoc(), NoteID);
          return true;
        }
      }
    }
  }

  return false;
}

clang::Stmt *StmtElaborator::elaborateBlockForStmt(const MacroSyntax *S) {
  assert (isa<CallSyntax>(S->getCall()));
  const CallSyntax *MacroCall = cast<CallSyntax>(S->getCall());
  clang::SourceLocation ForLoc = S->getCall()->getLoc();

  assert(isa<ArraySyntax>(MacroCall->getArguments()));
  const ArraySyntax *Arguments = cast<ArraySyntax>(MacroCall->getArguments());

  // First, ensure that there is only one range in the block condition.
  const Syntax *Range = nullptr;
  if (checkBlockForArgs(SemaRef, Arguments, Range))
    return nullptr;

  // TODO: support iterative block fors.
  if (!Range) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "for-block macro does not have a range");
    Diags.Report(ForLoc, DiagID);
    return nullptr;
  }

  // Construct a new for macro with the non-range header syntaxes at the
  // beginning of the loop body.
  llvm::SmallVector<const Syntax *, 4> HeaderSyntaxes;
  for (const Syntax *Arg : Arguments->children()) {
    if (Arg == Range)
      continue;
    HeaderSyntaxes.push_back(Arg);
  }

  const Syntax **NewBody;
  std::size_t BodySize = 0;
  if (const ArraySyntax *Block = dyn_cast<ArraySyntax>(S->getBlock())) {
    BodySize = Block->getNumChildren() + HeaderSyntaxes.size();
    NewBody = new (Context) const Syntax *[BodySize];
    std::copy(HeaderSyntaxes.begin(), HeaderSyntaxes.end(), NewBody);
    std::copy(Block->Elems, Block->Elems + Block->getNumChildren(),
              NewBody + HeaderSyntaxes.size());
  } else if (const ListSyntax *List = dyn_cast<ListSyntax>(S->getBlock())) {
    BodySize = List->getNumChildren() + HeaderSyntaxes.size();
    NewBody = new (Context) const Syntax *[BodySize];
    std::copy(HeaderSyntaxes.begin(), HeaderSyntaxes.end(), NewBody);
    std::copy(List->Elems, List->Elems + List->getNumChildren(),
              NewBody + HeaderSyntaxes.size());
  } else {
    BodySize = HeaderSyntaxes.size() + 1;
    NewBody = new (Context) const Syntax *[BodySize];
    std::copy(HeaderSyntaxes.begin(), HeaderSyntaxes.end(), NewBody);
    NewBody[HeaderSyntaxes.size() - 1] = S->getBlock();
  }

  const Syntax **RangeArray = new const Syntax *[1];
  RangeArray[0] = Range;
  ListSyntax *NewArgs = new (Context) ListSyntax(
    const_cast<Syntax **>(RangeArray), 1);
  const CallSyntax *OldCall = cast<CallSyntax>(S->getCall());
  CallSyntax *NewCall = new (Context) CallSyntax(
    const_cast<Syntax *>(OldCall->getCallee()),
    NewArgs);
  MacroSyntax *NewMacro = new (Context) MacroSyntax(
    NewCall,
    new (Context) ArraySyntax(const_cast<Syntax **>(NewBody), BodySize),
    const_cast<Syntax *>(S->getNext()));
  return StmtElaborator(Context, SemaRef).elaborateForStmt(NewMacro);
}

clang::Stmt *StmtElaborator::elaborateForStmt(const MacroSyntax *S) {
  assert(isa<CallSyntax>(S->getCall()));
  const CallSyntax *MacroCall = cast<CallSyntax>(S->getCall());
  // If the macro's arguments are an array, we're working with a block-for.
  if (isa<ArraySyntax>(MacroCall->getArguments()))
    return elaborateBlockForStmt(S);

  clang::SourceLocation ForLoc = S->getCall()->getLoc();

  assert (isa<ListSyntax>(MacroCall->getArguments()) && "Invalid macro block");
  const ListSyntax *Arguments = cast<ListSyntax>(MacroCall->getArguments());

  clang::Sema::GoldElaborationScopeRAII CxxScope(
    SemaRef.getCxxSema(),
    clang::Scope::BreakScope    |
    clang::Scope::ContinueScope |
    clang::Scope::DeclScope     |
    clang::Scope::ControlScope);
  Sema::ScopeRAII ForScope(SemaRef, SK_Control, S);

  // Check to see if we have a cartesian for macro, e.g. `for (x in a..b)`
  if (const CallSyntax *In = dyn_cast<CallSyntax>(Arguments->getChild(0))) {
    if (const CallSyntax *To = dyn_cast<CallSyntax>(In->getArgument(1))) {
      FusedOpKind Op = getFusedOpKind(SemaRef, To);
      if (Op == FOK_DotDot)
        return handleCartesianFor(Arguments, S->getBlock(), ForLoc);
    }
  }

  // FIXME: for now, we are assuming this is range-based; it may not be.
  clang::Stmt *ForRange = handleRangeBasedFor(Arguments, ForLoc);

  clang::Stmt *Body =
    StmtElaborator(Context, SemaRef).elaborateBlock(S->getBlock());
  if (!Body)
    return nullptr;

  clang::StmtResult Res =
    SemaRef.getCxxSema().FinishCXXForRangeStmt(ForRange, Body);

  if (Res.isInvalid())
    return nullptr;
  return Res.get();
}

clang::Stmt *
StmtElaborator::elaborateWhileStmt(const MacroSyntax *S) {
  assert(isa<CallSyntax>(S->getCall()));
  const CallSyntax *MacroCall = cast<CallSyntax>(S->getCall());
  clang::SourceLocation WhileLoc = S->getCall()->getLoc();

  clang::Sema::GoldElaborationScopeRAII CxxScope(
    SemaRef.getCxxSema(),
    clang::Scope::BreakScope    |
    clang::Scope::ContinueScope |
    clang::Scope::DeclScope     |
    clang::Scope::ControlScope);
  Sema::ScopeRAII WhileScope(SemaRef, SK_Control, S);


  // If the macro's arguments are an array, we're working with a block-while.

  clang::Expr *CondExpr = nullptr;
  clang::SourceLocation CondLoc;
  if (const auto *Block = dyn_cast<ArraySyntax>(MacroCall->getArguments())) {
    CondLoc = Block->getChild(0)->getLoc();
    CondExpr =
      ExprElaborator(Context, SemaRef).elaborateBlockCondition(Block);
  } else {
    assert (isa<ListSyntax>(MacroCall->getArguments()) && "Invalid macro block");
    const ListSyntax *Arguments = cast<ListSyntax>(MacroCall->getArguments());

    if (!Arguments->getNumChildren())
      return nullptr;
    if (Arguments->getNumChildren() > 1) {
      unsigned ErrID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "too many conditions in while loop");
      unsigned NoteID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Note,
                                      "did you mean to use `while:`?");
      Diags.Report(WhileLoc, ErrID);
      Diags.Report(WhileLoc, NoteID);
      return nullptr;
    }

    CondLoc = Arguments->getChild(0)->getLoc();
    CondExpr =
      ExprElaborator(Context, SemaRef).elaborateExpr(Arguments->getChild(0));
  }

  if (!CondExpr)
    return nullptr;

  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr, S->getLoc(), CondExpr,
                                        clang::Sema::ConditionKind::Boolean);
  Sema::ScopeRAII(SemaRef, SK_Block, S->getBlock());
  clang::Stmt *Body =
    StmtElaborator(Context, SemaRef).elaborateBlock(S->getBlock());
  if (!Body)
    return nullptr;

  clang::StmtResult While =
    SemaRef.getCxxSema().ActOnWhileStmt(WhileLoc, WhileLoc,
                                        Condition, WhileLoc, Body);
  if (While.isInvalid())
    return nullptr;
  return While.get();
}


clang::Stmt *
StmtElaborator::elaborateMacro(const MacroSyntax *S) {
  const AtomSyntax *Callee;
  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S->getCall()))
    Callee = cast<AtomSyntax>(Call->getCallee());
  else
    Callee = cast<AtomSyntax>(S->getCall());

  FusedOpKind OpKind = getFusedOpKind(SemaRef, Callee->getSpelling());

  switch (OpKind) {
  case FOK_If:
    return elaborateIfStmt(S);
  case FOK_Else:
    return elaborateElseStmt(S);
  case FOK_For:
    return elaborateForStmt(S);
  case FOK_While:
    return elaborateWhileStmt(S);
  default:
    break;
  }

  // If we meet these 2 criteria then we know we are block that needs to be
  // elaborated into a compound statement
  if (Callee->hasToken(tok::AnonymousKeyword)
      && Callee->getSpelling() == "__BLOCK__")
    return elaborateBlock(S->getBlock());

  if (Callee->hasToken(tok::TryBlock)
      && Callee->getSpelling() == "__TRY__")
    return elaborateTryCatchBlock(S);

  if (Callee->getSpelling() == "array")
    return elaborateArrayMacroStmt(S);
  if (Callee->getSpelling() == "using")
    return elaborateUsingMacroStmt(S);

  // Checking to see if we have a new keyword token, because this could
  // be a placement new.
  Token Tok = Callee->getToken();
  if (Tok.getKind() == tok::NewKeyword)
    return ExprElaborator(Context, SemaRef).elaborateNewExpr(S);

  return ExprElaborator(Context, SemaRef).elaborateInitListCall(S);
}

clang::Stmt *
StmtElaborator::elaborateBlock(const Syntax *S) {
  if (isa<ErrorSyntax>(S))
    return nullptr;
  SemaRef.getCxxSema().ActOnStartOfCompoundStmt(false);
  SemaRef.enterScope(SK_Block, S);

  llvm::SmallVector<clang::Stmt *, 16> Results;
  clang::SourceLocation StartLoc, EndLoc;
  if (auto *Array = dyn_cast<ArraySyntax>(S)) {
    elaborateBlockForArray(cast<ArraySyntax>(S), Results);
    // We are going to need to check for an empty block in this instance,
    // because that's also possible
    if (Array->getNumChildren() == 0) {
      StartLoc = Array->getLoc();
      EndLoc = Array->getLoc();
    } else {
      StartLoc = Array->getChild(0)->getLoc();
      EndLoc = Array->getChild(Array->getNumChildren() - 1)->getLoc();
    }
  } else if (auto *List = dyn_cast<ListSyntax>(S)) {
    elaborateBlockForList(cast<ListSyntax>(S), Results);
    if (List->getNumChildren() == 0) {
      StartLoc = List->getLoc();
      EndLoc = List->getLoc();
    } else {
      StartLoc = List->getChild(0)->getLoc();
      EndLoc = List->getChild(List->getNumChildren() - 1)->getLoc();
    }
  } else {
    // This is some sort of one-line block like `f(x : int) = x * x`
    // FIXME: Make sure this only creates return stmts in a function context.
    clang::Expr *ReturnVal =
      ExprElaborator(Context, SemaRef).elaborateExpr(S);
    if (!ReturnVal){
      SemaRef.leaveScope(S);
      return nullptr;
    }
    ExprMarker(CxxAST, SemaRef).Visit(ReturnVal);

    clang::StmtResult ReturnRes = SemaRef.getCxxSema().BuildReturnStmt(
                                                        S->getLoc(), ReturnVal);
    if (!ReturnRes.isInvalid())
      Results.push_back(ReturnRes.get());

    StartLoc = EndLoc = S->getLoc();
  }

  clang::Stmt *Block = SemaRef.getCxxSema().ActOnCompoundStmt(StartLoc, EndLoc,
                                          Results, /*isStmtExpr=*/false).get();
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
      if (!NewStmt)
        continue;
      if (clang::CompoundStmt *CS = dyn_cast<clang::CompoundStmt>(NewStmt)) {
        if (CS->isCppxVector()) {
          std::copy(CS->body_begin(), CS->body_end(), std::back_inserter(Results));
          continue;
        }
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
    if (clang::CompoundStmt *CS = dyn_cast<clang::CompoundStmt>(NewStmt)) {
      if (CS->isCppxVector()) {
        std::copy(CS->body_begin(), CS->body_end(), std::back_inserter(Results));
        continue;
      }
    }

    Results.push_back(NewStmt);
  }
}


clang::Stmt *
StmtElaborator::elaborateTryCatchBlock(const MacroSyntax *TCBlock) {
  // Keeping track of clang scope.
  // Other scope will be entered later on.
  Sema::ClangScopeRAII ClangScope(SemaRef, clang::Scope::DeclScope
                                  | clang::Scope::TryScope
                                  | clang::Scope::CompoundStmtScope,
                                  TCBlock->getCallLoc());
  assert(TCBlock && "Invalid block");
  auto *TCArray = cast<ArraySyntax>(TCBlock->getBlock());
  assert(TCArray->getNumChildren() >=1
         && "Improperly formatted try catch block.");
  auto *BlockMacro = cast<MacroSyntax>(TCArray->getChild(0));
  clang::Stmt *InnerStmt = elaborateBlock(BlockMacro->getBlock());
  llvm::SmallVector<clang::Stmt *, 16> CatchStmts;

  // Even if this retuns a nullptr, still try and continue and figure out if we
  // encountered an error or not.
  for (std::size_t CatchIndex = 1;
       CatchIndex < TCArray->getNumChildren();
       ++CatchIndex)
  {
    if (const auto *CatchMacro
                       = dyn_cast<MacroSyntax>(TCArray->getChild(CatchIndex))) {
      clang::Stmt *CatchStmt = elaborateCatch(CatchMacro);
      if (CatchStmt)
        CatchStmts.emplace_back(CatchStmt);
    } else {
      llvm_unreachable("Unexpected AST expected catch macro.");
    }
  }
  auto TryBlockStmt = SemaRef.getCxxSema().ActOnCXXTryBlock(
                                  TCBlock->getCallLoc(), InnerStmt, CatchStmts);
  return TryBlockStmt.get();
}

clang::Stmt *
StmtElaborator::elaborateCatch(const MacroSyntax *CatchBlock) {
  Sema::ClangScopeRAII ClangScope(SemaRef, clang::Scope::DeclScope
                                  | clang::Scope::ControlScope
                                  | clang::Scope::CatchScope,
                                  CatchBlock->getCallLoc());
  Sema::ScopeRAII GScope(SemaRef, SK_Catch, CatchBlock->getCall());
  clang::Decl *CatchName = nullptr;
  Elaborator DeclElab(SemaRef.getContext(), SemaRef);
  if (const auto *Call = dyn_cast<CallSyntax>(CatchBlock->getCall())) {
    if (const auto *ArgList = dyn_cast<ListSyntax>(Call->getArguments())) {
      if (ArgList->getNumChildren() > 0)
        CatchName = DeclElab.elaborateDeclSyntax(ArgList->getChild(0));

      // This is a recoverable error state.
      if (ArgList->getNumChildren() > 1)
        SemaRef.Diags.Report(ArgList->getChild(1)->getLoc(),
                             clang::diag::err_invalid_catch_parameter);
    }
  }

  // Elaborating the variable declaration associated with catch.
  clang::Stmt *CatchBlockStmt = elaborateBlock(CatchBlock->getBlock());
  auto StmtResult = SemaRef.getCxxSema().ActOnCXXCatchBlock(
                           CatchBlock->getCallLoc(), CatchName, CatchBlockStmt);
  return StmtResult.get();
}

clang::Stmt *
StmtElaborator::elaborateThrowStmt(const CallSyntax *S) {
  return ExprElaborator(Context, SemaRef).elaborateThrowExpr(S);
}

} // namespace gold
