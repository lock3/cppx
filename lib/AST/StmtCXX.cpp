//===--- StmtCXX.cpp - Classes for representing C++ statements ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Stmt class declared in StmtCXX.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/StmtCXX.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/Lex/Token.h"

using namespace clang;

QualType CXXCatchStmt::getCaughtType() const {
  if (ExceptionDecl)
    return ExceptionDecl->getType();
  return QualType();
}

CXXTryStmt *CXXTryStmt::Create(const ASTContext &C, SourceLocation tryLoc,
                               Stmt *tryBlock, ArrayRef<Stmt *> handlers) {
  const size_t Size = totalSizeToAlloc<Stmt *>(handlers.size() + 1);
  void *Mem = C.Allocate(Size, alignof(CXXTryStmt));
  return new (Mem) CXXTryStmt(tryLoc, tryBlock, handlers);
}

CXXTryStmt *CXXTryStmt::Create(const ASTContext &C, EmptyShell Empty,
                               unsigned numHandlers) {
  const size_t Size = totalSizeToAlloc<Stmt *>(numHandlers + 1);
  void *Mem = C.Allocate(Size, alignof(CXXTryStmt));
  return new (Mem) CXXTryStmt(Empty, numHandlers);
}

CXXTryStmt::CXXTryStmt(SourceLocation tryLoc, Stmt *tryBlock,
                       ArrayRef<Stmt *> handlers)
    : Stmt(CXXTryStmtClass), TryLoc(tryLoc), NumHandlers(handlers.size()) {
  Stmt **Stmts = getStmts();
  Stmts[0] = tryBlock;
  std::copy(handlers.begin(), handlers.end(), Stmts + 1);
}

CXXForRangeStmt::CXXForRangeStmt(DeclStmt *Range,
                                 DeclStmt *BeginStmt, DeclStmt *EndStmt,
                                 Expr *Cond, Expr *Inc, DeclStmt *LoopVar,
                                 Stmt *Body, SourceLocation FL,
                                 SourceLocation CAL, SourceLocation CL,
                                 SourceLocation RPL)
    : Stmt(CXXForRangeStmtClass), ForLoc(FL), CoawaitLoc(CAL), ColonLoc(CL),
      RParenLoc(RPL) {
  SubExprs[RANGE] = Range;
  SubExprs[BEGINSTMT] = BeginStmt;
  SubExprs[ENDSTMT] = EndStmt;
  SubExprs[COND] = Cond;
  SubExprs[INC] = Inc;
  SubExprs[LOOPVAR] = LoopVar;
  SubExprs[BODY] = Body;
}

Expr *CXXForRangeStmt::getRangeInit() {
  DeclStmt *RangeStmt = getRangeStmt();
  VarDecl *RangeDecl = dyn_cast_or_null<VarDecl>(RangeStmt->getSingleDecl());
  assert(RangeDecl && "for-range should have a single var decl");
  return RangeDecl->getInit();
}

const Expr *CXXForRangeStmt::getRangeInit() const {
  return const_cast<CXXForRangeStmt *>(this)->getRangeInit();
}

VarDecl *CXXForRangeStmt::getLoopVariable() {
  Decl *LV = cast<DeclStmt>(getLoopVarStmt())->getSingleDecl();
  assert(LV && "No loop variable in CXXForRangeStmt");
  return cast<VarDecl>(LV);
}

const VarDecl *CXXForRangeStmt::getLoopVariable() const {
  return const_cast<CXXForRangeStmt *>(this)->getLoopVariable();
}

CXXExpansionStmt::CXXExpansionStmt(StmtClass SC, DeclStmt *Range,
                                   DeclStmt *LoopVar, Stmt *Body, std::size_t N,
                                   SourceLocation FL, SourceLocation EL,
                                   SourceLocation CL, SourceLocation RPL)
    : Stmt(SC), ForLoc(FL), ColonLoc(CL), RParenLoc(RPL), Size(N),
      InstantiatedStmts(nullptr) {
  SubExprs[RANGE] = Range;
  SubExprs[LOOP] = LoopVar;
  SubExprs[BODY] = Body;
}

VarDecl *CXXExpansionStmt::getRangeVariable() {
  Decl *RV = cast<DeclStmt>(getRangeVarStmt())->getSingleDecl();
  assert(RV && "No range variable in CXXExpansionStmt");
  return cast<VarDecl>(RV);
}

VarDecl *CXXExpansionStmt::getLoopVariable() {
  Decl *LV = cast<DeclStmt>(getLoopVarStmt())->getSingleDecl();
  assert(LV && "No loop variable in CXXExpansionStmt");
  return cast<VarDecl>(LV);
}

const VarDecl *CXXExpansionStmt::getLoopVariable() const {
  return const_cast<CXXExpansionStmt *>(this)->getLoopVariable();
}

CXXTupleExpansionStmt::CXXTupleExpansionStmt(
    TemplateParameterList *TP, DeclStmt *RangeVar, DeclStmt *LoopVar,
    Stmt *Body, std::size_t N, SourceLocation FL, SourceLocation EL,
    SourceLocation CL, SourceLocation RPL)
    : CXXExpansionStmt(CXXTupleExpansionStmtClass, RangeVar, LoopVar, Body, N,
                       FL, EL, CL, RPL),
      Parms(TP) {}

NonTypeTemplateParmDecl *CXXTupleExpansionStmt::getPlaceholderParameter() {
  return cast<NonTypeTemplateParmDecl>(Parms->getParam(0));
}

Expr *CXXTupleExpansionStmt::getRangeInit() {
  DeclStmt *RangeStmt = getRangeVarStmt();
  VarDecl *RangeDecl = dyn_cast_or_null<VarDecl>(RangeStmt->getSingleDecl());
  assert(RangeDecl && "for-range should have a single var decl");
  return RangeDecl->getInit();
}

const Expr *CXXTupleExpansionStmt::getRangeInit() const {
  return const_cast<CXXTupleExpansionStmt *>(this)->getRangeInit();
}

CXXConstexprExpansionStmt::CXXConstexprExpansionStmt(DeclStmt *RangeVar,
						     DeclStmt *LoopVar,
						     Stmt *Body,
						     Stmt *BeginStmt,
						     Stmt *EndStmt,
						     Expr *BeginExpr,
						     Expr *NextCall,
						     SourceLocation FL,
						     SourceLocation CEL,
						     SourceLocation CL,
						     SourceLocation RPL)
  : CXXExpansionStmt(CXXConstexprExpansionStmtClass, RangeVar, LoopVar,
		     Body, std::size_t(), FL, CEL, CL, RPL),
    BeginStmt(BeginStmt), EndStmt(EndStmt), BeginExpr(BeginExpr), NextCall(NextCall)
{}

CXXPackExpansionStmt::CXXPackExpansionStmt(DeclStmt *RangeVar,
                                           DeclStmt *LoopVar, Stmt *Body,
                                           SourceLocation FL, SourceLocation EL,
                                           SourceLocation CL,
                                           SourceLocation RPL)
    : CXXExpansionStmt(CXXPackExpansionStmtClass, RangeVar, LoopVar, Body, 0,
                       FL, EL, CL, RPL) {}

CoroutineBodyStmt *CoroutineBodyStmt::Create(
    const ASTContext &C, CoroutineBodyStmt::CtorArgs const &Args) {
  std::size_t Size = totalSizeToAlloc<Stmt *>(
      CoroutineBodyStmt::FirstParamMove + Args.ParamMoves.size());

  void *Mem = C.Allocate(Size, alignof(CoroutineBodyStmt));
  return new (Mem) CoroutineBodyStmt(Args);
}

CoroutineBodyStmt *CoroutineBodyStmt::Create(const ASTContext &C, EmptyShell,
                                             unsigned NumParams) {
  std::size_t Size = totalSizeToAlloc<Stmt *>(
      CoroutineBodyStmt::FirstParamMove + NumParams);

  void *Mem = C.Allocate(Size, alignof(CoroutineBodyStmt));
  auto *Result = new (Mem) CoroutineBodyStmt(CtorArgs());
  Result->NumParams = NumParams;
  auto *ParamBegin = Result->getStoredStmts() + SubStmt::FirstParamMove;
  std::uninitialized_fill(ParamBegin, ParamBegin + NumParams,
                          static_cast<Stmt *>(nullptr));
  return Result;
}

CoroutineBodyStmt::CoroutineBodyStmt(CoroutineBodyStmt::CtorArgs const &Args)
    : Stmt(CoroutineBodyStmtClass), NumParams(Args.ParamMoves.size()) {
  Stmt **SubStmts = getStoredStmts();
  SubStmts[CoroutineBodyStmt::Body] = Args.Body;
  SubStmts[CoroutineBodyStmt::Promise] = Args.Promise;
  SubStmts[CoroutineBodyStmt::InitSuspend] = Args.InitialSuspend;
  SubStmts[CoroutineBodyStmt::FinalSuspend] = Args.FinalSuspend;
  SubStmts[CoroutineBodyStmt::OnException] = Args.OnException;
  SubStmts[CoroutineBodyStmt::OnFallthrough] = Args.OnFallthrough;
  SubStmts[CoroutineBodyStmt::Allocate] = Args.Allocate;
  SubStmts[CoroutineBodyStmt::Deallocate] = Args.Deallocate;
  SubStmts[CoroutineBodyStmt::ReturnValue] = Args.ReturnValue;
  SubStmts[CoroutineBodyStmt::ResultDecl] = Args.ResultDecl;
  SubStmts[CoroutineBodyStmt::ReturnStmt] = Args.ReturnStmt;
  SubStmts[CoroutineBodyStmt::ReturnStmtOnAllocFailure] =
      Args.ReturnStmtOnAllocFailure;
  std::copy(Args.ParamMoves.begin(), Args.ParamMoves.end(),
            const_cast<Stmt **>(getParamMoves().data()));
}
