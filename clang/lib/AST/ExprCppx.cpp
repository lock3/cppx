//===- ExprCXX.cpp - (C++) Expression AST Node Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Expr class declared in ExprCXX.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ExprCppx.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
// #include "clang/AST/Decl.h"
#include <cassert>
#include <cstddef>
#include <cstring>
#include <memory>

namespace clang {


SourceLocation CppxTypeLiteral::getBeginLoc() const {
  return Value->getTypeLoc().getBeginLoc();
}

SourceLocation CppxTypeLiteral::getEndLoc() const {
  return Value->getTypeLoc().getEndLoc();
}

CppxTypeLiteral*
CppxTypeLiteral::create(ASTContext &Context, QualType KindTy, ValueType Ty) {
  return new (Context) CppxTypeLiteral(KindTy, Ty);
}

CppxDeclRefExpr *
CppxDeclRefExpr::Create(ASTContext &Context, QualType KindTy, ValueType D,
                        SourceLocation Loc) {
  return new (Context) CppxDeclRefExpr(KindTy, D, Loc);
}

CppxPartialEvalExpr *CppxPartialEvalExpr::Create(ASTContext &Ctx,
                                                 gold::CppxPartialExprBase *E,
                                                 SourceLocation Loc) {
  return new (Ctx) CppxPartialEvalExpr(Ctx.VoidTy, E, Loc);
}

CppxPartialEvalExpr *CppxPartialEvalExpr::Create(ASTContext &Ctx,
                                             blue::CppxPartialNameAccessBase *E,
                                                 SourceLocation Loc) {
  return new (Ctx) CppxPartialEvalExpr(Ctx.VoidTy, E, Loc);
}

CppxDependentMemberAccessExpr::CppxDependentMemberAccessExpr(
      const ASTContext &Ctx, Expr *Base, QualType BaseType,
      SourceLocation OperatorLoc, DeclarationNameInfo MemberNameInfo,
      Expr *NameSpecExpr)
    : Expr(CppxDependentMemberAccessExprClass, Ctx.DependentTy, VK_LValue,
           OK_Ordinary),
      Base(Base), NameSpecifier(NameSpecExpr), BaseType(BaseType),
      MemberNameInfo(MemberNameInfo)
{
  CppxDependentMemberAccessExprBits.OperatorLoc = OperatorLoc;
  setDependence(computeDependence(this));
}

CppxDependentMemberAccessExpr::CppxDependentMemberAccessExpr(EmptyShell Empty)
  : Expr(CppxDependentMemberAccessExprClass, Empty) { }

CppxDependentMemberAccessExpr *
CppxDependentMemberAccessExpr::Create(const ASTContext &Ctx, Expr *Base,
                                      QualType BaseType,
                                      SourceLocation OperatorLoc,
                                      DeclarationNameInfo MemberNameInfo,
                                      Expr *NameSpecExpr) {
  void *Mem = Ctx.Allocate(sizeof(CppxDependentMemberAccessExpr),
                           alignof(CppxDependentMemberAccessExpr));
  assert(Base);
  return new (Mem) CppxDependentMemberAccessExpr(Ctx, Base, BaseType,
                                                 OperatorLoc, MemberNameInfo,
                                                 NameSpecExpr);
}

CppxDependentMemberAccessExpr *
CppxDependentMemberAccessExpr::CreateEmpty(const ASTContext &Ctx) {
  void *Mem = Ctx.Allocate(sizeof(CppxDependentMemberAccessExpr),
                           alignof(CppxDependentMemberAccessExpr));
  return new (Mem) CppxDependentMemberAccessExpr(EmptyShell());
}


CppxTemplateOrArrayExpr::CppxTemplateOrArrayExpr(const ASTContext &Ctx,
                                                 Stmt *E,
                                                 ArrayRef<Expr *> Args)
  :Expr(CppxTemplateOrArrayExprClass, Ctx.DependentTy, VK_LValue, OK_Ordinary),
  NumArgs(Args.size())
{
  TemplateOrArrayBits.OffsetToTrailingObjects = sizeof(CppxTemplateOrArrayExpr);
  setDependence(clang::ExprDependenceScope::ExprDependence::TypeValue
                |  clang::ExprDependenceScope::ExprDependence::TypeInstantiation
                |  clang::ExprDependenceScope::ExprDependence::ValueInstantiation
                |  clang::ExprDependenceScope::ExprDependence::TypeValueInstantiation);
  // Copying everything into the trailing arguments.
  getTrailingStmts()[0] = E;
  for (unsigned I = 0; I != Args.size(); ++I)
    setArg(I, Args[I]);
}

CppxTemplateOrArrayExpr::CppxTemplateOrArrayExpr(const ASTContext &Ctx, unsigned ArgCount, EmptyShell Empty)
    : Expr(CppxTemplateOrArrayExprClass, Empty) { }


CppxTemplateOrArrayExpr *
CppxTemplateOrArrayExpr::Create(const ASTContext &Ctx, Expr *BaseExpr,
                                ArrayRef<Expr *> Args) {
  void *Mem = Ctx.Allocate(sizeof(CppxTemplateOrArrayExpr)
                           + sizeOfTrailingObjects(Args.size()),
                           alignof(CppxTemplateOrArrayExpr));
  auto R =  new (Mem) CppxTemplateOrArrayExpr(Ctx, BaseExpr, Args);
  return R;
}

CppxTemplateOrArrayExpr *
CppxTemplateOrArrayExpr::CreateEmpty(const ASTContext &Ctx, unsigned NumArgs,
                                     EmptyShell Empty) {
  void *Mem = Ctx.Allocate(sizeof(CppxTemplateOrArrayExpr)
                           + sizeOfTrailingObjects(NumArgs),
                           alignof(CppxTemplateOrArrayExpr));
  return new (Mem) CppxTemplateOrArrayExpr(Ctx, NumArgs, Empty);
}


CppxCallOrConstructorExpr::CppxCallOrConstructorExpr(const ASTContext &Ctx,
                                                 Stmt *E,
                                                 ArrayRef<Expr *> Args)
  :Expr(CppxCallOrConstructorExprClass, Ctx.DependentTy, VK_LValue, OK_Ordinary),
  NumArgs(Args.size())
{
  CallOrConstructorBits.OffsetToTrailingObjects = sizeof(CppxCallOrConstructorExpr);
  setDependence(clang::ExprDependenceScope::ExprDependence::TypeValue
                |  clang::ExprDependenceScope::ExprDependence::TypeInstantiation
                |  clang::ExprDependenceScope::ExprDependence::ValueInstantiation
                |  clang::ExprDependenceScope::ExprDependence::TypeValueInstantiation);
  // Copying everything into the trailing arguments.
  getTrailingStmts()[0] = E;
  for (unsigned I = 0; I != Args.size(); ++I)
    setArg(I, Args[I]);
}

CppxCallOrConstructorExpr::CppxCallOrConstructorExpr(const ASTContext &Ctx, unsigned ArgCount, EmptyShell Empty)
    : Expr(CppxCallOrConstructorExprClass, Empty) { }


CppxCallOrConstructorExpr *
CppxCallOrConstructorExpr::Create(const ASTContext &Ctx, Expr *BaseExpr,
                                ArrayRef<Expr *> Args) {
  void *Mem = Ctx.Allocate(sizeof(CppxCallOrConstructorExpr)
                           + sizeOfTrailingObjects(Args.size()),
                           alignof(CppxCallOrConstructorExpr));
  auto R =  new (Mem) CppxCallOrConstructorExpr(Ctx, BaseExpr, Args);
  return R;
}

CppxCallOrConstructorExpr *
CppxCallOrConstructorExpr::CreateEmpty(const ASTContext &Ctx, unsigned NumArgs,
                                     EmptyShell Empty) {
  void *Mem = Ctx.Allocate(sizeof(CppxCallOrConstructorExpr)
                           + sizeOfTrailingObjects(NumArgs),
                           alignof(CppxCallOrConstructorExpr));
  return new (Mem) CppxCallOrConstructorExpr(Ctx, NumArgs, Empty);
}


CppxDerefOrPtrExpr::CppxDerefOrPtrExpr(const ASTContext &Ctx, Expr *E, SourceLocation L)
    : Expr(CppxDerefOrPtrExprClass, Ctx.DependentTy, VK_LValue, OK_Ordinary),
      Value(E), Loc(L) {
  setDependence(ExprDependenceScope::ExprDependence::TypeValue
              | ExprDependenceScope::ExprDependence::TypeInstantiation
              | ExprDependenceScope::ExprDependence::ValueInstantiation
              | ExprDependenceScope::ExprDependence::TypeValueInstantiation);
}
CppxDerefOrPtrExpr *CppxDerefOrPtrExpr::Create(const ASTContext &Ctx, Expr *E,
                                               SourceLocation L) {
  void *Mem = Ctx.Allocate(sizeof(CppxDerefOrPtrExpr),
                           alignof(CppxDerefOrPtrExpr));
  return new (Mem) CppxDerefOrPtrExpr(Ctx, E, L);
}

CppxWildcardExpr::CppxWildcardExpr(const ASTContext &C, SourceLocation Loc)
  : Expr(CppxWildcardExprClass, C.DependentTy, VK_LValue, OK_Ordinary),
    Loc(Loc)
{
  // Just set dependence so we crash if this reaches codegen.
  setDependence(ExprDependenceScope::ExprDependence::TypeValue
              | ExprDependenceScope::ExprDependence::TypeInstantiation
              | ExprDependenceScope::ExprDependence::ValueInstantiation
              | ExprDependenceScope::ExprDependence::TypeValueInstantiation);
}

CppxWildcardExpr *CppxWildcardExpr::Create(const ASTContext &C,
                                           SourceLocation Loc) {
  return new (C) CppxWildcardExpr(C, Loc);
}

// -----------------------------------------------------------------------------
CppxCXXScopeSpecExpr::CppxCXXScopeSpecExpr(ASTContext &Ctx, SourceLocation L, Expr *E)
  :Expr(CppxCXXScopeSpecExprClass, Ctx.VoidTy, VK_LValue, OK_Ordinary),
  Loc(L), SS(nullptr), CurExpr(E)
{ }

CppxCXXScopeSpecExpr *CppxCXXScopeSpecExpr::Create(ASTContext &C,
                                                   SourceLocation Loc,
                                                   Expr *Base) {
  return new (C) CppxCXXScopeSpecExpr(C, Loc, Base);
}


} // namespace clang
