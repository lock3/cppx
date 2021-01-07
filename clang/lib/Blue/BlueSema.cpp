//===- BlueSema.cpp - Semantic Analysis of Blue ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Blue::Sema class, which performs semantic analysis
//  for the Blue language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/TypeLocUtil.h"

#include "clang/Blue/BlueScope.h"
#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueSyntax.h"

namespace blue {

const llvm::StringMap<clang::QualType> Sema::createBuiltinTypeList() {
  return {
    {"void", CxxAST.VoidTy},
    {"bool", CxxAST.BoolTy},
    {"null_t", CxxAST.NullPtrTy},

    // character
    {"char", CxxAST.CharTy},

    // signed integers.
    {"int", CxxAST.IntTy},
  };
}
static llvm::StringMap<clang::BinaryOperatorKind> getBinOpMapping() {
  return llvm::StringMap<clang::BinaryOperatorKind>{
    {"+", clang::BO_Add},
    {"-", clang::BO_Sub},
    {"^", clang::BO_Xor},
    {"/", clang::BO_Div},
    {"%", clang::BO_Rem},
    {"*", clang::BO_Mul},
    {"|", clang::BO_Or},
    {"<<", clang::BO_Shl},
    {">>", clang::BO_Shr},
    {"||", clang::BO_LOr},
    {"&&", clang::BO_LAnd},
    {"<", clang::BO_LT},
    {">", clang::BO_GT},
    {"<=", clang::BO_LE},
    {">=", clang::BO_GE},
    {"==", clang::BO_EQ},
    {"!=", clang::BO_NE},
    {"=", clang::BO_Assign},
    {"+=", clang::BO_AddAssign},
    {"-=", clang::BO_SubAssign},
    {"*=", clang::BO_MulAssign},
    {"/=", clang::BO_DivAssign},
    {"%=", clang::BO_RemAssign},
    {"^=", clang::BO_XorAssign},
    {"|=", clang::BO_OrAssign},
    {"&=", clang::BO_AndAssign},
    {"<<=", clang::BO_ShlAssign},
    {">>=", clang::BO_ShrAssign}
  };
}

static llvm::StringMap<clang::UnaryOperatorKind> getUnaryOperatorMapping() {
  return llvm::StringMap<clang::UnaryOperatorKind>{
    // TODO: Figure out if this is pre or post inc/dec
    // {"++", clang::},
    // PreInc
    // PostInc
    // {"++", clang::},
    // PreDec
    // PostDec
    {"~", clang::UO_Not},
    {"!", clang::UO_LNot},
    {"^", clang::UO_Deref},
    {"&", clang::UO_AddrOf},
    {"+", clang::UO_Plus},
    {"-", clang::UO_Minus}
  };
}

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : Context(Context), CxxSema(CxxSema),
    CxxAST(Context.CxxAST),
    BinOpMap(getBinOpMapping()),
    UnaryOpMap(getUnaryOperatorMapping()),
    BuiltinTypes(createBuiltinTypeList())
{ }

Sema::~Sema() {
}

clang::Sema &Sema::getCxxSema() {
  return CxxSema;
}

clang::ASTContext &Sema::getCxxAST() {
  return CxxAST;
}

Scope *Sema::getCurrentScope() {
  return ScopeStack.empty() ? nullptr : ScopeStack.back();
}

void Sema::enterScope(Scope::Kind K, const Syntax *S) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new Scope(K, S, getCurrentScope()));
}

void Sema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

void Sema::pushScope(Scope *S) {
  assert(S && "Invalid scope");
  ScopeStack.push_back(S);
}

Scope *Sema::popScope() {
  Scope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S) {
  assert(S && "lookup in non-existent scope");

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "looking up non-existent name");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  // First check if this is a builtin type name.
  if (LookupKind == clang::Sema::LookupAnyName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end()) {
      if (BuiltinMapIter->second.isNull()) {
        // Diags.Report(clang::SourceLocation(),
        //              clang::diag::err_invalid_builtin_type) << Id;
        return false;
      }

      return true;
    }
  }

  // Iterate through all scopes and stop if we find something.
  for(; S; S = S->getParent()) {
    // FIXME: implement
  }

  return true;
}

clang::CppxTypeLiteral *Sema::buildTypeExpr(clang::QualType Ty,
                                            clang::SourceLocation Loc) {
  return buildAnyTypeExpr(CxxAST.CppxKindTy, Ty, Loc);
}

clang::CppxTypeLiteral *Sema::buildAnyTypeExpr(clang::QualType KindTy,
    clang::TypeSourceInfo *TInfo) {
  assert(TInfo && "Invalid type information.");
  return clang::CppxTypeLiteral::create(Context.CxxAST, KindTy, TInfo);
}

clang::CppxTypeLiteral *Sema::buildAnyTypeExpr(clang::QualType KindTy,
    clang::QualType Ty, clang::SourceLocation Loc) {
  return buildAnyTypeExpr(KindTy, gold::BuildAnyTypeLoc(Context.CxxAST, Ty, Loc));
}

} // end namespace Blue
