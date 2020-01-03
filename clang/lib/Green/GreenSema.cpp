//===- GreenSema.cpp - Semantic Analysis of Green ASTs --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GreenSema class, which performs semantic analysis
//  for the Green language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Stmt.h"
#include "clang/Sema/Lookup.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Green/Syntax.h"
#include "clang/Green/GreenScope.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/IdentifierMapper.h"
#include "clang/Green/Elaborator.h"

namespace green {

using namespace llvm;

GreenSema::GreenSema(SyntaxContext &Context, clang::Sema &CxxSema)
  : Context(Context), CxxSema(CxxSema), CurrentDecl()
{
  OperatorColonII = &Context.CxxAST.Idents.get("operator':'");
  OperatorExclaimII = &Context.CxxAST.Idents.get("operator'!'");
  OperatorEqualsII = &Context.CxxAST.Idents.get("operator'='");
  OperatorIfII = &Context.CxxAST.Idents.get("operator'if'");
  OperatorElseII = &Context.CxxAST.Idents.get("operator'else'");
}

GreenScope *GreenSema::getCurrentScope() {
  if (ScopeStack.empty())
    return nullptr;
  return ScopeStack.back();
}

void GreenSema::pushScope(GreenScope *S) {
  // FIXME: The scope should self-describe itself. We can't rely on
  // the existence of Clang structures at the time we push a scope.
  // if (S->isDeclarationScope())
  //   CxxSema.PushFunctionScope();

  ScopeStack.push_back(S);
}

GreenScope *GreenSema::popScope() {
  GreenScope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

void GreenSema::enterScope(ScopeKind K, const Syntax *S) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new GreenScope(K, S, getCurrentScope()));
}

void GreenSema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

GreenScope *GreenSema::saveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Queue the scope for subsequent deletion?
  GreenScope *Scope = getCurrentScope();
  popScope();
  return Scope;
}

clang::DeclContext *GreenSema::getCurrentCxxDeclContext() {
  return CurrentDecl->getCxxContext();
}

void GreenSema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);
  CurrentDecl = D;
}

void GreenSema::popDecl() {
  CurrentDecl = CurrentDecl->getOwner();
}

bool GreenSema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}

bool GreenSema::lookupUnqualifiedName(clang::LookupResult &R, GreenScope *S) {
  assert(S);

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "Invalid id");

  while (S) {
    // FIXME: This could find a set of declarations. Note that we could find
    // several declarations, some of which have not been elaborated.
    Declaration *Found = S->findDecl(Id);
    if (Found) {
      // FIXME: This is wrong! If we find a name that hasn't been elaborated,
      // then we actually need to elaborate it.
      assert(Found->Cxx && "Declaration not elaborated");
      clang::NamedDecl *ND = cast<clang::NamedDecl>(Found->Cxx);
      R.addDecl(ND);
      break;
    }
    S = S->getParent();
  }

  return R.empty();
}

} // namespace usyntax

