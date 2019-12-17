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
  : Context(Context), CxxSema(CxxSema)
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
  if (S->isDeclarationScope()) {
    clang::Decl *D = S->getDeclaration();

    if (isa<clang::FunctionDecl>(D))
      CxxSema.PushFunctionScope();
  }

  ScopeStack.push_back(S);
}

void GreenSema::enterScope(const Syntax *S, clang::Decl *D) {
  // FIXME: We're leaking scopes. We probably want to keep them bound
  // to the syntax for which they're created -- especially for syntaxes
  // that correspond to declarations, so that we can easily find their
  // associated lookup tables.
  pushScope(new (Context) GreenScope(S, D, getCurrentScope()));
}

GreenScope *GreenSema::popScope() {
  GreenScope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

void GreenSema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  popScope();
}

bool
GreenSema::LookupName(clang::LookupResult &R, GreenScope *S) {
  using namespace clang;

  DeclarationName Name = R.getLookupName();

  // If the scope is associated with a Declaration, we can just use the
  // declaration's lookup.
  if (S->isDeclarationScope()) {
    DeclContext *Ctx = cast<DeclContext>(S->getDeclaration());
    DeclContextLookupResult CtxResult = Ctx->lookup(Name);

    // Add any results to the LookupResult.
    for (auto *DeclIterator : CtxResult)
      R.addDecl(DeclIterator);

    if (CtxResult.empty() && S->getParent())
      return LookupName(R, S->getParent());
    else return CtxResult.empty();
  }

  return false;
}

} // namespace usyntax

