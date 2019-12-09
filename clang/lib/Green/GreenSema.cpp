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
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Green/Syntax.h"
#include "clang/Green/GreenScope.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/IdentifierMapper.h"
#include "clang/Green/Elaborator.h"

namespace green {

using namespace llvm;

GreenSema::GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, 
                     clang::Sema &ClangSema)
  : Context(Context), PP(PP), ClangSema(ClangSema)
{
  OperatorColonII = PP.getIdentifierInfo("operator':'");
  OperatorExclaimII = PP.getIdentifierInfo("operator'!'");
  OperatorEqualsII = PP.getIdentifierInfo("operator'='");
}

GreenScope *
GreenSema::getCurScope() {
  return ScopeStack.back();
}

void
GreenSema::PushScope(GreenScope *S) {
  ScopeStack.push_back(S);
}

GreenScope *
GreenSema::PopScope() {
  GreenScope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

bool
GreenSema::LookupName(clang::LookupResult &R, GreenScope *S) {
  using namespace clang;

  DeclarationName Name = R.getLookupName();

  // If the scope is associated with a Declaration, we can just use the
  // declaration's lookup.
  if (S->Association.is<Decl *>()) {
    DeclContext *Ctx = cast<DeclContext>(S->Association.get<Decl *>());
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

