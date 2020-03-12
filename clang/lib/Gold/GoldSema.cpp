//===- GoldSema.cpp - Semantic Analysis of Gold ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the gold::Sema class, which performs semantic analysis
//  for the Gold language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Sema/Lookup.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldElaborator.h"

namespace gold {

using namespace llvm;

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : CxxSema(CxxSema), CurrentDecl(), Context(Context),
    Diags(Context.CxxAST.getSourceManager().getDiagnostics())
{
  OperatorColonII = &Context.CxxAST.Idents.get("operator':'");
  OperatorExclaimII = &Context.CxxAST.Idents.get("operator'!'");
  OperatorEqualsII = &Context.CxxAST.Idents.get("operator'='");
  OperatorIfII = &Context.CxxAST.Idents.get("operator'if'");
  OperatorElseII = &Context.CxxAST.Idents.get("operator'else'");
  OperatorReturnII = &Context.CxxAST.Idents.get("operator'return'");
  OperatorReturnsII = &Context.CxxAST.Idents.get("operator'returns'");
}

Scope *Sema::getCurrentScope() {
  if (ScopeStack.empty())
    return nullptr;
  return ScopeStack.back();
}

void Sema::pushScope(Scope *S) {
  // FIXME: The scope should self-describe itself. We can't rely on
  // the existence of Clang structures at the time we push a scope.
  // if (S->isDeclarationScope())
  //   CxxSema.PushFunctionScope();

  ScopeStack.push_back(S);
}

Scope *Sema::popScope() {
  Scope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

void Sema::enterScope(ScopeKind K, const Syntax *S) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new Scope(K, S, getCurrentScope()));
}

void Sema::enterScope(clang::CXXRecordDecl* R, const Syntax* S) {
  pushScope(new Scope(SK_Class, S, getCurrentScope(), R));
}

void Sema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

Scope *Sema::saveScope(const Syntax *S) {
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Queue the scope for subsequent deletion?
  Scope *Scope = getCurrentScope();
  popScope();
  return Scope;
}

clang::DeclContext *Sema::getCurrentCxxDeclContext() {
  return CurrentDecl->getCxxContext();
}

void Sema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);

  // FIXME: this might be an incorrect assertion.
  assert(D->Cxx && isa<clang::DeclContext>(D->Cxx)
         && "No Cxx declaration to push.");

  CurrentDecl = D;
  getCxxSema().CurContext = clang::Decl::castToDeclContext(D->Cxx);
}

void Sema::popDecl() {
  CurrentDecl = CurrentDecl->getOwner();
  getCxxSema().CurContext = CurrentDecl ?
    clang::Decl::castToDeclContext(CurrentDecl->Cxx) : nullptr;
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S) {
  assert(S);

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "Invalid id");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  if (LookupKind == clang::Sema::LookupTagName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end())
      return true;
  }

  while (S) {
    std::set<Declaration *> Found = S->findDecl(Id);
    if (!Found.empty()) {
      for (auto *FoundDecl : Found) {
        // If we find a name that hasn't been elaborated,
        // then we actually need to elaborate it.
        if (!FoundDecl->Cxx)
          Elaborator(Context, *this).elaborateDeclEarly(FoundDecl);

        clang::NamedDecl *ND = cast<clang::NamedDecl>(FoundDecl->Cxx);

        // FIXME: check if this is a tag decl, not a type decl!
        if (LookupKind == clang::Sema::LookupTagName &&
            !isa<clang::TypeDecl>(ND)) {
          // FIXME: Give a proper diagnostic once we implement hiding.
          // unsigned DiagID = Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
          //                                         "Tag is hidden.");
          // Diags.Report(clang::SourceLocation(), DiagID);
          return false;
        }

        // If there is a described template, add that to the result instead
        // of the bare declaration.
        if (FoundDecl->declaresTemplate()) {
          if (auto *FD = dyn_cast<clang::FunctionDecl>(ND))
            R.addDecl(FD->getDescribedFunctionTemplate());
          else if (auto *VD = dyn_cast<clang::VarDecl>(ND))
            R.addDecl(VD->getDescribedVarTemplate());
        } else {
          R.addDecl(ND);
        }
      }

      break;
    }

    S = S->getParent();
  }

  return !R.empty();
}

} // namespace gold

