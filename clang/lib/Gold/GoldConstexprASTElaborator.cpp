//===- GoldConstexprASTElaborator.cpp -------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldConstexprASTElaborator.h"
#include "clang/Gold/ClangToGoldDeclBuilder.h"
#include "clang/Gold/GoldElaborator.h"

namespace gold {

template<typename DclTy>
static bool handleConstExprLookupAndElaboration(Sema &SemaRef, DclTy *FD) {
  Declaration *D = SemaRef.getDeclaration(FD);
  // This means we have a cycle and we can't continue,
  // because we have a loop.
  if (D->IsElaborating)
    return false;

  // Focing the issue with elaboration.
  if (phaseOf(D) != Phase::Initialization)
    // Not elaborated yet.
    Elaborator(SemaRef.getContext(), SemaRef).elaborateDef(D);

  // if we have a definition then we are good.
  if (FD->getDefinition())
    return true;

  gold::Scope *CurScope = D->ScopeForDecl;
  // Moving out of any class declaration because that would seriously mess
  // things up, that and we don't care about the things inside of classes
  // they should have already been handled including but not limited to their
  // defintions.
  // At the time of writing this the ENTIRE class declaration is handled
  // by type level elaboration.
  while(CurScope && CurScope->isClassScope()) CurScope = CurScope->getParent();

  // Making sure that we are have deep elaboration mode enabled
  // This will make sure that any contexts that we do actually visit
  // will be fully elaborated as needed for evaluation.
  Sema::EnterDeepElabRAII DeepElab(SemaRef);
  while(CurScope) {
    for(auto DclPair : CurScope->DeclMap) {
      if (DclPair.second->getId() == D->getId()) {
        // This may come up as a problem later?
        // Skip anything that's in progress for now.
        if (DclPair.second->IsElaborating)
          continue;

        // While this is usually reserved for early elaboration of classes
        // inside of classes this is also for this type of early elaboration,
        // because the same things need to happen.
        Sema::EnterNonNestedClassEarlyElaboration Resumer(SemaRef,
                                                          DclPair.second);

        // If this has a nested name specifier we need to compute that before
        // we can compare if the declaration is complete or not.
        if (phaseOf(DclPair.second) == Phase::Identification)
          Elaborator(SemaRef.getContext(), SemaRef).elaborateDecl(DclPair.second);

        // If we don't have a body then this declaration doesn't matter because
        // it's not a definition.
        if (!DclPair.second->Init)
          continue;

        auto FnDcl = dyn_cast<DclTy>(DclPair.second->Cxx);
        if (FnDcl) {

          bool IsRedecl = false;
          for (auto Redecl : FD->redecls())
            if (Redecl == FnDcl) {
              IsRedecl = true;
              break;
            }

          // The current declaration is a redeclaration of something else, not
          // the current function.
          if (!IsRedecl)
            continue;
        } else
          // we are not a function declaration, but we might have the same name
          // and be from a different scope, but different nested
          // name specifier.
          continue;

        if (phaseOf(DclPair.second) == Phase::Typing)
          Elaborator(SemaRef.getContext(), SemaRef).elaborateDef(DclPair.second);

        if (FD->getDefinition())
          return true;
      }
    }
    CurScope = CurScope->getParent();
  }
  return false;
}



// This may lead to unusual dependency cycles, in the event that we have
// conflicting constant expression that aren't implicitly in conflict.
// Fixing this is a task for later on down the line.
bool GoldConstexprASTElaborator::TraverseFunctionDecl(
    clang::FunctionDecl *FD) {
  handleConstExprLookupAndElaboration(SemaRef, FD);
  return Base::TraverseFunctionDecl(FD);
}

bool GoldConstexprASTElaborator::TraverseCXXRecordDecl(
    clang::CXXRecordDecl *RD) {
  handleConstExprLookupAndElaboration(SemaRef, RD);
  return Base::TraverseCXXRecordDecl(RD);
}

bool GoldConstexprASTElaborator::TraverseCXXConstructorDecl(
    clang::CXXConstructorDecl *CD) {

  clang::CXXRecordDecl *Parent = cast<clang::CXXRecordDecl>(CD->getDeclContext());
  if (isa<clang::ClassTemplateSpecializationDecl>(Parent)) {
    ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
    clang::SourceRange Range(Parent->getSourceRange());
    Declaration *D = SemaRef.getDeclaration(Parent);
    if (!D)
      Rebuilder.rebuild(Parent);
  }
  handleConstExprLookupAndElaboration(SemaRef, CD);
  return Base::TraverseCXXConstructorDecl(CD);
}

bool GoldConstexprASTElaborator::TraverseCXXDestructorDecl(
    clang::CXXDestructorDecl *DD) {
  clang::CXXRecordDecl *Parent = cast<clang::CXXRecordDecl>(DD->getDeclContext());
  if (isa<clang::ClassTemplateSpecializationDecl>(Parent)) {
    ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
    clang::SourceRange Range(Parent->getSourceRange());
    Declaration *D = SemaRef.getDeclaration(Parent);
    if (!D)
      Rebuilder.rebuild(Parent);
  }
  handleConstExprLookupAndElaboration(SemaRef, DD);
  return Base::TraverseCXXDestructorDecl(DD);
}

bool GoldConstexprASTElaborator::TraverseCXXMethodDecl(
    clang::CXXMethodDecl *MD) {
  clang::CXXRecordDecl *Parent = cast<clang::CXXRecordDecl>(MD->getDeclContext());
  if (isa<clang::ClassTemplateSpecializationDecl>(Parent)) {
    ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
    clang::SourceRange Range(Parent->getSourceRange());
    Declaration *D = SemaRef.getDeclaration(Parent);
    if (!D)
      Rebuilder.rebuild(Parent);
  }
  handleConstExprLookupAndElaboration(SemaRef, MD);
  return Base::TraverseCXXMethodDecl(MD);
}


// -----------------------------------------------------------------------------
// Expressions that reference declarations that I will need to interact with
// -----------------------------------------------------------------------------

bool GoldConstexprASTElaborator::TraverseCXXTemporaryObjectExpr(
    clang::CXXTemporaryObjectExpr *E) {
  clang::CXXConstructorDecl *CtorDcl = E->getConstructor();
  Base::TraverseDecl(CtorDcl);
  return Base::TraverseCXXTemporaryObjectExpr(E);
}

bool GoldConstexprASTElaborator::TraverseCXXUnresolvedConstructExpr(
    clang::CXXUnresolvedConstructExpr *E) {
  auto TD = E->getTypeSourceInfo()->getType()->getAsTagDecl();
  if (TD)
    Base::TraverseDecl(TD);
  return Base::TraverseCXXUnresolvedConstructExpr(E);
}

bool GoldConstexprASTElaborator::TraverseDeclRefExpr(clang::DeclRefExpr *E) {
  Base::TraverseDecl(E->getDecl());
  return Base::TraverseDeclRefExpr(E);
}

bool GoldConstexprASTElaborator::TraverseCallExpr(clang::CallExpr *E) {
  Base::TraverseDecl(E->getCalleeDecl());
  return Base::TraverseCallExpr(E);
}

bool GoldConstexprASTElaborator::TraverseCXXConstructExpr(
    clang::CXXConstructExpr *E) {
  Base::TraverseDecl(E->getConstructor());
  return Base::TraverseCXXConstructExpr(E);
}

bool GoldConstexprASTElaborator::TraverseCXXMemberCallExpr(
    clang::CXXMemberCallExpr *E) {
  Base::TraverseDecl(E->getRecordDecl());
  Base::TraverseDecl(E->getMethodDecl());
  return Base::TraverseCXXMemberCallExpr(E);
}

bool GoldConstexprASTElaborator::TraverseUnresolvedMemberExpr(
    clang::UnresolvedMemberExpr *E) {
  Base::TraverseDecl(E->getNamingClass());
  for (clang::Decl *D : E->decls())
    Base::TraverseDecl(D);
  return Base::TraverseUnresolvedMemberExpr(E);
}

bool GoldConstexprASTElaborator::TraverseUnresolvedLookupExpr(
    clang::UnresolvedLookupExpr *E) {
  for (clang::Decl *D : E->decls())
    Base::TraverseDecl(D);
  return Base::TraverseUnresolvedLookupExpr(E);
}

bool GoldConstexprASTElaborator::TraverseCXXOperatorCallExpr(
    clang::CXXOperatorCallExpr *E) {
  Base::TraverseDecl(E->getCalleeDecl());
  return Base::TraverseCXXOperatorCallExpr(E);
}


} // end namespace gold