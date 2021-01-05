//===- GoldConstexprASTElaborator.h ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Handles decl/def triggering elaboration inside of constant expressions for
//  a functions, methods, or variables where the definition is required, exists
//  but isn't defined until later.
//
//  We do this because we will need to identify possible function overloads
//  that would exist within the current scope and match those with other
//  declarations of the same kind, but defined later.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDCONSTEXPRASTELABORATOR_H
#define CLANG_GOLD_GOLDCONSTEXPRASTELABORATOR_H

#include "clang/Gold/GoldSema.h"
#include "clang/AST/RecursiveASTVisitor.h"


namespace gold {

class GoldConstexprASTElaborator
    : public clang::RecursiveASTVisitor<GoldConstexprASTElaborator> {
  using Base = clang::RecursiveASTVisitor<GoldConstexprASTElaborator>;

  Sema &SemaRef;
public:
  GoldConstexprASTElaborator(Sema &SemaRef)
      : SemaRef(SemaRef){ }

  bool TraverseFunctionDecl(clang::FunctionDecl *FD);
  bool TraverseCXXRecordDecl(clang::CXXRecordDecl *RD);
  bool TraverseCXXConstructorDecl(clang::CXXConstructorDecl *CD);
  bool TraverseCXXDestructorDecl(clang::CXXDestructorDecl *DD);
  bool TraverseCXXMethodDecl(clang::CXXMethodDecl *MD);


  bool TraverseCXXTemporaryObjectExpr(clang::CXXTemporaryObjectExpr *E);
  bool TraverseCXXUnresolvedConstructExpr(clang::CXXUnresolvedConstructExpr *E);
  bool TraverseDeclRefExpr(clang::DeclRefExpr *E);
  bool TraverseCallExpr(clang::CallExpr *E);
  bool TraverseCXXConstructExpr(clang::CXXConstructExpr *E);
  bool TraverseCXXMemberCallExpr(clang::CXXMemberCallExpr *E);
  bool TraverseUnresolvedMemberExpr(clang::UnresolvedMemberExpr *E);
  bool TraverseUnresolvedLookupExpr(clang::UnresolvedLookupExpr *E);
  bool TraverseCXXOperatorCallExpr(clang::CXXOperatorCallExpr *E);


  // bool TraverseDeclRefExpr(clang::DeclRefExpr *S) {
  //   llvm_unreachable("Working on it.");
  //   // if (!S)
  //   //   return true;
  //   // StmtStack.push_back(S);
  //   // bool Result = Base::TraverseStmt(S);
  //   // StmtStack.pop_back();
  //   // return Result;
  // }


};

}

#endif