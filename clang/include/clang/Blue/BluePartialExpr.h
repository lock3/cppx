//===- BluePartialExpr.h - Blue Language Parser ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the partial expression implementation member access
//  evaluation.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEPARTIALEXPR_H
#define CLANG_BLUE_BLUEPARTIALEXPR_H
#include "clang/AST/ExprCppx.h"
#include "clang/Blue/BlueSyntax.h"

namespace clang {
class LookupResult;
}

namespace blue {
class Sema;

enum class PartialExprKind : std::size_t {
  PEK_PartialNameAccessExprImpl
};

enum NameExprState {
  AwaitingBaseExpr,
  BuildingNormalNameAccessExpr,
  BuildingNamespaceExpr,
  BuildingBaseQualifiedExpr,
  BuildingTemplateQualifiedExpr
};

class PartialNameAccessExprImpl : public CppxPartialNameAccessBase {
  Sema &SemaRef;
  NameExprState State = AwaitingBaseExpr;
  bool InsideClass = false;
  bool AllowImplicitThisExpr = false;
  clang::Expr *CurExpr = nullptr;
  clang::Expr *NonNameLHS = nullptr;
  clang::CXXScopeSpec SS;
public:
  PartialNameAccessExprImpl(Sema &S)
    :CppxPartialNameAccessBase(PartialExprKind::PEK_PartialNameAccessExprImpl),
    SemaRef(S)
  { }

  virtual ~PartialNameAccessExprImpl() = default;

private:
  clang::Expr *normalAccess_appendName_updateTransition(
                             clang::SourceLocation Loc, clang::LookupResult &R);
  // bool isBaseOfCurrentInstance(clang::SourceLocation Loc, clang::QualType ThisTy);
  clang::Expr *baseQualified_appendName_updateTransition(clang::SourceLocation Loc,
                                                    clang::LookupResult &R);

  clang::Expr *NamespaceQualified_appendName_updateTransition(clang::SourceLocation Loc,
                                                    clang::LookupResult &R);
  clang::CXXRecordDecl *getCXXRecordFromLHS();
  void extenedSSFromExpr(clang::Expr *E);
  clang::Expr *buildNoMemberError(clang::SourceLocation Loc,
                                  clang::IdentifierInfo *Name);
  clang::Expr *buildKnownMemberExpr(clang::SourceLocation IdLoc,
                                    clang::IdentifierInfo *MemberId);

public:

  virtual clang::Expr *setIsWithinClass(bool IsInClassScope) override;
  virtual clang::Expr *allowUseOfImplicitThis(bool AllowImplicitThis) override;
  virtual clang::Expr *setBaseExpr(clang::Expr *) override;

  /// Return true if the given arguments can be handled applied to the
  /// partial expression, this could be template parameters or array access.
  virtual clang::Expr *appendName(clang::SourceLocation L,
                                  clang::IdentifierInfo *Id) override;
  virtual clang::Expr *appendElementExpr(clang::SourceLocation B,
                                         clang::SourceLocation E,
                                 clang::TemplateArgumentListInfo &TemplateArgs,
           llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ActualArgs,
           llvm::SmallVectorImpl<clang::Expr *> &OnlyExprArgs) override;
private:
  clang::Expr *doSingleRebuild(clang::NestedNameSpecifierLoc NNS,
                               clang::NestedNameSpecifier *CurNNS,
                               clang::Expr *LHS);
  clang::Expr *rebuildNestedNameSpecifier(clang::NestedNameSpecifierLoc NNS);
public:
  /// This is used to generate the complete expression.
  virtual clang::Expr *completeExpr() override;

  static bool classof(const CppxPartialNameAccessBase *S) {
    return S->getKind() == PartialExprKind::PEK_PartialNameAccessExprImpl;
  }

  clang::DiagnosticBuilder error(clang::SourceLocation Loc);
private:
};


}
#endif