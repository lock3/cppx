//===- GoldDependentExprTransformer.h -------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file is specifically used to generate code from a given dependent
//  expression, that could possibly change meaning or type.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLD_DEPENDENT_EXPR_TRANSFORMER_H
#define CLANG_GOLD_GOLD_DEPENDENT_EXPR_TRANSFORMER_H

#include "clang/Gold/GoldSema.h"

namespace gold {

/// This class takes an expression, and attempts to convert it into a valid
/// gold expression. In Gold expressions can result in types.
class DependentExprTransformer {
public:
  Sema &SemaRef;
  SyntaxContext &Context;
  const clang::MultiLevelTemplateArgumentList &TemplateArgs;
  clang::SourceLocation InstantiationLoc;
  clang::DeclarationName EntityName;

  DependentExprTransformer(Sema &S, SyntaxContext &Ctx,
                           const clang::MultiLevelTemplateArgumentList &TemplateArgs,
                           clang::SourceLocation Loc,
                           clang::DeclarationName Entity);

  clang::Expr *transformDependentExpr(clang::Expr *E);


  clang::Expr *transformCppxDependentMemberAccessExpr(
                                       clang::CppxDependentMemberAccessExpr *E);
  clang::Expr *transformCppxTemplateOrArrayExpr(clang::CppxTemplateOrArrayExpr *E);
  clang::Expr *transformCppxDerefOrPtrExpr(clang::CppxDerefOrPtrExpr* E);
  clang::Expr *transformTemplateInstantiation(
      clang::CppxTemplateOrArrayExpr *E, clang::TemplateDecl *D);
  /// This takes the given arguments and attempts to transform them
  /// into something that we could use for our template instantiation.
  bool transformTemplateArguments(clang::CppxTemplateOrArrayExpr *E,
                                  clang::TemplateArgumentListInfo &ArgInfo,
              llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArgs);

  clang::Expr *transformCppxLiteralType(clang::CppxTypeLiteral *E);
  clang::QualType transformType(clang::QualType Ty);
  clang::QualType transformSubstTemplateTypeParmType(
                                 const clang::SubstTemplateTypeParmType *STTPT);
  clang::QualType transformTemplateSpecializationType(
               const clang::TemplateSpecializationType* TemplateSpecialization);
  clang::QualType transformTypeExprType(const clang::CppxTypeExprType *Ty);
  clang::Expr *buildDeclRefExpr(clang::VarDecl *VD, clang::SourceLocation Loc);
  clang::Expr *buildUnresolvedCall(clang::Expr *LHS,
                                   clang::CXXRecordDecl *RD,
                                   clang::LookupResult &Overloads,
                                   clang::SourceLocation Loc,
                                   bool UnresolvedUsing = false);

  clang::Expr *buildMemberAccessExpr(clang::Expr *LHS, clang::QualType Ty,
                                     clang::NamedDecl *Dcl,
                                     clang::CXXRecordDecl *RD,
                                     clang::LookupResult &Overloads,
                                     clang::SourceLocation Loc,
                                     bool IsArrow,
                                     bool UnresolvedUsing = false);

};
}

#endif