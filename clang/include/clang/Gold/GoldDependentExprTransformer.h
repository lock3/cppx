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
                                   clang::DeclContext::lookup_result &Overloads,
                                   const clang::DeclarationNameInfo &DNI,
                                   clang::SourceLocation Loc);
  clang::Expr *lookupIncontext(clang::DeclContext *PreviousContext,
                               clang::Expr *Expr);
private:
  clang::DeclContext *LookupContext = nullptr;
  struct LookupContextRAII {
    DependentExprTransformer &Transformer;
    clang::DeclContext *PreviousDC = nullptr;
    LookupContextRAII(DependentExprTransformer &T,
                      clang::DeclContext *NextDC = nullptr)
      :Transformer(T), PreviousDC(T.LookupContext)
    {
      Transformer.LookupContext = NextDC;
    }

    void init(clang::DeclContext *DC) {
      Transformer.LookupContext = DC;
    }

    ~LookupContextRAII() {
      Transformer.LookupContext = PreviousDC;
    }
  };

};
}

#endif