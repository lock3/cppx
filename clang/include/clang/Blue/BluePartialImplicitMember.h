//===- BluePartialImplicitMember.h ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Partial expression for transforming member access expressions into
//  free function calls implicitly.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEPARTIALIMPLICITMEMBER_H
#define CLANG_BLUE_BLUEPARTIALIMPLICITMEMBER_H

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/AST/ExprCppx.h"

// #include "clang/AST/TemplateBase.h"

namespace clang {
class CppxTypeLiteral;
class Decl;
class DiagnosticsEngine;
class Expr;
class QualType;
class Sema;
class Stmt;
class NamedDecl;
// class CppxPartialEvalExpr;
} // namespace clang

namespace blue
{

class CppxPartialImplicitMemberTransform : public  CppxMemberAccessTransfrom {
  clang::Expr *BaseExpr;
  clang::DeclarationNameInfo NameInfo;
  Sema &SemaRef;
public:
  CppxPartialImplicitMemberTransform(Sema &Sema)
    :CppxMemberAccessTransfrom(PartialExprKind::ImplicitMemberTransform),
    SemaRef(Sema)
  { }

  virtual ~CppxPartialImplicitMemberTransform() = default;

  virtual clang::Expr *setLhsExpr(clang::Expr *E) override;
  virtual clang::Expr *getLhsExpr();

  virtual void setName(clang::DeclarationNameInfo DNI) override;

  virtual const clang::DeclarationNameInfo &getName() override;

  virtual clang::Expr *appendCall(clang::SourceLocation B,
                                  clang::SourceLocation E,
                                  const ExprList &Args) override;

  virtual clang::Expr *appendElementExpr(clang::SourceLocation Beginning,
                                         clang::SourceLocation EndingLoc,
                                  clang::TemplateArgumentListInfo &TemplateArgs,
               llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ActualArgs,
                        llvm::SmallVectorImpl<clang::Expr *> &OnlyExprArgs) override;

  static bool classof(const CppxMemberAccessTransfrom *X) {
    return X->getKind() == PartialExprKind::ImplicitMemberTransform;
  }
};

} // namespace blue

#endif
