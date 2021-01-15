//===- BlueElaborator.h - Blue Language Elaborator ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Translates concrete syntax into C++ AST trees.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEELABORATOR_H
#define CLANG_BLUE_BLUEELABORATOR_H

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Sema/Sema.h"

namespace clang {

class CppxTypeLiteral;
class Decl;
class DiagnosticsEngine;
class Expr;
class QualType;
class Sema;
class Stmt;

} // namespace clang

namespace blue
{
class Declarator;

/// Transforms Blue syntax into C++ ASTs.
class Elaborator
{
public:
  Elaborator(Sema &S)
    : SemaRef(S), CxxSema(S.getCxxSema())
  { }

  /// Returns the current state of C++ translation.
  clang::Sema &getCxxSema() {
    return CxxSema;
  }

  /// Returns the C++ AST context.
  clang::ASTContext &getCxxContext() {
    return getCxxSema().Context;
  }
  /// Returns the current blue sema instance.
  Sema &getBlueSema() { return SemaRef; }

  //===--------------------------------------------------------------------===//
  //                                  Identification                          //
  //===--------------------------------------------------------------------===//

  Declaration *createDeclaration(const Syntax *Def, Declarator *Dcl,
                                 const Syntax *Init);

  //===--------------------------------------------------------------------===//
  //                                  Elaboration                             //
  //===--------------------------------------------------------------------===//
  clang::Decl *elaborateTop(const Syntax *S);

  void identifyDeclaration(const Syntax *S);
  void buildDeclaration(const DefSyntax *S);
  clang::Decl *elaborateDecl(const Syntax *S);
  clang::Decl *elaborateDefDecl(const DefSyntax *S);
  clang::Decl *elaborateDeclarationTyping(Declaration *D);

  void elaborateParameters(const ListSyntax *S);
  void elaborateParameterGroup(const ListSyntax *S);
  void elaborateParameterList(const ListSyntax *S);
  clang::Decl *elaborateParameter(const Syntax *S);

  Declarator *getDeclarator(const Syntax *S);
  Declarator *getUnaryDeclarator(const UnarySyntax *S);
  Declarator *getBinaryDeclarator(const BinarySyntax *S);
  Declarator *getLeafDeclarator(const Syntax *S);
  Declarator *getImplicitAutoDeclarator();

  clang::Decl *elaborateDeclEarly(Declaration *D);

  clang::Expr *elaborateDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateTypeDeclarator(const Declarator *Dcl);
  clang::Expr *elaboratePointerDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateArrayDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateFunctionDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateTemplateDeclarator(const Declarator *Dcl);
  clang::Expr *elaborateImplicitTypeDeclarator(const Declarator *Dcl);


  clang::Decl *makeValueDecl(Declaration *D);
  clang::Decl *makeObjectDecl(Declaration *D, clang::Expr *Ty);
  clang::Decl *makeTypeDecl(Declaration *D, clang::QualType T);
  clang::Decl *makeFunctionDecl(Declaration *D);
  clang::Decl *makeTemplateDecl(Declaration *D);

  clang::CppxTypeLiteral *createFunctionType(Declarator *Dcl);

  void elaborateDefinition(const Syntax *S);
  void elaborateDefinitionInitialization(Declaration *D);
  void elaborateVarDef(Declaration *D);
  void elaborateFunctionDef(Declaration *D);


  clang::Expr *elaborateExpression(const Syntax *S);
  clang::Expr *elaborateConstantExpression(const Syntax *S);
  clang::Expr *doElaborateExpression(const Syntax *S);

  clang::Expr *elaborateLiteralExpression(const LiteralSyntax *S);
  clang::Expr *elaborateIdentifierExpression(const IdentifierSyntax *S);
  clang::Expr *elaborateListExpression(const ListSyntax *S);
  clang::Expr *elaborateSeqExpression(const SeqSyntax *S);
  clang::Expr *elaborateUnaryExpression(const UnarySyntax *S);
  clang::Expr *elaborateBinaryExpression(const BinarySyntax *S);
  clang::Expr *elaborateApplyExpression(clang::Expr *LHS,
                                        const BinarySyntax *S);


  clang::Expr *elaborateIntegerMetaFunction(const BinarySyntax *S);
  clang::Expr *elaborateCharacterMetaFunction(const BinarySyntax *S);
  clang::Expr *elaborateRealMetaFunction(const BinarySyntax *S);

  /// Stmts
  clang::Stmt *elaborateSeq(const SeqSyntax *S);
  clang::Stmt *elaborateStatement(const Syntax *S);
  clang::Stmt *elaborateUnaryStmt(const UnarySyntax *S);
  clang::Stmt *elaborateControlStmt(const ControlSyntax *S);
  clang::Stmt *elaborateIfStmt(const ControlSyntax *S);
  clang::Stmt *elaborateReturnStmt(const UnarySyntax *S);

  //===--------------------------------------------------------------------===//
  //                                Miscellaneous                             //
  //===--------------------------------------------------------------------===//
  void getParameters(Declaration *D,
                     llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params);

  // Diagnostics
  void Error(clang::SourceLocation Loc, llvm::StringRef Msg);

private:
  std::size_t ImplicitSemaDecls = 0;

  /// Context for the depth and index of template or auto parameters.
  struct SavedTemplateParamContext {
    unsigned Index = 0;
    unsigned Depth = 0;
  };

  SavedTemplateParamContext TempCtx;

  struct TemplateParamRAII {
    TemplateParamRAII(SavedTemplateParamContext &Ctx)
      : Ctx(Ctx), Old({Ctx.Index, Ctx.Depth})
      {}

    ~TemplateParamRAII() {
      Ctx.Index = Old.Index;
      Ctx.Depth = Old.Depth;
    }

  private:
    SavedTemplateParamContext &Ctx;
    SavedTemplateParamContext Old;
  };

private:
  Sema &SemaRef;

  clang::Sema &CxxSema;
};

} // namespace blue

#endif
