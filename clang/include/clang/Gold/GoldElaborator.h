//=== GoldElaborator.h - Elaboration for Gold Nodes -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Elaborator interface, which creates clang::Decl*
//  nodes out of Gold Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_ELABORATOR_H
#define CLANG_GOLD_ELABORATOR_H

#include "clang/AST/Type.h"
#include "clang/AST/Decl.h"

#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldLateElaboration.h"
#include "clang/Gold/GoldSyntaxContext.h"

#include <unordered_map>

namespace clang
{
class Preprocessor;
class Stmt;
class CXXRecordDecl;
} // namespace clang

namespace gold {

class Declarator;
class Declaration;
class Sema;

// Elaborator takes a gold::Syntax tree as input and outputs an equivalent
// C++ AST. It works closely with the ExprElaborator and StmtElaborator.
class Elaborator {
public:
  /// The AST context for the Gold language.
  SyntaxContext &Context;

  /// The state of translation for the Gold language.
  Sema &SemaRef;

public:
  Elaborator(SyntaxContext &Context, Sema &SemaRef);

  clang::Decl *elaborateFile(const Syntax *S);

  // Typing elaboration (2nd pass)
  clang::Decl *elaborateDeclType(const Syntax* D);
  clang::Decl *elaborateDecl(Declaration *D);
  clang::Decl *elaborateFunctionDecl(Declaration *D);
  clang::Decl *elaborateVariableDecl(Declaration *D);
  clang::Decl *elaborateTypeAlias(Declaration *D);
  clang::Decl *elaborateTemplateAliasOrVariable(Declaration *D,
      Declarator *TemplateParams);
  clang::Decl *elaborateParameterDecl(Declaration *D);
  clang::Decl *elaborateTemplateParamDecl(Declaration *D);

  // Definition elaboration (3rd pass)
  void elaborateDeclInit(const Syntax *S);
  void elaborateDef(Declaration *D);
  void elaborateFunctionDef(Declaration *D);
  void elaborateVariableInit(Declaration *D);
  void elaborateTemplateParamInit(Declaration *D);

  // Perform all three passes on a single declaration in one shot.
  // This is used to elaborate parameters and block-scope variables.
  clang::Decl *elaborateDeclSyntax(const Syntax* S);

  // Perform the latter two passes on a Declaration that was previously
  // identified. This is used when lookup finds an unelaborated declaration.
  clang::Decl *elaborateDeclEarly(Declaration *D);

  clang::Decl *elaborateDeclForArray(const ArraySyntax *S);
  clang::Decl *elaborateDeclForList(const ListSyntax *S);
  clang::Decl *elaborateDeclForCall(const CallSyntax *S);
  clang::Decl *elaborateDeclForAtom(const AtomSyntax *S);

  clang::Decl *elaborateTypeExpression(Declaration* Decl);

  // Type elaboration
  clang::Expr *elaborateType(Declarator *D);
  clang::Expr *elaboratePointerType(Declarator *D, clang::QualType T);
  clang::Expr *elaborateArrayType(Declarator *D, clang::QualType T);
  clang::Expr *elaborateFunctionType(Declarator *D, clang::QualType T);
  clang::Expr *elaborateExplicitType(Declarator *D, clang::QualType T);

  clang::Decl *elaborateTypeBody(Declaration *D, clang::CXXRecordDecl *R);
  clang::Decl *elaborateField(Declaration *D);
  void elaborateFieldInit(Declaration *D);


  // Identification (1st pass)
  Declaration *identifyDecl(const Syntax *S);

  // Semantic actions.
  void startFile(const Syntax *S);
  void finishFile(const Syntax *S);
  
  /// This returns true if part of the declaration was delayed.
  bool delayElaborateDeclType(const Syntax *S);
  
  /// Functionality associated with late elaboration and are used to either
  /// elaborate the full class or elaborate everything if they are able to.
  void delayElaborateMemberInitializer(Declaration *D);
  void delayElaborateMethodDecl(Declaration *D);
  void delayElaborateMethodDef(Declaration *D);
  void delayElaborationClassBody(Declaration *D);

  // TODO: This is going to need an implementation at some point becasue
  // currently I don't think we support default parameters.
  void delayElaborateDefaultArgument(Declaration *ParamDecl);


  /// Functions used for late elaboration processing.
  void finishDelayedElaboration(ElaboratingClass &Class);
  void lateElaborateAttributes(ElaboratingClass &Class);
  void lateElaborateMethodDecls(ElaboratingClass &Class);
  void lateElaborateMemberInitializers(ElaboratingClass &Class);
  void lateElaborateMethodDefs(ElaboratingClass &Class);
  
  /// Special callbacks used in order to interact a lateElaborated class.
  void lateElaborateAttribute(LateElaboratedAttributeDecl &Field);
  void lateElaborateMethodDef(LateElaboratedMethodDef &Method);
  void lateElaborateDefaultArgument(
      LateElaboratedDefaultArgument &DefaultParam);
  void lateElaborateMethodDecl(LateElaboratedMethodDeclaration &Method);
  void lateElaborateMemberInitializer(
      LateElaborateMemberInitializer &MemberInit);
};

/// Represents different kinds of fused operator strings, for example,
/// `operator'='` or `operator'return'`.
enum FusedOpKind {
  FOK_Unknown,
  FOK_Colon,
  FOK_Arrow,
  FOK_Exclaim,
  FOK_Equals,
  FOK_If,
  FOK_Else,
  FOK_Return,
  FOK_MemberAccess,
  FOK_For,
  FOK_While,
  FOK_In,
  FOK_DotDot,
  FOK_Const,
  FOK_Ref,
  FOK_RRef,
  FOK_Array
};

/// Convert a fused operator string like `operator'='` into an enum
FusedOpKind getFusedOpKind(Sema &SemaRef, llvm::StringRef Spelling);
FusedOpKind getFusedOpKind(Sema &SemaRef, const CallSyntax *S);

} // namespace gold

#endif
