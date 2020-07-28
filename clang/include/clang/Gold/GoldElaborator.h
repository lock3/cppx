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
struct AttrStatus;

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
  void checkCXXMethodDecl(clang::CXXMethodDecl *MD);
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

  /// Perform all three passes on a single declaration in one shot.
  /// This is used to elaborate parameters and block-scope variables.
  clang::Decl *elaborateDeclSyntax(const Syntax *S);

  /// elaborateParmDeclSyntax very similar to a call to elaborateDeclSyntax
  /// with the exception that this is ONLY ever used on a suspected
  /// parameter. 
  clang::Decl *elaborateParmDeclSyntax(const Syntax *S);

  /// Perform the latter two passes on a Declaration that was previously
  /// identified. This is used when lookup finds an unelaborated declaration.
  clang::Decl *elaborateDeclEarly(Declaration *D);

  // class type body elaboration.
  clang::Decl *elaborateTypeBody(Declaration *D, clang::CXXRecordDecl *R);
  clang::Decl *elaborateField(Declaration *D);
  void elaborateFieldInit(Declaration *D);

  // Identification (1st pass)
  Declaration *identifyDecl(const Syntax *S);

  // Semantic actions.
  void startFile(const Syntax *S);
  void finishFile(const Syntax *S);
  

  /// Complete class parsing/elaboration
  ///{
  /// This returns true if part of the declaration was delayed.
  bool delayElaborateDeclType(const Syntax *S);
  
  /// Functionality associated with late elaboration and are used to either
  /// elaborate the full class or elaborate everything if they are able to.
  void delayElaborateMemberInitializer(Declaration *D);
  void delayElaborateMethodDecl(Declaration *D);
  void delayElaborateMethodDef(Declaration *D);
  void delayElaborationClassBody(Declaration *D);

  void delayElaborateDefaultParam(Declaration *ParamDecl);


  /// Functions used for late elaboration processing.
  /// This only occur within a class.
  void finishDelayedElaboration(ElaboratingClass &Class);
  void lateElaborateAttributes(ElaboratingClass &Class);
  void lateElaborateMethodDecls(ElaboratingClass &Class);
  void lateElaborateDefaultParams(ElaboratingClass &Class);
  void lateElaborateMemberInitializers(ElaboratingClass &Class);
  void lateElaborateMethodDefs(ElaboratingClass &Class);
  
  /// Special callbacks used in order to interact a lateElaborated class.
  void lateElaborateAttribute(LateElaboratedAttributeDecl &Field);
  void lateElaborateMethodDef(LateElaboratedMethodDef &Method);
  void lateElaborateDefaultParams(LateElaboratedMethodDeclaration &MethodDecl);
  void lateElaborateDefaultParam(LateElaboratedDefaultArgument &DefaultParam);
  void lateElaborateMethodDecl(LateElaboratedMethodDeclaration &Method);
  void lateElaborateMemberInitializer(
      LateElaborateMemberInitializer &MemberInit);
  ///}
  
  /// This single function is responsible for applying attributes to things
  /// any type of declaration we create.
  void elaborateAttributes(Declaration *D);

  /// Methods that direct the declaration to modify the declaration to have the
  /// given attribute.
  void elaborateConstExprAttr(Declaration *D, const Syntax *S,
                              AttrStatus &Status);
  void elaborateInlineAttr(Declaration *D, const Syntax *S,
                           AttrStatus &Status);
  void elaborateExternAttr(Declaration *D, const Syntax *S,
                           AttrStatus &Status);
  void elaborateAccessSpecifierAttr(Declaration *D, const Syntax *S,
                                    AttrStatus &Status);
  void elaborateExceptionSpecAttr(Declaration *D, const Syntax *S,
                                  AttrStatus &Status);
  void elaborateStaticAttr(Declaration *D, const Syntax *S,
                           AttrStatus &Status);
  void elaborateThreadLocalAttr(Declaration *D, const Syntax *S,
                                AttrStatus &Status);
  void elaborateExplicitAttr(Declaration *D, const Syntax *S,
                             AttrStatus &Status);
  void elaborateVirtualAttr(Declaration *D, const Syntax *S,
                            AttrStatus &Status);
  void elaborateOverrideAttr(Declaration *D, const Syntax *S,
                             AttrStatus &Status);
  void elaborateFinalAttr(Declaration *D, const Syntax *S,
                          AttrStatus &Status);
  void elaborateConstAttr(Declaration *D, const Syntax *S,
                          AttrStatus &Status);
  void elaborateBitsAttr(Declaration *D, const Syntax *S,
                         AttrStatus &Status);
  void elaborateAlignAsAttr(Declaration *D, const Syntax *S,
                            AttrStatus &Status);
  void elaborateCarriesDependencyAttr(Declaration *D, const Syntax *S,
                                      AttrStatus &Status);
  void elaborateDeprecatedAttr(Declaration *D, const Syntax *S,
                               AttrStatus &Status);
  void elaborateMaybeUnusedAttr(Declaration *D, const Syntax *S,
                                AttrStatus &Status);
  void elaborateNoDiscardAttr(Declaration *D, const Syntax *S,
                              AttrStatus &Status);
  void elaborateNoReturnAttr(Declaration *D, const Syntax *S,
                           AttrStatus &Status);
  void elaborateUnknownAttr(Declaration *D, const Syntax *S,
                           AttrStatus &Status);

  /// This is the attribute that's used to indicate that we have an error.
  /// For example, if mutable makes it to the list of attributes then we have
  /// to indicate that it's an error some how.
  /// This is for attributes that are known to be an error when it's specified.
  void elaborateAttributeError(Declaration *D, const Syntax *S,
                               AttrStatus &Status);
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
