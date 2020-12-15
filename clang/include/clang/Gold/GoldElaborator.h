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
class NamedDecl;
class Preprocessor;
class Stmt;
class CXXRecordDecl;
class ParsedAttributes;
} // namespace clang

namespace gold {

class Declarator;
class Declaration;
class Sema;
struct AttrStatus;

enum AttrFormat {
  AF_Invalid,
  AF_Call,
  AF_Name
};

AttrFormat checkAttrFormatAndName(const Syntax *Attr, std::string &Name);

/// locateValidAttribute
/// This is a privately used template function, basucally taking the place of me
/// duplicating the same thing over and over again for each type of attribute
/// I need to find.
///
/// OnAttr is a callable with signature
///       bool (const Syntax *)
///   Returing true means you've located one of the possible attributes.
///   This should also be used to set the correct value of any possible return
///   value.
///     true = Matched an attribute.
///     false = didn't match an attribute.
///
/// IsSameAttr is a callable used to detect duplicates of the same attribute class.
///   This is the same as OnAttr with the exception that it doesn't set
///   the current desired attribute when it's true.
///   Signature:
///       bool (const Syntax *)
///     true = we hvae a failure (meaning that we have a duplicate attribute)
///     false = we don't have an attribute of the same class.
///
/// OnDuplicate is a callable with signature
///       void (const syntax *first, const Syntax *Duplicate)
///   This is used to create error messages.
template<typename OnAttr, typename IsSameAttr, typename OnDuplicate>
bool locateValidAttribute(Attributes& UnprocessedAttributes,
    OnAttr OnAttribute, IsSameAttr CheckAttr, OnDuplicate OnDup) {

  auto Iter = UnprocessedAttributes.begin();
  auto End = UnprocessedAttributes.end();
  for (;Iter != End; ++Iter) {
    if (OnAttribute(*Iter)) {
      break;
    }
  }
  const Syntax *AttribSpec = nullptr;
  bool didFail = false;
  if(Iter != End) {
    AttribSpec = *Iter;
    UnprocessedAttributes.erase(Iter);
    Iter = UnprocessedAttributes.begin();
    End = UnprocessedAttributes.end();
    for (;Iter != End; ++Iter) {
      if (CheckAttr(*Iter)) {
        OnDup(AttribSpec, *Iter);
        didFail = true;
      }
    }
  }
  return didFail;
}

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
  clang::Decl *elaborateDecl(Declaration *D);
  bool elaborateNestedNameForDecl(Declaration *D);
  clang::Decl *elaborateNestedNameNamespace(Declaration *D);
  clang::Decl *elaborateDeclContent(clang::Scope *InitialScope,
                                    Declaration *D);
  void buildTemplateParams(const Syntax *Params,
                           llvm::SmallVectorImpl<clang::NamedDecl *> &Res);

  clang::Decl *elaborateDeclType(const Syntax* D);
  clang::Decl *elaborateFunctionDecl(Declaration *D);
  void checkCXXMethodDecl(clang::CXXMethodDecl *MD);
  clang::Decl *elaborateVariableDecl(clang::Scope *InitialScope, Declaration *D);
  clang::Expr *elaborateDeducedVariableDecl(clang::Scope *InitialScope,
                                            Declaration *D);
  clang::Decl *elaborateTypeAlias(Declaration *D);
  clang::Decl *elaborateNsAlias(Declaration *D);
  clang::Decl *elaborateTemplateAliasOrVariable(Declaration *D);
  clang::Decl *elaborateParameterDecl(Declaration *D);
  clang::Decl *elaborateTemplateParamDecl(Declaration *D);

  // Definition elaboration (3rd pass)
  void elaborateDeclInit(const Syntax *S);
  void elaborateDef(Declaration *D);
  void elaborateFunctionDef(Declaration *D);
  void elaborateVariableInit(Declaration *D, bool IsEarly = false);
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

  /// Only elaborates upto phase 2.
  clang::Decl *elaborateDeclTypeEarly(Declaration *D);

  // class type body elaboration.
  clang::Decl *elaborateTypeBody(Declaration *D, clang::CXXRecordDecl *R);
  clang::Decl *elaborateField(Declaration *D, clang::TypeSourceInfo *TInfo);
  void elaborateFieldInit(Declaration *D);

  clang::Decl *elaborateEnumBody(Declaration* D, clang::Decl *EnumD);
  clang::Decl *elaborateEnumMemberDecl(const Syntax *S, clang::Decl *EnumD);
  bool elaborateEnumMemberInit(const Syntax *S);

  // Identification (1st pass)
  Declaration *identifyDecl(const Syntax *S);

  // Semantic actions.
  void startFile(const Syntax *S);
  void finishFile(const Syntax *S);


  /// Complete class parsing/elaboration
  ///{
  /// This returns true if part of the declaration was delayed.
  bool delayElaborateDeclType(clang::CXXRecordDecl *RD, const Syntax *S);

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
  void elaborateRefQualifierAttr(Declaration *D, const Syntax *S,
                                 AttrStatus &Status);
  void elaborateSystemAttribute(clang::Decl *D, const Syntax *S,
                                AttrStatus &Status,
                                clang::ParsedAttributes &Attrs);
  clang::IdentifierInfo *elaborateAttrScopeName(const Syntax * S);
  /// This is the attribute that's used to indicate that we have an error.
  /// For example, if mutable makes it to the list of attributes then we have
  /// to indicate that it's an error some how.
  /// This is for attributes that are known to be an error when it's specified.
  void elaborateAttributeError(Declaration *D, const Syntax *S,
                               AttrStatus &Status);

  // The amount of implicit Decl nodes generated by clang::Sema. We want to skip
  // these when we emit a TranslationUnitDecl.
  std::size_t ImplicitSemaDecls = 0;
};

// Handles a using macro, which represents several different kinds of entities
// in several different contexts.
clang::Decl *handleUsing(SyntaxContext &Ctx, Sema &SemaRef,
                         const Syntax *Arg, clang::SourceLocation UsingLoc);

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
  FOK_Brackets,
  FOK_Parens,
  FOK_Throw,
  FOK_Caret,
  FOK_DotCaret,
  FOK_Ampersand,
};

/// Convert a fused operator string like `operator'='` into an enum
FusedOpKind getFusedOpKind(Sema &SemaRef, llvm::StringRef Spelling);
FusedOpKind getFusedOpKind(Sema &SemaRef, const CallSyntax *S);

} // namespace gold

#endif
