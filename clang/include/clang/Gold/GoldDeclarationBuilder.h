//===- GoldDeclarationBuilder.h -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  A special elaborator whose entire job is constructing a declaration,
//  associated declarators, and determining the possible kind of declarator.
//  The DeclarationBuilder is also responsible for constructing appropriate
//  gold scopes (but not clang scopes)
//
//  The Declaration builder only participates in phase one of elaboration.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_DECLARATION_BUILDER_H
#define CLANG_GOLD_DECLARATION_BUILDER_H

#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

#include "llvm/ADT/SmallSet.h"

#include "clang/Gold/GoldSyntaxContext.h"

namespace clang{
class ASTContext;
class Sema;
}

namespace gold {
class Sema;
class SyntaxContext;

/// DeclarationBuilder
class DeclarationBuilder final {
  SyntaxContext &Context;
  Sema &SemaRef;

public:
  DeclarationBuilder(SyntaxContext &Ctx, Sema &S)
    :Context(Ctx), SemaRef(S)
  { }
  DeclarationBuilder(Sema &S);

  /// This constructs the declaration simiar to how make declarator
  /// did previously.
  Declaration *build(const Syntax *S);
private:
  std::string OriginalNameStorage;
  llvm::StringRef OriginalName;
  clang::IdentifierInfo *Id = nullptr;
  const OpInfoBase *OpInfo = nullptr;
  const Syntax *InitExpr = nullptr;
  InitKind InitOperatorUsed = IK_None;
  llvm::SmallSet<const Syntax*, 6> AdditionalNodesWithAttrs;

  const Syntax *ConversionTypeSyntax = nullptr;

  // Overridding setting, this is special because enums are so restructive
  // as to which declarations are actually allowed within them.
  bool EnableFunctions = true;
  bool EnableNamespaceDecl = true;
  bool EnableTags = true;
  bool EnableAliases = true; // template and namespace.
  bool EnableTemplateParameters = false;
  bool RequireTypeForVariable = false;
  bool EnableNestedNameSpecifiers = false;
  bool RequireAliasTypes = false;
  bool RequireTypeForFunctions = false;
  bool RequiresDeclOrError = false;
  bool AllowShortCtorAndDtorSyntax = false;
  bool IsInsideEnum = false;
  bool ContextDeclaresNewName = false;

  /// This checks to make sure that the declarator chain conforms to a specific
  /// structure. This only fails if the declarator chain doesn't satisfy
  /// the ordering requirements.
  bool verifyDeclaratorChain(const Syntax *DeclExpr, Declaration *TheDecl);

  /// This function attempts to verify that the declarator was created correctly
  /// and that it conforms to requirements placed on it.
  /// In the event of an error this \return true.
  bool checkDeclaration(const Syntax *DeclExpr, Declaration *TheDecl);

  /// This is limited to dealing within things that are within the enumeration.
  bool checkEnumDeclaration(const Syntax *DeclExpr, Declaration *TheDecl);

  /// This checks if the declaration is allowed to have nested name specifiers
  /// if those nested namespecifiers are allowed within the current context
  /// and if the declatation with a nested name is valid.
  bool checkNestedNameSpecifiers(const Syntax *DeclExpr,
                                 Declaration *TheDecl);

  /// This enforces RequireTypeForVariable, RequireAliasTypes,
  /// and RequireTypeForFunctions
  bool checkRequiresType(const Syntax *DeclExpr, Declaration *TheDecl);

  /// checks to see if we are a conversion operator and if we
  /// the we need to do some additional verification in order to assure that
  /// we can be declared int the given context etc, we are a function,
  /// and if we are not within a class we have a Nested name specifier.
  /// Returns false if there was no error, and true if there was.
  bool checkClassifiedConversionOperator(const Syntax *DeclExpr,
                                         Declaration *TheDecl);

  /// This attempts to verify that we are infact a user defined literal function.
  /// Returns true if there was an error and false if not.
  bool checkClassifyUserDefinedLiteralOperator(const Syntax *DeclExpr,
                                                Declaration *TheDecl);

  /// Given the structure and context, attempt to classify what kind of
  /// declaration we have and in the current context how it could be used.
  bool classifyDecl(const Syntax *DeclExpr, Declaration *TheDecl);


  // Special requirements.
  // - Can have scoped name declarations.
  // - Is a decl if it has an assignment, :, or !
  // - Can declare classes, enum, and unions,
  // - Can declare namespace, namespace aliases and template, template variables
  Declarator *handleNamespaceScope(const Syntax *S);

  // Special requirements
  // Must have a :
  // No function declarations
  // No namespace declarations alias or otherwise.
  // No declarations tags of any kind.
  // No template parameters/specializations for the parameter name
  // Cannot have a Nested name specifier for the name of the variable.
  Declarator *handleParameterScope(const Syntax *S);

  // Special requirements.
  // Same restrictions as handleParameterScope with the resulting
  // UnprocessedDeclKind not being different.
  Declarator *handleTemplateScope(const Syntax *S);

  // Special requirements
  // This is for a function body.
  // That means we can't have nested name specifiers on a declaration.
  //    - it implies that it's not a function declarations.
  // Any function declared within the body of a function must have a : operator
  // Cannot declare namespace declarations,
  // can declare namespace aliases.
  Declarator *handleFunctionScope(const Syntax *S);

  // Same as handleFunctionScope
  Declarator *handleBlockScope(const Syntax *S);

  // Special requirements
  // Names declared within the body of a class, or union cannot have a
  // nested name specifier(yet, they may need it for base class using access?)
  // Cannot declare a new namespace, aliases are alright.
  Declarator *handleClassScope(const Syntax *S);

  // Same as block scope
  // The declaration cannot be of a type, namespace, or tag
  Declarator *handleControlScope(const Syntax *S);

  // Special requirements
  // This allows the atom only identifier
  // No declarations other then name = value, or name
  Declarator *handleEnumScope(const Syntax *S);


  Declarator *handleCatchScope(const Syntax *S);

  /// Attempts to reach the end of a declarator chain an append a new
  /// declarator, specifically the type declarator, iff we are a conversion
  /// operator declaration.
  Declarator *appendConversionType(Declarator *CurDcl);

  /// This must be a call Either "operator'='", "operator':'", or "operator'in'"
  /// Operator in is a special case for us because it's just a ranged for loop.
  Declarator *makeDeclarator(const Syntax *S);
  Declarator *dispatchAndCreateDeclarator(const Syntax *S);
  Declarator *makeTopLevelDeclarator(const Syntax *S, Declarator *Next);
  Declarator *buildTemplateFunctionOrNameDeclarator(const Syntax *S,
                                                    Declarator *Next);
  Declarator *buildUsingDirectiveDeclarator(const MacroSyntax *S);

  Declarator *buildNestedNameSpec(const CallSyntax *S, Declarator *Next);
  Declarator *buildNestedTemplate(const ElemSyntax *S, Declarator *Next);
  Declarator *buildNestedOrRegularName(const Syntax *S, Declarator *Next);
  Declarator *buildNestedName(const Syntax *S, Declarator *Next);
  Declarator *buildNestedTemplateSpecializationOrName(const Syntax *S,
                                                      Declarator *Next);

  Declarator *buildNameDeclarator(const Syntax *S, Declarator *Next);
  Declarator *buildTemplateOrNameDeclarator(const Syntax *S, Declarator *Next);


  /// This is used to peek into a [] and verify that it is a declaration.
  /// The decision is based on if the contains a : because that's
  /// what's required for template parameter declarations.
  /// This is ONLY used for the main Name declaration.
  Declarator *mainElementTemplateOrSpecialization(const ElemSyntax *Elem,
                                                  Declarator *Next);

  // Internal processing functions
  UnknownDeclarator *handleUnknownADeclSyntax(const Syntax *S, Declarator *Next);
  ErrorDeclarator *handleErrorSyntax(const ErrorSyntax *S, Declarator *Next);
  GlobalNameSpecifierDeclarator *handleGlobalNameSpecifier(const CallSyntax *S,
                                                           Declarator *Next);
  NestedNameSpecifierDeclarator *handleNestedNameSpecifier(const AtomSyntax *S,
                                                           Declarator *Next);
  IdentifierDeclarator *handleIdentifier(const AtomSyntax *S, Declarator *Next);
  FunctionDeclarator *handleFunction(const CallSyntax *S, Declarator *Next);
  TypeDeclarator *handleType(const Syntax *S, Declarator *Next);
  TemplateParamsDeclarator *handleTemplateParams(const ElemSyntax *S,
                                                 Declarator *Next);
  ImplicitEmptyTemplateParamsDeclarator *
  handleImplicitTemplateParams(const ElemSyntax *Owner, Declarator *Next);

  SpecializationDeclarator *
  handleSpecialization(const ElemSyntax *SpecializationOwner, Declarator *Next);
};


}
#endif
