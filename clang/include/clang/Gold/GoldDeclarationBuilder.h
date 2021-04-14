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
  friend class DeclaratorBuilder;

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
};


}
#endif
