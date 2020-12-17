//===- ClangToGoldDeclBuilder.h -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This class is responsible for takeing a newly instantiated template
//  declaration, or one that we haven't seen yet, and didn't create.
//  and creating a dummy scope/declaration for that class, and members
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_CLANG_TO_GOLD_REBUILDER_H
#define CLANG_GOLD_CLANG_TO_GOLD_REBUILDER_H

#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldLateElaboration.h"
#include "clang/Gold/GoldOperatorInfo.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntaxContext.h"

#include <memory>
#include <vector>

namespace gold {

/// This class is responable for taking a CXXRecordDecl, which was created
/// as part of template instantiation, and converting that into a declaration
/// that can be used during lookup.
class ClangToGoldDeclRebuilder {
  SyntaxContext &Context;
  Sema &SemaRef;
  Declaration *RootDecl = nullptr;
  Scope *RootScope = nullptr;
  Declaration *ParentDecl = nullptr;
  clang::CXXRecordDecl *RD = nullptr;
  Declaration *CurDecl = nullptr;

public:
  ClangToGoldDeclRebuilder(SyntaxContext &Ctx, Sema &S)
    :Context(Ctx),
    SemaRef(S)
  { }

  /// Starts the rebuild process.
  Declaration *rebuild(clang::CXXRecordDecl *ToConvert);
  bool finishDecl(Declaration *D, clang::SourceRange Range);
  Declaration *generateDeclForDeclContext(clang::DeclContext *DC,
                                          const Syntax *AssociatedSyntax);

  Declaration *generateDeclForNNS(clang::NamespaceDecl *NS,
                                  const AtomSyntax *Name);

  /// This takes the name given as a string converts it to an identifier
  /// and sets the identifier to the name within the declaration
  /// rather than just using the one provided by a given declaration.
  Declaration *rebuildDeclWithNewName(Declaration *Parent,
                                      llvm::StringRef NewName,
                                      clang::Decl *D,
                                      Scope *ScopeForDecl);
private:
  gold::Scope *determineParentScope();
  bool rebuildMember(clang::Decl *Member);
  bool rebuildMember(clang::CXXRecordDecl *RD);
  bool rebuildMember(clang::FieldDecl *Field);
  bool rebuildMember(clang::VarDecl *StaticMember);
  bool rebuildMember(clang::CXXMethodDecl *Method);
  bool rebuildMember(clang::CXXConversionDecl *Conversion);
  bool rebuildMember(clang::CXXDestructorDecl *Dtor);
  bool rebuildMember(clang::CXXConstructorDecl *Ctor);
  bool rebuildMember(clang::VarTemplateDecl *VTD);
  bool rebuildMember(clang::TypeAliasTemplateDecl *TATD);
  bool rebuildMember(clang::TypeAliasDecl *TAD);
  bool rebuildMember(clang::ClassTemplateDecl* CTD);
  bool rebuildMember(clang::ClassTemplateSpecializationDecl *Tmplt);
  bool rebuildMember(clang::ClassTemplatePartialSpecializationDecl *Tmplt);
  bool rebuildMember(clang::FunctionTemplateDecl* CTD);
  bool rebuildMember(clang::EnumDecl *ED);
  bool rebuildMember(clang::EnumConstantDecl* ECD);
  bool rebuildMember(clang::UsingDecl *UD);
  // We may need clang::ClassScopeFunctionSpecializationDecl eventually.

  /// Saves the previous declaration, creates a new declaration, sets the
  /// current parent declaration, then sets the current declaration to the
  /// one that was newly created. This also creates the first child scope,
  /// and does the save restore for the scope.
  struct StateRAII;
};
} // namespace gold

#endif
