//===- IdentifierMapper.h - Tree that classifies identifiers as Clang nodes//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
// This file implements a traverser of Syntax Trees that maintains some context
// about the current tree. It is used to map an identifier to a Clang
// construct.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_IDENTIFIER_MAPPER_H
#define CLANG_GREEN_IDENTIFIER_MAPPER_H

#include "clang/AST/Type.h"
#include "llvm/ADT/PointerIntPair.h"

#include "clang/GreenAST/SyntaxContext.h"

#include <vector>

namespace clang {
class Decl;
class Preprocessor;
class Sema;
class StoredDeclsMap;
}

namespace usyntax {

class GreenSema;
struct Syntax;

using SyntaxVector = std::vector<Syntax *>;

class IdentifierMapper {
public:
  IdentifierMapper(SyntaxContext &Context, clang::Preprocessor &PP,
                      GreenSema &GSemaRef, clang::Sema &ClangSemaRef)
    : Context(Context), PP(PP), GSemaRef(GSemaRef), ClangSemaRef(ClangSemaRef)
  {
    VarContext = Normal;

    // if (!GSemaRef.CurContext->getLookupPtr())
    //   ;
      // GSemaRef.CurContext->CreateStoredDeclsMap(Context.ClangContext);
      // GSemaRef.CurContext->makeDeclVisibleInContextImpl(nullptr, false);
    // CurrentSDM =
    //   llvm::PointerIntPair<StoredDeclsMap *, 1>(
    //     GSemaRef.CurContext->getLookupPtr(),
    //     GSemaRef.CurContext->isDependentContext());
    // llvm::outs() << "Is SDM null? " << GSemaRef.CurContext->getLookupPtr() << '\n';
  }

  /// Map the given list of syntaxes.
  void MapSyntaxes(const SyntaxVector &Inputs);
  clang::Decl *MapSyntax(const Syntax *S);

  clang::Decl *MapMacro(const SyntaxMacro *S);
  clang::Decl *MapCall(const SyntaxCall *S);
  clang::Decl *MapIdentifier(const SyntaxIdent *S, clang::QualType InTy);

  clang::Decl *HandleOperatorEquals(const SyntaxMacro *S);

  clang::QualType HandlePrefixColon(const SyntaxMacro *S);

  /// Enumerations

  // There are different contexts in which we can create a variable: we might
  // be creating a regular VarDecl, or a ParmVarDecl while analyzing a function
  // declaration.
  enum VariableCreationContext {
    // This is a normal context, create VarDecls.
    Normal,

    // We're analyzing a function prototype, create ParmVarDecls.
    FunctionProto,
  };

  /// Public Members

  /// The current variable creation context (see VariableCreationContext
  /// definition).
  VariableCreationContext VarContext;

  SyntaxContext &Context;

private:
  /// Members

  /// Clang preprocessor. Gives access to Clang's identifier table.
  clang::Preprocessor &PP;

  /// A reference to the GreenSema object.
  GreenSema &GSemaRef;

  /// A reference to Clang's Sema object.
  clang::Sema &ClangSemaRef;

  /// The stored decl map of the DeclContext we are currently in.
  /// The int is equal to 1 if it is a dependent context.
  llvm::PointerIntPair<clang::StoredDeclsMap *, 1> CurrentSDM;

public:
  /// RAII Objects

  // An RAII object that sets and resets the VariableCreationContext.
  struct VariableContextRAII {
    VariableContextRAII(VariableCreationContext &VarContext,
                        VariableCreationContext New)
      : VarContext(VarContext) {
        StoredContext = VarContext;
        VarContext = New;
      }

    ~VariableContextRAII() {
      VarContext = StoredContext;
    }

  private:
    VariableCreationContext &VarContext;
    VariableCreationContext StoredContext;
  };

private:
  /// Maps

  // Maps from strings to builtin-types.
  // TODO: complete this.
  const std::unordered_map<std::string, clang::QualType> BuiltInTypes = {
    {"int", Context.ClangContext.IntTy},
    {"unsigned", Context.ClangContext.UnsignedIntTy},
    {"float", Context.ClangContext.FloatTy},
    {"double", Context.ClangContext.DoubleTy},
  };
};

} // namespace usyntax

#endif
