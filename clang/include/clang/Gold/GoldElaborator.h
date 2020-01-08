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
#include "clang/Gold/GoldSyntaxContext.h"

#include <unordered_map>

namespace clang
{
class Preprocessor;
class Stmt;
} // namespace clang

namespace gold {

class Declarator;
class Declaration;
class Sema;

// Elaborator takes a gold::Syntax tree as input and outputs a clang::Decl *
// node that represents its equivalent C++ AST. It works closely with the
// ExprElaborator and StmtElaborator.
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
  clang::Decl *elaborateParameterDecl(Declaration *D);

  // Definition elaboration (3rd pass)
  void elaborateDeclInit(const Syntax *S);
  void elaborateDef(Declaration *D);
  void elaborateFunctionDef(Declaration *D);
  void elaborateVariableInit(Declaration *D);

  // Perform all three passes on a single declaration in one shot.
  // This is used to elaborate parameters and block-scope variables.
  clang::Decl *elaborateDeclSyntax(const Syntax* S);

  clang::Decl *elaborateDeclForArray(const ArraySyntax *S);
  clang::Decl *elaborateDeclForList(const ListSyntax *S);
  clang::Decl *elaborateDeclForCall(const CallSyntax *S);
  clang::Decl *elaborateDeclForAtom(const AtomSyntax *S);

  // Type elaboration
  clang::QualType elaborateType(Declarator *D);
  clang::QualType elaboratePointerType(Declarator *D, clang::QualType T);
  clang::QualType elaborateArrayType(Declarator *D, clang::QualType T);
  clang::QualType elaborateFunctionType(Declarator *D, clang::QualType T);
  clang::QualType elaborateExplicitType(Declarator *D, clang::QualType T);

  // Identification (1st pass)
  void identifyDecl(const Syntax *S);
  void identifyDeclFromCall(const CallSyntax *S);

  // Get the clang::QualType described by an operator':' call.
  clang::QualType getOperatorColonType(const CallSyntax *S) const;

  // Semantic actions.

  void startFile(const Syntax *S);
  void finishFile(const Syntax *S);

  // Dictionary of built in types.
  //
  // FIXME: This should be initialized in the constructor.
  const std::unordered_map<std::string, clang::QualType> BuiltinTypes = {
    {"void", Context.CxxAST.VoidTy},
    {"bool", Context.CxxAST.BoolTy},
    {"char", Context.CxxAST.CharTy},
    {"wchar_t", Context.CxxAST.WideCharTy},
    {"wint_t", Context.CxxAST.WIntTy},
    {"char8_t", Context.CxxAST.Char8Ty},
    {"char16_t", Context.CxxAST.Char16Ty},
    {"char32_t", Context.CxxAST.Char32Ty},
    {"signed char", Context.CxxAST.SignedCharTy},
    {"short", Context.CxxAST.ShortTy},
    {"short int", Context.CxxAST.ShortTy},
    {"int", Context.CxxAST.IntTy},
    {"long", Context.CxxAST.LongTy},
    {"long int", Context.CxxAST.LongTy},
    {"long long", Context.CxxAST.LongLongTy},
    {"long long int", Context.CxxAST.LongLongTy},
    {"int128_t", Context.CxxAST.Int128Ty},
    {"unsigned char", Context.CxxAST.UnsignedCharTy},
    {"unsigned short", Context.CxxAST.UnsignedShortTy},
    {"unsigned short int", Context.CxxAST.UnsignedShortTy},
    {"unsigned", Context.CxxAST.UnsignedIntTy},
    {"unsigned int", Context.CxxAST.UnsignedIntTy},
    {"unsigned long", Context.CxxAST.UnsignedLongTy},
    {"unsigned long int", Context.CxxAST.UnsignedLongTy},
    {"unsigned long long", Context.CxxAST.UnsignedLongLongTy},
    {"unsigned long long int", Context.CxxAST.UnsignedLongLongTy},
    {"uint128_t", Context.CxxAST.UnsignedInt128Ty},
    {"float", Context.CxxAST.FloatTy},
    {"double", Context.CxxAST.DoubleTy},
    {"long double", Context.CxxAST.LongDoubleTy},
    {"float128_t", Context.CxxAST.Float128Ty},
  };
};

} // namespace gold

#endif
