//=== Elaborator.h - Elaboration for Green Nodes --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Elaborator interface, which creates clang::Type and
//  clang::Decl nodes out of Green Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_ELABORATOR_H
#define CLANG_GREEN_ELABORATOR_H

#include "clang/AST/Type.h"
#include "clang/AST/Decl.h"
#include "clang/Green/Syntax.h"
#include "clang/Green/SyntaxContext.h"

#include <unordered_map>

namespace clang
{
class Preprocessor;
class Stmt;
} // namespace clang

namespace green {

class GreenSema;

class Elaborator {
  /// The AST context for the Green language.
  SyntaxContext &Context;

  /// The state of translation for the Green language.
  GreenSema &SemaRef;

  /// The preprocessor.
  ///
  /// FIXME: Remove this.
  clang::Preprocessor &PP;
public:
  Elaborator(SyntaxContext &Context, GreenSema &SemaRef);

  clang::Decl *elaborateFile(const Syntax *S);
  clang::Decl *elaborateTopLevelDecl(const Syntax* S);

  clang::Decl *elaborateDecl(const Syntax *S);
  clang::Decl *elaborateDeclForArray(const ArraySyntax *S);
  clang::Decl *elaborateDeclForList(const ListSyntax *S);
  clang::Decl *elaborateDeclForCall(const CallSyntax *S);
  clang::Decl *elaborateDeclForAtom(const AtomSyntax *S);

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

} // namespace green

#endif
