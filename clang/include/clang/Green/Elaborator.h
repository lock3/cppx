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
} // namespace clang

namespace green {

class GreenSema;

class Elaborator {

  SyntaxContext &Context;

  GreenSema &SemaRef;

  clang::Preprocessor &PP;
public:
  Elaborator(SyntaxContext &Context, GreenSema &SemaRef);

  clang::Decl *elaborateFile(const ArraySyntax *S);

  clang::Decl *elaborateDecl(const Syntax *S);
  clang::Decl *elaborateDeclForArray(const ArraySyntax *S);
  clang::Decl *elaborateDeclForList(const ListSyntax *S);
  clang::Decl *elaborateDeclForCall(const CallSyntax *S);
  clang::Decl *elaborateDeclForAtom(const AtomSyntax *S);


    // Dictionary of built in types.
  const std::unordered_map<std::string, clang::QualType> BuiltinTypes = {
    {"void", Context.ClangContext.VoidTy},
    {"bool", Context.ClangContext.BoolTy},
    {"char", Context.ClangContext.CharTy},
    {"wchar_t", Context.ClangContext.WideCharTy},
    {"wint_t", Context.ClangContext.WIntTy},
    {"char8_t", Context.ClangContext.Char8Ty},
    {"char16_t", Context.ClangContext.Char16Ty},
    {"char32_t", Context.ClangContext.Char32Ty},
    {"signed char", Context.ClangContext.SignedCharTy},
    {"short", Context.ClangContext.ShortTy},
    {"short int", Context.ClangContext.ShortTy},
    {"int", Context.ClangContext.IntTy},
    {"long", Context.ClangContext.LongTy},
    {"long int", Context.ClangContext.LongTy},
    {"long long", Context.ClangContext.LongLongTy},
    {"long long int", Context.ClangContext.LongLongTy},
    {"int128_t", Context.ClangContext.Int128Ty},
    {"unsigned char", Context.ClangContext.UnsignedCharTy},
    {"unsigned short", Context.ClangContext.UnsignedShortTy},
    {"unsigned short int", Context.ClangContext.UnsignedShortTy},
    {"unsigned", Context.ClangContext.UnsignedIntTy},
    {"unsigned int", Context.ClangContext.UnsignedIntTy},
    {"unsigned long", Context.ClangContext.UnsignedLongTy},
    {"unsigned long int", Context.ClangContext.UnsignedLongTy},
    {"unsigned long long", Context.ClangContext.UnsignedLongLongTy},
    {"unsigned long long int", Context.ClangContext.UnsignedLongLongTy},
    {"uint128_t", Context.ClangContext.UnsignedInt128Ty},
    {"float", Context.ClangContext.FloatTy},
    {"double", Context.ClangContext.DoubleTy},
    {"long double", Context.ClangContext.LongDoubleTy},
    {"float128_t", Context.ClangContext.Float128Ty},
  };
};

} // namespace green

#endif
