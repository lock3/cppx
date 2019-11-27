//===- GreenSema.cpp - Semantic Analysis of Green ASTs --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GreenSema class, which performs semantic analysis
//  for the Green language.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Green/Syntax.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/IdentifierMapper.h"
#include "clang/Green/RequireType.h"

namespace green {

using namespace llvm;

GreenSema::GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, 
                     clang::Sema &ClangSema)
  : Context(Context), PP(PP), ClangSema(ClangSema)
{}

void
GreenSema::MapIdentifiers(const ArraySyntax *S)
{
  IdentifierMapper Mapper(Context, *this, PP);
  Mapper.MapIdentifiers(S);
}

void
GreenSema::RequireTypes() {
  TypeRequirer R(Context, *this);
  for (auto MapIter : IdentifierMapping)
    R.RequireType(MapIter.first, MapIter.second);
}

} // namespace usyntax

