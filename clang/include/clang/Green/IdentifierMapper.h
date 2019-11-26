//===- IdentifierMapper.h - Mapping of names to their point of declaration ===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements an interface that maps identifiers to the Syntax node
//  that introduces them.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_IDENTIFIERMAPPER_H
#define CLANG_GREEN_IDENTIFIERMAPPER_H

namespace clang {

class Preprocessor;

} // namespace clang

namespace green {

class SyntaxContext;
class GreenSema;
struct ArraySyntax;
struct AtomSyntax;
struct ListSyntax;
struct CallSyntax;
struct ElemSyntax;
struct MacroSyntax;

class IdentifierMapper {
public:
  IdentifierMapper(SyntaxContext &Context, GreenSema &GSemaRef,
                   clang::Preprocessor &PP);

  void MapIdentifiers(const ArraySyntax *S);

private:
  void MapAtom(const AtomSyntax *S);
  void MapList(const ListSyntax *S);
  void MapCall(const CallSyntax *S);
  void MapElem(const ElemSyntax *S);
  void MapMacro(const MacroSyntax *S);

  void HandleOperatorColon(const CallSyntax *S);
  void HandleOperatorExclaim(const CallSyntax *S);

  // The current top level Syntax we are analyzing; names will map to this.
  const Syntax *CurrentTopLevelSyntax;

  // If we're mapping an operator'!' call, it will take precedence over an
  // operator':' call as the CurrentTopLevelSyntax, so we have to keep track
  // of when we do that.
  // FIXME: inelegant solution
  bool MappingOperatorExclaim = false;

  // Tokenizations of strings we frequently compare.
  clang::IdentifierInfo *OperatorExclaimII;
  clang::IdentifierInfo *OperatorColonII;

  SyntaxContext &Context;
  GreenSema &GSemaRef;
  clang::Preprocessor &PP;
};

} // namespace green

#endif
