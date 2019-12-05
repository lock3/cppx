//===- IdentifierMapper.cpp - Mapping of names to declarations ------------===//
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

#include "clang/Green/GreenSema.h"
#include "clang/Green/Tokens.h"
#include "clang/Green/IdentifierMapper.h"
#include "clang/Green/Syntax.h"
#include "clang/Green/SyntaxContext.h"
#include "clang/Lex/Preprocessor.h"

#include "llvm/Support/raw_ostream.h"

namespace green {

using namespace llvm;

IdentifierMapper::IdentifierMapper(SyntaxContext &Context, GreenSema &GSemaRef,
                                   clang::Preprocessor &PP)
  : Context(Context), GSemaRef(GSemaRef), PP(PP)
{
  OperatorExclaimII = PP.getIdentifierInfo("operator'!'");
  OperatorColonII = PP.getIdentifierInfo("operator':'");
  OperatorEqualsII = PP.getIdentifierInfo("operator'='");
}

void
IdentifierMapper::identifyDecls(const ArraySyntax *S) {
  for (const Syntax *Child : S->children()) {
    CurrentTopLevelSyntax = S;
    if (isa<ListSyntax>(Child))
      MapList(cast<ListSyntax>(Child));
  }
}

void
IdentifierMapper::MapList(const ListSyntax *S) {
  for (const Syntax *Child : S->children()) {
    if (isa<CallSyntax>(Child)) {
      MapCall(cast<CallSyntax>(Child));
      continue;
    } else if (isa<AtomSyntax>(Child)) {
      const AtomSyntax *Name = cast<AtomSyntax>(Child);

      // This might be a number or string literal.
      if (!Name->Tok.hasKind(tok::Identifier))
        continue;

      clang::IdentifierInfo *II =
        PP.getIdentifierInfo(Name->Tok.getSpelling());
      if (isa<ArraySyntax>(CurrentTopLevelSyntax))
        GSemaRef.IdentifierMapping.insert({II, Name});
    }
  }
}

void
IdentifierMapper::MapCall(const CallSyntax *S) {
  // If the Callee of the function is an atom, it could be either an operator
  // or an untyped variable. If it is the latter, then just map it now.
  if (isa<AtomSyntax>(S->Callee())) {
    const AtomSyntax *CalleeAtom = cast<AtomSyntax>(S->Callee());
    std::string Spelling = CalleeAtom->Tok.getSpelling();
    if (PP.getIdentifierInfo(Spelling) == OperatorColonII) {
      return HandleOperatorColon(S);
    }
    else if (PP.getIdentifierInfo(Spelling) == OperatorExclaimII) {
      return HandleOperatorExclaim(S);
    } else if (PP.getIdentifierInfo(Spelling) == OperatorEqualsII) {
      return handleOperatorEquals(S);
    } else {
      clang::IdentifierInfo *II =
        PP.getIdentifierInfo(CalleeAtom->Tok.getSpelling());
      GSemaRef.IdentifierMapping.insert({II, CurrentTopLevelSyntax});
      identifyDecls(cast<ArraySyntax>(S->Args()));
    }
  }
}

// Map a declaration of the form
// identifier : T
// or
// identifier(...) : T
void
IdentifierMapper::HandleOperatorColon(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) &&
         "Callee of operator syntax is not an atom.");
  if (isa<ListSyntax>(S->Args())) {
    if (!MappingOperatorExclaim && !MappingOperatorEquals)
      CurrentTopLevelSyntax = S;
    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    // Case 1: Handle a typed variable.
    if (isa<AtomSyntax>(ArgList->Elems[0])) {
      const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);
      clang::IdentifierInfo *II =
        PP.getIdentifierInfo(Name->Tok.getSpelling());

      GSemaRef.IdentifierMapping.insert({II, CurrentTopLevelSyntax});

    // Case 2: Handle a function with a return type.
    } else if (isa<CallSyntax>(ArgList->Elems[0])) {
      MapCall(cast<CallSyntax>(ArgList->Elems[0]));
    }
  }
}

// Map a declaration of the form
// \code
// fn(args):T!
//   body
// \endcode
// where `args` and `T` are optional.
void
IdentifierMapper::HandleOperatorExclaim(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) &&
         "Callee of operator syntax is not an atom.");
  if (isa<ListSyntax>(S->Args())) {
    MappingOperatorExclaim = true;
    CurrentTopLevelSyntax = S;

    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    // Begin by mapping the call syntax that declares the function.
    assert(isa<CallSyntax>(ArgList->Elems[0]) && "operator! does not declare a call.");
    MapCall(cast<CallSyntax>(ArgList->Elems[0]));

    MappingOperatorExclaim = false;

    // FIXME: For now we are going to try just identifying top level decls, so
    // we'll move this to its own function later.
    // Map the body of the defined function.
    // assert(isa<ArraySyntax>(ArgList->Elems[1]) && "Function body not an array.");
    // identifyDecls(cast<ArraySyntax>(ArgList->Elems[1]));
  }
}

// Identify a declaration of the form
// \code
// decl = expr
// \endcode
void
IdentifierMapper::handleOperatorEquals(const CallSyntax *S) {
  CurrentTopLevelSyntax = S;

  if (isa<ListSyntax>(S->Args())) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    MappingOperatorEquals = true;

    // This might be a typed variable or single-line function definition.
    if (isa<CallSyntax>(ArgList->Elems[0]))
      return MapCall(cast<CallSyntax>(ArgList->Elems[0]));
    else if (isa<AtomSyntax>(ArgList->Elems[0])) {
      clang::IdentifierInfo *Spelling = PP.getIdentifierInfo(
          cast<AtomSyntax>(ArgList->Elems[0])->Tok.getSpelling());
        GSemaRef.IdentifierMapping.insert({Spelling, CurrentTopLevelSyntax});
      }

    MappingOperatorEquals = false;
  }
}

} // namespace green
