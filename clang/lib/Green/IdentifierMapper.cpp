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
}

void
IdentifierMapper::identifyDecls(const ArraySyntax *S) {
  for (const Syntax *Child : S->children()) {
    CurrentTopLevelSyntax = S;
    if (isa<ListSyntax>(Child))
      mapList(cast<ListSyntax>(Child));
  }
}

void
IdentifierMapper::mapList(const ListSyntax *S) {
  for (const Syntax *Child : S->children()) {
    if (isa<CallSyntax>(Child)) {
      mapCall(cast<CallSyntax>(Child));
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
IdentifierMapper::mapCall(const CallSyntax *S) {
  // If the Callee of the function is an atom, it could be either an operator
  // or an untyped variable. If it is the latter, then just map it now.
  if (isa<AtomSyntax>(S->Callee())) {
    const AtomSyntax *CalleeAtom = cast<AtomSyntax>(S->Callee());
    std::string Spelling = CalleeAtom->Tok.getSpelling();
    if (PP.getIdentifierInfo(Spelling) == GSemaRef.OperatorColonII) {
      return handleOperatorColon(S);
    }
    else if (PP.getIdentifierInfo(Spelling) == GSemaRef.OperatorExclaimII) {
      return handleOperatorExclaim(S);
    } else if (PP.getIdentifierInfo(Spelling) == GSemaRef.OperatorEqualsII) {
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
IdentifierMapper::handleOperatorColon(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) &&
         "Callee of operator syntax is not an atom.");
  if (isa<ListSyntax>(S->Args())) {
    if (!MappingOperatorExclaim && !MappingOperatorEquals)
      CurrentTopLevelSyntax = S;
    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    // Case 1: handle a typed variable.
    if (isa<AtomSyntax>(ArgList->Elems[0])) {
      const AtomSyntax *Name = cast<AtomSyntax>(ArgList->Elems[0]);
      clang::IdentifierInfo *II =
        PP.getIdentifierInfo(Name->Tok.getSpelling());

      GSemaRef.IdentifierMapping.insert({II, CurrentTopLevelSyntax});

    // Case 2: handle a function with a return type.
    } else if (isa<CallSyntax>(ArgList->Elems[0])) {
      mapCall(cast<CallSyntax>(ArgList->Elems[0]));
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
IdentifierMapper::handleOperatorExclaim(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) &&
         "Callee of operator syntax is not an atom.");
  if (isa<ListSyntax>(S->Args())) {
    MappingOperatorExclaim = true;
    CurrentTopLevelSyntax = S;

    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    // Begin by mapping the call syntax that declares the function.
    assert(isa<CallSyntax>(ArgList->Elems[0]) && "operator! does not declare a call.");
    mapCall(cast<CallSyntax>(ArgList->Elems[0]));

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

    const Syntax *Declarator = ArgList->Elems[0];

    // This might be a typed variable or single-line function definition.
    if (isa<CallSyntax>(Declarator))
      return mapCall(cast<CallSyntax>(Declarator));
    else if (isa<AtomSyntax>(Declarator)) {
      clang::IdentifierInfo *Spelling = PP.getIdentifierInfo(
        cast<AtomSyntax>(Declarator)->Tok.getSpelling());
      GSemaRef.IdentifierMapping.insert({Spelling, CurrentTopLevelSyntax});
    }

    MappingOperatorEquals = false;
  }
}

} // namespace green
