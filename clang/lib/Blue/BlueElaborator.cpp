//===- BlueElaborator.cpp - Blue Language Elaborator ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Translates concrete syntax into C++ AST trees.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"

#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Blue/BlueDeclarator.h"

#include <iostream>

namespace blue {

clang::Decl *Elaborator::elaborateTop(const Syntax *S)
{
  const TopSyntax *Top = cast<TopSyntax>(S);
  for (const Syntax *S : Top->children())
    elaborateDecl(S);

  return getCxxContext().getTranslationUnitDecl();
}

clang::Decl* Elaborator::elaborateDecl(const Syntax *S)
{
  switch (S->getKind()) {
  case Syntax::Def:
    return elaborateDefDecl(static_cast<const DefSyntax *>(S));
  default:
    break;
  }
  llvm_unreachable("invalid declaration");
}

clang::Decl *Elaborator::elaborateDefDecl(const DefSyntax *S) {
  // Build the declarator.
  Declarator *Dcl = getDeclarator(S->getDeclarator());

  if (Dcl->declaresValue())
    return makeValue(S, Dcl);

  if (Dcl->declaresFunction())
    return makeFunction(S, Dcl);

  if (Dcl->declaresTemplate())
    return makeTemplate(S, Dcl);

  llvm_unreachable("Invalid declarator");
}

void Elaborator::elaborateParameters(const ListSyntax *S) {
  if (S->isSemicolonSeparated())
    return elaborateParameterGroup(S);
  else
    return elaborateParameterList(S);
}

void Elaborator::elaborateParameterGroup(const ListSyntax *S) {
  for (const Syntax *SS : S->children())
    elaborateParameterList(cast<ListSyntax>(SS));
}

void Elaborator::elaborateParameterList(const ListSyntax *S) {
  for (const Syntax *SS : S->children())
    elaborateParameter(SS);

  // FIXME: Examine the parameters we just created. We might be able
  // to back-propagate types to some of them. For example, if we have;
  //
  //    a, b : int
  //
  // Then this should be equivalent to a : int, b: int.
}

clang::Decl *Elaborator::elaborateParameter(const Syntax *S) {
  if (!isa<DefSyntax>(S)) {
    Error(S->getLocation(), "invalid parameter syntax");
    return nullptr;
  }

  // FIXME: Implement me.
  const auto *Def = cast<DefSyntax>(S);
  Declarator *Dcl = getDeclarator(Def->getDeclarator());
  (void)Dcl;
  return nullptr;
}

// A signature is a sequence of unary (pointer) and binary (application)
// operators that can be linearized to describe a declaration.
//
// For applications, the left-hand side of a binary operator is always a
// parameter or type list describing a function or template (i.e., mapping)
// type. The right hand side denotes the type or kind of value produced by
// the function's evaluation or instantiation.
//
// For pointers, the operand is the type of object pointed at.
//
// The right-most leaf of the tree ultimately determines the type of the
// object accessed by the declaration.
//
// The first term in the list is always an identifier, which is established
// by the function above.
Declarator *Elaborator::getDeclarator(const Syntax *S) {
  if (const auto *U = dyn_cast<UnarySyntax>(S))
    return getUnaryDeclarator(U);
  if (const auto* B = dyn_cast<BinarySyntax>(S))
    return getBinaryDeclarator(B);
  return getLeafDeclarator(S);
}

Declarator *Elaborator::getUnaryDeclarator(const UnarySyntax *S) {
  if (S->getOperator().hasKind(tok::Caret)) {
    Declarator *Dcl = getDeclarator(S->getOperand());
    return new Declarator(Declarator::Pointer, S, Dcl);
  }

  Error(S->getLocation(), "invalid operator in declarator");
  return nullptr;
}

Declarator *Elaborator::getBinaryDeclarator(const BinarySyntax *S) {
  if (S->isApplication()) {
    Declarator *Dcl = getDeclarator(S->getRightOperand());

    // FIXME: This should be an error, not an assertion.
    //
    // TODO: Can the S be anything other than a list?
    //
    // TODO: Can we limit the way in which mappings compose? For example,
    // what is this declaring?
    //
    //    x : (a) [t : type] int;
    //
    // It appears to be a function returning a variable template. I don't
    // know if there's any way to write a metafunction that returns a
    // template.
    //
    // This should be disallowed.
    const auto *List = cast<ListSyntax>(S);

    // Elaborate function parameters.
    // enterScope(Scope::Parameter);
    elaborateParameters(List);
    // leaveScope();

    if (List->isParenList())
      return new Declarator(Declarator::Function, S, Dcl);

    // FIXME: This is only a template declarator if there are template
    // parameters in the list.
    if (List->isBracketList())
      return new Declarator(Declarator::Template, S, Dcl);

    // FIXME: Braces are possible, but invalid. This should be an error,
    // not an assertion.
    Error(List->getLocation(), "invalid list in declarator");
    return nullptr;
  }

  // TODO: We could support binary type composition (e.g., T1 * T2) as
  // an alternative spelling of product types. However, this most likely
  // needs to be wrapped in parens, so it should end up as a leaf. Maybe
  // this is a non-issue.
  Error(S->getLocation(), "invalid operator in declarator");
  return nullptr;
}

Declarator *Elaborator::getLeafDeclarator(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Identifier:
  case Syntax::List:
    return new Declarator(Declarator::Type, S);
  default:
    break;
  }
  llvm_unreachable("Invalid type expression");
}

clang::Decl *Elaborator::makeValue(const Syntax *S, Declarator* Dcl)
{
  return nullptr;
}

clang::Decl *Elaborator::makeFunction(const Syntax *S, Declarator* Dcl)
{
  return nullptr;
}

clang::Decl *Elaborator::makeTemplate(const Syntax *S, Declarator* Dcl)
{
  return nullptr;
}


// Diagnostics

void Elaborator::Error(clang::SourceLocation Loc, llvm::StringRef Msg) {
    SemaRef.Diags.Report(Loc, clang::diag::err_blue_elaboration) << Msg;
}


} // namespace blue
