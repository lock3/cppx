//===- GoldDeclarator.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implementation for the declarator class.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldDeclarator.h"
#include "clang/Gold/GoldDeclaration.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

#include "llvm/Support/raw_ostream.h"

namespace gold{

UnknownDeclarator *Declarator::getAsUnknown() {
  return cast<UnknownDeclarator>(this);
}
const UnknownDeclarator *Declarator::getAsUnknown() const {
  return cast<UnknownDeclarator>(this);
}
ErrorDeclarator *Declarator::getAsError() {
  return cast<ErrorDeclarator>(this);
}
const ErrorDeclarator *Declarator::getAsError() const {
  return cast<ErrorDeclarator>(this);
}
GlobalNameSpecifierDeclarator *Declarator::getAsGlobalNameSpecifier() {
  return cast<GlobalNameSpecifierDeclarator>(this);
}
const GlobalNameSpecifierDeclarator *Declarator::getAsGlobalNameSpecifier() const {
  return cast<GlobalNameSpecifierDeclarator>(this);
}
IdentifierDeclarator *Declarator::getAsIdentifier() {
  return cast<IdentifierDeclarator>(this);
}
const IdentifierDeclarator *Declarator::getAsIdentifier() const {
  return cast<IdentifierDeclarator>(this);
}
NestedNameSpecifierDeclarator *Declarator::getAsNestedNameSpecifier() {
  return cast<NestedNameSpecifierDeclarator>(this);
}
const NestedNameSpecifierDeclarator *
Declarator::getAsNestedNameSpecifier() const {
  return cast<NestedNameSpecifierDeclarator>(this);
}
FunctionDeclarator *Declarator::getAsFunction() {
  return cast<FunctionDeclarator>(this);
}
const FunctionDeclarator *Declarator::getAsFunction() const {
  return cast<FunctionDeclarator>(this);
}
TypeDeclarator *Declarator::getAsType() {
  return cast<TypeDeclarator>(this);
}
const TypeDeclarator *Declarator::getAsType() const {
  return cast<TypeDeclarator>(this);
}
TemplateParamsDeclarator *Declarator::getAsTemplateParams() {
  return cast<TemplateParamsDeclarator>(this);
}
const TemplateParamsDeclarator *Declarator::getAsTemplateParams() const {
  return cast<TemplateParamsDeclarator>(this);
}
ImplicitEmptyTemplateParamsDeclarator *
Declarator::getAsImplicitEmptyTemplateParams() {
  return cast<ImplicitEmptyTemplateParamsDeclarator>(this);
}
const ImplicitEmptyTemplateParamsDeclarator *
Declarator::getAsImplicitEmptyTemplateParams() const {
  return cast<ImplicitEmptyTemplateParamsDeclarator>(this);
}

SpecializationDeclarator *Declarator::getAsSpecialization() {
  return cast<SpecializationDeclarator>(this);
}

const SpecializationDeclarator *Declarator::getAsSpecialization() const {
  return cast<SpecializationDeclarator>(this);
}

void Declarator::printSequence(llvm::raw_ostream &os) const {

  const Declarator *D = this;
  do {
    os << D->getString(true);
    if (D->Next)
      os << " -> ";

    D = D->Next;
  }  while (D);

  os << '\n';
}

void Declarator::recordAttributes(const Syntax* AttrNode) {
  if (AttrNode->getAttributes().empty())
    return;
  if (!UnprocessedAttributes) {
    Attributes Attrs;
    UnprocessedAttributes = std::move(Attrs);
  }
  AttributeNode = AttrNode;
  for (const Attribute * A : AttrNode->getAttributes()) {
    UnprocessedAttributes->emplace_back(A->getArg());
  }
}

// ------------------ UnknownDeclarator ----------------------------------------
clang::SourceLocation UnknownDeclarator::getLoc() const {
  return UnknownDeclSyntax->getLoc();
}

std::string UnknownDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[UNKNOWN] <UNKNOWN>";
  return "<UNKNOWN>";
}

// ------------------ ErrorDeclarator ------------------------------------------
clang::SourceLocation ErrorDeclarator::getLoc() const {
  return Err->getLoc();
}

std::string ErrorDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Error] <ERROR>";
  return "<ERROR>";
}

// ------------------ GlobalNameSpecifierDeclarator ----------------------------
clang::SourceLocation GlobalNameSpecifierDeclarator::getLoc() const {
  return Name->getLoc();
}

std::string GlobalNameSpecifierDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[GlobalNameSpecifier] '.' ";
  return "'.'";
}

// ------------------ IdentifierDeclarator -------------------------------------
clang::SourceLocation IdentifierDeclarator::getLoc() const {
  return Name->getLoc();
}

std::string IdentifierDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Name] " + Name->getSpelling();
  return Name->getSpelling();
}

// ------------------ NestedNameSpecifierDeclarator ----------------------------
clang::SourceLocation NestedNameSpecifierDeclarator::getLoc() const {
  return Name->getLoc();
}

std::string NestedNameSpecifierDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Nested Name] " + Name->getSpelling();
  return Name->getSpelling();
}

// ------------------ FunctionDeclarator ---------------------------------------
clang::SourceLocation FunctionDeclarator::getLoc() const {
  return Params->getLoc();
}

std::string FunctionDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Function] (" + std::to_string(getParams()->getNumChildren()) +")";
  return "(" + std::to_string(getParams()->getNumChildren()) +")";
}

const ListSyntax *FunctionDeclarator::getParams() const {
  return cast<ListSyntax>(Params->getArguments());
}

// ------------------ TypeDeclarator -------------------------------------------
clang::SourceLocation TypeDeclarator::getLoc() const {
  return TyExpr->getLoc();
}

std::string TypeDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Type] ";
  if (const AtomSyntax *Nm = dyn_cast<AtomSyntax>(TyExpr)) {
    Ret += Nm->getSpelling();
  } else {
    Ret += "complex-type-expression";
  }
  return Ret;
}

// ------------------ TemplateParamsDeclarator ---------------------------------
clang::SourceLocation TemplateParamsDeclarator::getLoc() const {
  return Params->getLoc();
}

std::string TemplateParamsDeclarator::getString(bool IncludeKind) const {
  // FIXME: Figure out how to convert the given information into a
  // meaningful representation.
  std::string Ret;
  if (IncludeKind)
    Ret += "[Template Parameters] ";
  Ret += "[" + std::to_string(getParams()->getNumChildren()) + "]";
  return Ret;
}

const Syntax *TemplateParamsDeclarator::getSyntax() const {
  return getParams();
}

const ListSyntax *TemplateParamsDeclarator::getParams() const {
  return cast<ListSyntax>(Params->getArguments());
}

// ------------------ ImplicitEmptyTemplateParamsDeclarator --------------------
clang::SourceLocation ImplicitEmptyTemplateParamsDeclarator::getLoc() const {
  return Owner->getLoc();
}

std::string
ImplicitEmptyTemplateParamsDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Implicit Template Parameters] ";
  Ret += "[]";
  return Ret;
}

const Syntax *ImplicitEmptyTemplateParamsDeclarator::getSyntax() const {
  return Owner;
}

// ------------------ SpecializationDeclarator ------------------------------ //
clang::SourceLocation SpecializationDeclarator::getLoc() const {
  return Args->getLoc();
}

std::string SpecializationDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Specialization] ";
  Ret += "[" + std::to_string(getArgs()->getNumChildren()) + "]";
  return Ret;
}

bool SpecializationDeclarator::HasArguments() const {
  return getArgs()->getNumChildren();
}

const ListSyntax *SpecializationDeclarator::getArgs() const {
  return cast<ListSyntax>(Args->getArguments());
}

// ------------------ UsingDirectiveDeclarator ------------------------------ //
const Syntax *UsingDirectiveDeclarator::getArgs() {
  return Args;
}

clang::SourceLocation UsingDirectiveDeclarator::getLoc() const {
  return UsingLoc;
}

// We always include the kind with a using declarator.
std::string UsingDirectiveDeclarator::getString(bool IncludeKind) const {
  unsigned I = 0;
  for (auto *Child : Args->children())
    ++I;

  return "Using {" + std::to_string(I) + "}";
}

} // end namepsace gold
