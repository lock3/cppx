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

// static llvm::StringRef getCallName(const CallSyntax *S) {
//   // Get the bottom-left most element, which should be an
//   // atom syntax naming the function.
//   while (S->getNumArguments()) {
//     const Syntax *L = S->getArgument(0);
//     if (const auto *Atom = dyn_cast<AtomSyntax>(L))
//       return Atom->getSpelling();

//     S = cast<CallSyntax>(L);
//   }

//   // We got here because the parameter list was empty.
//   return "(void)";
// }

// static const char* getDeclaratorKindName(DeclaratorKind DK) {
//   switch(DK) {
//   case DK_Unknown:
//     return "Unknown";
//   case DK_Identifier:
//     return "Identifier";
//   case DK_TemplateParams:
//     return "TemplateType";
//   case DK_Function:
//     return "Function";
//   case DK_Type:
//     return "Type";
//   case DK_GlobalNamespecifier:
//     return "GlobalNamespecifier";
//   case DK_NestedNameSpecifier:
//     return "NameSpecifier";
//   case DK_ImplicitEmptyTemplateParams:
//     return "ImplicitEmptyTemplateParams";
//   case DK_PartialSpecialization:
//     return "PartialSpecialization";
//   case DK_ExplicitSpecialization:
//     return "ExplicitSpecialization";
//   case DK_Error:
//     return "Error";
//   default:
//     llvm_unreachable("Invalid declarator Kind.");
//   }
// }

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
ExplicitSpecializationDeclarator *Declarator::getAsExplicitSpecialization() {
  return cast<ExplicitSpecializationDeclarator>(this);
}
const ExplicitSpecializationDeclarator *
Declarator::getAsExplicitSpecialization() const {
  return cast<ExplicitSpecializationDeclarator>(this);
}
PartialSpecializationDeclarator *Declarator::getAsPartialSpecialization() {
  return cast<PartialSpecializationDeclarator>(this);
}
const PartialSpecializationDeclarator *
Declarator::getAsPartialSpecialization() const {
  return cast<PartialSpecializationDeclarator>(this);
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
    return "[Name] " + Name->getSpelling().str();
  return Name->getSpelling().str();
}

// ------------------ NestedNameSpecifierDeclarator ----------------------------
clang::SourceLocation NestedNameSpecifierDeclarator::getLoc() const {
  return Name->getLoc();
}

std::string NestedNameSpecifierDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Nested Name] " + Name->getSpelling().str();
  return Name->getSpelling().str();
}

// ------------------ FunctionDeclarator ---------------------------------------
clang::SourceLocation FunctionDeclarator::getLoc() const {
  return Params->getLoc();
}

std::string FunctionDeclarator::getString(bool IncludeKind) const {
  if (IncludeKind)
    return "[Function] (" + std::to_string(Params->getNumChildren()) +")";
  return "(" + std::to_string(Params->getNumChildren()) +")";
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
    Ret += Nm->getSpelling().str();
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
  Ret += "[" + std::to_string(Params->getNumChildren()) + "]";
  return Ret;
}

// ------------------ ImplicitEmptyTemplateParamsDeclarator --------------------
clang::SourceLocation ImplicitEmptyTemplateParamsDeclarator::getLoc() const {
  return Owner->getLoc();
}

std::string ImplicitEmptyTemplateParamsDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Implicit Template Parameters] ";
  Ret += "[]";
  return Ret;
}

// ------------------ ExplicitSpecializationDeclarator -------------------------
clang::SourceLocation ExplicitSpecializationDeclarator::getLoc() const {
  return Args->getLoc();
}

std::string ExplicitSpecializationDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Explicit Specialization] ";
  Ret += "[" + std::to_string(Args->getNumChildren()) + "]";
  return Ret;
}

// ------------------ PartialSpecializationDeclarator --------------------------
clang::SourceLocation PartialSpecializationDeclarator::getLoc() const {
  return Args->getLoc();
}

std::string PartialSpecializationDeclarator::getString(bool IncludeKind) const {
  std::string Ret;
  if (IncludeKind)
    Ret += "[Partial Specialization] ";
  Ret += "[" + std::to_string(Args->getNumChildren()) + "]";
  return Ret;
}

}