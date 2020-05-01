//===- GoldScope.cpp - Simple scope used in Gold parsing ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implements the Scope interface.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

namespace gold {

// The identifier, if present, is the first node in the declarator.
const Syntax *Declarator::getId() const {
  if (Kind == DK_Identifier)
    return Data.Id;
  return nullptr;
}

// The type specifier, if present, is the last node in the declarator.
const Syntax *Declarator::getType() const {
  const Declarator *D = this;
  while (D->Next) {
    D = D->Next;
  }
  if (D->Kind == DK_Type)
    return D->Data.Type;
  return nullptr;
}

clang::SourceLocation Declarator::getLoc() const {
  switch (getKind()) {
  case DK_Identifier:
  case DK_Function:
    return getId()->getLoc();
  case DK_Type:
    return getType()->getLoc();
  default:
    llvm_unreachable("We are unable to actually get valid source location for "
        "declarator.");
    return clang::SourceLocation();
  }
}

static llvm::StringRef getCallName(const CallSyntax *S) {
  // Get the bottom-left most element, which should be an
  // atom syntax naming the function.
  while (S->getNumArguments()) {
    const Syntax *L = S->getArgument(0);
    if (const auto *Atom = dyn_cast<AtomSyntax>(L))
      return Atom->getSpelling();

    S = cast<CallSyntax>(L);
  }

  // We got here because the parameter list was empty.
  return "(void)";
}

std::string Declarator::getString() const {
  // FIXME: This needs to properly elaborate all parts of the declarator.
  if (getKind() == DK_Type) {
    if (isa<AtomSyntax>(Data.Type)) {
      // TODO: Figure out how to correctly print types. that are not simple identifiers.
      return cast<AtomSyntax>(Data.Type)->getSpelling();
    } else {
      return "Some complex type expression\n";
    }
  } else if (isFunction()) {
    return getCallName(cast<CallSyntax>(Call));
  } else if (isIdentifier()) {
    return cast<AtomSyntax>(Data.Id)->getSpelling();
  } else if (getKind() == DK_Pointer) {
    return "^";
  } else if (getKind() == DK_Array) {
    if (Data.Index)
      return '[' + std::string(cast<AtomSyntax>(Data.Index)->getSpelling()) + ']';
    return "[]";
  } else {
    return "[unimplemented]";
  }
}

static const char* getDeclaratorKindStr(DeclaratorKind DK) {
  switch(DK){
  case DeclaratorKind::DK_Unknown:
    return "DK_Unknown";
  case DeclaratorKind::DK_Identifier:
    return "DK_Identifier";
  case DeclaratorKind::DK_TemplateType:
    return "DK_TemplateType";
  case DeclaratorKind::DK_Pointer:
    return "DK_Pointer";
  case DeclaratorKind::DK_Array:
    return "DK_Array";
  case DeclaratorKind::DK_Function:
    return "DK_Function";
  case DeclaratorKind::DK_Type:
    return "DK_Type";
  default:
    llvm_unreachable("Invalid declarator Kind.");
  }
}


void Declarator::printSequence(llvm::raw_ostream &os) const {

  const Declarator *D = this;
  do {
    os << getDeclaratorKindStr(D->getKind()) << ": "<< D->getString();
    if (D->Next)
      os << " -> ";

    D = D->Next;
  }  while (D);

  os << '\n';
}

void Declarator::recordAttributes(const Syntax* AttrNode) {
  if (AttrNode->getAttributes().empty())
    return;
  AttributeNode = AttrNode;
  Attributes Attrs;
  for (const Attribute * A : AttrNode->getAttributes()) {
    Attrs.emplace_back(A->getArg());
  }
  UnprocessedAttributes = std::move(Attrs);
}

Declaration::~Declaration() {
  delete SavedScope;
}

// A declarator declares a variable, if it does not declare a function.
bool Declaration::declaresVariable() const {
  return !declaresFunction() /*&& !declaresType()*/;
}

bool Declaration::declaresType() const {  
  const Declarator* D = Decl;
  while (D && D->Kind != DK_Type) {
    D = D->Next;
  }
  if (D)
    if (D->Kind == DK_Type)
      if (const auto *Atom = dyn_cast<AtomSyntax>(D->Data.Type))
        return Atom->getSpelling() == "type";
  return false;
}

bool Declaration::declaresRecord() const {
  if (!declaresType())
    return false;
  if (Cxx)
    return isa<clang::CXXRecordDecl>(Cxx);
  if (const MacroSyntax *Macro = dyn_cast_or_null<MacroSyntax>(Init))
    return cast<AtomSyntax>(Macro->getCall())->hasToken(tok::ClassKeyword);
  return false;
}

bool Declaration::declaresTemplateType() const {
  const Declarator *D = Decl;
  while (D && D->Kind != DK_TemplateType) {
    D = D->Next;
  }
  if (!D)
    return false;
  return D && D->Call && clang::isa<ElemSyntax>(D->Call);
}

// A declarator declares a function if it's first non-id declarator is
// declares parameters.
bool Declaration::declaresFunction() const {
  assert(Decl);
  const Declarator *D = Decl;
  if (D->Kind == DK_Identifier)
    D = D->Next;
  if (D)
    return D->Kind == DK_Function;
  return false;
}

bool Declaration::declaresMemberVariable() const {
  return declaresVariable() && Cxx && clang::isa<clang::FieldDecl>(Cxx);
}

bool Declaration::declaresMemberFunction() const {
  return declaresFunction() && Cxx && clang::isa<clang::CXXMethodDecl>(Cxx);
}

bool Declaration::declaresConstructor() const {
  return declaresFunction() && Cxx
    && clang::isa<clang::CXXConstructorDecl>(Cxx);
}

bool Declaration::declaresDestructor() const {
  return declaresFunction() && Cxx
    && clang::isa<clang::CXXConstructorDecl>(Cxx);
}

// A declarator declares a template if it's first non-id declarator is
// declares template parameters.
// FIXME: this might not work for specializations.
bool Declaration::declaresTemplate() const {
  assert(Decl);
  const Declarator *D = Decl;
  // TODO: In the future we would need to extend this definition to make sure
  // that everything works as expected whe we do have an identifier that
  // is infact also a template name.
  if (D->Kind != DK_Function)
    return false;
  if (D->Kind == DK_Identifier)
    D = D->Next;
  if (D)
    return D->Data.ParamInfo.TemplateParams;
  return false;
}

bool Declaration::declaresTypeAlias() const {
  return Cxx && isa<clang::TypeAliasDecl>(Cxx);
}

const Syntax *Declaration::getTemplateParams() const {
  assert(Decl);
  const Declarator *D = Decl;
  while (D && D->Kind == DK_Identifier)
    D = D->Next;
  if (D)
    return D->Data.ParamInfo.TemplateParams;
  return nullptr;
}

const Declarator *Declaration::getFirstTemplateDeclarator() const {
  const Declarator *D = Decl;
  while (D && D->Kind != DK_TemplateType) {
    D = D->Next;
  }
  return D;
}

Declarator *Declaration::getFirstTemplateDeclarator() {
  Declarator *D = Decl;
  while (D && D->Kind != DK_TemplateType) {
    D = D->Next;
  }
  return D;
}

clang::DeclContext *Declaration::getCxxContext() const {
  return clang::Decl::castToDeclContext(Cxx);
}

void Declaration::setPreviousDecl(Declaration *Prev) {
  Prev->Next = this;
  First = Prev->First;
  Next = First;
}


void Scope::addUserDefinedType(clang::IdentifierInfo* Id, clang::QualType QualTy) {
  auto ItPair = TypeIdMap.try_emplace(Id, QualTy);
  if(!ItPair.second) {
    // TODO: Figure out how to print correct diagnostics here.
    // llvm::outs() << "Duplicate Identifer located. Name: " << Id->getName() << " QualType Given: ";
    // QualTy.dump();
    // llvm::outs() << "\n";
    assert(false && "Invalid typename.");
  }
}

clang::QualType Scope::getUserDefinedType(clang::IdentifierInfo* Id) const {
  auto It = TypeIdMap.find(Id);
  if(It == TypeIdMap.end()) {
    return clang::QualType();
  }
  return It->second;
}

static llvm::StringRef getScopeKindName(ScopeKind K) {
  switch (K) {
  case SK_Namespace:
    return "Namespace";

  case SK_Parameter:
    return "Parameter";

  case SK_Template:
    return "Template";

  case SK_Function:
    return "Function";

  case SK_Block:
    return "Block";

  case SK_Class:
    return "Class";

  case SK_Control:
    return "Control";
  default:
    llvm_unreachable("Invalid scope kind");
  }
}

void Scope::dump(llvm::raw_ostream &os) const {
  os << getScopeKindName(getKind()) << '\n';
  for (auto D : IdMap)
    os << D.first->getName() << '\n';
}

void Scope::dump() const {
  dump(llvm::errs());
}

} // namespace gold
