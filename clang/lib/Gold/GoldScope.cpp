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
    return Call->getLoc();
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

static const char* getDeclaratorKindName(DeclaratorKind DK) {
  switch(DK) {
  case DK_Unknown:
    return "Unknown";
  case DK_Identifier:
    return "Identifier";
  case DK_TemplateType:
    return "TemplateType";
  // case DK_Pointer:
  //   return "Pointer";
  // case DK_Array:
  //   return "Array";
  case DK_Function:
    return "Function";
  case DK_Type:
    return "Type";
  // case DK_Const:
  //   return "Const";
  // case DK_Ref:
  //   return "Ref";
  // case DK_RRef:
  //   return "RRef";
  case DK_Error:
    return "Error";
  default:
    llvm_unreachable("Invalid declarator Kind.");
  }
}
std::string Declarator::getString(bool IncludeKind) const {
  using namespace std::string_literals;
  // FIXME: This needs to properly elaborate all parts of the declarator.
  if (getKind() == DK_Type) {
    if (isa<AtomSyntax>(Data.Type)) {
      // TODO: Figure out how to correctly print types. that are
      // not simple identifiers.
      if (IncludeKind) {
        return "("s + getDeclaratorKindName(getKind()) + ") "
            + cast<AtomSyntax>(Data.Type)->getSpelling().str();
      }
      return cast<AtomSyntax>(Data.Type)->getSpelling().str();
    } else {
      return "Some complex type expression\n";
    }
  } else if (isFunction()) {
    return getCallName(cast<CallSyntax>(Call)).str();
  } else if (isIdentifier()) {
    return cast<AtomSyntax>(Data.Id)->getSpelling().str();
  // } else if (getKind() == DK_Pointer) {
  //   return "^";
  // } else if (getKind() == DK_Array) {
  //   if (Data.Index)
  //     return '[' + std::string(cast<AtomSyntax>(Data.Index)->getSpelling()) + ']';
  //   return "[]";
  // } else if (getKind() == DK_Const) {
  //   return "const";
  } else {
    return "[unimplemented]";
  }
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

clang::SourceLocation Declaration::getEndOfDecl() const {
  const Declarator *D = Decl;
  if (!D)
    return clang::SourceLocation();

  if (Init)
    return Init->getLoc();

  while(D->Next) {
    D = D->Next;
  }
  return D->getLoc();
}

// A declarator declares a variable, if it does not declare a function.
bool Declaration::declaresVariable() const {
  return !declaresFunction();
}

bool Declaration::templateHasDefaultParameters() const {
  // if (declaresFunctionTemplate()) {
  //   assert(false);
  // }
  // if (declaresTemplateType()) {
  //   const Declarator *TemplateInfo = getFirstTemplateDeclarator();
  //   // for (TemplateInfo)
  //   assert(false);
  // }
  // return false;
  // TODO: This is necessary for figuring out if a template parameter has
  // delayed evaluation or not.
  llvm_unreachable("This isn't implemented yet, but it may need to be in the "
      "near future.");
}

bool Declaration::declaresInitializedVariable() const {
  return declaresVariable() && Init;
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
  // if (!declaresType())
  //   return false;
  if (Cxx)
    return isa<clang::CXXRecordDecl>(Cxx);
  if (Init)
    if (const MacroSyntax *Macro = dyn_cast<MacroSyntax>(Init)) {
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Macro->getCall()))
        return Atom->hasToken(tok::ClassKeyword);
      if (const CallSyntax *ClsWithBases = dyn_cast<CallSyntax>(Macro->getCall()))
        if (const AtomSyntax *Callee
                  = dyn_cast<AtomSyntax>(ClsWithBases->getCallee()))
            return Callee->hasToken(tok::ClassKeyword);
    }
  return false;
}

bool Declaration::declaresNamespace() const {
  if (Cxx)
    return isa<clang::NamespaceDecl>(Cxx);
  if (const MacroSyntax *Macro = dyn_cast_or_null<MacroSyntax>(Init))
    return cast<AtomSyntax>(Macro->getCall())->hasToken(tok::NamespaceKeyword);
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
bool Declaration::declaresFunctionTemplate() const {
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

bool Declaration::declIsStatic() const {
  const Declarator *D = Decl;
  if (!D) {
    return false;
  }
  if (!D->UnprocessedAttributes)
    return false;
  
  auto Iter = std::find_if(D->UnprocessedAttributes->begin(),
      D->UnprocessedAttributes->end(), [](const Syntax *S) -> bool{
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(S)) {
          if (Atom->getSpelling() == "static") {
            return true;
          }
        }
        return false;
      });
  return Iter != D->UnprocessedAttributes->end();
}

bool Declaration::declaresFunctionDecl() const {
  return declaresFunction() && !Init;
}

bool Declaration::decalaresFunctionDef() const {
  return declaresFunction() && Init;
}

bool Declaration::declaresInlineInitializedStaticVarDecl() const {
  if (!Cxx)
    return false;
  clang::VarDecl *VD = dyn_cast<clang::VarDecl>(Cxx);
  if (!VD) 
    return false;
  return VD->isInline() && VD->getStorageClass() == clang::SC_Static;
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

const Declarator *Declaration::getIdDeclarator() const {
  const Declarator *D = Decl;
  while (D && D->Kind != DK_Identifier) {
    D = D->Next;
  }
  return D;
}

Declarator *Declaration::getIdDeclarator() {
  Declarator *D = Decl;
  while (D && D->Kind != DK_Identifier) {
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

bool Declaration::isDeclaredWithinClass() const {
  const Scope *Cur = ScopeForDecl;
  while(Cur) {
    if (Cur->getKind() == SK_Class)
      return true;
    Cur = Cur->getParent();
  }
  return false;
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
  }

  llvm_unreachable("invalid scope");
}

void Scope::dump(llvm::raw_ostream &os) const {
  os << getScopeKindName(getKind()) << '\n';
  if (getKind() == SK_Template) {
    for(auto D : IdMap) {
      os << D.first->getName();
      if (D.second->Cxx)
        D.second->Cxx->dump(os << " " << D.first << " ");
      else
        os << "\n";
    }
  } else 
    for (auto D : IdMap)
      os << D.first->getName() << '\n';
}

void Scope::dump() const {
  dump(llvm::errs());
}
void Scope::dumpScopeChain() const {
  const Scope *Cur = this;
  while (Cur) {
    llvm::outs() << "-----------------------\n";
    Cur->dump();
    Cur = Cur->getParent();
  }
  llvm::outs() << "-----------------------\n";
}

Phase phaseOf(Declaration *D) {
  return D->CurrentPhase;
}

} // namespace gold
