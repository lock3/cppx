//===- GoldDeclaration.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implementation for the gold declaration.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldDeclaration.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

#include "llvm/Support/raw_ostream.h"
namespace gold {

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
  // llvm_unreachable("Declaration::declaresVariable broken!");
  return !declaresFunction();
}

bool Declaration::templateHasDefaultParameters() const {
  // TODO: This is necessary for figuring out if a template parameter has
  // delayed evaluation or not.
  llvm_unreachable("This isn't implemented yet, but it may need to be in the "
      "near future.");
}

bool Declaration::declaresInitializedVariable() const {
  // llvm_unreachable("Declaration::declaresInitializedVariable broken!");
  return declaresVariable() && Init;
}

bool Declaration::declaresType() const {
  // llvm_unreachable("Declaration::declaresType broken!");
  const Declarator* D = Decl;
  // while (D && D->getKind() != DK_Type) {
  //   D = D->Next;
  // }
  if (TypeDcl)
    if (const auto *Atom = dyn_cast<AtomSyntax>(D->getAsType()->getTyExpr()))
      return Atom->getSpelling() == "type";
  return false;
}

bool Declaration::declaresForwardRecordDecl() const {
  // llvm_unreachable("Declaration::declaresForwardRecordDecl() broken!");
  if (declaresInitializedVariable()) {
    if (const AtomSyntax *RHS = dyn_cast<AtomSyntax>(Init)) {
      return RHS->hasToken(tok::ClassKeyword)
             || RHS->hasToken(tok::UnionKeyword)
             || RHS->hasToken(tok::EnumKeyword);
    } else if (const CallSyntax *Call = dyn_cast<CallSyntax>(Init)) {
      if (const AtomSyntax *Nm = dyn_cast<AtomSyntax>(Call->getCallee())) {
        return Nm->hasToken(tok::EnumKeyword);
      }
    }
  }
  return false;
}

bool Declaration::declaresTag() const {
  // llvm_unreachable("Declaration::declaresTag broken!");
  if (Cxx)
    return isa<clang::CXXRecordDecl>(Cxx);
  if (Init)
    if (const MacroSyntax *Macro = dyn_cast<MacroSyntax>(Init)) {
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Macro->getCall()))
        return Atom->hasToken(tok::ClassKeyword)
               || Atom->hasToken(tok::UnionKeyword)
               || Atom->hasToken(tok::EnumKeyword);
      if (const CallSyntax *ClsWithBases = dyn_cast<CallSyntax>(Macro->getCall()))
        if (const AtomSyntax *Callee
                  = dyn_cast<AtomSyntax>(ClsWithBases->getCallee()))
          return Callee->hasToken(tok::ClassKeyword)
                  || Callee->hasToken(tok::UnionKeyword)
                  || Callee->hasToken(tok::EnumKeyword);
    }
  return false;
}

bool Declaration::getTagName(const AtomSyntax *&NameNode) const {
  // llvm_unreachable("Declaration::getTagName broken!");
  if (Init)
    if (const MacroSyntax *Macro = dyn_cast<MacroSyntax>(Init)) {
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Macro->getCall()))
        if (Atom->hasToken(tok::ClassKeyword)
            || Atom->hasToken(tok::UnionKeyword)
            || Atom->hasToken(tok::EnumKeyword)) {
          NameNode = Atom;
          return true;
        }
      if (const CallSyntax *ClsWithBases = dyn_cast<CallSyntax>(Macro->getCall()))
        if (const AtomSyntax *Callee
                  = dyn_cast<AtomSyntax>(ClsWithBases->getCallee()))
          if (Callee->hasToken(tok::ClassKeyword)
              || Callee->hasToken(tok::UnionKeyword)
              || Callee->hasToken(tok::EnumKeyword)) {
            NameNode = Callee;
            return true;
          }
    }
  return false;
}

bool Declaration::declaresNamespace() const {
  // llvm_unreachable("Declaration::declaresNamespace broken!");
  if (Cxx)
    return isa<clang::NamespaceDecl>(Cxx);
  if (const MacroSyntax *Macro = dyn_cast_or_null<MacroSyntax>(Init))
    return cast<AtomSyntax>(Macro->getCall())->hasToken(tok::NamespaceKeyword);
  return false;
}

bool Declaration::declaresTemplateType() const {
  // llvm_unreachable("Declaration::declaresTemplateType broken!");
  // const Declarator *D = Decl;
  // while (D && D->getKind() != DK_TemplateParams) {
  //   D = D->Next;
  // }
  // if (!D)
  //   return false;
  return TemplateParameters && !FunctionDcl;
  // return D && D->Call && clang::isa<ElemSyntax>(D->Call);
}

// A declarator declares a function if it's first non-id declarator is
// declares parameters.
bool Declaration::declaresFunction() const {
  // llvm_unreachable("Declaration::declaresFunction broken!");
  // assert(Decl);
  // const Declarator *D = Decl;
  // if (D->getKind() == DK_Identifier)
  //   D = D->Next;
  // if (D)
  //   return D->getKind() == DK_Function;
  // return false;
  return FunctionDcl;
}

bool Declaration::declaresFunctionWithImplicitReturn() const {
  // llvm_unreachable("Declaration::declaresFunctionWithImplicitReturn broken!");
  if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      // Something is very wrong here?!
      return false;
    if (const CallSyntax *Call = dyn_cast<CallSyntax>(Op)){
      if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
        if (Name->getSpelling() == "operator'='") {
          return true;
        }
      }
    }
  }
  return false;
}

bool Declaration::declaresPossiblePureVirtualFunction() const {
  // llvm_unreachable("Declaration::declaresPossiblePureVirtualFunction broken!");
  if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    if (const CallSyntax *Call = dyn_cast<CallSyntax>(Op))
      if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Call->getCallee()))
        if (Name->getSpelling() == "operator'='")
          if (const LiteralSyntax *Lit
                                = dyn_cast<LiteralSyntax>(Call->getArgument(1)))
            if (Lit->getToken().getKind() == tok::DecimalInteger)
              if (Lit->getSpelling() == "0")
                return true;
  }
  return false;
}

static bool isSpecialExpectedAssignedFuncValue(const Syntax *Op, TokenKind TK) {
  // llvm_unreachable("isSpecialExpectedAssignedFuncValue broken!");
  if (const CallSyntax *Call = dyn_cast<CallSyntax>(Op))
    if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Call->getCallee()))
      if (Name->getSpelling() == "operator'='")
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Call->getArgument(1)))
          if (Atom->getToken().getKind() == TK)
            return true;
  return false;
}

bool Declaration::declaresDefaultedFunction() const {
  // llvm_unreachable("Declaration::declaresDefaultedFunction broken!");
 if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    return isSpecialExpectedAssignedFuncValue(Op, tok::DefaultKeyword);
  }
  return false;
}


bool Declaration::declaresDeletedFunction() const {
  // llvm_unreachable("Declaration::declaresDeletedFunction broken!");
 if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    return isSpecialExpectedAssignedFuncValue(Op, tok::DeleteKeyword);
  }
  return false;
}

bool Declaration::declaresMemberVariable() const {
  // llvm_unreachable("Declaration::declaresMemberVariable broken!");
  return declaresVariable() && Cxx && clang::isa<clang::FieldDecl>(Cxx);
}

bool Declaration::declaresMemberFunction() const {
  // llvm_unreachable("Declaration::declaresMemberFunction broken!");
  return declaresFunction() && Cxx && clang::isa<clang::CXXMethodDecl>(Cxx);
}

bool Declaration::declaresConstructor() const {
  // llvm_unreachable("Declaration::declaresConstructor broken!");
  return declaresFunction() && Cxx
    && clang::isa<clang::CXXConstructorDecl>(Cxx);
}

bool Declaration::declaresDestructor() const {
  // llvm_unreachable("Declaration::declaresDestructor broken!");
  return declaresFunction() && Cxx && clang::isa<clang::CXXConstructorDecl>(Cxx);
}

// A declarator declares a template if it's first non-id declarator is
// declares template parameters.
// FIXME: this might not work for specializations.
bool Declaration::declaresFunctionTemplate() const {
  // llvm_unreachable("Declaration::declaresFunctionTemplate broken!");
  // assert(Decl);
  // const Declarator *D = Decl;
  // TODO: In the future we would need to extend this definition to make sure
  // that everything works as expected whe we do have an identifier that
  // is infact also a template name.
  return FunctionDcl && TemplateParameters;
  // while (D && D->getKind() == DK_Identifier)
  //   D = D->Next;
  // if (!D)
  //   return false;
  // if (D->getKind() != DK_Function)
  //   return false;
  // return D->Data.ParamInfo.TemplateParams;
}


bool Declaration::declaresOperatorOverload() const {
  // llvm_unreachable("Declaration::declaresOperatorOverload broken!");
  if (!OpInfo)
    return false;
  return declaresFunction();
}

bool Declaration::declaresTypeAlias() const {
  // llvm_unreachable("Declaration::declaresTypeAlias broken!");
  return Cxx && isa<clang::TypeAliasDecl>(Cxx);
}

bool Declaration::declIsStatic() const {
  // llvm_unreachable("Declaration::declIsStatic broken!");
  // const Declarator *D = Decl;
  // D = D->Next;
  // if (!D) {
  //   return false;
  // }
  if (!IdDcl->UnprocessedAttributes)
    return false;

  auto Iter = std::find_if(IdDcl->UnprocessedAttributes->begin(),
      IdDcl->UnprocessedAttributes->end(), [](const Syntax *S) -> bool{
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(S)) {
          if (Atom->getSpelling() == "static") {
            return true;
          }
        }
        return false;
      });
  return Iter != IdDcl->UnprocessedAttributes->end();
}

bool Declaration::declaresFunctionDecl() const {
  // llvm_unreachable("Declaration::declaresFunctionDecl broken!");
  return declaresFunction() && !Init;
}

bool Declaration::decalaresFunctionDef() const {
  // llvm_unreachable("Declaration::decalaresFunctionDef broken!");
  return declaresFunction() && Init;
}

bool Declaration::declaresInlineInitializedStaticVarDecl() const {
  // llvm_unreachable("Declaration::declaresInlineInitializedStaticVarDecl broken!");
  if (!Cxx)
    return false;
  clang::VarDecl *VD = dyn_cast<clang::VarDecl>(Cxx);
  if (!VD)
    return false;
  return VD->isInline() && VD->getStorageClass() == clang::SC_Static;
}

const Syntax *Declaration::getTemplateParams() const {
  // llvm_unreachable("Declaration::getTemplateParams broken!");
  if (!TemplateParameters) {
    return nullptr;
  }
  TemplateParamsDeclarator *TPD
                           = cast<TemplateParamsDeclarator>(TemplateParameters);
  return TPD->getParams();
  // assert(Decl);
  // const Declarator *D = Decl;
  // while (D && D->getKind() == DK_Identifier)
  //   D = D->Next;
  // if (D)
  //   return D->Data.ParamInfo.TemplateParams;
  // return nullptr;
}

const Declarator *Declaration::getFirstTemplateDeclarator() const {
  const Declarator *D = Decl;
  while (D && D->getKind() != DK_TemplateParams) {
    D = D->Next;
  }
  return D;
}

Declarator *Declaration::getFirstTemplateDeclarator() {
  // llvm_unreachable("Declaration::getFirstTemplateDeclarator broken!");
  Declarator *D = Decl;
  while (D && D->getKind() != DK_TemplateParams) {
    D = D->Next;
  }
  return D;
}

const Declarator *Declaration::getIdDeclarator() const {
  // llvm_unreachable("Declaration::getIdDeclarator broken!");
  return IdDcl;
  // const Declarator *D = Decl;
  // while (D && D->getKind() != DK_Identifier) {
  //   D = D->Next;
  // }
  // return D;
}

Declarator *Declaration::getIdDeclarator() {
  // llvm_unreachable("Declaration::getIdDeclarator broken!");
  return IdDcl;
  // Declarator *D = Decl;
  // while (D && D->getKind() != DK_Identifier) {
  //   D = D->Next;
  // }
  // return D;
}

const Declarator *Declaration::getFirstDeclarator(DeclaratorKind DK) const {
  // llvm_unreachable("Declaration::getFirstDeclarator broken!");
  const Declarator *D = Decl;
  while (D && D->getKind() != DK) {
    D = D->Next;
  }
  return D;
}

Declarator *Declaration::getFirstDeclarator(DeclaratorKind DK) {
  // llvm_unreachable("Declaration::getFirstDeclarator broken!");
  Declarator *D = Decl;
  while (D && D->getKind() != DK) {
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
}