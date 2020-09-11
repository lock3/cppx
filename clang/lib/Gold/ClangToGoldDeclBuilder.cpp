//===- ClangToGoldDeclBuilder.cpp -----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implementation for the clang to gold converter.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/ClangToGoldDeclBuilder.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"

namespace gold {

static const Syntax *buildIdentifierNode(SyntaxContext &Ctx,
                                         const char* Text,
                                         clang::SourceLocation Loc) {
  Symbol Sym = getSymbol(Text);
  Token Tok(tok::Identifier, Loc, Sym);
  return new (Ctx) AtomSyntax(Tok);
}

static const Syntax *buildDummyASTNode(SyntaxContext &Ctx,
                                         clang::SourceLocation Loc) {
  return buildIdentifierNode(Ctx, "Dummy", Loc);
}

// Defining this here to avoid conflict with Sema.
struct ClangToGoldDeclRebuilder::StateRAII {
  ClangToGoldDeclRebuilder &Rebuilder;
  Sema::OptionalScopeRAII ChildScope;
  Declaration *PrevParent = nullptr;
  Declaration *PrevDecl = nullptr;
  const Syntax *DummyNode = nullptr;
private:
  void createDecl(UnevaluatedDeclKind SuspectedDeclKind,
                  clang::NamedDecl *NmdDcl) {
    Rebuilder.ParentDecl = Rebuilder.CurDecl;
    DummyNode = buildDummyASTNode(Rebuilder.Context,
                                  NmdDcl->getLocation());
    Rebuilder.CurDecl =  new Declaration(Rebuilder.ParentDecl,
                                         DummyNode, nullptr, nullptr,
                                         SuspectedDeclKind);
    Rebuilder.CurDecl->Id = NmdDcl->getDeclName().getAsIdentifierInfo();
    Rebuilder.CurDecl->CurrentPhase = Phase::Initialization;
    Rebuilder.CurDecl->ScopeForDecl = Rebuilder.SemaRef.getCurrentScope();
    Rebuilder.CurDecl->ParentDecl = PrevDecl;
    Rebuilder.SemaRef.setDeclForDeclaration(Rebuilder.CurDecl, NmdDcl);
    Rebuilder.SemaRef.getCurrentScope()->addDecl(Rebuilder.CurDecl);
  }
public:
  StateRAII(ClangToGoldDeclRebuilder &Builder,
            UnevaluatedDeclKind SuspectedDeclKind, clang::NamedDecl *NmdDcl)
    :Rebuilder(Builder),
    ChildScope(Rebuilder.SemaRef),
    PrevParent(Rebuilder.ParentDecl),
    PrevDecl(Rebuilder.CurDecl)
  {
    createDecl(SuspectedDeclKind, NmdDcl);
  }

  StateRAII(ClangToGoldDeclRebuilder &Builder,
            UnevaluatedDeclKind SuspectedDeclKind, clang::NamedDecl *NmdDcl,
            ScopeKind SK, bool SetAsEntity = false, bool SaveScope = true)
    :Rebuilder(Builder),
    ChildScope(Rebuilder.SemaRef),
    PrevParent(Rebuilder.ParentDecl),
    PrevDecl(Rebuilder.CurDecl)
  {
    createDecl(SuspectedDeclKind, NmdDcl);

    if (SaveScope)
      ChildScope.Init(SK, DummyNode, &Rebuilder.CurDecl->SavedScope);
    else
      ChildScope.Init(SK, DummyNode);

    if (SetAsEntity)
      Rebuilder.SemaRef.getCurrentScope()->Entity = Rebuilder.CurDecl;
  }

  ~StateRAII() {
    Rebuilder.CurDecl = PrevDecl;
    Rebuilder.ParentDecl = PrevParent;
  }

};

static void dumpDeclInfo(int Depth, Declaration *D, llvm::raw_ostream &Out);
static llvm::raw_ostream &indent(llvm::raw_ostream &Out, int Depth) {
  for(int i = 0; i < Depth; ++i)
    Out << "  ";
  return Out;
}
static void dumpScopeInfo(int Depth, Scope *S, llvm::raw_ostream &Out) {
  indent(Out, Depth) << "Scope: " << S->getKindStr() << " (" << S << ")\n";
  ++Depth;
  if (S->Entity) {
    indent(Out, Depth) << "Entity: " << S->Entity->getId()->getName()
                       << " (" << S->Entity << ")\n";
  }
  for(auto DPair : S->DeclMap) {
    dumpDeclInfo(Depth, DPair.second, Out);
  }
}
static void dumpDeclInfo(int Depth, Declaration *D, llvm::raw_ostream &Out) {
  indent(Out, Depth) << "Declaration: " << D->getId()->getName() << " (" << D << ")\n";
  ++Depth;
  indent(Out, Depth) << "Parent: " << D->ParentDecl << "\n";
  indent(Out, Depth) << "UDK: " << unevaluatedDeclKindToStr(D->getKind()) << "\n";
  indent(Out, Depth) << "Cxx = " << D->Cxx << "\n";
  D->Cxx->dump();
  indent(Out, Depth) << "Scope for decl = " << D->ScopeForDecl << "\n";
  indent(Out, Depth) << "phase = " << phaseToStr(D->CurrentPhase) << "\n";
  indent(Out, Depth) << "Saved Scope = " << D->SavedScope << "\n";
  if (D->SavedScope) {
    dumpScopeInfo(Depth, D->SavedScope, Out);
  }
}

static void dumpScopeStructure(gold::Scope *ScopeRoot, Declaration *Root,
                               llvm::raw_ostream &Out = llvm::outs()) {
  Out << "Root Scope: " << ScopeRoot->getKindStr() << " (" << ScopeRoot << ")\n";
  dumpDeclInfo(0, Root, Out);
}

Declaration *ClangToGoldDeclRebuilder::rebuild(clang::CXXRecordDecl *ToConvert) {
  RD = ToConvert;
  RootScope = determineParentScope();
  if (!RootScope)
    // FIXME: Create an error message for this?!
    llvm_unreachable("Failed to determine parent scope.");
  // llvm_unreachable("Working on it. rebuild");

  // Resuming the parent scope that the parent declaration was declared in.
  Sema::ResumeScopeRAII ParentScopeRAII(SemaRef, RootScope,
                                        RootScope->getConcreteTerm());
  CurDecl = ParentDecl;
  StateRAII RecordState(*this, ParentDecl->getKind(), RD, SK_Template,
                        false, false);
  Sema::ScopeRAII RecordBodyScope(SemaRef, ParentDecl->SavedScope->getKind(),
                                  RootScope->getConcreteTerm(),
                                  &CurDecl->SavedScope);
  SemaRef.getCurrentScope()->Entity = CurDecl;
  RootDecl = CurDecl;

  // Next thing to do is Create additional declarations based off the contents
  // of the body of the actual declaration.
  for(clang::Decl *ChildDecl : ToConvert->decls()) {
    if (rebuildMember(ChildDecl)){
      dumpScopeStructure(RootScope, RootDecl);
      llvm_unreachable("Create an error message for this that makes sense.");
    }
  }
  // llvm::outs() << "Complete declaration structure.\n";
  // dumpScopeStructure(RootScope, RootDecl);
  return CurDecl;
}

gold::Scope *ClangToGoldDeclRebuilder::determineParentScope() {
  if (isa<clang::ClassTemplatePartialSpecializationDecl>(RD)) {
    // Logically this should never happen!.
    llvm_unreachable("Cannot determine the scope of an implicit "
                     "partial specialization.");
  }

  auto *CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(RD);
  if (!CTSD)
    llvm_unreachable("Not sure if this could actually ever happe.");

  clang::ClassTemplateDecl *CTD = CTSD->getSpecializedTemplate();
  assert(CTD && "Failed to get original template.");
  ParentDecl = SemaRef.getDeclaration(CTD);
  return ParentDecl->ScopeForDecl;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::Decl *Member) {
  using namespace clang;
  switch(Member->getKind()) {
  case clang::Decl::CXXRecord:{
    return rebuildMember(cast<CXXRecordDecl>(Member));
  }
  break;
  case clang::Decl::Field:{
    return rebuildMember(cast<FieldDecl>(Member));
  }
  break;
  case clang::Decl::Var:{
    return rebuildMember(cast<VarDecl>(Member));
  }
  break;
  case clang::Decl::CXXMethod:{
    return rebuildMember(cast<CXXMethodDecl>(Member));
  }
  break;
  case clang::Decl::CXXConversion:{
    return rebuildMember(cast<CXXConversionDecl>(Member));
  }
  break;
  case clang::Decl::CXXDestructor:{
    return rebuildMember(cast<CXXDestructorDecl>(Member));
  }
  break;
  case clang::Decl::CXXConstructor:{
    return rebuildMember(cast<CXXConstructorDecl>(Member));
  }
  break;
  case clang::Decl::NamespaceAlias:{
    return rebuildMember(cast<NamespaceAliasDecl>(Member));
  }
  break;
  case clang::Decl::VarTemplate:{
    return rebuildMember(cast<VarTemplateDecl>(Member));
  }
  break;
  case clang::Decl::TypeAliasTemplate:{
    return rebuildMember(cast<TypeAliasTemplateDecl>(Member));
  }
  break;
  case clang::Decl::TypeAlias:{
    return rebuildMember(cast<TypeAliasDecl>(Member));
  }
  break;
  case clang::Decl::ClassTemplate:{
    return rebuildMember(cast<ClassTemplateDecl>(Member));
  }
  break;
  case clang::Decl::ClassTemplateSpecialization:
  case clang::Decl::ClassTemplatePartialSpecialization:
    llvm_unreachable("I'm not sure if this can be reached in here or not");
  break;
  case clang::Decl::FunctionTemplate:{
    return rebuildMember(cast<FunctionTemplateDecl>(Member));
  }
  break;
  default:
    Member->dump();
    llvm_unreachable("I thought that this was impossible to occur within a class.");
  }
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXRecordDecl *RD) {
  if (RD->isImplicit())
    // Already implicitly in scope.
    return false;
  llvm_unreachable("Not implemented yet rebuildMember clang::CXXRecordDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::FieldDecl *Field) {
  StateRAII FieldState(*this, UDK_DeductionOnlyVariable, Field);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::VarDecl *StaticMember) {
  StateRAII FieldState(*this, UDK_DeductionOnlyVariable, StaticMember);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXMethodDecl *Method) {
  StateRAII FieldState(*this, UDK_MemberFunction, Method);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXConversionDecl *Conversion) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::CXXConversionDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXDestructorDecl *Dtor) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::CXXDestructorDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXConstructorDecl *Ctor) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::CXXConstructor");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::NamespaceAliasDecl *Ns) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::NamespaceAliasDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::VarTemplateDecl *VTD) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::VarTemplateDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::TypeAliasTemplateDecl *TATD) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::TypeAliasTemplateDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::TypeAliasDecl *TAD) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::TypeAliasDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::ClassTemplateDecl* CTD) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::ClassTemplateDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(
    clang::ClassTemplateSpecializationDecl *Tmplt) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::ClassTemplateSpecializationDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(
    clang::ClassTemplatePartialSpecializationDecl *Tmplt) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::ClassTemplatePartialSpecializationDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::FunctionTemplateDecl* CTD) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember clang::FunctionTemplateDecl");
}

bool
ClangToGoldDeclRebuilder::rebuildOperatorOverload(clang::CXXMethodDecl *Method) {
  dumpScopeStructure(RootScope, RootDecl);
  llvm_unreachable("Not implemented yet rebuildMember rebuildOperatorOverload");
}


}
