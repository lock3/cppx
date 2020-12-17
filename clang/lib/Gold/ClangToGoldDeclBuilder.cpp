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

#include "clang/Sema/Template.h"

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

static const Syntax *buildIdentifierNode(SyntaxContext &Ctx,
                                         llvm::StringRef Text,
                                         clang::SourceLocation Loc) {
  Symbol Sym = getSymbol(Text.str());
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
  Sema::OptionalResumeScopeRAII PreviousChildScope;
  Declaration *PrevParent = nullptr;
  Declaration *PrevDecl = nullptr;
  const Syntax *DummyNode = nullptr;
private:
  void createDecl(UnevaluatedDeclKind SuspectedDeclKind,
                  clang::NamedDecl *NmdDcl, bool AddToParentScope) {
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
    if (AddToParentScope)
      Rebuilder.SemaRef.getCurrentScope()->addDecl(Rebuilder.CurDecl);
  }
public:
  StateRAII(ClangToGoldDeclRebuilder &Builder,
            UnevaluatedDeclKind SuspectedDeclKind, clang::NamedDecl *NmdDcl)
    :Rebuilder(Builder),
    ChildScope(Rebuilder.SemaRef),
    PreviousChildScope(Rebuilder.SemaRef),
    PrevParent(Rebuilder.ParentDecl),
    PrevDecl(Rebuilder.CurDecl)
  {
    createDecl(SuspectedDeclKind, NmdDcl, true);
  }

  StateRAII(ClangToGoldDeclRebuilder &Builder,
            UnevaluatedDeclKind SuspectedDeclKind, clang::NamedDecl *NmdDcl,
            ScopeKind SK, bool SetAsEntity = false, bool SaveScope = true,
            bool AddToParentScope = true)
    :Rebuilder(Builder),
    ChildScope(Rebuilder.SemaRef),
    PreviousChildScope(Rebuilder.SemaRef),
    PrevParent(Rebuilder.ParentDecl),
    PrevDecl(Rebuilder.CurDecl)
  {
    createDecl(SuspectedDeclKind, NmdDcl, AddToParentScope);

    if (SaveScope)
      ChildScope.Init(SK, DummyNode, &Rebuilder.CurDecl->SavedScope);
    else
      ChildScope.Init(SK, DummyNode);

    if (SetAsEntity)
      Rebuilder.SemaRef.getCurrentScope()->Entity = Rebuilder.CurDecl;
  }

  // Resuming an existing declaration
  StateRAII(ClangToGoldDeclRebuilder &Builder, Declaration *Decl)
    :Rebuilder(Builder),
    ChildScope(Rebuilder.SemaRef),
    PreviousChildScope(Rebuilder.SemaRef),
    PrevParent(Decl->ParentDecl),
    PrevDecl(Decl)
  {
    assert(Decl->SavedScope && "Invalid declaration given");
    Rebuilder.CurDecl = Decl;
    Rebuilder.ParentDecl = PrevParent;
    PreviousChildScope.Init(Decl->SavedScope,
                            Decl->SavedScope->getConcreteTerm());
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

  // Resuming the parent scope that the parent declaration was declared in.
  Sema::ResumeScopeRAII ParentScopeRAII(SemaRef, RootScope,
                                        RootScope->getConcreteTerm());
  CurDecl = ParentDecl;
  StateRAII RecordState(*this, ParentDecl->getKind(), RD, SK_Template,
                        false, false, false);
  Sema::ScopeRAII RecordBodyScope(SemaRef, ParentDecl->SavedScope->getKind(),
                                  RootScope->getConcreteTerm(),
                                  &CurDecl->SavedScope);
  SemaRef.getCurrentScope()->Entity = CurDecl;
  RootDecl = CurDecl;

  // Next thing to do is Create additional declarations based off the contents
  // of the body of the actual declaration.
  for(clang::Decl *ChildDecl : ToConvert->decls()) {
    if (rebuildMember(ChildDecl)){
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "invalid class member");
      SemaRef.Diags.Report(ChildDecl->getBeginLoc(), DiagID);
      return nullptr;
    }
  }
  // llvm::outs() << "Complete declaration structure.\n";
  // dumpScopeStructure(RootScope, RootDecl);
  return CurDecl;
}

bool ClangToGoldDeclRebuilder::finishDecl(Declaration *D,
                                          clang::SourceRange Range) {
  // This is technically a completed type?
  if (!D->NeedToBeElaboratedByClangBeforeUse)
    return false;

  if (!D->defines<clang::EnumDecl>()) {
    llvm_unreachable("Not sure how to handle this declaration.");
  }
  clang::TagDecl *TD = dyn_cast<clang::TagDecl>(D->Cxx);
  if (!TD) {
    // FIXME: Create an error message here?
    return true;
  }
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::QualType Type(TD->getTypeForDecl(), 0);
  if (SemaRef.getCxxSema().RequireCompleteType(Loc, Type,
                                   clang::diag::err_incomplete_nested_name_spec,
                                               Range))
    return true;

   // Fixed enum types are complete, but they aren't valid as scopes
   // until we see a definition, so awkwardly pull out this special
   // case.
  auto *EnumD = dyn_cast<clang::EnumDecl>(TD);
  if (!EnumD)
    return false;
  if (EnumD->isCompleteDefinition()) {
    // If we know about the definition but it is not visible, complain.
    clang::NamedDecl *SuggestedDef = nullptr;
    if (!SemaRef.getCxxSema().hasVisibleDefinition(EnumD,
                                                   &SuggestedDef,
                                                   /*OnlyNeedComplete*/false))
    {
      // If the user is going to see an error here, recover by making the
      // definition visible.
      bool TreatAsComplete = !SemaRef.getCxxSema().isSFINAEContext();
      SemaRef.getCxxSema().diagnoseMissingImport(Loc, SuggestedDef,
                                  clang::Sema::MissingImportKind::Definition,
                                                 /*Recover*/TreatAsComplete);
      return !TreatAsComplete;
    }
  } else {
    // Try to instantiate the definition, if this is a specialization of an
    // enumeration temploid.
    if (clang::EnumDecl *Pattern = EnumD->getInstantiatedFromMemberEnum()) {
      clang::MemberSpecializationInfo *MSI = EnumD->getMemberSpecializationInfo();
      if (MSI->getTemplateSpecializationKind() != clang::TSK_ExplicitSpecialization) {
        if (SemaRef.getCxxSema().InstantiateEnum(Loc, EnumD, Pattern,
                        SemaRef.getCxxSema().getTemplateInstantiationArgs(EnumD),
                                                clang::TSK_ImplicitInstantiation))
        {
          return true;
        }
      }
    } else {
      SemaRef.getCxxSema().Diag(Loc,
                                clang::diag::err_incomplete_nested_name_spec)
                                << Type << Range;
      return true;

    }
  }

  // This will let us re-enter the incompleted state of the enum.
  StateRAII DclState(*this, D);
  for (auto Dcl : TD->decls()) {
    if (rebuildMember(Dcl)) {
      return true;
    }
  }
  return false;
}

Declaration *
ClangToGoldDeclRebuilder::generateDeclForDeclContext(clang::DeclContext *DC,
                                                     const Syntax *S)
{
  assert(DC && "Invalid decl context");
  clang::Decl *Dcl = cast<clang::Decl>(DC);
  auto *D = new Declaration(SemaRef.getCurrentDecl(), S, /*Declarator=*/nullptr,
                            /*Init=*/nullptr, UDK_None);
  SemaRef.setDeclForDeclaration(D, Dcl);
  D->CurrentPhase = Phase::Initialization;
  D->ScopeForDecl = SemaRef.getCurrentScope();
  D->ParentDecl = SemaRef.getCurrentDecl();
  return D;
}

Declaration *ClangToGoldDeclRebuilder::generateDeclForNNS(
                             clang::NamespaceDecl *NS, const AtomSyntax *Name) {
  assert(NS && "Invalid namespace");
  assert(Name && "Invalid name");
  auto *D = new Declaration(SemaRef.getCurrentDecl(), Name,
                            /*Declarator=*/nullptr, /*Init=*/nullptr,
                            UDK_Namespace);
  SemaRef.setDeclForDeclaration(D, NS);
  D->CurrentPhase = Phase::Initialization;
  D->Id = NS->getIdentifier();
  D->ScopeForDecl = SemaRef.getCurrentScope();
  D->ParentDecl = SemaRef.getCurrentDecl();
  return D;
}

Declaration *
ClangToGoldDeclRebuilder::rebuildDeclWithNewName(Declaration *Parent,
                                                 llvm::StringRef NewName,
                                                 clang::Decl *Dcl,
                                                 Scope *ScopeForDecl){
  const Syntax *Name = buildIdentifierNode(Context, NewName, Dcl->getLocation());
  auto *D = new Declaration(Parent, Name, /*Declarator=*/nullptr,
                            /*Init=*/nullptr, UDK_OperatorOverload);
  SemaRef.setDeclForDeclaration(D, Dcl);
  D->CurrentPhase = Phase::Initialization;
  D->Id = &Context.CxxAST.Idents.get(NewName);
  D->ScopeForDecl = ScopeForDecl;
  D->ParentDecl = Parent;

  // Recording this declaration inside of it's given scope.
  ScopeForDecl->addDecl(D);
  return D;
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
  case clang::Decl::CXXRecord:
    return rebuildMember(cast<CXXRecordDecl>(Member));
  case clang::Decl::Field:
    return rebuildMember(cast<FieldDecl>(Member));
  case clang::Decl::Var:
    return rebuildMember(cast<VarDecl>(Member));
  case clang::Decl::CXXMethod:
    return rebuildMember(cast<CXXMethodDecl>(Member));
  case clang::Decl::CXXConversion:
    return rebuildMember(cast<CXXConversionDecl>(Member));
  case clang::Decl::CXXDestructor:
    return rebuildMember(cast<CXXDestructorDecl>(Member));
  case clang::Decl::CXXConstructor:
    return rebuildMember(cast<CXXConstructorDecl>(Member));
  case clang::Decl::VarTemplate:
    return rebuildMember(cast<VarTemplateDecl>(Member));
  case clang::Decl::TypeAliasTemplate:
    return rebuildMember(cast<TypeAliasTemplateDecl>(Member));
  case clang::Decl::TypeAlias:
    return rebuildMember(cast<TypeAliasDecl>(Member));
  case clang::Decl::ClassTemplate:
    return rebuildMember(cast<ClassTemplateDecl>(Member));
  case clang::Decl::ClassTemplateSpecialization:
    return rebuildMember(cast<clang::ClassTemplateSpecializationDecl>(Member));
  case clang::Decl::ClassTemplatePartialSpecialization:
    return rebuildMember(
        cast<clang::ClassTemplatePartialSpecializationDecl>(Member));
  case clang::Decl::FunctionTemplate:
    return rebuildMember(cast<FunctionTemplateDecl>(Member));
  case clang::Decl::Enum:
    return rebuildMember(cast<EnumDecl>(Member));
  case clang::Decl::EnumConstant:
    return rebuildMember(cast<EnumConstantDecl>(Member));
  case clang::Decl::Using:
    return rebuildMember(cast<UsingDecl>(Member));

  // Basically these, are not needed for additional lookup, only the principal
  // template is needed for lookup.
  case clang::Decl::VarTemplateSpecialization:
  case clang::Decl::VarTemplatePartialSpecialization:
    return false;

  // Everything is handled by the top level UsingDecl
  case clang::Decl::UsingShadow:
    return false;

  default:
    return true;
  }
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXRecordDecl *RD) {
  if (RD->isImplicit())
    // Already implicitly in scope.
    return false;
  ScopeKind SK = SK_Class;
  UnevaluatedDeclKind UDK = UDK_Class;
  switch (RD->getTagKind()) {
  case clang::TTK_Union:
    UDK = UDK_Union;
    SK = SK_Class;
    break;
  case clang::TTK_Struct:
  case clang::TTK_Class:
    SK = SK_Class;
    UDK = UDK_Class;
    break;
  case clang::TTK_Enum:
    SK = SK_Enum;
    UDK = UDK_Enum;
    break;
  case clang::TTK_Interface:
    llvm_unreachable("Unsupported tag declaration.");
  } // switch (RD->getTagKind())

  StateRAII RecordState(*this, UDK, RD, SK);
  return false;
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
  StateRAII FieldState(*this, UDK_ConversionOperator, Conversion);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXDestructorDecl *Dtor) {
  StateRAII CtorState(*this, UDK_Destructor, Dtor);
  CurDecl->Id = SemaRef.DestructorII;
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::CXXConstructorDecl *Ctor) {
  StateRAII CtorState(*this, UDK_Constructor, Ctor);
  CurDecl->Id = SemaRef.ConstructorII;
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::VarTemplateDecl *VTD) {
  StateRAII CtorState(*this, UDK_VarTemplateDecl, VTD);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::TypeAliasTemplateDecl *TATD) {
  StateRAII CtorState(*this, UDK_VarTemplateDecl, TATD);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::TypeAliasDecl *TAD) {
  StateRAII CtorState(*this, UDK_TypeAlias, TAD);
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::ClassTemplateDecl* CTD) {
  clang::CXXRecordDecl *RD = CTD->getTemplatedDecl();
  ScopeKind SK = SK_Class;
  UnevaluatedDeclKind UDK = UDK_Class;
  switch (RD->getTagKind()) {
  case clang::TTK_Union:
    UDK = UDK_Union;
    SK = SK_Class;
    break;
  case clang::TTK_Struct:
  case clang::TTK_Class:
    SK = SK_Class;
    UDK = UDK_Class;
    break;
  case clang::TTK_Enum:
    SK = SK_Enum;
    UDK = UDK_Enum;
    break;
  case clang::TTK_Interface:
    llvm_unreachable("Unsupported tag declaration.");
  } // switch (RD->getTagKind())

  StateRAII ClsTmpltDclState(*this, UDK, CTD, SK);
  return false;
  // return rebuildMember(CTD->getTemplatedDecl());
}

bool
ClangToGoldDeclRebuilder::rebuildMember(
    clang::ClassTemplateSpecializationDecl *Tmplt) {
  // We handle the specializations differently because they are actually picked up
  // by the clang side of things.
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(
    clang::ClassTemplatePartialSpecializationDecl *Tmplt) {
  return false;
}

bool
ClangToGoldDeclRebuilder::rebuildMember(clang::FunctionTemplateDecl* CTD) {
  return rebuildMember(cast<clang::CXXMethodDecl>(CTD->getTemplatedDecl()));
}

bool ClangToGoldDeclRebuilder::rebuildMember(clang::EnumDecl* ED) {
  StateRAII EnumState(*this, UDK_Enum, ED, SK_Enum, true, true, true);
  CurDecl->NeedToBeElaboratedByClangBeforeUse = true;
  return false;
}

bool ClangToGoldDeclRebuilder::rebuildMember(clang::EnumConstantDecl* ECD) {
  StateRAII EnumState(*this, UDK_EnumConstant, ECD, SK_Enum);
  return false;
}

bool ClangToGoldDeclRebuilder::rebuildMember(clang::UsingDecl *UD) {
  for (auto Shadow : cast<clang::UsingDecl>(UD)->shadows())
    SemaRef.getCurrentScope()->Shadows.insert(Shadow);
  return false;
}

}
