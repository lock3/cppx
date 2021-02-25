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

#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Blue/BlueDeclarator.h"
#include "clang/Basic/Diagnostic.h"

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Sema/CXXFieldCollector.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"

#include "clang/Blue/BlueExprMarker.h"

#include <iostream>

namespace blue {
static clang::Expr *buildIdExpr(Sema &SemaRef, llvm::StringRef Id,
                                clang::SourceLocation Loc);
static auto Error(Sema &SemaRef, clang::SourceLocation Loc) {
  return SemaRef.getCxxSema().Diags.Report(Loc,
                                           clang::diag::err_blue_elaboration);
}
bool decomposeNNS(Sema &SemaRef, llvm::SmallVectorImpl<const Syntax *> &NNSChain,
                  const Syntax *S) {
  if (auto IdS = dyn_cast<IdentifierSyntax>(S)) {
    NNSChain.emplace_back(IdS);
    return false;
  }
  if (auto InfixOp = dyn_cast_or_null<InfixSyntax>(S)) {
    if (decomposeNNS(SemaRef, NNSChain, InfixOp->getOperand(0))) {
      // This indicates there was an error. that wasn't reported.
      return true;
    }
    auto RHS = InfixOp->getOperand(1);
    if (auto RHSId = dyn_cast_or_null<IdentifierSyntax>(RHS)) {
      NNSChain.emplace_back(RHSId);
      return false;
    } else {
      Error(SemaRef, RHS->getLocation())
        << "invalid namespace nested name declaration";
      return true;
    }
  }

  Error(SemaRef, S->getLocation())
    << "invalid namespace nested name declaration";
  return true;
}

Declaration *Elaborator::createDeclaration(const Syntax *Def,
                                           Declarator *Dcl,
                                           const Syntax *Init) {
  if (const DeclarationSyntax *DS = dyn_cast<DeclarationSyntax>(Def)) {
    if (DS->IntroKind == DeclarationSyntax::Namespace)
      return createNamespaceDecl(DS, Dcl, Init);
  } else if (const PrefixSyntax *PS = dyn_cast<PrefixSyntax>(Def)) {
    if (PS->getOperation().hasKind(tok::UsingKeyword))
      return createUsingDecl(PS, Dcl, Init);
    return nullptr;
  }

  Declaration *TheDecl =
    new Declaration(SemaRef.getCurrentDecl(), Def, Dcl, Init);


  if (const DeclarationSyntax *Name = dyn_cast<DeclarationSyntax>(Def)) {
    if (const IdentifierSyntax *Id
        = dyn_cast_or_null<IdentifierSyntax>(Name->getDeclarator())) {
        TheDecl->Id = &SemaRef.getCxxAST().Idents.get({Id->getSpelling()});
    } else {
      // llvm_unreachable("Some how we have an invalid identifier.");
    }
  } else if (const IdentifierSyntax *Id = dyn_cast<IdentifierSyntax>(Def)) {
    TheDecl->Id = &SemaRef.getCxxAST().Idents.get({Id->getSpelling()});
  }

  Scope *CurScope = SemaRef.getCurrentScope();
  CurScope->addDecl(TheDecl);
  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  TheDecl->ScopeForDecl = CurScope;
  TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();

  return TheDecl;
}

template<typename Iterator>
static DeclarationSyntax *buildDummyNNSSyntax(Iterator Start,
                                              Iterator End,
                                              Syntax *TyExpr,
                                              EnclosureSyntax *FinalInit){
  if ((Start + 1) == End) {
    // This means we are at the end.
    return new DeclarationSyntax(
      const_cast<Syntax*>(*Start), TyExpr, nullptr, FinalInit,
                                 DeclarationSyntax::Namespace);
  }
  auto NestedNS = buildDummyNNSSyntax(Start + 1, End, TyExpr, FinalInit);
  Syntax **ListAlloc = new Syntax *[1];
  ListAlloc[0] = NestedNS;
  Syntax *List = new ListSyntax(ListAlloc, 1);
  Syntax *Enc = new EnclosureSyntax(FinalInit->getOpen(), FinalInit->getClose(),
                                    List);
  return new DeclarationSyntax(const_cast<Syntax*>(*Start), TyExpr, nullptr,
                               Enc, DeclarationSyntax::Namespace);
}

Declaration *Elaborator::createNamespaceDecl(const DeclarationSyntax *Def,
                                             Declarator *Dcl,
                                             const Syntax *Init) {

  llvm::SmallVector<const Syntax *, 16> NNSChain;
  // This is a special case when we have a nested name declararation for
  if (auto InfixOp = dyn_cast<InfixSyntax>(Def->getDeclarator())) {
    if (decomposeNNS(SemaRef, NNSChain, InfixOp)) {
      // Building an dummy declaration so that we can return sometthing in the
      // event of an error decomposing the nested namespaced.
      Declaration *TheDecl =
        new Declaration(SemaRef.getCurrentDecl(), Def, Dcl, Init);
      Scope *CurScope = SemaRef.getCurrentScope();
      CurScope->addDecl(TheDecl);
      TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
      TheDecl->ScopeForDecl = CurScope;
      TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
      return TheDecl;
    }
  } else if(auto IdS = dyn_cast<IdentifierSyntax>(Def->getDeclarator())) {
    NNSChain.emplace_back(IdS);
  }
  if (NNSChain.empty()) {
    llvm_unreachable("Anonymous namespace names not implemented yet.");
  }
  EnclosureSyntax *Initializer = const_cast<EnclosureSyntax *>(
                                  cast<EnclosureSyntax>(Init));
  Declaration *TheDecl = nullptr;
  const DeclarationSyntax *ToGetNameFrom = Def;
  if (NNSChain.size() != 1) {
    auto RebuildNS = buildDummyNNSSyntax(NNSChain.begin(), NNSChain.end(),
                                         const_cast<Syntax *>(Def->getType()),
                                         Initializer);
    TheDecl = new Declaration(SemaRef.getCurrentDecl(), Def, Dcl,
                              RebuildNS->getInitializer());
    ToGetNameFrom = RebuildNS;
  } else {
    TheDecl = new Declaration(SemaRef.getCurrentDecl(), Def, Dcl, Initializer);
  }
  if (const IdentifierSyntax *Id
      = dyn_cast_or_null<IdentifierSyntax>(ToGetNameFrom->getDeclarator())) {
      TheDecl->Id = &SemaRef.getCxxAST().Idents.get({Id->getSpelling()});
      llvm::outs() << "Decl id = " << TheDecl->Id->getName() << "\n";
  } else {
    // llvm_unreachable("Some how we have an invalid identifier.");
    llvm::outs()<< "I messed up there's not identifier.!\n";
  }
  Scope *CurScope = SemaRef.getCurrentScope();
  CurScope->addDecl(TheDecl);
  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  TheDecl->ScopeForDecl = CurScope;
  TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
  return TheDecl;
}

Declaration *Elaborator::createUsingDecl(const PrefixSyntax *Def,
                                         Declarator *Dcl,
                                         const Syntax *Init) {
  Declaration *TheDecl =
    new Declaration(SemaRef.getCurrentDecl(), Def, Dcl, Init);
  Scope *CurScope = SemaRef.getCurrentScope();
  CurScope->addDecl(TheDecl);
  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  TheDecl->ScopeForDecl = CurScope;
  TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
  return TheDecl;
}

clang::Decl *Elaborator::elaborateFile(const Syntax *S) {
  if (!S)
    return nullptr;

  // This is missed during initialziation because of the language setting.
  SemaRef.getCxxSema().FieldCollector.reset(new clang::CXXFieldCollector());
  // Setting up clang scopes.
  clang::Scope *Scope = SemaRef.enterClangScope(clang::Scope::DeclScope);
  Scope->setEntity(getCxxContext().getTranslationUnitDecl());
  SemaRef.getCxxSema().ActOnTranslationUnitScope(Scope);
  SemaRef.getCxxSema().Initialize();

  // Count the decls generated by clang::Sema::Initialize().
  clang::TranslationUnitDecl *TU = getCxxContext().getTranslationUnitDecl();
  for (auto D = TU->decls_begin(); D != TU->decls_end(); ++D)
    ++ImplicitSemaDecls;

  Declaration *D = new Declaration(S);
  D->setCxx(SemaRef, TU);

  SemaRef.setTUDecl(D);
  const FileSyntax *Top = cast<FileSyntax>(S);
  Sema::ScopeRAII NamespaceScope(SemaRef, Scope::Namespace, S);
  D->SavedScope = SemaRef.getCurrentScope();
  SemaRef.pushDecl(D);

  assert(isa<SequenceSyntax>(Top->getDeclarations()) && "invalid file");
  const SequenceSyntax *Declarations =
    cast<SequenceSyntax>(Top->getDeclarations());

  for (const Syntax *SS : Declarations->children()) {
      identifyDeclaration(SS);
  }

  for (const Syntax *SS : Declarations->children())
      elaborateDecl(SS);

  for (const Syntax *SS : Declarations->children())
    elaborateDefinition(SS);

  SemaRef.getCxxSema().ActOnEndOfTranslationUnit();
  SemaRef.popDecl();

  return TU;
}

Declaration *Elaborator::buildDeclaration(const DeclarationSyntax *S) {
  Declarator *Dcl = getDeclarator(S->getType());
  return createDeclaration(S, Dcl, S->getInitializer());
}

Declaration *Elaborator::buildDeclaration(const PrefixSyntax *S) {
  Declarator *Dcl = new Declarator(Declarator::Using, S,
                                   getDeclarator(S->getOperand()));
  return createDeclaration(S, Dcl, S->getOperand());
}

Declaration *Elaborator::identifyDeclaration(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Declaration:
    return buildDeclaration(cast<DeclarationSyntax>(S));
  case Syntax::Prefix:
    return buildDeclaration(cast<PrefixSyntax>(S));
  default:
    break;
  }
  return nullptr;
}

clang::Decl *Elaborator::elaborateDecl(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Declaration:
    return elaborateDefDecl(cast<DeclarationSyntax>(S));
  case Syntax::Prefix:
    return elaboratePrefixDecl(cast<PrefixSyntax>(S));
  default:
    break;
  }
  llvm_unreachable("Invalid declaration?\n");
  // TODO: Remove this eventaully, when we are not doing elaboration.
  // This will allow us to pretend to evaluate expressions inside the global
  // scope of a probram.
  return nullptr;
  // llvm_unreachable("invalid declaration");
}


clang::Decl *Elaborator::elaborateDefDecl(const DeclarationSyntax *S) {
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    return nullptr;

  return elaborateDeclarationTyping(D);
}

static clang::Decl *handleUsing(Sema &SemaRef,
                                const Syntax *Arg,
                                clang::SourceLocation UsingLoc) {
  Sema::ExtendQualifiedLookupRAII ExQual(SemaRef);
  clang::SourceLocation ArgLoc = Arg->getLocation();
  clang::DiagnosticsEngine &Diags = SemaRef.getCxxSema().Diags;
  clang::Expr *E = Elaborator(SemaRef).elaborateExpression(Arg);
  if (!E)
    return nullptr;

  clang::Scope *CxxScope = SemaRef.getCurClangScope();
  clang::CXXScopeSpec SS;
  clang::ParsedAttributesView AttrView;
  clang::UnqualifiedId Name;
  clang::SourceLocation TypenameLoc;
  clang::AccessSpecifier AS = SemaRef.scopeIsClass() ?
    clang::AS_public : clang::AS_none;

  if (clang::CppxDeclRefExpr *CDRE = dyn_cast<clang::CppxDeclRefExpr>(E)) {
    // using namespace declaration
    if (CDRE->getType()->isNamespaceType()) {
      if (SemaRef.scopeIsClass()) {
        Diags.Report(Arg->getLocation(),
                     clang::diag::err_using_namespace_in_class);
        return nullptr;
      }

      clang::CppxNamespaceDecl *NS =
        cast<clang::CppxNamespaceDecl>(CDRE->getValue());

      clang::Decl *UD = SemaRef.getCxxSema().ActOnUsingDirective(
        CxxScope, UsingLoc, Arg->getLocation(), SS, Arg->getLocation(),
        NS->getIdentifier(), AttrView);
      if (!UD)
        return nullptr;

      SemaRef.getCurrentScope()->UsingDirectives.insert(
        cast<clang::UsingDirectiveDecl>(UD));
      return UD;
    }
  } else if (clang::DeclRefExpr *DRE = dyn_cast<clang::DeclRefExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    blue::Declaration *D = SemaRef.getDeclaration(DRE->getDecl());
    Name.setIdentifier(D->Id, D->getEndOfDecl());
    Name.StartLocation = Name.EndLocation = Arg->getLocation();
  } else if (auto *ULE = dyn_cast<clang::UnresolvedLookupExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    Name.setIdentifier(ULE->getName().getAsIdentifierInfo(),
                       ULE->getNameLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLocation();
  } else if (auto *UME = dyn_cast<clang::UnresolvedMemberExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    Name.setIdentifier(UME->getName().getAsIdentifierInfo(),
                       UME->getNameLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLocation();
  } else if (auto *TyLit = dyn_cast<clang::CppxTypeLiteral>(E)) {
    clang::TypeSourceInfo *TInfo = TyLit->getValue();
    if (!TInfo)
      return nullptr;
    Name.setIdentifier(TInfo->getType().getBaseTypeIdentifier(),
                       TyLit->getExprLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLocation();

    if (TInfo->getType()->isDependentType())
      TypenameLoc = Arg->getLocation();
  } else if (auto *DME = dyn_cast<clang::CppxDependentMemberAccessExpr>(E)) {
    Name.setIdentifier(DME->getMemberNameInfo().getName().getAsIdentifierInfo(),
                       DME->getExprLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLocation();
  } else if (auto *WE = dyn_cast<clang::CppxWildcardExpr>(E)) {
    if (SemaRef.scopeIsClass()) {
      Diags.Report(Arg->getLocation(),
                   clang::diag::err_using_namespace_in_class);
      return nullptr;
    }


    auto IdInfo =
      SemaRef.getLookupScopeName();
    // We have `using ._;`, we can't use the global scope.
    if (IdInfo.first) {
      unsigned DiagID =
        Diags.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                              "importing global scope has no effect");
      Diags.Report(ArgLoc, DiagID);
      return nullptr;
    }

    if (!IdInfo.second) {
      unsigned DiagID =
        Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                              "name does not name a namespace");
      Diags.Report(ArgLoc, DiagID);
      return nullptr;
    }

    clang::Decl *UD = SemaRef.getCxxSema().ActOnUsingDirective(
      CxxScope, UsingLoc, Arg->getLocation(), SS, Arg->getLocation(),
      IdInfo.second, AttrView);
    if (!UD)
      return nullptr;

    SemaRef.getCurrentScope()->UsingDirectives.insert(
      cast<clang::UsingDirectiveDecl>(UD));
    return UD;
  } else {
    unsigned DiagID =
      Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "invalid using declaration argument");
    Diags.Report(ArgLoc, DiagID);
    return nullptr;
  }

  clang::Decl *D = SemaRef.getCxxSema().ActOnUsingDeclaration(
    CxxScope, AS, UsingLoc, TypenameLoc,
    SemaRef.CurNNSContext, Name, clang::SourceLocation(), AttrView);
  if (!D)
    return nullptr;
  // FIXME: if this comes from an operator'.', elaborate lhs to
  // differentiate classes and namespaces.
  if (clang::UsingDecl *UD = dyn_cast<clang::UsingDecl>(D)) {
    for (auto Shadow : cast<clang::UsingDecl>(UD)->shadows())
      SemaRef.getCurrentScope()->Shadows.insert(Shadow);
  }

  return D;
}

clang::Decl *Elaborator::elaboratePrefixDecl(const PrefixSyntax *S) {
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (S->getOperation().hasKind(tok::UsingKeyword)) {
    clang::Decl *Ret = handleUsing(SemaRef, S->getOperand(),
                                   S->getOperation().getLocation());
    D->CurrentPhase = Phase::Initialization;
    return Ret;
  }

  // Reaching this point could only be an error on our part.
  llvm_unreachable("invalid prefix declaration");
}

clang::Decl *Elaborator::elaborateDeclarationTyping(Declaration *D) {
  if (phaseOf(D) >= Phase::Typing)
    return D->getCxx();

  // This are optionally created for template parameters.
  OptionalScopeRAII TemplateParamScope(SemaRef);
  OptioanlClangScopeRAII ClangTemplateScope(SemaRef);

  // Handling template declarations.
  if (D->Decl->declaresTemplate())
    elaborateTemplateParameters(TemplateParamScope, ClangTemplateScope, D,
                                D->Decl);

  return doElaborateDeclarationTyping(D);
}


void Elaborator::elaborateTemplateParameters(OptionalScopeRAII &TemplateScope,
                                             OptioanlClangScopeRAII &ClangTemplateScope,
                                             Declaration *D, Declarator *Dcl) {

  clang::TemplateParameterList *ParamList = nullptr;

  // Initializing the scopes we have to deal with.
  TemplateScope.Init(Scope::Template, Dcl->getInfo(),
                   &Dcl->DeclInfo.ParamScope);
  ClangTemplateScope.Init(clang::Scope::TemplateParamScope, Dcl->getLocation());

  // Constructing actual parameters.
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  // if (!TPD->isImplicitlyEmpty()) {
    // Elaborator El(SemaRef.getContext(), SemaRef);
  auto TS = cast<TemplateSyntax>(Dcl->getInfo());
  auto Enc = dyn_cast<EnclosureSyntax>(TS->getParameters());
  if (!Enc)
    llvm_unreachable("Invalid format for templates declarations.");
  if (!Enc->getOperand())
    llvm_unreachable("Invalid format for template declarations.");
  auto List = dyn_cast<ListSyntax>(Enc->getOperand());
  if (!List) {
    llvm_unreachable("invalid template syntax.");
  }
  buildTemplateParams(List, TemplateParamDecls);
  // }

  ParamList = getCxxSema().ActOnTemplateParameterList(
                               /*unsigned Depth*/SemaRef.computeTemplateDepth(),
                                           /*ExportLoc*/clang::SourceLocation(),
                                              /*TemplateLoc*/Dcl->getLocation(),
                                                /*LAngleLoc*/Dcl->getLocation(),
                                                             TemplateParamDecls,
                                                /*RAngleLoc*/Dcl->getLocation(),
                                                     /*RequiresClause*/nullptr);
  Dcl->ClangParamList = ParamList;

  // Recording template parameters for use during declaration construction.
  D->TemplateParamStorage.push_back(ParamList);
}

void Elaborator::buildTemplateParams(const ListSyntax *Params,
                               llvm::SmallVectorImpl<clang::NamedDecl *> &Res) {
  std::size_t I = 0;
  for (const Syntax *P : Params->children()) {
    // Elaborator Elab(Context, SemaRef);
    Declaration *D = elaborateTemplateParameter(P);
    if (!D)
      continue;
    clang::Decl *CxxDecl = D->getCxx();
    clang::NamedDecl *ND = cast_or_null<clang::NamedDecl>(CxxDecl);
    // Just skip this on error.
    if (!ND)
      continue;

    unsigned Depth = SemaRef.computeTemplateDepth();
    if (auto *TP = dyn_cast<clang::NonTypeTemplateParmDecl>(ND)) {
      TP->setPosition(I);
      TP->setDepth(I);
    } else if (auto *TP = dyn_cast<clang::TemplateTemplateParmDecl>(ND)) {
      TP->setDepth(Depth);
      TP->setPosition(I);
    } else if (auto *TP = dyn_cast<clang::TemplateTypeParmDecl>(ND)) {
      // Set the index.
      auto *Ty = TP->getTypeForDecl()->castAs<clang::TemplateTypeParmType>();
      clang::QualType NewTy = getCxxContext().getTemplateTypeParmType(Depth, I,
                                                          Ty->isParameterPack(),
                                                                 Ty->getDecl());
      TP->setTypeForDecl(NewTy.getTypePtr());
    } else {
      llvm_unreachable("Invalid template parameter");
    }

    // Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    // assert(D && "Didn't find associated declaration");
    Res.push_back(ND);

    ++I;
  }
}


clang::Decl *Elaborator::doElaborateDeclarationTyping(Declaration *D) {
  if (!D)
    return nullptr;
  switch(D->getIntroducerKind()) {
  case DeclarationSyntax::Variable:
    return makeValueDecl(D);
  case DeclarationSyntax::Function:
    return makeFunctionDecl(D);
  case DeclarationSyntax::Type:
    if (D->declaratorContainsClass())
      return makeClass(D);
    else
      if (D->Decl->declaresTemplate())
        return elaborateTypeAliasOrVariableTemplate(D);
    return makeValueDecl(D);
  case DeclarationSyntax::Super:
    Error(D->asDef()->getErrorLocation(), "invalid base class declaration");
    return nullptr;
  case DeclarationSyntax::Namespace:
    return makeNamespace(D);
  case DeclarationSyntax::Unknown:
  default:
    // TODO: This may need to be specially refactored so that we can
    // identify when something is a parameter.
    return makeValueDecl(D);
  }
}


clang::Decl *Elaborator::elaborateTypeAliasOrVariableTemplate(Declaration *D) {
  bool InClass = D->isDeclaredInClass();

  // Checking if we are a nested template decl/class.
  clang::MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  // This REQUIRES that we have specified type for now. But in order to do this
  // correctly we can't construct a templated type right off the bat we need
  // to figure out
  // ExprElaborator Elab(Context, SemaRef);
  Declarator *TyDcl = D->Decl->Next;
  if (!TyDcl) {
    Error(D->Def->getLocation(),"Deduced alias types not supported.");
    return nullptr;
  }

  // Elaborating what I suspect will be a type portion of the declarator.
  clang::Expr *TypeExpr = elaborateDeclarator(TyDcl);
  if (!TypeExpr) {
    getCxxSema().Diags.Report(TyDcl->getLocation(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  if (!TypeExpr->getType()->isTypeOfTypes()) {
    if (TypeExpr->getType()->isNamespaceType()) {
      getCxxSema().Diags.Report(TyDcl->getLocation(),
                           clang::diag::err_templated_namespace_type);
      return nullptr;
    }
    if (TypeExpr->getType()->isTemplateType()) {
      llvm_unreachable("Template variables not implemented yet");
    }
    getCxxSema().Diags.Report(TyDcl->getLocation(),
                         clang::diag::err_declaration_type_not_a_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TypeExprInfo
               = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                   TyDcl->getLocation());
  if (!TypeExprInfo)
    return nullptr;

  // Constructing the elaboration name.
  clang::IdentifierInfo *IdInfo = D->Id;
  clang::UnqualifiedId Id;
  // assert(D->IdDcl && "We are some how missing an identifier declarator?!\n");
  Id.setIdentifier(IdInfo, D->Def->getLocation());
  clang::SourceLocation Loc = D->Def->getLocation();

  auto Init = D->getInitializer();
  if (!TypeExprInfo->getType()->isTypeOfTypes()) {
    // bool DeclIsStatic = false;
    // if (isStaticMember(SemaRef, D, DeclIsStatic)) {
    //   return nullptr;
    // }

    // Emit an error message here.
    // if (InClass && !DeclIsStatic) {
    //   SemaRef.Diags.Report(D->IdDcl->getLoc(),
    //                        clang::diag::err_template_member)
    //                        << D->getId()->getName();
    //   return nullptr;
    // }
    clang::StorageClass TS = clang::SC_None;
    // if (DeclIsStatic) {
    //   TS = clang::SC_Static;
    // }
    clang::VarDecl *VDecl = clang::VarDecl::Create(getCxxContext(),
                                               SemaRef.getCurClangDeclContext(),
                                                   Loc, Loc, IdInfo,
                                                   TypeExprInfo->getType(),
                                                   TypeExprInfo, TS);
    VDecl->setImplicitlyInline();
    clang::DeclarationName DeclName = IdInfo;
    clang::VarTemplateDecl *VTD = clang::VarTemplateDecl::Create(
                                                      getCxxContext(),
                                                      VDecl->getDeclContext(),
                                                      Loc, DeclName, MTP.back(),
                                                      VDecl);
    if (InClass) {
      VTD->setAccess(clang::AS_public);
      VDecl->setAccess(clang::AS_public);
    }
    SemaRef.getCxxSema().PushOnScopeChains(VTD, SemaRef.getCurClangScope(),
                                           true);
    D->CurrentPhase = Phase::Typing;
    // SemaRef.setDeclForDeclaration(D, VTD);
    D->setCxx(SemaRef, VTD);
  } else {
    if (!Init) {
      getCxxSema().Diags.Report(Loc, clang::diag::err_templated_namespace_type);
      return nullptr;
    }

    // Attempting to elaborate the type expression.
    clang::Expr *InitTyExpr = elaborateExpression(Init);
    if (!InitTyExpr)
      return nullptr;
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InitTyExpr,
                                                           Init->getLocation());
    if (!TInfo)
      return nullptr;
    clang::ParsedType PT;
    PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
    clang::TypeResult TR(PT);
    clang::Decl *TypeAlias = SemaRef.getCxxSema().ActOnAliasDeclaration(
        SemaRef.getCurClangScope(), clang::AS_public, MTP, Loc, Id,
        clang::ParsedAttributesView(), TR, nullptr);

    // SemaRef.setDeclForDeclaration(D, TypeAlias);
    D->setCxx(SemaRef, TypeAlias);
    // Only the type alias is fully elaborated at this point in time.
    // if (InClass) {
    //   D->Cxx->setAccess(clang::AS_public);
    // }
    D->CurrentPhase = Phase::Initialization;
  }

  // Making sure that if we are in side of a class/record we explicitly set the
  // current access to public.
  // if (D->Cxx) {
  //   elaborateAttributes(D);
  // }
  return D->getCxx();
}

Declaration *Elaborator::elaborateTemplateParameter(const Syntax *Parm) {
  Declaration *D = identifyDeclaration(Parm);
  if (!D) {
    // TODO: Create an error message for this.
    llvm_unreachable("Invalid parameter");
  }
  // clang::DeclContext *Owner = D->getOwningDeclContext();
//   if (isa<clang::LinkageSpecDecl>(Owner)) {
//     SemaRef.Diags.Report(D->Op->getLoc(),
//                          clang::diag::err_invalid_extern_c)
//                          << /*a template parameter*/2;
//     return nullptr;
//   }

//   ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TyExpr = elaborateDeclarator(D->Decl);
  if (!TyExpr){
    getCxxSema().Diags.Report(D->Decl->getLocation(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  bool IsPack = false;

//   // Checking to see if we are a parameter pack
//   // This technically doesn't have a spot in the AST only as a boolean
//   // associated with template parameters.
//   if (auto Call = dyn_cast<CallSyntax>(TySyntax)) {
//     if (auto AtomName = dyn_cast<AtomSyntax>(Call->getCallee())) {
//       if (AtomName->hasToken(tok::Ellipsis)) {
//         assert(Call->getNumArguments() == 1
//                && "Invalid number of arguments to ellipsis within AST");
//         TySyntax = Call->getArgument(0);
//         IsPack = true;
//         D->EllipsisLoc = AtomName->getLoc();
//       }
//     }
//   }

  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TyExpr,
                                                        D->Decl->getLocation());
  if (!TInfo)
    return nullptr;
  clang::DeclContext *Owner = D->DeclaringContext;
  clang::IdentifierInfo *Id = D->Id;
  clang::SourceLocation Loc = D->Def->getLocation();

  // This is a template type or template template parameter decl.
  if (TInfo->getType()->getAs<clang::CppxKindType>()) {
    using TemplateTemplate = clang::TemplateTemplateParmDecl;
    using TemplateType = clang::TemplateTypeParmDecl;
    clang::Decl *ReturnedDecl = nullptr;
    if (D->Decl->declaresTemplate())
      ReturnedDecl = TemplateTemplate::Create(getCxxContext(), Owner, Loc, 0,
                                        0, /*Pack=*/IsPack, Id,
                                        D->TemplateParamStorage.front());
    else
      ReturnedDecl = TemplateType::Create(getCxxContext(), Owner, Loc, Loc, 0, 0,
                                    Id, /*TypenameKW=*/true, /*Pack=*/IsPack);

    D->CurrentPhase = Phase::Typing;
    D->setCxx(SemaRef, ReturnedDecl);
    return D;
  }

  // The depth and position of the parameter will be set later.
  auto *NTTP =
    clang::NonTypeTemplateParmDecl::Create(getCxxContext(), Owner, Loc, Loc,
                                           0, 0, Id, TInfo->getType(),
                                           /*Pack=*/IsPack, TInfo);
  D->setCxx(SemaRef, NTTP);
  D->CurrentPhase = Phase::Typing;
  return D;
}

void Elaborator::elaborateParameters(const ListSyntax *S) {
  return elaborateParameterList(S);
}

void Elaborator::getParameters(Declaration *D,
                               Declarator *FuncDeclarator,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  const auto *Enc = dyn_cast<EnclosureSyntax>(FuncDeclarator->getInfo());
  if (!Enc)
    return;
  if (!Enc->getTerm())
    return;
  const ListSyntax *ParamList = dyn_cast<ListSyntax>(Enc->getTerm());
  if (!ParamList)
    return;

  // FIXME: implement me
  // if (ParamList->isSemicolonSeparated())
  //   return;

  Scope *S = FuncDeclarator->DeclInfo.ParamScope;
  for (const Syntax *SS : ParamList->children()) {
    Declaration *Param = S->findDecl(SS);
    // FIXME: elaborate the entire function type if this happens?
    if (!Param)
      continue;
    assert(isa<clang::ParmVarDecl>(Param->getCxx()));
    Params.push_back(cast<clang::ParmVarDecl>(Param->getCxx()));
  }
}

void Elaborator::elaborateParameterList(const ListSyntax *S) {
  if (!S)
    return;
  for (const Syntax *SS : S->children()) {
    // I may need to make sure that we are not inside of a method declaration.
    // Make sure to skip the this keyword
    if (auto PD = dyn_cast<DeclarationSyntax>(SS)) {
      if (PD->declaratorIsThis())
        continue;
      elaborateParameter(SS);
    } else {
      Error(SS->getLocation(), "Invalid parameter, not a declaration.");
      assert(false);
    }
  }

  // FIXME: Examine the parameters we just created. We might be able
  // to back-propagate types to some of them. For example, if we have;
  //
  //    a, b : int
  //
  // Then this should be equivalent to a : int, b: int.
}

clang::Decl *Elaborator::elaborateParameter(const Syntax *S, bool CtrlParam) {
  if (!isa<DeclarationSyntax>(S) && !isa<IdentifierSyntax>(S)) {
    Error(S->getLocation(), "invalid parameter syntax");
    return nullptr;
  }

  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::SourceLocation Loc = S->getLocation();

  if (const IdentifierSyntax *Id = dyn_cast<IdentifierSyntax>(S)) {
    if (CtrlParam) {
      unsigned DiagID =
        CxxSema.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "non-function parameter requires type");
      CxxSema.Diags.Report(S->getLocation(), DiagID);
      return nullptr;
    }

    // FIXME: create context for auto parameters to keep track of their
    // index and depth.
    Declaration *TheDecl = createDeclaration(S, nullptr, nullptr);

    clang::IdentifierInfo *II = &CxxAST.Idents.get({Id->getSpelling()});
    clang::IdentifierInfo *TypeName =
      CxxSema.InventAbbreviatedTemplateParameterTypeName(II, TempCtx.Index);

    using clang::TemplateTypeParmDecl;
    TemplateTypeParmDecl *TheType =
      TemplateTypeParmDecl::Create(CxxAST, CxxAST.getTranslationUnitDecl(),
                                   clang::SourceLocation(),
                                   Id->getLocation(), TempCtx.Depth,
                                   TempCtx.Index, TypeName, /*Typename=*/false,
                                   /*ParameterPack=*/false);
    TheType->setImplicit();
    ++TempCtx.Index;

    clang::CppxTypeLiteral *TyLit =
      SemaRef.buildTypeExpr(clang::QualType(TheType->getTypeForDecl(), 0),
                            Id->getLocation());
    clang::DeclarationName Name(II);
    clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(TyLit)->getValue();
    clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
    clang::ParmVarDecl *PVD =
      clang::ParmVarDecl::Create(CxxAST, Owner, Loc, Loc,
                                 Name, T->getType(), T,
                                 clang::SC_Auto, /*def=*/nullptr);
    TheDecl->setCxx(SemaRef, PVD);
    return PVD;
  }

  // FIXME: There is a lot of duplication with makeObjectDecl here.
  // In Gold it's just one function.
  const auto *Def = cast<DeclarationSyntax>(S);
  Declarator *Dcl = getDeclarator(Def->getType());
  clang::Expr *Ty = elaborateDeclarator(Dcl);
  if (!Ty)
    return nullptr;

  // FIXME: This needs to be refactored so it's created during phase identification.
  // Create the Blue Declaration
  Declaration *TheDecl = createDeclaration(Def, Dcl, Def->getInitializer());

  // Create the Clang Decl Node
  clang::IdentifierInfo *Id = TheDecl->Id;
  clang::DeclarationName Name(Id);

  if(!Ty->getType()->isTypeOfTypes()) {
    Error(Ty->getExprLoc(), "expected type");
    return nullptr;
  }

  clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(Ty)->getValue();
  Token Spec = Def->getParamPassingSpecifier();
  if (Spec) {
    switch (Spec.getKind()) {
    case tok::OutKeyword:
      T = SemaRef.buildTypeExpr(
        getCxxContext().getLValueReferenceType(
          T->getType()
        ),
        T->getTypeLoc().getBeginLoc()
      )->getValue();
      break;
    case tok::InoutKeyword:
      T = SemaRef.buildTypeExpr(
        getCxxContext().getLValueReferenceType(
          T->getType()
        ),
        T->getTypeLoc().getBeginLoc()
      )->getValue();
      break;
    case tok::MoveKeyword:
      T = SemaRef.buildTypeExpr(
        getCxxContext().getRValueReferenceType(
          T->getType()
        ),
        T->getTypeLoc().getBeginLoc()
      )->getValue();
      break;
    case tok::ForwardKeyword:
      llvm_unreachable("Not sure about 'forward' .");
      break;
    case tok::InKeyword:
      // This is const ref.
      T = SemaRef.buildTypeExpr(
        getCxxContext().getLValueReferenceType(
          getCxxContext().getConstType(
            T->getType()
          )
        ),
        T->getTypeLoc().getBeginLoc()
      )->getValue();
      break;
    default:
      llvm_unreachable("Invalid/uknown parameter specifier");
    }
  }

  // Create the parameters in the translation unit decl for now, we'll
  // move them into the function later.
  // FIXME: replace this with TU
  clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
  clang::Decl *Final = nullptr;

  if (!CtrlParam)
    Final = clang::ParmVarDecl::Create(CxxAST, Owner, Loc, Loc, Name,
                                       T->getType(), T, clang::SC_Auto,
                                       /*def=*/nullptr);
  else
    Final = clang::VarDecl::Create(CxxAST, Owner, Loc, Loc, Name, T->getType(),
                                   T, clang::SC_None);

  TheDecl->setCxx(SemaRef, Final);
  return Final;
}


// Declarator construction

// A declarator is a sequence of unary (pointer) and binary (application)
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
  if (!S)
    return getImplicitAutoDeclarator();

  if (const FunctionSyntax *Fn = dyn_cast<FunctionSyntax>(S)) {

    // TODO: We may need to handle the multi-return slightly different then
    // a typical return statement, not sure yet.
    return new Declarator(Declarator::Function, Fn->getParameters(),
                          getDeclarator(Fn->getResult()));

  }

  if (const EnclosureSyntax *E = dyn_cast<EnclosureSyntax>(S)) {
    if (E->isParenEnclosure()) {
      return new Declarator(Declarator::Function, S,
                            getImplicitAutoDeclarator());
    } else if (E->isBracketEnclosure()) {
      llvm_unreachable("unhandled bracked enclosure");
    } else {
      // A brace enclosed list could appear at the end of a function-type,
      // but probably not on its own.
      Error(S->getLocation(), "brace-enclosed list is not a type");
      return nullptr;
    }
  }
  if (auto AS = dyn_cast<ArraySyntax>(S))
    return getArrayDeclarator(AS);
  if (auto PS = dyn_cast<PrefixSyntax>(S)) {
    if (PS->getOperation().hasKind(tok::Caret))
      return getPointerDeclarator(PS);
  }
  if (auto TS = dyn_cast<TemplateSyntax>(S))
      return getTemplateDeclarator(TS);

  return getLeafDeclarator(S);
}


// Declarator *Elaborator::getUnaryDeclarator(const UnarySyntax *S) {
//   // if (S->getOperator().hasKind(tok::Caret)) {
//   //   Declarator *Dcl = getDeclarator(S->getOperand());
//   //   return new Declarator(Declarator::Pointer, S, Dcl);
//   // }

//   // Error(S->getLocation(), "invalid operator in declarator");
//   return nullptr;
// }

// Declarator *Elaborator::getBinaryDeclarator(const BinarySyntax *S) {
  // if (S->isApplication()) {
  //   if (!S->getLeftOperand() || isa<ErrorSyntax>(S->getLeftOperand())) {
  //     Error(S->getLocation(), "invalid declaration");
  //     return nullptr;
  //   }

  //   if (const BinarySyntax *L = dyn_cast<BinarySyntax>(S->getLeftOperand())) {
  //     // auto *Dcl = new Declarator(Declarator::Array, L->getLeftOperand(),
  //     //                             getBinaryDeclarator(L));
  //     Declarator *Dcl = getBinaryDeclarator(L);
  //     Declarator *Last = Dcl;
  //     while (Last->getNext())
  //       Last = Last->getNext();

  //     Last->Next = getDeclarator(S->getRightOperand());
  //     return Dcl;
  //   }

  //   if (const ListSyntax *L = dyn_cast<ListSyntax>(S->getLeftOperand())) {
  //     // if (!isa<ListSyntax>(S->getRightOperand()))
  //     //   return new Declarator(Declarator::Array, S)

  //     if (L->isBracketList())
  //       return new Declarator(Declarator::Array, S->getLeftOperand(),
  //                             getDeclarator(S->getRightOperand()));
  //   }

  //   return new Declarator(Declarator::Type, S);
  // }

  // if (S->getOperator().hasKind(tok::MinusGreater)) {
  //   // TODO: Rewrite this so that it works as we'd expect it to.
  //   // At the time of writing this the parsing was handled incorrectly and the
  //   // template binary operator was ending up as part of the function binary operator
  //   // rather then as it's parent.
  //   /*
  // // Currently parsed as (before rebuilding the parser).
  // Top 0x7ffff220ef70
  // `-Def 0x7ffff220ef20 foo
  //   |-Binary 0x7ffff220ee90 ->
  //   | |-Binary 0x7ffff220ee60 =>
  //   | | |-List 0x7ffff220ed80
  //   | | | `-Def 0x7ffff220e9d0 T
  //   | | |   |-Literal 0x7ffff224bf50 type
  //   | | |   `-<<<NULL>>>
  //   | | `-List 0x7ffff220ee10
  //   | |   `-Identifier 0x7ffff220edd0 x
  //   | `-Literal 0x7ffff220ed20 void
  //   `-Seq 0x7ffff220eec0
  //   */
  //   if (isa<ListSyntax>(S->getLeftOperand())) {
  //     Declarator *Ret = getDeclarator(S->getRightOperand());
  //     return new Declarator(Declarator::Function, S->getLeftOperand(), Ret);
  //   }

  //   // This is where the special function template declaration is handled.
  //   if (auto Templ = dyn_cast<BinarySyntax>(S->getLeftOperand())) {
  //     if (Templ->getOperator().hasKind(tok::EqualGreater)) {
  //       Declarator *TyDcl = getDeclarator(S->getRightOperand());
  //       Declarator *FnDcl = new Declarator(Declarator::Function,
  //                                         Templ->getRightOperand(),
  //                                         TyDcl);
  //       return new Declarator(Declarator::Template, Templ->getLeftOperand(),
  //                             FnDcl);
  //     }
  //   }
  //   llvm_unreachable("Unknown function syntax.");
  // }
  // // Qualified type name using . in expression.
  // if (S->getOperator().hasKind(tok::Dot))
  //   return new Declarator(Declarator::Type, S);

  // // This is to support class templates.
  // if (S->getOperator().hasKind(tok::EqualGreater)) {
  //   Declarator *TyDcl = getDeclarator(S->getRightOperand());
  //   return new Declarator(Declarator::Template, S->getLeftOperand(), TyDcl);
  // }

  // // TODO: We could support binary type composition (e.g., T1 * T2) as
  // // an alternative spelling of product types. However, this most likely
  // // needs to be wrapped in parens, so it should end up as a leaf. Maybe
  // // this is a non-issue.
  // Error(S->getLocation(), "invalid operator in declarator");
//   return nullptr;
// }


Declarator *Elaborator::getArrayDeclarator(const ArraySyntax *AS) {
  if (!AS->getResult()){
    AS->dump();
    llvm_unreachable("Invalid AST structure");
  }
  return new Declarator(Declarator::Array, AS, getDeclarator(AS->getResult()));
}

Declarator *Elaborator::getPointerDeclarator(const PrefixSyntax *PS) {
  assert(PS->getOperation().hasKind(tok::Caret) && "Invalid pointer declarator.");
  return new Declarator(Declarator::Pointer, PS, getDeclarator(PS->getOperand()));
}

Declarator *Elaborator::getTemplateDeclarator(const TemplateSyntax *TS) {
  return new Declarator(Declarator::Template, TS,
                        getDeclarator(TS->getResult()));
}

Declarator *Elaborator::getLeafDeclarator(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Literal: {
    auto Lit = dyn_cast<LiteralSyntax>(S);
    if (Lit->getToken().hasKind(tok::ClassKeyword)) {
      return new Declarator(Declarator::Class, S);
    }
  }

  LLVM_FALLTHROUGH;
  case Syntax::Identifier:
    return new Declarator(Declarator::Type, S);
  case Syntax::Infix:
  case Syntax::Call:
    return new Declarator(Declarator::Type, S);
  default:
    break;
  }
  llvm::errs() << "Dumping unrecognized syntax.\n";
  S->dump();
  llvm_unreachable("Invalid type expression");
}

Declarator *Elaborator::getImplicitAutoDeclarator() {
  return new Declarator(Declarator::ImplicitType, nullptr);
}


clang::Decl *Elaborator::elaborateDeclEarly(Declaration *D) {
  auto *Ret = elaborateDeclarationTyping(D);
  if (SemaRef.DeepElaborationMode)
    elaborateDefinitionInitialization(D);
  return Ret;
}

// Declaration construction

clang::Decl *Elaborator::makeValueDecl(Declaration *D) {
  // Elaborate the declarator.
  if (D->declaratorContainsFunction()) {
    Error(D->getErrorLocation(), "Function declaration missing introducer.");
    return nullptr;
  }

  // FIXME: An ill-typed declaration isn't the end of the world. Can we
  // poison the declaration and move on?
  clang::Expr *E = elaborateDeclarator(D->Decl);
  if (!E)
    return nullptr;

  Sema::DeclarationElaborationRAII DeclElab(SemaRef, D);
  clang::QualType T;
  if (E->getType()->isTypeOfTypes())
    T = cast<clang::CppxTypeLiteral>(E)->getValue()->getType();

  Sema::DeepElaborationModeRAII ElabMode(SemaRef, false);
  if (T->isUndeducedType()) {
    ElabMode.setMode(true);
    // Doing a quick check to see if the RHS is a type expression.
    if (D->hasInitializer()) {
      auto E = elaborateExpression(D->getInitializer());
      if (!E)
        // TODO: May want an invalid declaration error here.
        return nullptr;

      if (E->getType()->isKindType()) {
        return makeTypeDecl(D, T);
      }
    }
  }
  if (T->isKindType()) {
    ElabMode.setMode(true);
    return makeTypeDecl(D, T);
  } else {
    // Handling field declarations slightly different then variable declarations.
    if (D->declaredWithinClassBody())
      return makeFieldDecl(D, E);
    // Need to check if the declaration is auto because if that's the
    // case we need to force early elaboration instead of letting it go.
    return makeObjectDecl(D, E);
  }
}

static inline clang::StorageClass getDefaultVariableStorageClass(Sema &SemaRef) {
  return SemaRef.getCurrentScope()->isBlockScope() ||
    SemaRef.getCurrentScope()->isControlScope()
    ? clang::SC_Auto
    : clang::SC_None;
}

clang::Decl *Elaborator::makeObjectDecl(Declaration *D, clang::Expr *Ty) {
  auto *Def = cast<DeclarationSyntax>(D->Def);
  if (!Def)
    return nullptr;

  // Create the Clang Decl Node
  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::IdentifierInfo *Id = D->Id;
  clang::DeclarationName Name(Id);
  clang::SourceLocation Loc = Def->getLocation();

  assert(Ty->getType()->isTypeOfTypes() && "type of declaration is not a type");
  clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(Ty)->getValue();

  clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
  clang::VarDecl *VD =
    clang::VarDecl::Create(CxxAST, Owner, Loc, Loc, Id, T->getType(), T,
                           getDefaultVariableStorageClass(SemaRef));
  Owner->addDecl(VD);
  D->setCxx(SemaRef, VD);
  D->CurrentPhase = Phase::Typing;
  // Checking for redeclaration, this will emit an error message if this is a
  // duplicate variable within the same current scope.
  SemaRef.checkForRedeclaration(D);

  if (SemaRef.DeepElaborationMode)
    elaborateDefinitionInitialization(D);
  return VD;
}

clang::Decl *Elaborator::makeTypeDecl(Declaration *D, clang::QualType T) {
  if (!D->hasInitializer()) {
    SemaRef.getCxxSema().Diags.Report(D->Def->getLocation(),
                                      clang::diag::err_expected_type);
    return nullptr;
  }
  const Syntax *Init = D->getInitializer();
  clang::Expr *TyExpr = elaborateExpression(Init);
  if (!TyExpr)
    return nullptr;
  clang::ParsedType PT;
  clang::TypeSourceInfo *TInfo =
    SemaRef.getTypeSourceInfoFromExpr(TyExpr, Init->getLocation());
  if (!TInfo)
    return nullptr;

  PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
  D->CurrentPhase = Phase::Initialization;

  clang::IdentifierInfo *IdInfo = D->Id;
  clang::UnqualifiedId Id;
  Id.setIdentifier(IdInfo, D->Def->getBeginLocation());
  clang::SourceLocation Loc = Init->getLocation();
  clang::MultiTemplateParamsArg MTP;


  // Constructing the type alias on the way out because we need to correctly
  // construct its internal type before continuing.
  clang::TypeResult TR(PT);
  clang::Decl *TypeAlias = SemaRef.getCxxSema().ActOnAliasDeclaration(
      SemaRef.getCurClangScope(), clang::AS_public, MTP, Loc, Id,
      clang::ParsedAttributesView(), TR, nullptr);
  D->setCxx(SemaRef, TypeAlias);
  // SemaRef.addDeclToDecl(TypeAlias, D);
  SemaRef.checkForRedeclaration(D);
  return TypeAlias;
}

clang::CppxTypeLiteral *Elaborator::createFunctionType(Declaration *D,
                                                       Declarator *Dcl) {
  const EnclosureSyntax *ParamTerm = dyn_cast<EnclosureSyntax>(Dcl->getInfo());
  if (!ParamTerm)
    return nullptr;

  const ListSyntax *ParamList = nullptr;
  if (ParamTerm->getTerm()) {
    ParamList = dyn_cast<ListSyntax>(ParamTerm->getTerm());
    if (!ParamList)
      return nullptr;
  }

  clang::SourceLocation Loc = ParamTerm->getOpen().getLocation();
  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  Sema::ScopeRAII ParamScope(SemaRef, Scope::Parameter, ParamTerm);
  Dcl->DeclInfo.ParamScope = SemaRef.getCurrentScope();
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  unsigned N = 0;
  if (!ParamList) {
    N = 0;

  } else {
    N = ParamList->getNumChildren();
    elaborateParameters(ParamList);
    bool hasThisParam = false;
    for (unsigned I = 0; I < N; ++I) {
      const Syntax *P = ParamList->getOperand(I);
      if (auto PD = dyn_cast<DeclarationSyntax>(P)) {
        if (PD->declaratorIsThis()) {
          hasThisParam = true;
          continue;
        }
      }
      Declaration *BluePD = SemaRef.getCurrentScope()->findDecl(P);
      if(!BluePD) {
        continue;
      }
      assert(isa<clang::ParmVarDecl>(BluePD->getCxx()) &&
            "Parameter is not a ParmVarDecl");
      clang::ParmVarDecl *PVD = cast<clang::ParmVarDecl>(BluePD->getCxx());

      CxxAST.setParameterIndex(PVD, I - int(hasThisParam));
      PVD->setScopeInfo(0, I);
      Types.push_back(PVD->getType());
      Params.push_back(PVD);
    }
  }

  // FIXME: We need to configure parts of the prototype (e.g., noexcept).
  clang::FunctionProtoType::ExtProtoInfo EPI;
  clang::QualType ReturnType = CxxAST.getAutoDeductType();
  if (Dcl->getNext()) {
    clang::Expr *RetExpr = elaborateDeclarator(Dcl->getNext());
    if (!RetExpr)
      return nullptr;
    if (!RetExpr->getType()->isTypeOfTypes()) {
      Error(RetExpr->getExprLoc(), "expected type in function return");
      return nullptr;
    }

    clang::CppxTypeLiteral *RetTyLit = cast<clang::CppxTypeLiteral>(RetExpr);
    ReturnType = RetTyLit->getValue()->getType();
  }
  if (ParamList)
    if (ParamList->getNumChildren() != 0) {
      if (auto PossibleThis
          = dyn_cast<DeclarationSyntax>(ParamList->getOperand(0))) {
        if (PossibleThis->declaratorIsThis()) {
          D->FunctionThisParam = PossibleThis;
          for (unsigned I = 0; I < PossibleThis->NumParamSpecs; ++I) {
            D->ThisParamSpecifiers.emplace_back(PossibleThis->ParamSpecs[I]);
          }
        }
      } else if (auto IdThis
                 = dyn_cast<IdentifierSyntax>(ParamList->getOperand(0))) {
        if (IdThis->getToken().getSpelling() == "this") {
          D->HasIdentifierOnlyThis = true;
          D->FunctionThisParam = IdThis;
        }
      }
    }

  clang::QualType FnTy = CxxAST.getFunctionType(ReturnType, Types, EPI);
  return SemaRef.buildFunctionTypeExpr(FnTy, Loc, Loc, Loc,
                                       clang::SourceRange(Loc, Loc),
                                       Loc, Params);
}

static void deduceDependentAutoReturn(Sema &SemaRef,
                                      clang::FunctionDecl *FD) {
  if (FD->getReturnType()->isUndeducedAutoType()) {
    clang::QualType OldTy = FD->getType();
    const clang::FunctionProtoType *FPT =
      OldTy->getAs<clang::FunctionProtoType>();
    clang::ASTContext &CxxAST = SemaRef.getCxxAST();
    clang::QualType NewRet =
      SemaRef.getCxxSema().SubstAutoType(FD->getReturnType(),
                                         CxxAST.DependentTy);
    clang::QualType NewTy =
      CxxAST.getFunctionType(NewRet, FPT->getParamTypes(),
                             FPT->getExtProtoInfo());
    if (OldTy.hasQualifiers())
      NewTy = clang::QualType(NewTy.getTypePtr(),
                              OldTy.getQualifiers().getAsOpaqueValue());
    FD->setType(NewTy);
  }
}

bool Elaborator::buildMethod(Declaration *Fn, clang::DeclarationName const &Name,
                             clang::FunctionDecl **FD, clang::TypeSourceInfo *Ty,
                             clang::CXXRecordDecl *RD) {
  clang::SourceLocation ExLoc = Fn->Def->getLocation();
  clang::SourceLocation FnLoc = ExLoc;
  clang::SourceLocation NameLoc = Fn->Def->getLocation();
  auto *FPT = Ty->getType()->getAs<clang::FunctionProtoType>();

  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(ExLoc);

  // Attempting to apply changes to a function type.
  if (Name.getNameKind() == clang::DeclarationName::CXXConstructorName
      || Name.getNameKind() == clang::DeclarationName::CXXDestructorName) {
    if (FPT->getReturnType()->isUndeducedType()) {
      // double verifying function type.
      auto TypeDcl = Fn->getFirstDeclarator(Declarator::Type);
      if (!TypeDcl) {
        // The we set the default type to void instead because we are a
        // constructor.
        auto ParamTys = FPT->getParamTypes();
        llvm::SmallVector<clang::QualType, 10> ParamTypes(ParamTys.begin(),
                                                          ParamTys.end());
        clang::QualType FnTy = SemaRef.getCxxSema().BuildFunctionType(
          getCxxContext().VoidTy, ParamTypes, FnLoc, clang::DeclarationName(),
          FPT->getExtProtoInfo());
        if (FnTy->isFunctionProtoType()) {
          FPT = FnTy->getAs<clang::FunctionProtoType>();

          // We have to do this to switch to the correct return type for a
          // constructor/destructor
          auto FnTyLoc = Ty->getTypeLoc().getAs<clang::FunctionTypeLoc>();
          auto P = FnTyLoc.getParams();
          clang::SmallVector<clang::ParmVarDecl *, 16> Parms(P.begin(), P.end());
          Ty = gold::BuildFunctionTypeLoc(getCxxContext(), FnTy,
                                          FnTyLoc.getLocalRangeBegin(),
                                          FnTyLoc.getLParenLoc(),
                                          FnTyLoc.getRParenLoc(),
                                          FnTyLoc.getExceptionSpecRange(),
                                          FnTyLoc.getLocalRangeEnd(),
                                          Parms);
        } else {
          getCxxSema().Diags.Report(FnLoc,
                    clang::diag::err_invalid_return_type_for_ctor_or_dtor) << 0;
          return true;
        }
      }
    }

    if (FPT->getReturnType() != getCxxContext().VoidTy) {
      getCxxSema().Diags.Report(FnLoc,
                          clang::diag::err_invalid_return_type_for_ctor_or_dtor)
                          << 0;
      return true;
    }

    clang::ExplicitSpecifier
      ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
    clang::CXXMethodDecl *Method = nullptr;

    if (Name.getNameKind() == clang::DeclarationName::CXXConstructorName)
      *FD = Method =
        clang::CXXConstructorDecl::Create(getCxxContext(), RD, ExLoc, DNI,
                                          Ty->getType(), Ty, ES, false,
                              false, clang::ConstexprSpecKind::CSK_unspecified);
    else if (Name.getNameKind() == clang::DeclarationName::CXXDestructorName)
      *FD = Method =
        clang::CXXDestructorDecl::Create(getCxxContext(), RD, ExLoc, DNI,
                                        Ty->getType(), Ty, false, false,
                                     clang::ConstexprSpecKind::CSK_unspecified);

    Method->setImplicit(false);
    Method->setDefaulted(false);
    Method->setBody(nullptr);

    // Build an exception specification pointing back at this member.
    clang::FunctionProtoType::ExtProtoInfo EPI;
    EPI.ExceptionSpec.Type = clang::EST_None;
    EPI.ExceptionSpec.SourceDecl = Method;

    // Set the calling convention to the default for C++ instance methods.
    EPI.ExtInfo = EPI.ExtInfo.withCallingConv(
      getCxxContext().getDefaultCallingConvention(/*IsVariadic=*/false,
                                                  /*IsCXXMethod=*/true));
    clang::LangAS AS = SemaRef.getCxxSema().getDefaultCXXMethodAddrSpace();
    if (AS != clang::LangAS::Default)
      EPI.TypeQuals.addAddressSpace(AS);

    const clang::FunctionProtoType *FPT
      = cast<clang::FunctionProtoType>(Ty->getType().getTypePtr());
    if (Name.getNameKind() == clang::DeclarationName::CXXDestructorName
        && FPT->getNumParams() != 0) {
      getCxxSema().Diags.Report(ExLoc,
                           clang::diag::err_destructor_with_params);
      return false;
    }

    auto VoidFnTy =
      getCxxContext().getFunctionType(getCxxContext().VoidTy,
              Name.getNameKind() == clang::DeclarationName::CXXConstructorName ?
                                     FPT->getParamTypes() : clang::None,
                                     EPI);
    Method->setType(VoidFnTy);
  } else if (Name.getNameKind()
             == clang::DeclarationName::CXXConversionFunctionName) {
    // TODO: Figure out what this is checking for and what things  I'll have
    // to enfoce instead of sema ref.
    // SemaRef.CheckConversionDeclarator(D, R, SC);
    // if (D.isInvalidType())
    //   return nullptr;
    clang::ExplicitSpecifier
      ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
    // IsVirtualOkay = true;
    const auto *Proto = Ty->getType()->castAs<clang::FunctionProtoType>();
    if (Proto->getNumParams() > 0) {
      getCxxSema().Diags.Report(NameLoc,
                                clang::diag::err_conv_function_with_params);
      return false;
    }
    clang::QualType ConvType = Proto->getReturnType();
    // FIXME: I need to enforce this.
    // C++ [class.conv.fct]p4:
    //   The conversion-type-id shall not represent a function type nor
    //   an array type.
    if (ConvType->isArrayType()) {
      getCxxSema().Diags.Report(NameLoc,
                                clang::diag::err_conv_function_to_array);
      return false;
    } else if (ConvType->isFunctionType()) {
      getCxxSema().Diags.Report(NameLoc,
                                clang::diag::err_conv_function_to_function);
      return false;
    }

    *FD = clang::CXXConversionDecl::Create(getCxxContext(), RD, ExLoc, DNI,
                                          Ty->getType(), Ty,
                                          /*isinline*/false, ES,
                                      clang::ConstexprSpecKind::CSK_unspecified,
                                          ExLoc);
  } else {
    clang::StorageClass SC = clang::SC_None;
    *FD = clang::CXXMethodDecl::Create(getCxxContext(), RD, ExLoc, DNI,
                                       Ty->getType(), Ty,
                                       SC, /*isInline*/true,
                                       clang::ConstexprSpecKind::CSK_unspecified,
                                       ExLoc);
  }

  (*FD)->setAccess(clang::AS_public);
  return false;
}

/// This is only designed to work on normal operator names, this doesn't work
/// on any operator, such as the assignment or construction operator which require
/// looking into the parameters in order to figure out the actual name of
/// the function and if it's a constructor, destructor, or assignment operator.
static bool getBasicOperatorName(Sema &SemaRef, clang::SourceLocation Loc,
                                 llvm::StringRef OpName,
                                 clang::DeclarationName &Name) {
  if (OpName == "==") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_EqualEqual);
    return false;
  }
  if (OpName == "!=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_ExclaimEqual);
    return false;
  }
  if (OpName == "<") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Less);
    return false;

  }
  if (OpName == ">") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Greater);
    return false;
  }
  if (OpName == "<=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_LessEqual);
    return false;
  }
  if (OpName == ">=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_GreaterEqual);
    return false;
  }
  if (OpName == "+") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Plus);
    return false;
  }
  if (OpName == "-") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Minus);
    return false;
  }
  if (OpName == "*") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Star);
    return false;
  }
  if (OpName == "/") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Slash);
    return false;
  }
  if (OpName == "%") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_Percent);
    return false;
  }
  if (OpName == "+=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_PlusEqual);
    return false;
  }
  if (OpName == "-=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_MinusEqual);
    return false;
  }
  if (OpName == "*=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_StarEqual);
    return false;
  }
  if (OpName == "/=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_SlashEqual);
    return false;
  }
  if (OpName == "%=") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_PercentEqual);
    return false;
  }

  if (OpName == "<<") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_LessLess);
    return false;
  }
  if (OpName == ">>") {
    Name = SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
        clang::OO_GreaterGreater);
    return false;
  }
  SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_blue_elaboration)
    << "operator " << OpName << " cannot be overloaded.";
  return true;
}

static clang::DeclarationName getFunctionName(Sema &SemaRef,
                                              Declaration *D,
                                              clang::TypeSourceInfo *FnTInfo,
                                              bool InClass,
                                              const clang::RecordDecl *RD) {
  clang::SourceLocation Loc = D->asDef()->getLocation();
  // This may cause issues if someone uses this without being inside
  // of a class.
  auto IdAtom = dyn_cast<AtomSyntax>(D->asDef()->getDeclarator());
  if (!IdAtom)
    llvm_unreachable("lambda not implemented yet.");

  if (IdAtom->getToken().isFused()) {
    // auto FPT = cast<clang::FunctionProtoType>(
    //     FnTInfo->getType().getTypePtr());
    auto Dcl = D->getFirstDeclarator(Declarator::Function);

    // Figuring out operator actual name.
    llvm::StringRef OpName (IdAtom->getToken().getCStrPtr());
    if (OpName == "=") {
      // We need to figure out if this is a constructor, destructor,
      // or assignment operator.
      const DeclarationSyntax *ThisDecl = nullptr;

      if (auto Enc = dyn_cast_or_null<EnclosureSyntax>(Dcl->getInfo()))
        if (auto LS = dyn_cast_or_null<ListSyntax>(Enc->getOperand()))
          if (LS->getNumChildren() != 0)
            ThisDecl = dyn_cast_or_null<DeclarationSyntax>(LS->getOperand(0));

      if (!ThisDecl) {
        SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_blue_elaboration)
          << "operator = cannot be a static method and must contain "
              "the this parameter";
        return D->Id;
      }
      Token Spec = ThisDecl->getParamPassingSpecifier();
      if (!Spec) {
        return SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
            clang::OO_Equal);
      } else {
        clang::QualType RecordTy = SemaRef.getCxxAST().getTypeDeclType(RD);
        clang::CanQualType Ty = SemaRef.getCxxAST().getCanonicalType(RecordTy);
        switch (Spec.getKind()) {
        case tok::OutKeyword:
          return SemaRef.getCxxAST().DeclarationNames.getCXXConstructorName(Ty);

        case tok::InoutKeyword:
          return SemaRef.getCxxAST().DeclarationNames.getCXXOperatorName(
            clang::OO_Equal);

        case tok::MoveKeyword:
          return SemaRef.getCxxAST().DeclarationNames.getCXXDestructorName(Ty);

        case tok::ForwardKeyword:
          LLVM_FALLTHROUGH;
        case tok::InKeyword:
          SemaRef.getCxxSema().Diags.Report(Spec.getLocation(),
                                            clang::diag::err_blue_elaboration)
                                            << "invalid parameter specifier.";
          return clang::DeclarationName(D->Id);
        default:
          llvm_unreachable("Invalid/uknown parameter specifier");
        }
      }
    } else {
      clang::DeclarationName Name;
      if (!getBasicOperatorName(SemaRef, Loc, OpName,
                                Name))
        return Name;
      else{
        SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_blue_elaboration)
          << "invalid operator declaration.";
        return clang::DeclarationName(D->Id);
      }
    }
  }
  return clang::DeclarationName(D->Id);

}

static void lookupFunctionRedecls(Sema &SemaRef, clang::Scope *FoundScope,
                           clang::LookupResult &Previous) {
  while ((FoundScope->getFlags() & clang::Scope::DeclScope) == 0 ||
         (FoundScope->getFlags() & clang::Scope::TemplateParamScope) != 0)
    FoundScope = FoundScope->getParent();

  assert(FoundScope && "Scope not found");
  SemaRef.getCxxSema().LookupName(Previous, FoundScope, false);
}

clang::Decl *Elaborator::makeFunctionDecl(Declaration *D) {
  bool InClass = D->ScopeForDecl->isClassScope();
  // bool InClass = isa<clang::TagDecl>(ResolvedCtx);
  clang::CXXRecordDecl *RD = nullptr;
  if (InClass) {
    clang::Decl *ScopesDecl = D->Ctx->getCxx();
    assert(ScopesDecl && "Invalid declaration for scope.");
    RD = dyn_cast<clang::CXXRecordDecl>(ScopesDecl);
    assert(RD && "Class scope doesn't contain declaration.");
  }


  // TODO: We don't have syntax for user defined literals yet.
  // if (D->declaresUserDefinedLiteral()) {
  //   clang::UnqualifiedId UnqualId;
  //   UnqualId.setLiteralOperatorId(D->UDLSuffixId,
  //                                 D->IdDcl->getLoc(),
  //                          D->IdDcl->getIdentifier()->getFusionArg()->getLoc());
  //   if (SemaRef.getCxxSema().checkLiteralOperatorId(D->ScopeSpec, UnqualId)) {
  //     return nullptr;
  //   }
  // }



  Declarator *Dclrtr = D->getFirstDeclarator(Declarator::Function);

  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::SourceLocation Loc = D->Def->getLocation();

  TemplateParamRAII TempParamContextGuard(TempCtx);
  clang::FunctionDecl *FD = nullptr;
  clang::CppxTypeLiteral *FnTy = nullptr;
  FnTy = createFunctionType(D, Dclrtr);
  if (!FnTy)
    return nullptr;
  clang::TypeSourceInfo *FnTInfo = FnTy->getValue();

  // Get name info for the AST.
  clang::DeclarationName Name =
    getFunctionName(SemaRef, D, FnTInfo, InClass, RD);
  if (Name.isEmpty())
    return nullptr;
  clang::DeclarationNameInfo DNI(Name, Loc);
  // TODO: I'm not sure this is necessary, because we don't have any functions
  // that have a decl-def pattern
  clang::LookupResult Previous(CxxSema, DNI,
                               clang::Sema::LookupOrdinaryName,
                               CxxSema.forRedeclarationInCurContext());
  clang::Scope *CxxScope = SemaRef.getCurClangScope();
  lookupFunctionRedecls(SemaRef, CxxScope, Previous);

  clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
  if (InClass) {
    if (buildMethod(D, Name, &FD, FnTInfo, RD))
      return nullptr;
  } else {
    FD = clang::FunctionDecl::Create(CxxAST, Owner, Loc, Loc, Name,
                                    FnTInfo->getType(), FnTInfo, clang::SC_None);
  }
  if (!FD)
    return nullptr;
  bool Template = D->declaratorContainsTemplate();
  if (Template) {
    Declarator *TmpltDclrtr = D->Decl;
    clang::SourceLocation Loc = TmpltDclrtr->getLocation();
    auto *FTD = clang::FunctionTemplateDecl::Create(getCxxContext(),
                                                    Owner, Loc,
                                                    FD->getDeclName(),
                                                    TmpltDclrtr->ClangParamList,
                                                    FD);
    FTD->setLexicalDeclContext(Owner);
    FD->setDescribedFunctionTemplate(FTD);
    Owner->addDecl(FTD);
    if (InClass)
      FTD->setAccess(clang::AS_public);

    // An auto return type here is always dependent.
    if (FD->getReturnType()->isUndeducedAutoType())
      deduceDependentAutoReturn(SemaRef, FD);
  }
  if (FD->isMain()) {
    clang::AttributeFactory Attrs;
    clang::DeclSpec DS(Attrs);
    CxxSema.CheckMain(FD, DS);
  }

  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getParameters(D, Dclrtr, Params);
  FD->setParams(Params);
  // Move parameters into this declaration context.
  for (auto *PD : Params) {
    PD->setDeclContext(FD);
    PD->setOwningFunction(FD);
  }



  if (!Template)
    Owner->addDecl(FD);

  // Owner->addDecl(FD);
  D->setCxx(SemaRef, FD);
  D->CurrentPhase = Phase::Typing;
  clang::QualType CompletedFnTy = FD->getType();
  const clang::FunctionProtoType *FPT =
      CompletedFnTy->getAs<clang::FunctionProtoType>();
  auto EPI = FPT->getExtProtoInfo();
  for (Token Tok : D->ThisParamSpecifiers) {
    switch(Tok.getKind()) {
      case tok::InKeyword:
        EPI.TypeQuals.addConst();
        break;
      case tok::OutKeyword:
      case tok::InoutKeyword:
        break;
      case tok::MoveKeyword:
        if (!isa<clang::CXXDestructorDecl>(*FD)) {
          EPI.RefQualifier = clang::RQ_RValue;
        }
        break;
      case tok::ForwardKeyword:
        EPI.RefQualifier = clang::RQ_RValue;
        break;
      case tok::Identifier:
        D->FunctionThisParam->dump();
        llvm_unreachable("Unhandled this identifier");
      default:
        llvm::outs() << "Unhandled this prefix token.\n";
        D->FunctionThisParam->dump();
        llvm_unreachable("Unhandled this prefix");
    }
  }
  if (!FD)
    return nullptr;
  if (SemaRef.rebuildFunctionType(FD, FD->getBeginLoc(), FPT,
                                  FPT->getExtInfo(), EPI,
                                  FPT->getExceptionSpecInfo())) {
    return nullptr;
  }
  CxxSema.CheckFunctionDeclaration(CxxScope, FD, Previous, false);
  // FIXME: this is not necessarily what should happen.
  if (FD->isInvalidDecl())
    return nullptr;
  return FD;
}


clang::Decl *Elaborator::makeClass(Declaration *D) {
  using namespace clang;
  D->CurrentPhase = Phase::Typing;

  // Checking if we are a nested template decl/class.
  bool WithinClass = D->ScopeForDecl->getKind() == blue::Scope::Class;
  MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  bool IsOwned = false;
  bool IsDependent = false;
  CXXScopeSpec SS;
  TypeResult UnderlyingType;
  AccessSpecifier AS = AS_none;
  if (WithinClass)
    AS = AS_public;

  clang::SourceLocation IdLoc = D->Def->getLocation();
  clang::TypeSpecifierType TST = clang::DeclSpec::TST_struct;
  bool ScopeEnumUsesClassTag = false;
  clang::SourceLocation ScopedEnumClassKW;
  blue::Scope::Kind SK = blue::Scope::Class;
  // TODO: Refactor this so that we actually check the TST kind against
  // the class/enum/union identifier.
  // switch(D->getKind()) {
  // case UDK_Class:
  //   TST = clang::DeclSpec::TST_struct;
  //   break;
  // case UDK_Union:
  //   TST = clang::DeclSpec::TST_union;
  //   break;
  // case UDK_Enum:
  //   llvm_unreachable("");
  //   // TST = clang::DeclSpec::TST_enum;
  //   // ScopeEnumUsesClassTag = true;
  //   // if (const MacroSyntax *MS = dyn_cast<MacroSyntax>(D->Init)) {
  //   //   UnderlyingType = getUnderlyingEnumType(Context, SemaRef, MS->getCall());
  //   // } else {
  //   //   llvm_unreachable("Invalid tree syntax.");
  //   // }
  //   // ScopedEnumClassKW = D->IdDcl->getLoc();
  //   // SK = SK_Enum;
  //   break;
  // default:
  //   llvm_unreachable("Incorrectly identified tag type");
  // }
  // auto ClsDef = cast<DeclarationSyntax>(D->Def);
  auto ClsEnc = cast<EnclosureSyntax>(D->getInitializer());
  auto ClsBody  = dyn_cast_or_null<ListSyntax>(ClsEnc->getOperand());
  Decl *Declaration = nullptr;
  // if (D->SpecializationArgs) {
  //   Declaration = handleClassSpecialization(Context, SemaRef, D, TST, MTP);
  // } else {
  Declaration = SemaRef.getCxxSema().ActOnTag(
    SemaRef.getCurClangScope(), TST, /*Metafunction=*/nullptr,
    clang::Sema::TUK_Definition, ClsEnc->getOpen().getLocation(), SS, D->Id,
    IdLoc, clang::ParsedAttributesView(), AS,
    /*ModulePrivateLoc=*/SourceLocation(),
    MTP, IsOwned, IsDependent, ScopedEnumClassKW, ScopeEnumUsesClassTag,
    UnderlyingType, /*IsTypeSpecifier=*/false, /*IsTemplateParamOrArg=*/false);
  // }

  TagDecl *Tag = nullptr;
  if (!Declaration) {
    return nullptr;
  }
  if(isa<CXXRecordDecl>(Declaration)) {
    Tag = cast<CXXRecordDecl>(Declaration);
  } else if (isa<ClassTemplateDecl>(Declaration)) {
    ClassTemplateDecl *TempTemplateDecl = cast<ClassTemplateDecl>(Declaration);
    D->setCxx(SemaRef, TempTemplateDecl);
    Tag = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  } else if (isa<EnumDecl>(Declaration)) {
    Tag = cast<TagDecl>(Declaration);
  }

  D->setCxx(SemaRef, Tag);
  // Elab.elaborateAttributes(D);

  Sema::ScopeRAII ClassBodyScope(SemaRef, SK, D->Def, &D->SavedScope);
  SemaRef.getCurrentScope()->Entity = D;

  Sema::ClangScopeRAII ClangClassScopeBody(SemaRef,
            (Tag->isEnum() ? clang::Scope::EnumScope : clang::Scope::ClassScope)
                                           | clang::Scope::DeclScope,
                                     ClsEnc->getOpen().getLocation());


  // Need to do this before the next step because this is actually pushed on to
  // the stack a by the next function called.
  SemaRef.getCxxSema().ActOnTagStartDefinition(SemaRef.getCurClangScope(), Tag);

  // This keeps the declContext working correctly.
  Sema::DeclContextRAII DCTracking(SemaRef, D, true);
  if (TST == clang::DeclSpec::TST_enum) {
    llvm_unreachable("Enum body not implemented yet.");
    // Elab.elaborateEnumBody(D, Tag);
    // if (Tag->isInvalidDecl()) {
    //   // Need to make sure that this isn't elaborated as a variable later on.
    //   D->CurrentPhase = Phase::Initialization;
    //   return Tag;
    // }
  } else {
    // This handles processing for class, struct, and union bodies.
    // This keeps track of class nesting.
    Sema::ElaboratingClassDefRAII ClsElabState(SemaRef, D,
                                              !SemaRef.isElaboratingClass());
    CXXRecordDecl *ClsDecl = cast<CXXRecordDecl>(Tag);
    llvm::SmallVector<blue::Declaration *, 64> DeclBodyList;
    if (ClsEnc->getOperand())
      identifyDeclsInClassBody(D, ClsBody, ClsDecl, DeclBodyList);
    unsigned DeclIndex = 0;
    makeBases(DeclIndex, DeclBodyList, ClsDecl);

    D->IsElaborating = false;

    // This is really the only time we could possible allow this to occur.
    SemaRef.getCxxSema().ActOnStartCXXMemberDeclarations(
                                                     SemaRef.getCurClangScope(),
                                                         ClsDecl,
                                                         SourceLocation(), true,
                                                         SourceLocation());
    if (ClsEnc->getOperand()) {
      // auto List = dyn_cast<ListSyntax>(ClsEnc->getOperand());
      // assert(List && "invalid class tree.");
      // Since all declarations have already been added, we don't need to do another
      // Reordering scan.
      // Doing possible delaying of member declaration/initialziation.
      // for (const Syntax *SS : ClsBody->children())
      for(; DeclIndex < DeclBodyList.size(); ++DeclIndex)
        delayElaborateDeclType(ClsDecl, DeclBodyList[DeclIndex]);
    }

    D->CurrentPhase = Phase::Initialization;
    if (!WithinClass) {
      ElaboratingClass &LateElabClass = SemaRef.getCurrentElaboratingClass();
      // Elab.finishDelayedElaboration(LateElabClass);
      lateElaborateAttributes(LateElabClass);
      lateElaborateMethodDecls(LateElabClass);
      lateElaborateDefaultParams(LateElabClass);
      // We call this because no new declarations can be added after this point.
      // This is only called for the top level class.
      SemaRef.getCxxSema().ActOnFinishCXXMemberDecls();

      SemaRef.getCxxSema().ActOnFinishCXXMemberSpecification(
        SemaRef.getCurClangScope(), SourceLocation(), ClsDecl, SourceLocation(),
        SourceLocation(), ParsedAttributesView());

      lateElaborateMemberInitializers(LateElabClass);
      lateElaborateMethodDefs(LateElabClass);
      SemaRef.getCxxSema().ActOnFinishCXXNonNestedClass(ClsDecl);
    } else {
      SemaRef.getCxxSema().ActOnFinishCXXMemberSpecification(
        SemaRef.getCurClangScope(), SourceLocation(), ClsDecl, SourceLocation(),
        SourceLocation(), ParsedAttributesView());
    }
  }

  clang::Decl *TempDeclPtr = Tag;
  SemaRef.getCxxSema().ActOnTagFinishDefinition(SemaRef.getCurClangScope(),
                                                TempDeclPtr, SourceRange());
  return Tag;
}

bool Elaborator::makeBases(unsigned &DeclIndex,
                           llvm::SmallVectorImpl<Declaration *> & DeclBodyList,
                           clang::CXXRecordDecl *R) {

  // Evaluating each individual child expression. Some could be template names.
  // It's also worth noting that these type of bases could have attributes
  // associated with each expression.
  Sema::ClangScopeRAII InheritanceScope(SemaRef, clang::Scope::DeclScope |
      clang::Scope::ClassScope | clang::Scope::ClassInheritanceScope,
      clang::SourceLocation());

  llvm::SmallVector<clang::CXXBaseSpecifier *, 4> GivenBaseClasses;
  bool didError = false;
  while(!DeclBodyList.empty() && DeclIndex < DeclBodyList.size() &&
        (
          DeclBodyList[DeclIndex]->getIntroducerKind()
            == DeclarationSyntax::Super))
  {
    Declaration *CurrentBase = DeclBodyList[DeclIndex];
    clang::SourceLocation Loc = CurrentBase->getErrorLocation();
    clang::Expr *BaseExpr = elaborateExpression(CurrentBase->asDef()->getType());
    if (!BaseExpr) {
      didError = true;
      getCxxSema().Diags.Report(CurrentBase->getErrorLocation(),
                           clang::diag::err_failed_to_translate_expr);
      ++DeclIndex;
      continue;
    }

    // TODO: Need to create processing for the base specifier virtual?
    // I'm not sure that blue has virtual base classes yet.
    if ((BaseExpr->isTypeDependent() || BaseExpr->isValueDependent()
        || BaseExpr->getType()->isDependentType())
        && !isa<clang::CppxTypeLiteral>(BaseExpr)) {
      // Updating a dependent expression that may or may not have a result type.
      BaseExpr = SemaRef.buildTypeExprTypeFromExpr(BaseExpr, Loc);
    }
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(BaseExpr,
                                                                     Loc);
    if (!TInfo) {
      ++DeclIndex;
      didError = true;
      continue;
    }
    clang::AccessSpecifier AS = clang::AS_public;
    bool IsVirtualBase = false;
    clang::ParsedType PT = getCxxSema().CreateParsedType(TInfo->getType(),TInfo);
    clang::ParsedAttributes Attributes(SemaRef.AttrFactory);
    auto BaseResult = getCxxSema()
      .ActOnBaseSpecifier(R, clang::SourceRange(Loc, Loc),
                          Attributes, IsVirtualBase, AS, PT,
                          Loc, clang::SourceLocation());

    if (BaseResult.isInvalid()) {
      ++DeclIndex;
      didError = true;
      continue;
    }
    GivenBaseClasses.emplace_back(BaseResult.get());
    ++DeclIndex;
  }
  if (!DeclBodyList.empty())
    SemaRef.getCxxSema().ActOnBaseSpecifiers(R, GivenBaseClasses);
  return didError;
}

clang::Decl *Elaborator::makeTemplateDecl(Declaration *D) {
  llvm::outs() << "TEMPLATE!\n";
  return nullptr;
}

clang::Decl *Elaborator::makeFieldDecl(Declaration *D, clang::Expr *Ty) {
  auto TInfo = SemaRef.getTypeSourceInfoFromExpr(Ty, Ty->getExprLoc());
  if (!TInfo)
    return nullptr;
  clang::Decl *Ctxt = SemaRef.getCurrentDecl()->getCxx();
  clang::CXXRecordDecl *Owner = dyn_cast<clang::CXXRecordDecl>(Ctxt);
  // Get the type of the entity.
  if(!Owner) {
    // This occurs when we are within an extern "C" decl
    getCxxSema().Diags.Report(D->Def->getLocation(),
                              clang::diag::err_invalid_extern_c)
                              << /* a member */0;
    return nullptr;
  }

  clang::SourceLocation Loc = D->Def->getLocation();
  // clang::SourceLocation LocEnd = D->getEndOfDecl();
  clang::DeclarationName DN = D->Id;
  clang::InClassInitStyle InitStyle = clang::InClassInitStyle::ICIS_NoInit;
  if (D->hasInitializer())
    InitStyle = clang::InClassInitStyle::ICIS_ListInit;

  // TODO: implement static
  // bool DeclIsStatic = false;
  // if (isStaticMember(SemaRef, D, DeclIsStatic)) {
  //   return nullptr;
  // }
  clang::Decl *Field = nullptr;
  // if (DeclIsStatic) {
  //   // In this case we are creating a static member variable.
  //   clang::VarDecl *VDecl= clang::VarDecl::Create(Context.CxxAST, Owner, Loc,
  //                                                 LocEnd, D->getId(),
  //                                                 TInfo->getType(), TInfo,
  //                                                 clang::SC_Static);
  //   VDecl->setAccess(clang::AS_public);
  //   Field = VDecl;
  // } else {
    bool Mutable = false;
    // TODO: implement Mutability
    // if (isMutable(SemaRef, D, Mutable))
    //   return nullptr;
    // We are create field within a class.
    Field = SemaRef.getCxxSema().CheckFieldDecl(DN, TInfo->getType(),
                                                TInfo, /*RecordDecl=*/Owner,
                                                Loc, Mutable,
                                                /*BitWidth=*/nullptr, InitStyle,
                                                Loc, clang::AS_public, nullptr);
  // }
  Owner->addDecl(Field);
  D->setCxx(SemaRef, Field);
  // SemaRef.setDeclForDeclaration(D, Field);
  D->CurrentPhase = Phase::Typing;
  // elaborateAttributes(D);
  return Field;
}

clang::Decl *Elaborator::makeNamespace(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  using namespace clang;
  // Create and enter a namespace scope.
  CppxNamespaceDecl *NSDecl = nullptr;
  clang::Scope *NSScope = SemaRef.enterClangScope(clang::Scope::DeclScope);

  // FIXME: keep track of nested namespaces?
  OptionalScopeRAII NewScope(SemaRef);
  OptionalResumeScopeRAII ResumedScope(SemaRef);
  UsingDirectiveDecl *UD = nullptr;
  AttributeFactory Attrs;
  ParsedAttributes ParsedAttrs(Attrs);
  auto DS = dyn_cast<DeclarationSyntax>(D->Def);
  if (!DS)
    llvm_unreachable("I'm not sure this can happen.");
  SourceLocation NameLoc = DS->getLocation();
  SourceLocation BeginBracketLoc;
  SourceLocation EndingLoc;
  if (auto Enc = dyn_cast<EnclosureSyntax>(D->getInitializer())) {
    BeginBracketLoc =Enc->getOpen().getLocation();
    EndingLoc = Enc->getClose().getLocation();
  }
  NSDecl = SemaRef.ActOnStartNamespaceDef(NSScope,
                                          SourceLocation(),
                                          NameLoc,
                                          NameLoc,
                                          D->Id,
                                          BeginBracketLoc,
                                          ParsedAttrs, UD);
  if (NSDecl->isInvalidDecl()) {
    // Making sure that if something does go wrong that we properly recover
    // from it.
    SemaRef.leaveClangScope(clang::SourceLocation());
    return nullptr;
  }

  // Resume or create a new scope for the current namespace.
  // This is to allow the representations to all share the same scope.
  // This makes it easier to handle lookup for those elements of the scope.
  if (!NSDecl->BlueScope) {
    NewScope.Init(Scope::Namespace, D->Init, &NSDecl->BlueScope);
    NSDecl->BlueScope = SemaRef.getCurrentScope();
  } else {
    ResumedScope.Init(NSDecl->BlueScope, NSDecl->BlueScope->getTerm(), false);
  }
  D->setCxx(SemaRef, NSDecl);
  // Elab.elaborateAttributes(D);

  SemaRef.pushDecl(D);

  if (auto Enc = dyn_cast_or_null<EnclosureSyntax>(D->getInitializer())) {
    if (auto NSBody = dyn_cast_or_null<MultiarySyntax>(Enc->getOperand())) {
      for (const Syntax *SS : NSBody->children())
        identifyDeclaration(SS);

      for (const Syntax *SS : NSBody->children())
        elaborateDecl(SS);

      for (const Syntax *SS : NSBody->children())
        elaborateDefinition(SS);
    }
  }

  NSDecl->BlueScope = SemaRef.getCurrentScope();
  SemaRef.getCxxSema().ActOnFinishNamespaceDef(NSDecl, EndingLoc);
  SemaRef.leaveClangScope(EndingLoc);
  SemaRef.popDecl();


  // FIXME: We should be returning a DeclGroupPtr to the NSDecl grouped
  // with the implicit UsingDecl, UD.
  return NSDecl;
}


// Type elaboration
//
// TODO: Can we combine this with the creating the declarator? It would save
// a pass over the AST. We could store the type directly in the declarator
// node (which we might do anyway), along with intermediate structures
// created to manage the declaration.

/// Return the type of entity declared by Dcl and establish any semantic
/// state needed to process the declaration and its initializer.
clang::Expr *Elaborator::elaborateDeclarator(const Declarator *Dcl) {
  switch (Dcl->getKind()) {
  case Declarator::Type:
    return elaborateTypeDeclarator(Dcl);
  case Declarator::Pointer:
    return elaboratePointerDeclarator(Dcl);
  case Declarator::Array:
    return elaborateArrayDeclarator(Dcl);
  case Declarator::Function:
    return elaborateFunctionDeclarator(Dcl);
  case Declarator::Template:
    return elaborateTemplateDeclarator(Dcl);
  case Declarator::Class:
    return elaborateClassDeclarator(Dcl);
  case Declarator::ImplicitType:
    return elaborateImplicitTypeDeclarator(Dcl);
  }
  llvm_unreachable("Unhandled kind of declarator.");
}

/// Elaborate declarations of the form 'T' as an expression.
clang::Expr *Elaborator::elaborateTypeDeclarator(const Declarator *Dcl) {
  clang::Expr *E = elaborateExpression(Dcl->getInfo());
  if (!E)
    return nullptr;

  clang::QualType T = E->getType();
  if (!T->isKindType()) {
    Error(Dcl->getLocation(), "invalid type");
    return nullptr;
  }
  return E;
}

/// Elaborate declarations of the form '^E'.
clang::Expr *Elaborator::elaboratePointerDeclarator(const Declarator *Dcl) {
  clang::Expr *E = elaborateDeclarator(Dcl->getNext());
  if (!E)
    return nullptr;

  // If this apears within a declarator then it must be a type.
  auto TInfo = SemaRef.getTypeSourceInfoFromExpr(E, E->getExprLoc());
  if (!TInfo)
    return nullptr;
  clang::QualType RetType = getCxxContext().getPointerType(TInfo->getType());
  return SemaRef.buildTypeExpr(RetType, Dcl->getLocation());
}

/// Elaborate declarations of the form "[E]+ T".
clang::Expr *Elaborator::elaborateArrayDeclarator(const Declarator *Dcl) {
  assert(Dcl->getKind() == Declarator::Array
         && isa<ArraySyntax>(Dcl->getInfo()));

  if (!Dcl->getNext()) {
    llvm::errs() << "Dumping declarator info\n";
    Dcl->getInfo()->dump();
    llvm_unreachable("Invalid array declarator missing type.");
  }

  // Evaluating inner type expression.
  clang::Expr *TyExpr = elaborateDeclarator(Dcl->getNext());
  if (!TyExpr)
    return nullptr;

  auto TInfo = SemaRef.getTypeSourceInfoFromExpr(TyExpr, Dcl->getLocation());
  if (!TInfo)
    return nullptr;
  auto AS = cast<ArraySyntax>(Dcl->getInfo());
  auto BoundsEnc = cast<EnclosureSyntax>(AS->getBounds());
  if (!BoundsEnc->getOperand()) {
    // FIXME:/TODO: We need to support implicit array size.
    Error(BoundsEnc->getOpen().getLocation(),
          "invalid array declaration, no size given.");
    return nullptr;
  }
  clang::QualType ArrayType = TInfo->getType();
  auto SizeList = cast<ListSyntax>(BoundsEnc->getOperand());
  for (auto Val : SizeList->reverseChildren()) {
    clang::Expr *ArrSizeExpr = elaborateConstantExpression(Val);
    if (!ArrSizeExpr) {
      // We should try and evaluate all array values.
      Error(Val->getLocation(), "invalid array index");
      continue;
    }

    clang::Expr::EvalResult IdxResult;
    clang::Expr::EvalContext EvalCtx(CxxAST,
                                     getCxxSema().GetReflectionCallbackObj());
    if (!ArrSizeExpr->EvaluateAsConstantExpr(IdxResult, EvalCtx)) {
      getCxxSema().Diags.Report(ArrSizeExpr->getExprLoc(),
                                clang::diag::err_expr_not_cce)
                                << /*array size*/3;
      return nullptr;
    }

    clang::SourceRange Range(ArrSizeExpr->getExprLoc(),
                            ArrSizeExpr->getExprLoc());
    if (IdxResult.Val.isInt()) {
      ArrayType = CxxSema.BuildArrayType(
        ArrayType, clang::ArrayType::Normal,
        clang::IntegerLiteral::Create(CxxAST, IdxResult.Val.getInt(),
                                      ArrSizeExpr->getType(),
                                      ArrSizeExpr->getExprLoc()),
        /*quals*/0,
        Range, clang::DeclarationName());
    } else {
      ArrayType = CxxSema.BuildArrayType(ArrayType, clang::ArrayType::Normal,
                                         ArrSizeExpr, /*quals*/0,
                                         Range, clang::DeclarationName());
    }
  }
  return SemaRef.buildTypeExpr(ArrayType, Dcl->getLocation());
}

/// Elaborate declarations of the form '(parms) T'.
/// This returns a type expression of the form `(parms) -> T`.
clang::Expr *Elaborator::elaborateFunctionDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Not implemented yet");
}

clang::Expr *Elaborator::elaborateClassDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Class Declaration not implemented.");
}

/// Elaborate declarations of the form '[parms] T'.
clang::Expr *Elaborator::elaborateTemplateDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Not implemented");
}

clang::Expr *Elaborator::elaborateImplicitTypeDeclarator(const Declarator *Dcl) {
  clang::QualType Ty = getCxxContext().getAutoDeductType();
  return SemaRef.buildTypeExpr(Ty, clang::SourceLocation());
}

clang::Decl *Elaborator::identifyDeclsInClassBody(Declaration *D,
                                                  const ListSyntax *L,
                                                  clang::CXXRecordDecl *R,
                               llvm::SmallVectorImpl<Declaration *> &DeclList) {
  if(!D->hasInitializer()) {
    // FIXME: Handle forward declarations here? I think.
    llvm_unreachable("Type forward declarations are not implemented yet.");
    return nullptr;
  }

  // auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
  // assert(MacroRoot && "Invalid AST structure.");
  // auto const* BodyArray = MacroRoot->getBlock();
  D->CurrentPhase = Phase::Typing;
  Scope *S = SemaRef.getCurrentScope();
  // for (auto const* ChildDecl : BodyArray->children()) {
  for (const Syntax *SS : L->children()) {
    identifyDeclaration(SS);
    Declaration *Member = S->findDecl(SS);
    if (!Member){
      Error(SS->getLocation(), "invalid member declaration");
      continue;
    }
    DeclList.emplace_back(Member);
  }

  return D->getCxx();
}

// Expression elaboration
clang::Expr *Elaborator::elaborateExpression(const Syntax *S) {
  if (!S)
    return nullptr;

  return doElaborateExpression(S);
}

clang::Expr *Elaborator::elaborateConstantExpression(const Syntax *S) {
  clang::EnterExpressionEvaluationContext ConstantEvaluated(getCxxSema(),
                   clang::Sema::ExpressionEvaluationContext::ConstantEvaluated);
  Sema::DeepElaborationModeRAII ElabMode(SemaRef, true);
  clang::Expr *Res = doElaborateExpression(S);
  if (!Res)
    return Res;

  // This attempts to make sure that all referenced functions are actually
  // in scope, and completely elaborated.

  // TODO: I may need to re-implement the constant expression triggering
  // elaboration from gold.
  // SemaRef.elaborateConstexpr(Res);

  auto ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(Res);
  if (ConstExpr.isInvalid())
    return nullptr;
  return ConstExpr.get();
}

clang::Expr *Elaborator::doElaborateExpression(const Syntax *S) {
  assert(S && "invalid expression");
  switch (S->getKind()) {
  case Syntax::Literal:
    return elaborateLiteralExpression(cast<LiteralSyntax>(S));
  case Syntax::Identifier:
    return elaborateIdentifierExpression(cast<IdentifierSyntax>(S));
  case Syntax::Call:
    return elaborateCallExpression(cast<CallSyntax>(S));
  case Syntax::Prefix:
      return elaboratePrefixExpression(cast<PrefixSyntax>(S));
  case Syntax::Postfix:
      return elaboratePostfixExpression(cast<PostfixSyntax>(S));
  case Syntax::Infix:
      return elaborateInfixExpression(cast<InfixSyntax>(S));
  case Syntax::Index:
      return elaborateIndexExpression(cast<IndexSyntax>(S));
  case Syntax::Pair:
      return elaboratePairExpression(cast<PairSyntax>(S));
  case Syntax::Triple:
      return elaborateTripleExpression(cast<TripleSyntax>(S));
  case Syntax::Enclosure:
    llvm_unreachable("Enclosure syntax is unavailable.");
  case Syntax::List:
      return elaborateListExpression(cast<ListSyntax>(S));
  case Syntax::Sequence:
      return elaborateSequenceExpression(cast<SequenceSyntax>(S));
  default:
    break;
  }

  S->dump();
  llvm_unreachable("Unexpected syntax tree");
}



static bool alwaysFitsInto64Bits(unsigned Radix, unsigned NumDigits) {
  switch (Radix) {
  case 2:
    return NumDigits <= 64;
  case 8:
    return NumDigits <= 64 / 3; // Digits are groups of 3 bits.
  case 10:
    return NumDigits <= 19; // floor(log10(2^64))
  case 16:
    return NumDigits <= 64 / 4; // Digits are groups of 4 bits.
  default:
    llvm_unreachable("impossible Radix");
  }
}

static bool checkOverflow(unsigned Radix, llvm::StringRef Literal,
                          llvm::APInt &Val) {
  const unsigned NumDigits = Literal.size();

  auto isDigitSeparator = [](char C) -> bool {
    return C == '\'';
  };

  if (alwaysFitsInto64Bits(Radix, NumDigits)) {
    uint64_t N = 0;
    for (const char *Ptr = Literal.begin(); Ptr != Literal.end(); ++Ptr)
      if (!isDigitSeparator(*Ptr))
        N = N * Radix + llvm::hexDigitValue(*Ptr);

    // This will truncate the value to Val's input width. Simply check
    // for overflow by comparing.
    Val = N;
    return Val.getZExtValue() != N;
  }

  Val = 0;
  const char *Ptr = Literal.begin();

  llvm::APInt RadixVal(Val.getBitWidth(), Radix);
  llvm::APInt CharVal(Val.getBitWidth(), 0);
  llvm::APInt OldVal = Val;

  bool OverflowOccurred = false;
  while (Ptr < Literal.end()) {
    if (isDigitSeparator(*Ptr)) {
      ++Ptr;
      continue;
    }

    unsigned C = llvm::hexDigitValue(*Ptr++);

    // If this letter is out of bound for this radix, reject it.
    assert(C < Radix && "checkOverflow called with wrong radix");

    CharVal = C;

    // Add the digit to the value in the appropriate radix.  If adding in digits
    // made the value smaller, then this overflowed.
    OldVal = Val;

    // Multiply by radix, did overflow occur on the multiply?
    Val *= RadixVal;
    OverflowOccurred |= Val.udiv(RadixVal) != OldVal;

    // Add value, did overflow occur on the value?
    //   (a + b) ult b  <=> overflow
    Val += CharVal;
    OverflowOccurred |= Val.ult(CharVal);
  }
  return OverflowOccurred;
}

static clang::IntegerLiteral *
createIntegerLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                     const LiteralSyntax *S, std::size_t Base = 10) {
  unsigned Width = S->Suffix.BitWidth;
  bool Signed = S->Suffix.IsSigned;

  // In case we didn't set either flag, this is signed by default.
  if (!Signed && !S->Suffix.IsUnsigned)
    Signed = true;

  unsigned TargetIntWidth = CxxAST.getTargetInfo().getIntWidth();
  if (!Width)
    Width = TargetIntWidth;

  clang::QualType IntTy = CxxAST.getIntTypeForBitwidth(Width, Signed);
  if (IntTy.isNull()) {
    if (Width <= TargetIntWidth)
      IntTy = Signed ? CxxAST.IntTy : CxxAST.UnsignedIntTy;
    else if (Width <= CxxAST.getTargetInfo().getLongWidth())
      IntTy = Signed ? CxxAST.LongTy : CxxAST.UnsignedLongTy;
    else
      IntTy = Signed ? CxxAST.LongLongTy : CxxAST.UnsignedLongLongTy;
  }

  if (Width != CxxAST.getIntWidth(IntTy)) {
    clang::SourceLocation Loc = S->getLocation();
    SemaRef.getCxxSema().Diags.Report(Loc,
      clang::diag::err_integer_bitwidth_mismatch)
      << IntTy << Width << CxxAST.getIntWidth(IntTy);
    return nullptr;
  }

  // skip over any [0.] prefix
  std::string Spelling = Base == 10 ? S->getSpelling() :
    S->getSpelling().substr(2);

  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

  llvm::APInt Value(Width, Spelling, Base);
  Value = Value.zextOrTrunc(Width);

  if (checkOverflow(Base, Spelling, Value)) {
    SemaRef.getCxxSema().Diags.Report(S->getLocation(),
                                     clang::diag::err_integer_literal_too_large)
      << /* Unsigned */ 1;
    return nullptr;
  }

  return clang::IntegerLiteral::Create(CxxAST, Value, IntTy, S->getLocation());
}

static clang::FloatingLiteral *
createFloatLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                   const LiteralSyntax *S) {
  // // If we don't have a specified type, just create a default float.
  // clang::QualType FloatType = CxxAST.FloatTy;
  // if (S->Suffix.IsDouble)
  //   FloatType = CxxAST.DoubleTy;
  // else if (S->Suffix.IsHalf)
  //   FloatType = CxxAST.Float16Ty;
  // else if (S->Suffix.IsQuarter) {
  //   unsigned DiagID =
  //     SemaRef.getCxxSema().Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
  //                                   "minifloats not yet supported by Clang");
  //   SemaRef.getCxxSema().Diags.Report(S->getLocation(), DiagID);
  //   return nullptr;
  // }

  // const llvm::fltSemantics &Format = CxxAST.getFloatTypeSemantics(FloatType);
  // using llvm::APFloat;
  // APFloat Val = llvm::APFloat(Format);

  // std::string Spelling = S->getSpelling();
  // auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  // while(It != std::end(Spelling)) {
  //   Spelling.erase(It);
  //   It = std::find(Spelling.begin(), Spelling.end(), '\'');
  // }

  // auto StatusOrErr =
  //   Val.convertFromString(Spelling, APFloat::rmNearestTiesToEven);
  // assert(StatusOrErr && "Invalid floating point representation");
  // return clang::FloatingLiteral::Create(CxxAST, Val, /*Exact=*/true,
  //                                       FloatType, S->getLocation());
  llvm_unreachable("floating literal not implemented yet");
}

static clang::FloatingLiteral *
createExponentLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                      const LiteralSyntax *S, clang::SourceLocation Loc) {
  // std::string Spelling = S->getSpelling();
  // assert((Spelling.find_first_of("E") != std::string::npos ||
  //        Spelling.find_first_of("e") != std::string::npos) &&
  //        "non-exponent");
  // auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  // while(It != std::end(Spelling)) {
  //   Spelling.erase(It);
  //   It = std::find(Spelling.begin(), Spelling.end(), '\'');
  // }

  // const llvm::fltSemantics &Format =
  //   CxxAST.getFloatTypeSemantics(CxxAST.DoubleTy);
  // llvm::APFloat Val(Format);
  // auto StatusOrErr =
  //   Val.convertFromString(Spelling, llvm::APFloat::rmNearestTiesToEven);
  // assert(StatusOrErr && "invalid floating point representation");
  // if (llvm::errorToBool(StatusOrErr.takeError()))
  //   return nullptr;

  // llvm::APFloat::opStatus Result = *StatusOrErr;
  // if ((Result & llvm::APFloat::opOverflow) ||
  //     ((Result & llvm::APFloat::opUnderflow) && Val.isZero())) {
  //   unsigned Diagnostic;
  //   llvm::SmallString<20> Buffer;
  //   if (Result & llvm::APFloat::opOverflow) {
  //     Diagnostic = clang::diag::warn_float_overflow;
  //     llvm::APFloat::getLargest(Format).toString(Buffer);
  //   } else {
  //     Diagnostic = clang::diag::warn_float_underflow;
  //     llvm::APFloat::getSmallest(Format).toString(Buffer);
  //   }

  //   SemaRef.getCxxSema().Diags.Report(Loc, Diagnostic)
  //     << CxxAST.DoubleTy
  //     << llvm::StringRef(Buffer.data(), Buffer.size());
  // }

  // clang::QualType FloatType = CxxAST.FloatTy;
  // if (S->Suffix.IsDouble)
  //   FloatType = CxxAST.DoubleTy;

  // bool isExact = (Result == llvm::APFloat::opOK);
  // return clang::FloatingLiteral::Create(CxxAST, Val, isExact, FloatType, Loc);
  llvm_unreachable("exponent literal not implemented yet.");
}

/// This was copied from clang/lib/lex/LiteralSupport.cpp:91, and modified.
static unsigned processCharEscape(Sema &SemaRef, clang::SourceLocation Loc,
    const char *&ThisTokBuf, const char *ThisTokEnd,
    bool &HadError, unsigned CharWidth) {

  // Skip the '\' char.
  ++ThisTokBuf;

  // We know that this character can't be off the end of the buffer, because
  // that would have been \", which would not have been the end of string.
  unsigned ResultChar = *ThisTokBuf++;

  switch (ResultChar) {
  // These map to themselves.
  case '\\': case '\'': case '"': case '?': break;

    // These have fixed mappings.
  case 'a':
    ResultChar = 7;
    break;
  case 'b':
    ResultChar = 8;
    break;
  case 'e':
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << "e";
    ResultChar = 27;
    break;
  case 'E':
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << "E";
    ResultChar = 27;
    break;
  case 'f':
    ResultChar = 12;
    break;
  case 'n':
    ResultChar = 10;
    break;
  case 'r':
    ResultChar = 13;
    break;
  case 't':
    ResultChar = 9;
    break;
  case 'v':
    ResultChar = 11;
    break;
  case 'x': { // Hex escape.
    ResultChar = 0;
    if (ThisTokBuf == ThisTokEnd || !clang::isHexDigit(*ThisTokBuf)) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_hex_escape_no_digits)
          << "x";
      HadError = true;
      break;
    }

    // Hex escapes are a maximal series of hex digits.
    bool Overflow = false;
    for (; ThisTokBuf != ThisTokEnd; ++ThisTokBuf) {
      int CharVal = llvm::hexDigitValue(ThisTokBuf[0]);
      if (CharVal == -1) break;
      // About to shift out a digit?
      if (ResultChar & 0xF0000000)
        Overflow = true;
      ResultChar <<= 4;
      ResultChar |= CharVal;
    }

    // See if any bits will be truncated when evaluated as a character.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      Overflow = true;
      ResultChar &= ~0U >> (32-CharWidth);
    }

    // Check for overflow.
    if (Overflow)   // Too many digits to fit in
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_escape_too_large)
          << 0;
    break;
  }
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7': {
    // Octal escapes.
    --ThisTokBuf;
    ResultChar = 0;

    // Octal escapes are a series of octal digits with maximum length 3.
    // "\0123" is a two digit sequence equal to "\012" "3".
    unsigned NumDigits = 0;
    do {
      ResultChar <<= 3;
      ResultChar |= *ThisTokBuf++ - '0';
      ++NumDigits;
    } while (ThisTokBuf != ThisTokEnd && NumDigits < 3 &&
             ThisTokBuf[0] >= '0' && ThisTokBuf[0] <= '7');

    // Check for overflow.  Reject '\777', but not L'\777'.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_escape_too_large)
          << 1;
      ResultChar &= ~0U >> (32-CharWidth);
    }
    break;
  }

    // Otherwise, these are not valid escapes.
  case '(': case '{': case '[': case '%':
    // GCC accepts these as extensions.  We warn about them as such though.
    // TODO: We need to determine if we need to suppor this or not.
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << std::string(1, ResultChar);
    break;
  default:

    if (clang::isPrintable(ResultChar))
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
          << std::string(1, ResultChar);
    else
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
          <<  "x" + llvm::utohexstr(ResultChar);
    break;
  }

  return ResultChar;
}

/// readCharacter attempts to read the next character in a literal value
/// if there's an error true is returned, and otherwise the result is false.
///
/// The Iter will be advanced to the position of the next character in the
/// string.
///
/// This function will indicate an error when Iter == End. It's important to
/// set test that value before the next call to this function.
static bool readCharacter(Sema &SemaRef, clang::SourceLocation Loc,
                          const char *&Iter, const char *End, unsigned &Value,
                          bool &Escape) {
  assert (Iter <= End && "Invalid character");

  // Process an escape sequence if we encounter one, otherwise do a simple
  // character literal read.
  if (*Iter == '\\') {
    Escape = true;
    bool DidError = false;
    Value = processCharEscape(SemaRef, Loc, Iter, End, DidError, 8u);
    return DidError;
  } else {
    Escape = false;
    Value = *Iter;
    ++Iter;
  }

  return false;
}

static clang::CharacterLiteral *
createCharLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                  Token T, clang::SourceLocation Loc) {
  auto Spelling = T.getSpelling();
  assert(Spelling[0] == '\'' && "atom is not a character");

  Spelling = Spelling.substr(1, Spelling.size());
  Spelling = Spelling.substr(0, Spelling.find_last_of('\''));
  if (Spelling.empty()) {
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_empty_character);
    return nullptr;
  }

  llvm::SmallString<16> CharBuffer;
  CharBuffer.append(Spelling.begin(), Spelling.end());
  unsigned Character = 0;
  bool EscapeSeq;
  const char *CharBegin = CharBuffer.data();
  const char *CharEnd = CharBuffer.data() + CharBuffer.size();
  if (readCharacter(SemaRef, Loc, CharBegin, CharEnd, Character, EscapeSeq)) {
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
      << CharBuffer.data();
    return nullptr;
  }

  // A multi-character character constant is actually valid, so we'll just
  // warn and move on.
  if (!EscapeSeq && Spelling.size() > 1) {
    unsigned DiagID =
      SemaRef.getCxxSema().Diags.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                    "multi-character character constant");
    SemaRef.getCxxSema().Diags.Report(Loc, DiagID);
  }

  return new (CxxAST) clang::CharacterLiteral(Character,
                                              clang::CharacterLiteral::UTF8,
                                              SemaRef.DefaultCharTy, Loc);
}

static clang::CharacterLiteral *
createUTF8Literal(clang::ASTContext &CxxAST, Sema &SemaRef,
                  Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling();
  Spelling = Spelling.substr(Spelling.find_first_not_of("0c"), Spelling.size());
  unsigned Value = (unsigned)std::stoi(Spelling, 0, 16);

  // FIXME: warn on overflow?

  return new (CxxAST)
    clang::CharacterLiteral(Value, clang::CharacterLiteral::UTF8,
                            CxxAST.Char8Ty, Loc);
}

static clang::CharacterLiteral *
createUnicodeLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                     Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling();
  Spelling = Spelling.substr(Spelling.find_first_not_of("0u"), Spelling.size());
  unsigned Value = (unsigned)std::stoi(Spelling, 0, 16);

  // FIXME: warn on overflow?

  clang::CharacterLiteral::CharacterKind CharKind;
  clang::QualType CharType;
  if (Value <= 0xFF) {
    CharKind = clang::CharacterLiteral::UTF8;
    CharType = CxxAST.Char8Ty;
  } else if (Value <= 0xFFFF) {
    CharKind = clang::CharacterLiteral::UTF16;
    CharType = CxxAST.Char16Ty;
  } else if (Value <= 0xFFFFFFFF) {
    CharKind = clang::CharacterLiteral::UTF32;
    CharType = CxxAST.Char32Ty;
  } else {
    return nullptr;
  }

  return new (CxxAST) clang::CharacterLiteral(Value, CharKind, CharType, Loc);
}

static clang::StringLiteral *
createStringLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                    Token T, const Syntax *StrNode) {
  auto Str = T.getSpelling();
  clang::Token CTok;
  CTok.startToken();
  CTok.setKind(clang::tok::utf8_string_literal);
  CTok.setLocation(StrNode->getLocation());
  CTok.setLength(Str.size());
  llvm::SmallVector<clang::Token, 1> StrTokens;
  StrTokens.push_back(CTok);
  clang::StringLiteralParser StrParser(StrTokens, CxxAST.getSourceManager(),
                                       CxxAST.getLangOpts(),
                                       CxxAST.getTargetInfo(),
                                       &SemaRef.getCxxSema().Diags);

  clang::QualType StrTy = CxxAST.getStringLiteralArrayType(
                          SemaRef.DefaultCharTy, StrParser.GetNumStringChars());
  auto EncodingKind = clang::StringLiteral::Ascii;
  // if (SemaRef.insideAttributeExpr()) {
  //   EncodingKind = ;
  // }
  return clang::StringLiteral::Create(CxxAST, StrParser.GetString(),
                                      EncodingKind,
                                      false, StrTy, StrNode->getLocation());
}

static clang::CXXBoolLiteralExpr *
createBoolLiteral(clang::ASTContext &CxxAST, Token T,
                  clang::SourceLocation Loc) {
  return new (CxxAST) clang::CXXBoolLiteralExpr(T.hasKind(tok::TrueKeyword),
                                                CxxAST.BoolTy, Loc);
}

static clang::CXXNullPtrLiteralExpr *
createNullLiteral(clang::ASTContext &CxxAST, clang::SourceLocation Loc) {
  return new (CxxAST) clang::CXXNullPtrLiteralExpr(CxxAST.NullPtrTy, Loc);
}

clang::Expr *Elaborator::elaborateLiteralExpression(const LiteralSyntax *S) {
  // Other things we need in the future.
  // DecimalExponent
  // HexadecimalCharacter
  // UnicodeCharacter
  const Token& Tok = S->getToken();
  switch (Tok.getKind()) {
  case tok::BitAndKeyword:{
    SemaRef.buildBitAnd();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;
  case tok::BitOrKeyword:{
    SemaRef.buildBitOr();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;
  case tok::BitXOrKeyword:{
    SemaRef.buildBitXOr();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;
  case tok::BitShlKeyword:{
    SemaRef.buildBitShl();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;
  case tok::BitShrKeyword:{
    SemaRef.buildBitShr();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;
  case tok::BitNotKeyword:{
    SemaRef.buildBitNot();
    return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
  }
  break;

  case tok::DecimalInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S);
  case tok::DecimalFloat:
    return createFloatLiteral(getCxxContext(), SemaRef, S);
  case tok::BinaryInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S, 2);
  case tok::HexadecimalInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S, /*Base=*/16);
  case tok::HexadecimalFloat:
    llvm_unreachable("Hexadecimal float not implemented");
    break;
  case tok::Character:
    return createCharLiteral(getCxxContext(), SemaRef, Tok, S->getLocation());
  case tok::String:
    return createStringLiteral(getCxxContext(), SemaRef, Tok, S);
  case tok::TrueKeyword:
  case tok::FalseKeyword:
    return createBoolLiteral(getCxxContext(), Tok, S->getLocation());
  case tok::NullKeyword:
    return createNullLiteral(getCxxContext(), S->getLocation());
  case tok::VoidKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().VoidTy, Tok.getLocation());
  case tok::BoolKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().BoolTy, Tok.getLocation());
  case tok::ByteKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().UnsignedCharTy,
                                 Tok.getLocation());
  case tok::IntKeyword:
    // FIXME: Support arbitrary length integer types via the lexer.
    return SemaRef.buildTypeExpr(getCxxContext().IntTy,
                                 Tok.getLocation());
  case tok::FloatKeyword:
    // FIXME: Support arbitrary length floating point types vie the lexer.
    return SemaRef.buildTypeExpr(getCxxContext().DoubleTy,
                                 Tok.getLocation());
  case tok::TypeKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().CppxKindTy,
                                 Tok.getLocation());
  default:
    break;
  }
  S->dump();
  llvm_unreachable("Not implemented");
}

void Elaborator::elaborateDefinition(const Syntax *S) {
  auto Decl = SemaRef.getCurrentScope()->findDecl(S);
  if (!Decl)
    return;
  // Attempt to process the current declaration again.
  Sema::DeclarationElaborationRAII DeclElab(SemaRef, Decl);
  elaborateDefinitionInitialization(Decl);
}

void Elaborator::elaborateDefinitionInitialization(Declaration *D) {
  // If the current phase isn't typing then bail.
  if (phaseOf(D) != Phase::Typing)
    return;
  if (D->isFieldDecl())
    return elaborateFieldInit(D);
  if (D->isVariableDecl())
    return elaborateVarDef(D);
  if (D->isFunctionDecl())
    return elaborateFunctionDef(D);

  llvm_unreachable("Elaboration for this kind of declaration isn't "
                   "implemented yet.");
}

void Elaborator::elaborateVarDef(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  if (!D->getCxx())
    return;


  // If this isn't early elaboration then we have to actually track it.
  // if (!IsEarly)
  //   ElabTracker.init(D);

  // Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);
  // clang::Expr *InitExpr = nullptr;
  // clang::VarDecl *VD = nullptr;
  // Sema::DeclInitializationScope ClangInitScope(SemaRef, D);
  // bool NeedsConstEvaluation = false;
  // if (D->defines<clang::VarTemplateDecl>()) {
  //   if (SemaRef.checkForRedefinition<clang::VarTemplateDecl>(D))
  //     return;

  //   // We need to attempt to re-enter the template context for this variable.
  //   OptResumeScope.Init(D->SavedScope, D->Op);
  //   clang::VarTemplateDecl *VTD = cast<clang::VarTemplateDecl>(D->Cxx);
  //   VD = VTD->getTemplatedDecl();
  // } else {
  //   if (SemaRef.checkForRedefinition<clang::VarDecl>(D))
  //     return;
  //   VD = cast<clang::VarDecl>(D->Cxx);
  // }

  // if (VD->isConstexpr())
  //   NeedsConstEvaluation = true;
  clang::VarDecl *VD = cast<clang::VarDecl>(D->getCxx());
  auto *Def = D->asDef();
  if (!Def)
    return;
  if (!D->getInitializer()) {
    // if (isa<clang::ParmVarDecl>(VD))
    //   return;
    // FIXME: We probably want to synthesize some kind of initializer here.
    // Not quite sure how we want to do this.
    //
    // FIXME: What if D has type auto? Surely this is an error. For example:
    //
    //    x : auto
    //
    // declares an undeduced-type variable with no initializer. Presumably
    // this should be an error.

    // This handles implcit initialization/constructor calls for variables
    // that don't have a = sign on first use, but have a type.
    // That includes complex types.
    getCxxSema().ActOnUninitializedDecl(VD);
    getCxxSema().FinalizeDeclaration(VD);
    return;
  }
  // if (D->defines<clang::VarTemplateDecl>()) {
  //   // I may need to revisit this in the furture becaus this might not be
  //   // the right thing to do in this case.
  //   VD->setInit(InitExpr);
  // } else {

  // if (D->isDeclaredWithinClass() && !VD->isInlineSpecified()
  //     && (!VD->getType().isConstant(Context.CxxAST) && !VD->isConstexpr())) {
  //   SemaRef.Diags.Report(D->IdDcl->getLoc(),
  //                       clang::diag::err_in_class_initializer_non_const);
  //   return;
  // }
  // if (auto LS = dyn_cast<ListSyntax>(Def->getInitializer())) {
  // }
  clang::Expr *InitExpr = nullptr;;
  if (auto CtorArgs = dyn_cast<EnclosureSyntax>(D->getInitializer())) {
    if (CtorArgs->getOperand()) {
      llvm_unreachable("Constructor with arguments not implemented yet.");
    } else
      InitExpr = elaborateExplicitDefaultCtorCall(VD, CtorArgs);
  } else {
    InitExpr = elaborateExpression(D->getInitializer());
  }
  if (!InitExpr)
    return;
  // Update the initializer.
  getCxxSema().AddInitializerToDecl(VD, InitExpr, /*DirectInit=*/false);
}

clang::Expr *Elaborator::elaborateExplicitDefaultCtorCall(clang::VarDecl *D,
                                                    const EnclosureSyntax *ES) {
  llvm::SmallVector<clang::Expr *, 8> Args;
  auto TInfo = D->getTypeSourceInfo();
  clang::ParsedType PTy;
  PTy = getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
  clang::ExprResult ConstructorExpr =
      getCxxSema().ActOnCXXTypeConstructExpr(PTy, clang::SourceLocation(),
                                             Args, clang::SourceLocation(),
                                             /*ListInitialization*/false);
  if (!ConstructorExpr.get()) {
    Error(ES->getOpen().getLocation(), "invalid constructor");
    return nullptr;
  }
  auto Temp = getCxxSema().TemporaryMaterializationConversion(
      ConstructorExpr.get());
  return Temp.get();
}

void Elaborator::elaborateFieldInit(Declaration *D) {
  assert(D && "Missing Declaration.");
  if (!D->hasInitializer())
    return;
  D->CurrentPhase = Phase::Initialization;
  getCxxSema().ActOnStartCXXInClassMemberInitializer();

  using EEC = clang::Sema::ExpressionEvaluationContext;
  clang::EnterExpressionEvaluationContext EEContext(getCxxSema(),
                                                    EEC::PotentiallyEvaluated,
                                                    D->getCxx());
  clang::Expr *InitExpr = elaborateExpression(D->getInitializer());
  if (!InitExpr) {
    return;
  }

  getCxxSema().ActOnFinishCXXInClassMemberInitializer(D->getCxx(),
                                                      D->Def->getLocation(),
                                                      InitExpr);
}

void Elaborator::elaborateFunctionDef(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;

  if (!D->getCxx())
    return;
  if (!D->Init)
    return;

  // We saved the parameter scope while elaborating this function's type,
  // so push it on before we enter the function scope.
  // assert(D->Decl->declaresFunction());
  Declarator *FnDclrtr = D->getFirstDeclarator(Declarator::Function);
  Scope *ParamScope = FnDclrtr->DeclInfo.ParamScope;
  ResumeScopeRAII FnDclScope(SemaRef, ParamScope, ParamScope->getTerm());

  Declaration *CurrentDeclaration = SemaRef.getCurrentDecl();
  // Entering clang scope. for function definition.
  SemaRef.enterClangScope(clang::Scope::FnScope |clang::Scope::DeclScope |
                          clang::Scope::CompoundStmtScope);
  clang::Decl *FuncDecl =
    SemaRef.getCxxSema().ActOnStartOfFunctionDef(SemaRef.getCurClangScope(),
                                                 D->getCxx());


  Sema::ScopeRAII FnScope(SemaRef, Scope::Function, D->Init);
  SemaRef.setCurrentDecl(D);
  auto Enclosure = dyn_cast<EnclosureSyntax>(D->Init);
  clang::Stmt *Body = elaborateEnclosureStmt(Enclosure);
  SemaRef.setClangDeclContext(cast<clang::FunctionDecl>(D->getCxx()));
  SemaRef.getCxxSema().ActOnFinishFunctionBody(FuncDecl, Body);

  // Return the current decl to whatever it was before.
  SemaRef.setCurrentDecl(CurrentDeclaration);
}

/// This creates the correct expression in order to correctly reference
/// a type, variable, single function, or any thing else that may
/// be returned from by the look up.
static clang::Expr *BuildReferenceToDecl(Sema &SemaRef,
                                         clang::SourceLocation Loc,
                                         clang::LookupResult &R,
                                         bool IsKnownOverload = false);

clang::Expr *buildIdExpr(Sema &SemaRef,
    llvm::StringRef Id,
    clang::SourceLocation Loc) {
  // Check for builtin types.

  // Easy case of a wildcard expression.
  if (Id == "_")
    return clang::CppxWildcardExpr::Create(SemaRef.getCxxAST(), Loc);

  // Doing variable lookup.
  clang::IdentifierInfo *II = &SemaRef.getCxxAST().Idents.get(Id);
  clang::LookupResult R(SemaRef.getCxxSema(), {{II}, Loc},
                        clang::Sema::LookupOrdinaryName);
  R.setTemplateNameLookup(true);

  if (SemaRef.isQualifiedLookupContext()) {
    SemaRef.lookupQualifiedName(R);
  } else {
    auto BuiltinMapIter = SemaRef.BuiltinTypes.find(Id);
    if (BuiltinMapIter != SemaRef.BuiltinTypes.end())
      return SemaRef.buildTypeExpr(BuiltinMapIter->second, Loc);
    if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
      SemaRef.getCxxSema().Diags.Report(Loc,
                              clang::diag::err_identifier_not_declared_in_scope)
                                        << II->getName();
      return nullptr;
    }
  }


  // SemaRef.lookupUnqualifiedName(R);
  R.resolveKind();
  switch (R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
      // Need to figure out if the potential overload is a member function
      // or not.
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(),
                                                 R.getNamingClass(),
                                                clang::NestedNameSpecifierLoc(),
                                                 R.getLookupNameInfo(),
                                                 /*ADL=*/true, true,
                                                 R.begin(),
                                                 R.end());
  }
  case clang::LookupResult::Found:
    return BuildReferenceToDecl(SemaRef, Loc, R, false);
  case clang::LookupResult::NotFoundInCurrentInstantiation:
  case clang::LookupResult::NotFound: {
    SemaRef.getCxxSema().Diags.Report(Loc,
                              clang::diag::err_undeclared_var_use)
                              << Id;
    return nullptr;
  }
  case clang::LookupResult::FoundUnresolvedValue:
    // FIXME: I need to figure out when this can occur, then create
    // that situiation wihtin a test and build appropriate error message.
    // I suspect that this may have something to do with variable template
    // declarations.
    llvm_unreachable("Not sure how handle unresolved values.");
  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }
  return nullptr;
}

clang::Expr *Elaborator::elaborateIdentifierExpression(const IdentifierSyntax *S) {
  return buildIdExpr(SemaRef, S->getSpelling(), S->getLocation());
}

clang::Expr *Elaborator::elaborateCallExpression(const CallSyntax *S) {
  // Checking for special case functions.
  auto Callee = S->getApplicant();
  auto LS = dyn_cast<LiteralSyntax>(Callee);
  if (LS) {
    if (LS->getSpelling() == "integer")
      return elaborateIntegerMetaFunction(S);
    if (LS->getSpelling() == "real")
      return elaborateRealMetaFunction(S);
    if (LS->getSpelling() == "character")
      return elaborateCharacterMetaFunction(S);
  }

  // Handling non special case elaboration.
  clang::Expr *IdExpr = doElaborateExpression(Callee);
  if (!IdExpr)
    return IdExpr;
  const Syntax *ArgSyn = S->getArguments();
  if (!ArgSyn)
    llvm_unreachable("Missing calls arguments from tree.");
  auto ArgEnclosure = dyn_cast<EnclosureSyntax>(ArgSyn);
  if (!ArgEnclosure) {
    llvm::outs() << "Dumping unknown call type.\n";
    S->dump();
    llvm_unreachable("Invalid enclosure syntax");
  }
  if (ArgEnclosure->isParenEnclosure()) {
      return elaborateFunctionCall(IdExpr, S);
  } else if (ArgEnclosure->isBracketEnclosure()) {
    if (!ArgEnclosure->getOperand())
      // I'm not really sure this can happen.
      llvm_unreachable("Invalid empty template instantiation.");

    auto LS = cast<ListSyntax>(ArgEnclosure->getOperand());
    if (IdExpr->getType()->isTemplateType())
        return elaborateClassTemplateSelection(IdExpr, ArgEnclosure, LS);
    if (!isa<clang::OverloadExpr>(IdExpr))
      return elaborateArraySubscriptExpr(IdExpr, LS);


    return elabotateTemplateInstantiationWithArgs(ArgEnclosure, IdExpr, LS);


  } else if (ArgEnclosure->isBraceEnclosure()) {
    llvm::outs() << "Dumping brace\n";
    S->dump();
    llvm_unreachable("Not sure what brace syntax is.");
  } else {
    llvm::outs() << "Dumping unknown call type.\n";
    S->dump();
    llvm_unreachable("Special kind of unknown call.");
  }

//   if (const auto *DRE = dyn_cast<clang::DeclRefExpr>(LHS)) {
//     if (DRE->getType()->isArrayType())
//       return elaborateArraySubscriptExpr(LHS, S);
//   }

//   if (auto *ULE = dyn_cast<clang::UnresolvedLookupExpr>(LHS)) {
//     if (auto LS = dyn_cast<ListSyntax>(S->getRightOperand())) {
//       if (LS->isBracketList())
//         return elabotateTemplateInstantiationWithArgs(ULE, LS);
//       return elaborateFunctionCall(ULE, S);
//     } else {
//       llvm_unreachable("This is a VERY strange kind of function call!");
//     }
//   }

//   if (const auto *ASE = dyn_cast<clang::ArraySubscriptExpr>(LHS)) {
//     return elaborateArraySubscriptExpr(LHS, S);
//   }

//   // TODO: I need to figure out and dispatch all of the different possible
//   // situations that could occur here.
//   // 1. Template instantiation
//   // 2. Function Call - Needs verifications
//   // 3. Array declaration - Needs verifications
//   // 4. Template declarations - Needs verifications
//   // 5. Block attached to something possibly? - Needs verifications
//   if (LHS->getType()->isKindType()) {

//     // This could also be template instantiation.
//     llvm_unreachable("Constructor not implemented yet");
//   }
//   if (LHS->getType()->isTemplateType()) {
//     if (auto LS = dyn_cast<ListSyntax>(S->getRightOperand())) {
//       // Handle class template instantiation.
//       if (LS->isBracketList()) {
//           return elaborateClassTemplateSelection(LHS, LS);
//       }
//     }
//     // I need to mark this an error!.
//     Error(LHS->getExprLoc(), "invalid use of a template.");
//     return nullptr;
//   }

//   llvm::errs() << "---------------------------------------------------------\n";
//   S->dump();
//   llvm::errs() << "---------------------------------------------------------\n";
//   LHS->dump();
//   llvm_unreachable("apply expression Not implemented yet!?\n");
}

clang::Expr *Elaborator::elaboratePrefixExpression(const PrefixSyntax *S) {
  auto Operand = elaborateExpression(S->getOperand());
  if (!Operand)
    return Operand;
  clang::SourceLocation Loc = S->getLocation();
  clang::QualType Ty = Operand->getType();
  if (Ty->isTypeOfTypes()) {
    if (S->getOperation().hasKind(tok::Caret)) {
      // FIXME: how can we assert that this is a declarator?
      // If this apears within a declarator then it must be a type.
      auto TInfo = SemaRef.getTypeSourceInfoFromExpr(Operand,
                                                     Operand->getExprLoc());
      if (!TInfo)
        return nullptr;

      clang::QualType RetType = getCxxContext().getPointerType(TInfo->getType());
      return SemaRef.buildTypeExpr(RetType, Loc);
    }
    getCxxSema().Diags.Report(Loc, clang::diag::err_invalid_type_operand)
                              << 0/*unary*/;
    return nullptr;
  }

  auto OpIter = SemaRef.UnaryPrefixOpMap.find(S->getOperation().getSpelling());
  clang::UnaryOperatorKind Op;
  if (OpIter == SemaRef.UnaryPrefixOpMap.end()) {
    Error(Loc, "invalid unary operator");
    return nullptr;
  } else {
    Op = OpIter->second;
  }

  return CxxSema.BuildUnaryOp(/*scope*/nullptr, Loc, Op, Operand).get();
}

clang::Expr *Elaborator::elaboratePostfixExpression(const PostfixSyntax *S) {
  // llvm_unreachable("elaboratePostfixExpression not implemented yet");
  auto Operand = elaborateExpression(S->getOperand());
  if (!Operand)
    return Operand;
  clang::SourceLocation Loc = S->getLocation();
  clang::QualType Ty = Operand->getType();
  if (Ty->isTypeOfTypes() || Ty->isNamespaceType() || Ty->isTemplateType()) {
    getCxxSema().Diags.Report(Loc, clang::diag::err_invalid_type_operand)
                              << 0/*unary*/;
    return nullptr;
  }

  auto OpIter = SemaRef.UnaryPostfixOpMap.find(S->getOperation().getSpelling());
  clang::UnaryOperatorKind Op;
  if (OpIter == SemaRef.UnaryPostfixOpMap.end()) {
    Error(Loc, "invalid unary operator");
    return nullptr;
  } else {
    Op = OpIter->second;
  }
  return CxxSema.BuildUnaryOp(/*scope*/nullptr, Loc, Op, Operand).get();
}

clang::Expr *Elaborator::elaborateInfixExpression(const InfixSyntax *S) {
  auto LHS = elaborateExpression(S->getOperand(0));
  if (!LHS)
    return nullptr;

  if (S->getOperation().hasKind(tok::Dot))
    return elaborateMemberAccess(LHS, S);

  auto RHS = elaborateExpression(S->getOperand(1));
  if (!RHS)
    return nullptr;

  auto OpIter = SemaRef.BinOpMap.find(S->getOperation().getSpelling());
  if (OpIter == SemaRef.BinOpMap.end()) {
    Error(S->getLocation(), "invalid binary operator");
    return nullptr;
  }
  clang::ExprResult Res = SemaRef.getCxxSema().BuildBinOp(/*Scope=*/nullptr,
                                                          S->getLocation(),
                                                          OpIter->second, LHS,
                                                          RHS);
  return Res.get();
}

clang::Expr *Elaborator::elaborateIndexExpression(const IndexSyntax *S) {
  llvm_unreachable("elaborateIndexExpression not implemented yet");
}

clang::Expr *Elaborator::elaboratePairExpression(const PairSyntax *S) {
  llvm_unreachable("elaboratePairExpression not implemented yet");
}

clang::Expr *Elaborator::elaborateTripleExpression(const TripleSyntax *S) {
  llvm_unreachable("elaborateTripleExpression not implemented yet");
}

clang::Expr *Elaborator::elaborateListExpression(const ListSyntax *S) {
  // We probably never get here in normal elaboration, but syntactically
  // valid errors may cause lists to show up where they should not.
  SemaRef.getCxxSema().Diags.Report(S->getLocation(),
                                    clang::diag::err_failed_to_translate_expr);
  return nullptr;
}

clang::Expr *Elaborator::elaborateSequenceExpression(const SequenceSyntax *S) {
  llvm_unreachable("elaborateSequenceExpression not implemented yet");
}



clang::Expr *BuildReferenceToDecl(Sema &SemaRef,
                                  clang::SourceLocation Loc,
                                  clang::LookupResult &R,
                                  bool IsKnownOverload) {
  std::string Name = R.getLookupName().getAsString();
  if (IsKnownOverload) {
    llvm_unreachable("We haven't implemented overload references yet.");
  }
  // assert(FoundDecl && "Incorrectly set found declaration.");
  if (clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>()) {
    // clang::QualType FoundTy = VD->getType();
    // If the user annotated the DeclRefExpr with an incorrect type.
    // if (!Ty.isNull() && Ty != FoundTy) {
    //   SemaRef.getCxxSema().Diags.Report(Loc,
    //     clang::diag::err_type_annotation_mismatch) << FoundTy << Ty;
    //   return nullptr;
    // }

    if (isa<clang::FieldDecl>(VD)) {
      // FIXME: Write a test for this!
      // Building this access.
      // clang::FieldDecl* Field = cast<clang::FieldDecl>(VD);
      // clang::RecordDecl* RD = Field->getParent();
      // clang::QualType ThisTy(RD->getTypeForDecl(), 0);
      // clang::QualType ThisPtrTy = SemaRef.getContext().CxxAST.getPointerType(
      //                                                                 ThisTy);
      // clang::Expr* This = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
      //                                                           ThisPtrTy,
      //                                                           true);

      // clang::DeclAccessPair FoundDecl = clang::DeclAccessPair::make(Field,
      //                                                     Field->getAccess());
      // clang::CXXScopeSpec SS;
      // clang::ExprResult MemberExpr
      //     = SemaRef.getCxxSema().BuildFieldReferenceExpr(This, true,
      //                                                 clang::SourceLocation(),
      //                                                     SS, Field, FoundDecl,
      //                                                     DNI);
      // clang::Expr *Ret = MemberExpr.get();
      // if (!Ret) {
      //   SemaRef.Diags.Report(Loc, clang::diag::err_no_member)
      //       << Field << ThisTy;
      // }
      // ExprMarker(Context.CxxAST, SemaRef).Visit(Ret);
      // return Ret;
      llvm_unreachable("Reference to a Field decl not implemented yet.");
    }
    // Need to check if the result is a CXXMethodDecl because that's a
    // ValueDecl.
    if(isa<clang::CXXMethodDecl>(VD)) {
      // FIXME: Write a test for this!
      // clang::CXXScopeSpec SS;
      // clang::SourceLocation Loc;
      // // This may need to change into a different type of function call
      // // base on given arguments, because this could be an issue.
      // return SemaRef.getCxxSema().BuildPossibleImplicitMemberExpr(SS, Loc, R,
      //                                                             nullptr,
      //                                       SemaRef.getCurClangScope()).get();
      // llvm_unreachable("Reference to a CXX Method decl not implemented yet.");
    }

    if(isa<clang::FunctionDecl>(VD)) {
      // // Correctly rebuilding the declaration name info.
      const clang::DeclarationNameInfo &DNI = R.getLookupNameInfo();
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(),
                                                  R.getNamingClass(),
                                              clang::NestedNameSpecifierLoc(),
                                                  DNI,
                                                  /*ADL=*/true, true,
                                                  R.begin(), R.end());
    }
    // Simply assuming that this is a variable declaration.
    clang::QualType ResultType = VD->getType();
    if (ResultType.getTypePtr()->isReferenceType())
      ResultType = ResultType.getTypePtr()->getPointeeType();

    clang::ExprValueKind ValueKind =
      SemaRef.getCxxSema().getValueKindForDeclReference(ResultType,
                                                        VD, Loc);
    clang::DeclRefExpr *DRE =
      SemaRef.getCxxSema().BuildDeclRefExpr(VD, ResultType, ValueKind,
                                            R.getLookupNameInfo(),
                                            clang::NestedNameSpecifierLoc(),
                                            VD, clang::SourceLocation(),
                                            nullptr);
    ExprMarker(SemaRef.getCxxAST(), SemaRef).Visit(DRE);
    return DRE;
  }

  // Processing the case when the returned result is a type.
  if (const clang::TagDecl *TD = R.getAsSingle<clang::TagDecl>())
    return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);

  if (clang::ClassTemplateDecl *CTD
                                = R.getAsSingle<clang::ClassTemplateDecl>())
    return SemaRef.buildTemplateType(CTD, Loc);

  if (auto *NS = R.getAsSingle<clang::CppxNamespaceDecl>())
    return SemaRef.buildNSDeclRef(NS, Loc);


  if (auto *TD = R.getAsSingle<clang::TypeDecl>())
    return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);

  if (auto *TD = R.getAsSingle<clang::TemplateDecl>())
    return SemaRef.buildTemplateType(TD, Loc);

  SemaRef.getCxxSema().Diags.Report(Loc,
                              clang::diag::err_identifier_not_declared_in_scope)
                                   << Name;
  return nullptr;
}



// clang::Expr *Elaborator::elaborateListExpression(const ListSyntax *S) {
//   llvm_unreachable("Not implemented");
// }

// clang::Expr *Elaborator::elaborateSeqExpression(const SeqSyntax *S) {
//   llvm_unreachable("Not implemented");
// }

// clang::Expr *Elaborator::elaborateUnaryExpression(const UnarySyntax *S) {
//   auto Operand = elaborateExpression(S->getOperand());
//   clang::SourceLocation Loc = S->getLocation();

//   if (!Operand)
//     return nullptr;
//   clang::QualType Ty = Operand->getType();
//   if (Ty->isTypeOfTypes()) {
//     if (S->getOperator().hasKind(tok::Caret)) {
//       // FIXME: how can we assert that this is a declarator?
//       // If this apears within a declarator then it must be a type.
//       auto TInfo = SemaRef.getTypeSourceInfoFromExpr(Operand,
//                                                      Operand->getExprLoc());
//       if (!TInfo)
//         return nullptr;

//       clang::QualType RetType = getCxxContext().getPointerType(TInfo->getType());
//       return SemaRef.buildTypeExpr(RetType, Loc);
//     }
//     getCxxSema().Diags.Report(Loc, clang::diag::err_invalid_type_operand)
//                               << 0/*unary*/;
//     return nullptr;
//   }

//   if (S->getOperator().hasKind(tok::Caret)) {
//     getCxxSema().Diags.Report(Loc, clang::diag::err_prefix_caret_on_non_type);
//     return nullptr;
//   }

//   auto OpIter = SemaRef.UnaryOpMap.find(S->getOperatorSpelling());
//   clang::UnaryOperatorKind Op;
//   if (OpIter == SemaRef.UnaryOpMap.end()) {
//     if (S->getOperator().hasKind(tok::PlusPlus))
//       Op = S->isPostfix() ? clang::UO_PostInc : clang::UO_PreInc;
//     else if (S->getOperator().hasKind(tok::MinusMinus))
//       Op = S->isPostfix() ? clang::UO_PostDec : clang::UO_PreDec;
//     else {
//       Error(Loc, "invalid unary operator");
//       return nullptr;
//     }
//   } else {
//     Op = OpIter->second;
//   }

//   return CxxSema.BuildUnaryOp(/*scope*/nullptr, Loc, Op, Operand).get();
// }

// clang::Expr *Elaborator::elaborateBinaryExpression(const BinarySyntax *S) {
//   if (S->isApplication()) {
//     const Syntax *LHSSyntax = S->getLeftOperand();
//     auto LS = dyn_cast<LiteralSyntax>(LHSSyntax);
//     if (LS) {
//       if (LS->getSpelling() == "integer")
//         return elaborateIntegerMetaFunction(S);
//       if (LS->getSpelling() == "real")
//         return elaborateRealMetaFunction(S);
//       if (LS->getSpelling() == "character")
//         return elaborateCharacterMetaFunction(S);
//     }
//   }

//   auto LHS = elaborateExpression(S->getLeftOperand());
//   if (!LHS)
//     return nullptr;

//   if (S->isApplication())
//     return elaborateApplyExpression(LHS, S);

//   if (S->isMemberAccess())
//     return elaborateMemberAccess(LHS, S);

//   auto RHS = elaborateExpression(S->getRightOperand());
//   if (!RHS)
//     return nullptr;

//   auto OpIter = SemaRef.BinOpMap.find(S->getOperatorSpelling());
//   if (OpIter == SemaRef.BinOpMap.end()) {

//     llvm::outs() << "Missing binary op spelling = " <<S->getOperatorSpelling() << "\n";
//     Error(S->getLocation(), "invalid binary operator");
//     return nullptr;
//   }
//   clang::ExprResult Res = SemaRef.getCxxSema().BuildBinOp(/*Scope=*/nullptr,
//                                                           S->getLocation(),
//                                                           OpIter->second, LHS,
//                                                           RHS);
//   return Res.get();
// }

// clang::Stmt *Elaborator::elaborateSeq(const SeqSyntax *S) {
//   SemaRef.getCxxSema().ActOnStartOfCompoundStmt(false);
//   Sema::ScopeRAII BlockScope(SemaRef, Scope::Block, S);

//   llvm::SmallVector<clang::Stmt *, 16> Results;
//   clang::SourceLocation StartLoc = S->getBeginLocation();
//   clang::SourceLocation EndLoc = S->getEndLocation();
//   StartLoc = EndLoc = S->getLocation();
//   for (const Syntax *Child : S->children()) {
//     clang::Stmt *Statement = elaborateStatement(Child);
//     if (!Statement)
//       continue;
//     Results.push_back(Statement);
//   }

//   clang::Stmt *Block = SemaRef.getCxxSema().ActOnCompoundStmt(StartLoc, EndLoc,
//                                           Results, /*isStmtExpr=*/false).get();
//   return Block;
// }

clang::Stmt *Elaborator::elaborateEnclosureStmt(const EnclosureSyntax *S) {
  if (!S->getOperand()) {
    getCxxSema().ActOnStartOfCompoundStmt(false);
    Sema::ScopeRAII BlockScope(SemaRef, Scope::Block, S);
    llvm::SmallVector<clang::Stmt *, 1> Results;
    clang::Stmt *Block = getCxxSema().ActOnCompoundStmt(S->getOpen().getLocation(),
                                                        S->getClose().getLocation(),
                                          Results, /*isStmtExpr=*/false).get();
    return Block;
  }

  auto Body = dyn_cast<ListSyntax>(S->getOperand());
  if (!Body) {
    S->getOperand()->dump();
    llvm_unreachable("Not sure if this is valid syntax or not.");
  }
  return elaborateListSyntaxStmt(Body);
}

clang::Stmt *Elaborator::elaborateListSyntaxStmt(const ListSyntax *S) {
  getCxxSema().ActOnStartOfCompoundStmt(true);
  Sema::ScopeRAII BlockScope(SemaRef, Scope::Block, S);

  llvm::SmallVector<clang::Stmt *, 16> Results;
  clang::SourceLocation StartLoc = S->getBeginLocation();
  clang::SourceLocation EndLoc = S->getEndLocation();
  StartLoc = EndLoc = S->getLocation();
  for (const Syntax *Child : S->children()) {
    clang::Stmt *Statement = elaborateStatement(Child);
    if (!Statement)
      continue;
    Results.push_back(Statement);
  }
  clang::Stmt *Block = getCxxSema().ActOnCompoundStmt(StartLoc, EndLoc,
                                          Results, /*isStmtExpr=*/false).get();
  return Block;
}

clang::Stmt *Elaborator::elaborateSequenceStmt(const SequenceSyntax *S) {
  llvm_unreachable("Working on it.");
}

clang::Stmt *Elaborator::elaborateStatement(const Syntax *S) {
  if (!S)
    return nullptr;

  if (auto DS = dyn_cast<DeclarationSyntax>(S))
    return elaborateDeclStmt(DS);

  if (auto U = dyn_cast<PrefixSyntax>(S)) {
    if (U->getOperation().hasKind(tok::ReturnKeyword))
      return elaborateReturnStmt(U);
  }

  if (const ControlSyntax *C = dyn_cast<ControlSyntax>(S))
    return elaborateControlStmt(C);
  if (const EnclosureSyntax *E = dyn_cast<EnclosureSyntax>(S))
    return elaborateEnclosureStmt(E);
  // else if (isa<ErrorSyntax>(S))
  //   return nullptr;

  // TODO: elaborate by syntax type: i.e. atom, etc. See GoldStmtElaborator
  // CUrrently only Handling expressions.
  // If the statement kind is unknown then simply punt and
  // elaborate an expression.
  clang::Expr *E = elaborateExpression(S);
  if (!E)
    return nullptr;
  auto CheckExpr = SemaRef.getCxxSema().CheckPlaceholderExpr(E);
  if (CheckExpr.isInvalid())
    return nullptr;
  auto ExprStmt =
    SemaRef.getCxxSema().ActOnExprStmt(CheckExpr.get(), /*discardedValue*/true);
  if (ExprStmt.isInvalid())
    return nullptr;

  return ExprStmt.get();
}

clang::Stmt *Elaborator::elaborateDeclStmt(const DeclarationSyntax *S) {
  auto D = buildDeclaration(S);
  if (!D) {
    Error(S->getLocation(), "invalid vairable declaration");
    return nullptr;
  }

  // This should already emit an error.
  auto TypedDecl = elaborateDeclarationTyping(D);
  if (!TypedDecl)
    return nullptr;
  elaborateDefinitionInitialization(D);
  auto DclGrp = SemaRef.getCxxSema().ConvertDeclToDeclGroup(D->getCxx());
  auto DclStmt = getCxxSema().ActOnDeclStmt(DclGrp, D->Def->getLocation(),
                                            D->Def->getLocation());
  return DclStmt.get();
}

clang::Stmt *Elaborator::elaborateReturnStmt(const PrefixSyntax *S) {
  assert(S->getOperation().hasKind(tok::ReturnKeyword)
         && "Invalid return statement");
  clang::Expr *Val = nullptr;

  if (S->getOperand())
    // if this elaborates to null, we'll just let clang::Sema handle the error
    Val = elaborateExpression(S->getOperand());

  auto ReturnResult = SemaRef.getCxxSema().ActOnReturnStmt(
    S->getOperation().getLocation(), Val, SemaRef.getCurClangScope());

  return ReturnResult.get();
}

clang::Stmt *Elaborator::elaborateControlStmt(const ControlSyntax *S) {
  switch (S->getControl().getKind()) {
  case tok::IfKeyword:
    return elaborateIfStmt(S);
  case tok::WhileKeyword:
    return elaborateWhileStmt(S);
  case tok::ForKeyword:
    return elaborateForStmt(S);
  case tok::DoKeyword:
    return elaborateDoStmt(S);
  case tok::LetKeyword:
    return elaborateLetStmt(S);

  default:
    break;
  }

  llvm_unreachable("invalid or unimplemented control structure");
}

clang::Stmt *Elaborator::elaborateIfStmt(const ControlSyntax *S) {
  assert(S->getControl().hasKind(tok::IfKeyword) && "invalid if syntax");

  // TODO: implement constexpr if
  bool IsConstExprIfStmt = false;

  // Elaborate the condition
  clang::Expr *ConditionExpr = elaborateExpression(S->getHead());
  if (!ConditionExpr)
    return nullptr;
  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr,
                                        S->getLocation(), ConditionExpr,
                                        IsConstExprIfStmt ?
                                        clang::Sema::ConditionKind::ConstexprIf:
                                        clang::Sema::ConditionKind::Boolean);

  // Get the block, this *must* be a pair.
  // if (isa<ErrorSyntax>(S->getBlock()))
    // return nullptr;
  const PairSyntax *Block = cast<PairSyntax>(S->getBody());
  const Syntax *ThenBlock = Block->getOperand(0);
  const Syntax *ElseBlock = Block->getOperand(1);

  // Elaborate the then block
  SemaRef.enterScope(Scope::Block, ThenBlock);
  clang::Stmt *Then = elaborateStatement(ThenBlock);
  SemaRef.leaveScope(ThenBlock);
  if (!Then)
    return nullptr;

  // Elaborate the else block
  clang::Stmt *Else = nullptr;
  if (ElseBlock) {
    SemaRef.enterScope(Scope::Block, ElseBlock);
    Else = elaborateStatement(ElseBlock);
    SemaRef.leaveScope(ElseBlock);
  }
  clang::SourceLocation ElseLoc = Else ?
    Else->getBeginLoc() : clang::SourceLocation();

  // Create the statement and return
  clang::StmtResult If = CxxSema.ActOnIfStmt(
    S->getLocation(), /*Constexpr=*/false, ConditionExpr->getExprLoc(),
    /*InitStmt=*/nullptr, Condition, ElseLoc, Then, ElseLoc, Else);
  return If.get();
}

clang::Stmt *Elaborator::elaborateWhileStmt(const ControlSyntax *S) {
  assert(S->getControl().hasKind(tok::WhileKeyword) && "invalid while syntax");
  Sema::ScopeRAII WhileScope(SemaRef, Scope::Control, S);

  // Elaborate the condition
  clang::Expr *CondExpr =
    elaborateExpression(S->getHead());
  if (!CondExpr)
    return nullptr;
  clang::Sema::ConditionResult Condition =
    SemaRef.getCxxSema().ActOnCondition(/*Scope=*/nullptr, S->getLocation(),
                                        CondExpr,
                                        clang::Sema::ConditionKind::Boolean);

  // Elaborate the block
  const Syntax *BlockSyntax = S->getBody();
  SemaRef.enterScope(Scope::Block, BlockSyntax);
  clang::Stmt *Block = elaborateStatement(BlockSyntax);
  SemaRef.leaveScope(BlockSyntax);
  if (!Block)
    return nullptr;

  clang::StmtResult While =
    SemaRef.getCxxSema().ActOnWhileStmt(S->getLocation(),
                                        S->getHead()->getLocation(),
                                        Condition,
                                        S->getBody()->getLocation(),
                                        Block);
  return While.get();
}

clang::Stmt *Elaborator::elaborateForStmt(const ControlSyntax *S) {
  assert(S->getControl().hasKind(tok::ForKeyword) && "invalid for syntax");
  clang::SourceLocation ForLoc = S->getControl().getLocation();
  Sema::ScopeRAII ForScope(SemaRef, Scope::Control, S);
  clang::Sema::GoldElaborationScopeRAII CxxScope(
    CxxSema,
    clang::Scope::BreakScope    |
    clang::Scope::ContinueScope |
    clang::Scope::DeclScope     |
    clang::Scope::ControlScope);

  if (!S->getHead() /*|| isa<ErrorSyntax>(S->getSignature())*/) {
    Error(S->getLocation(), "failed to translate statement");
    return nullptr;
  }

  // Elaborate the conditions
  unsigned I = 0;
  const Syntax *Conditions[3] = {nullptr, nullptr, nullptr};
  for (const Syntax *C : S->getHead()->children()) {
    if (I > 2) {
      // Conceivably, if we got to this point, C cannot be null.
      Error(C->getLocation(), "too many arguments in for-loop signature");
      return nullptr;
    }

    Conditions[I++] = C;
  }

  clang::Stmt *TheDeclStmt = elaborateStatement(Conditions[0]);
  clang::Expr *CondExpr = elaborateExpression(Conditions[1]);
  clang::SourceLocation CondLoc = Conditions[1] ?
    Conditions[1]->getLocation() : S->getLocation();
  clang::Sema::ConditionResult Condition =
    CxxSema.ActOnCondition(CxxSema.getCurScope(), CondLoc, CondExpr,
                           clang::Sema::ConditionKind::Boolean);
  clang::Expr *IncExpr = elaborateExpression(Conditions[2]);
  clang::Sema::FullExprArg FullIncExpr =
    CxxSema.MakeFullDiscardedValueExpr(IncExpr);

  // Elaborate the block
  const Syntax *BlockSyntax = S->getBody();
  SemaRef.enterScope(Scope::Block, BlockSyntax);
  clang::Stmt *Block = elaborateStatement(BlockSyntax);
  SemaRef.leaveScope(BlockSyntax);
  if (!Block)
    return nullptr;

  return CxxSema.ActOnForStmt(ForLoc, clang::SourceLocation(),
                              TheDeclStmt, Condition, FullIncExpr,
                              clang::SourceLocation(), Block).get();
}

clang::Stmt *Elaborator::elaborateDoStmt(const ControlSyntax *S) {
  assert(S->getControl().hasKind(tok::DoKeyword) && "invalid do syntax");
  Sema::ScopeRAII DoScope(SemaRef, Scope::Control, S);

  // Elaborate the condition
  clang::Expr *CondExpr =
    elaborateExpression(S->getHead());
  if (!CondExpr)
    return nullptr;

  // Elaborate the block
  const Syntax *BlockSyntax = S->getBody();
  SemaRef.enterScope(Scope::Block, BlockSyntax);
  clang::Stmt *Block = elaborateStatement(BlockSyntax);
  SemaRef.leaveScope(BlockSyntax);
  if (!Block)
    return nullptr;

  clang::StmtResult Do =
    SemaRef.getCxxSema().ActOnDoStmt(S->getLocation(),
                                     Block, S->getHead()->getLocation(),
                                     S->getHead()->getLocation(),
                                     CondExpr,
                                     S->getHead()->getEndLocation());
  return Do.get();
}

clang::Stmt *Elaborator::elaborateLetStmt(const ControlSyntax *S) {
  assert(S->getControl().hasKind(tok::LetKeyword) && "invalid let syntax");

  Sema::ScopeRAII LetScope(SemaRef, Scope::Control, S);
  // FIXME: is this a statement expression?
  clang::Sema::CompoundScopeRAII CxxScope(CxxSema, /*IsStmtExpr=*/true);


  // Elaborate any head declarations. We aren't concerned with their validity.
  if (!S->getHead() || !isa<EnclosureSyntax>(S->getHead()))
    return nullptr;
  EnclosureSyntax *Head = cast<EnclosureSyntax>(S->getHead());
  if (!Head->getTerm() || !isa<ListSyntax>(Head->getTerm()))
    return nullptr;
  llvm::SmallVector<clang::Stmt *, 4> Stmts;
  for (const Syntax *H : Head->getTerm()->children()) {
    if (!isa<DeclarationSyntax>(H)) {
      unsigned DiagID =
        CxxSema.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "expected declaration");
      CxxSema.Diags.Report(H->getLocation(), DiagID);
      continue;
    }

    clang::Stmt *TheDecl = elaborateDeclStmt(cast<DeclarationSyntax>(H));
    Stmts.push_back(TheDecl);
  }

  const Syntax *Body = isa<EnclosureSyntax>(S->getBody()) ?
    cast<EnclosureSyntax>(S->getBody())->getTerm() : S->getBody();

  if (isa<ListSyntax>(Body)) {
    for (const Syntax *C : Body->children()) {
      clang::Stmt *CC = elaborateStatement(C);
      if (!CC)
        continue;
      Stmts.push_back(CC);
    }
  } else {
    clang::Stmt *CC = elaborateStatement(Body);
    if (CC)
      Stmts.push_back(CC);
  }

  return CxxSema.ActOnCompoundStmt(S->getLocation(), S->getEndLocation(),
                                   Stmts, /*StmtExpr=*/true).get();
}

clang::Expr *Elaborator::elaborateArraySubscriptExpr(clang::Expr *Base,
                                                     const ListSyntax *Args) {
  if (Args->getNumChildren() != 1) {
    Error(Args->getLocation(), "too many arguments in array subscript");
    return nullptr;
  }

  clang::Expr *IndexExpr = elaborateExpression(Args->getOperand(0));
  if (!IndexExpr)
    return nullptr;

  auto SubscriptExpr = CxxSema.ActOnArraySubscriptExpr(
    SemaRef.getCurClangScope(), Base, IndexExpr->getExprLoc(),
    IndexExpr, IndexExpr->getExprLoc());
  if (SubscriptExpr.isInvalid())
    return nullptr;
  return SubscriptExpr.get();
}

clang::Expr *Elaborator::elaborateFunctionCall(clang::Expr *Base,
                                               const CallSyntax *Op) {
  auto Enc = dyn_cast<EnclosureSyntax>(Op->getOperand(1));
  if (!Enc)
    llvm_unreachable("invalid syntax");

  llvm::SmallVector<clang::Expr *, 4> ArgExprs;
  if (Enc->getOperand()) {
    const ListSyntax *Args = dyn_cast<ListSyntax>(Enc->getOperand());
    if (!Args) {
      Error(Enc->getOperand()->getLocation(), "expected function arguments");
      return nullptr;
    }

    for (const Syntax *A : Args->children()) {
      clang::Expr *Argument = elaborateExpression(A);
      if (!A)
        continue;

      ArgExprs.push_back(Argument);
    }

  }
  // // FIXME: what needs to happen here?
  // if (!Base->hasExplicitTemplateArgs()) {
  //   for (auto D : Base->decls()) {
  //     if (auto *FD = dyn_cast<clang::FunctionDecl>(D)) {
  //       if (FD->getTemplatedKind() ==
  //           clang::FunctionDecl::TK_FunctionTemplateSpecialization) {
  //         ;
  //       }
  //     }
  //   }
  // }

  // try and make the call and see what happens.
  clang::ExprResult Call = CxxSema.ActOnCallExpr(
    CxxSema.getCurScope(), Base, Enc->getOpen().getLocation(),
    ArgExprs, Enc->getClose().getLocation());

  return Call.get();
}


void Elaborator::elaborateTemplateArgs(const EnclosureSyntax *Enc,
                                       const ListSyntax *ArgList,
                                  clang::TemplateArgumentListInfo &TemplateArgs,
                   llvm::SmallVectorImpl<clang::TemplateArgument> &ActualArgs) {
  for (const Syntax *SS : ArgList->children()) {
    clang::Expr *ParamExpression = elaborateConstantExpression(SS);
    if (!ParamExpression)
      continue;

    if (ParamExpression->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ParamTInfo
        = SemaRef.getTypeSourceInfoFromExpr(ParamExpression,
                                            ArgList->getLocation());
      if (!ParamTInfo)
        continue;
      clang::TemplateArgument Arg(ParamTInfo->getType());
      TemplateArgs.addArgument({Arg, ParamTInfo});
      ActualArgs.emplace_back(Arg);
    } else {
      clang::TemplateArgument Arg(ParamExpression,
                                  clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ParamExpression});
      ActualArgs.emplace_back(Arg);
    }
  }
}

clang::Expr *
Elaborator::elabotateTemplateInstantiationWithArgs(const EnclosureSyntax *Enc,
                                                   clang::Expr *E,
                                                   const ListSyntax *ArgList) {

//   // This could be an attemped lambda instantiation.
//   if (clang::DeclRefExpr *DRE = dyn_cast<clang::DeclRefExpr>(E)) {
//     if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(DRE->getDecl())) {
//       if (VD->getInit() && isa<clang::LambdaExpr>(VD->getInit())) {
//         unsigned DiagID =
//           SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
//                                         "cannot explicitly instantiate lambda");
//         SemaRef.Diags.Report(Elem->getLoc(), DiagID);
//         return nullptr;
//       }
//     }
//   }

  clang::OverloadExpr *OverloadExpr = dyn_cast<clang::OverloadExpr>(E);

  // Create a normal array access.
  if (!OverloadExpr) {
    llvm_unreachable("Array is handled in another branch of the code.");
    // llvm::SmallVector<clang::Expr *, 4> ArgExprs;
    // for (const Syntax *SS : Elem->getArguments()->children()) {
    //   clang::Expr *Res = elaborateExpression(SS);
    //   if (!Res)
    //     return nullptr;
    //   ArgExprs.push_back(Res);
    // }

    // if (ArgExprs.size() == 0) {
    //   SemaRef.Diags.Report(Elem->getArguments()->getLoc(),
    //                        clang::diag::err_expected_expression);
    //   return nullptr;
    // }

    // // Create the first subscript out of the base expression.
    // auto SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
    //   SemaRef.getCurClangScope(), E, ArgExprs[0]->getExprLoc(),
    //   ArgExprs[0], ArgExprs[0]->getExprLoc());
    // if (SubscriptExpr.isInvalid())
    //   return nullptr;

    // // Then use the previous subscripts as bases, recursively.
    // for (unsigned I = 1; I < ArgExprs.size(); ++I) {
    //   SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
    //     SemaRef.getCurClangScope(), SubscriptExpr.get(),
    //     ArgExprs[I]->getExprLoc(), ArgExprs[I], ArgExprs[I]->getExprLoc());

    //   // We don't know what will happen if we try to recover, so just quit.
    //   if (SubscriptExpr.isInvalid())
    //     return nullptr;
    // }

    // return SubscriptExpr.get();
  }

  clang::SourceLocation LocStart = Enc->getOpen().getLocation();
  clang::SourceLocation LocEnd = Enc->getClose().getLocation();

  // We have an overload set, meaning this must be some kind of
  // overloaded function or function template.
  clang::TemplateArgumentListInfo TemplateArgs(LocStart, LocEnd);
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  elaborateTemplateArgs(Enc, ArgList, TemplateArgs, ActualArgs);
  clang::TemplateArgumentList
    TemplateArgList(clang::TemplateArgumentList::OnStack, ActualArgs);

  if (OverloadExpr->getNumDecls() == 1) {
    clang::NamedDecl *ND = *OverloadExpr->decls_begin();
    if (isa<clang::TemplateDecl>(ND)) {
      // We need to instantiate the template with parameters.
      if (clang::UnresolvedMemberExpr *MemAccess
              = dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
        auto *FTD = dyn_cast<clang::FunctionTemplateDecl>(ND);
        clang::FunctionDecl *FD =
          getCxxSema().InstantiateFunctionDeclaration(FTD, &TemplateArgList,
                                                      LocStart);
        if (!FD) {
          Error(LocStart, "function template instantiation failure");
          return nullptr;
        }

        getCxxSema().InstantiateFunctionDefinition(LocStart, FD,
                                                   true, true, false);
        return clang::MemberExpr::Create(getCxxContext(), MemAccess->getBase(),
                                         MemAccess->isArrow(),
                                         MemAccess->getOperatorLoc(),
                                         MemAccess->getQualifierLoc(),
                                         clang::SourceLocation(), FD,
                               clang::DeclAccessPair::make(FD, ND->getAccess()),
                                         MemAccess->getMemberNameInfo(),
                                         &TemplateArgs, E->getType(),
                                         MemAccess->getValueKind(),
                                         MemAccess->getObjectKind(),
                                         clang::NonOdrUseReason::NOUR_None);
      }
    } else if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(ND)) {
      if (FD->getTemplatedKind() == clang::FunctionDecl::TK_NonTemplate) {
        Error(LocStart, "function is not a template");
        return nullptr;
      }

      clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate();
      if (!FTD) {
        Error(LocStart,  "function template does not have template parameters");
        return nullptr;
      }

      clang::FunctionDecl *InstantiatedFunc =
        getCxxSema().InstantiateFunctionDeclaration(FTD,
                                                    &TemplateArgList,
                                                    LocStart);
      getCxxSema().InstantiateFunctionDefinition(LocStart, InstantiatedFunc,
                                                 true, true, false);
      clang::LookupResult ResultTemp(getCxxSema(), OverloadExpr->getNameInfo(),
                                     clang::Sema::LookupAnyName);
      ResultTemp.addDecl(InstantiatedFunc);
      return clang::UnresolvedLookupExpr::Create(getCxxContext(),
                                                 OverloadExpr->getNamingClass(),
                                                OverloadExpr->getQualifierLoc(),
                                                 OverloadExpr->getNameInfo(),
                                                 /*ADL=*/true, false,
                                                 ResultTemp.begin(),
                                                 ResultTemp.end());
    }
  }


  if (isa<clang::UnresolvedLookupExpr>(OverloadExpr)) {
    clang::LookupResult ResultTemp(SemaRef.getCxxSema(),
                                   OverloadExpr->getNameInfo(),
                                   clang::Sema::LookupAnyName);
    ResultTemp.setTemplateNameLookup(true);
    for (clang::NamedDecl *ND : OverloadExpr->decls()) {
      if(clang::FunctionDecl *FD = ND->getAsFunction()){
        if(clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate()) {
          ResultTemp.addDecl(FTD);
        }
      }
    }

    ResultTemp.resolveKind();
    if (ResultTemp.empty()) {
      Error(LocStart, "unresolved template resolves to non-template");
      return nullptr;
    }

    return clang::UnresolvedLookupExpr::Create(getCxxContext(),
                                               OverloadExpr->getNamingClass(),
                                               OverloadExpr->getQualifierLoc(),
                                               OverloadExpr->getNameLoc(),
                                               OverloadExpr->getNameInfo(),
                                               /*ADL=*/true, &TemplateArgs,
                                               ResultTemp.begin(),
                                               ResultTemp.end());
  }

  if (clang::UnresolvedMemberExpr *UME =
     dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
    clang::LookupResult ResultTemp(getCxxSema(), OverloadExpr->getNameInfo(),
                                   clang::Sema::LookupAnyName);
    ResultTemp.setTemplateNameLookup(true);
    for (clang::NamedDecl *ND : OverloadExpr->decls()) {
      ND = getCxxSema().getAsTemplateNameDecl(ND, true, true);
      if(ND)
        if(clang::FunctionDecl *FD = ND->getAsFunction())
          if(clang::FunctionTemplateDecl *FTD
             = FD->getDescribedFunctionTemplate()) {
            ResultTemp.addDecl(FTD);
          }
    }

    ResultTemp.resolveKind();
    if (ResultTemp.empty()) {
      Error(LocStart, "unresolved template resolves to non-template");
      return nullptr;
    }

    return clang::UnresolvedMemberExpr::Create(getCxxContext(),
                                               UME->hasUnresolvedUsing(),
                                               UME->getBase(),
                                               UME->getBaseType(),
                                               UME->isArrow(),
                                               UME->getOperatorLoc(),
                                               UME->getQualifierLoc(),
                                               UME->getTemplateKeywordLoc(),
                                               UME->getNameInfo(),
                                               &TemplateArgs,
                                               ResultTemp.begin(),
                                               ResultTemp.end());
  }

  Error(LocStart, "cannot compile this type of overload yet");
  return nullptr;
}



clang::Expr *
Elaborator::elaborateClassTemplateSelection(clang::Expr *IdExpr,
                                            const EnclosureSyntax *Enc,
                                            const ListSyntax *ArgList) {

  clang::SourceLocation LocStart = Enc->getOpen().getLocation();
  clang::SourceLocation LocEnd = Enc->getClose().getLocation();
  clang::TemplateArgumentListInfo TemplateArgs(LocEnd, LocStart);
  llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

  if (elaborateClassTemplateArguments(Enc, ArgList, TemplateArgs, ParsedArguments))
    return nullptr;

  clang::Decl *Decl = SemaRef.getDeclFromExpr(IdExpr, IdExpr->getExprLoc());
  if (!Decl)
    return nullptr;

  clang::TemplateDecl *CTD = dyn_cast<clang::TemplateDecl>(Decl);
  assert(CTD && "Invalid CppxDeclRefExpr");

  clang::CXXScopeSpec SS;
  clang::TemplateName TName(CTD);
  clang::Sema::TemplateTy TemplateTyName = clang::Sema::TemplateTy::make(TName);
  clang::IdentifierInfo *II = CTD->getIdentifier();
  clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
  clang::SourceLocation Loc = ArgList->getLocation();
  if (clang::VarTemplateDecl *VTD = dyn_cast<clang::VarTemplateDecl>(CTD)) {
    clang::DeclarationNameInfo DNI(VTD->getDeclName(), Loc);
    clang::LookupResult R(getCxxSema(), DNI, clang::Sema::LookupAnyName);
    R.addDecl(VTD);
    clang::ExprResult ER = getCxxSema().BuildTemplateIdExpr(SS, Loc, R, false,
                                                            &TemplateArgs);
    if (ER.isInvalid())
      return nullptr;
    return ER.get();
  } else {
    clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
      SemaRef.getCurClangScope(), SS, /*TemplateKWLoc*/ Loc,
      TemplateTyName, II, IdExpr->getExprLoc(),
      /*LAngleLoc*/Loc, InArgs, /*RAngleLoc*/ Loc, false, false);

    if (Result.isInvalid()) {
      getCxxSema().Diags.Report(IdExpr->getExprLoc(),
                                clang::diag::err_failed_to_translate_expr);
      return nullptr;
    }

    clang::QualType Ty(Result.get().get());
    const clang::LocInfoType *TL = cast<clang::LocInfoType>(Ty.getTypePtr());
    return SemaRef.buildTypeExpr(TL->getType(), ArgList->getLocation());
  }

}
bool Elaborator::elaborateClassTemplateArguments(
  const EnclosureSyntax *Enc,
    const ListSyntax *Args, clang::TemplateArgumentListInfo &ArgInfo,
    llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArgs) {

  for(const Syntax *SyntaxArg : Args->children()) {

    clang::Expr *ArgExpr = elaborateConstantExpression(SyntaxArg);
    if (!ArgExpr) {
      getCxxSema().Diags.Report(SyntaxArg->getLocation(),
                                clang::diag::err_failed_to_translate_expr);
      continue;
    }

    auto TemplateArg = SemaRef.convertExprToTemplateArg(ArgExpr);
    if (TemplateArg.isInvalid())
      // TODO: Figure out if this needs an error message or not.
      // I assume that the errore message should be delivered prior to this.
      return true;

    ParsedArgs.emplace_back(TemplateArg);

    // Also building template Argument Info.
    if (ArgExpr->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ArgTInfo = SemaRef.getTypeSourceInfoFromExpr(
                                             ArgExpr, SyntaxArg->getLocation());
      if (!ArgTInfo)
        return true;
      clang::TemplateArgument Arg(ArgTInfo->getType());
      ArgInfo.addArgument({Arg, ArgTInfo});
    } else if (ArgExpr->getType()->isTemplateType()) {
      clang::Decl *D = SemaRef.getDeclFromExpr(ArgExpr, SyntaxArg->getLocation());
      if (!D)
        llvm_unreachable("Invalid template declaration reference.");

      clang::TemplateDecl *TD = cast<clang::TemplateDecl>(D);
      clang::TemplateName Template(TD);
      if (Template.isNull())
        return true;
      clang::TemplateArgument Arg(Template);
      clang::TemplateArgumentLocInfo TALoc(getCxxContext(),
                                           clang::NestedNameSpecifierLoc(),
                                           SyntaxArg->getLocation(),
                                           clang::SourceLocation());
      ArgInfo.addArgument({Arg, TALoc});
    } else {
      clang::TemplateArgument Arg(ArgExpr, clang::TemplateArgument::Expression);

      // TODO: I will need to migrate the const elaboration
      // This attempts to make sure that all referenced functions are actually
      // in scope, and completely elaborated.
      // SemaRef.elaborateConstexpr(ArgExpr);

      ArgInfo.addArgument({Arg, ArgExpr});
    }
  }
  return false;
}

clang::Expr *Elaborator::elaborateMemberAccess(clang::Expr *LHS,
                                               const InfixSyntax *S) {
  clang::QualType Ty = LHS->getType();
  if (Ty->isKindType())
    return elaborateTypeNameAccess(LHS, S);
  if (Ty->isCppxNamespaceType())
    return elaborateNestedNamespaceAccess(LHS, S);

  return elaborateMemberAccessOp(LHS, S);
  // llvm_unreachable("Elaborator::elaborateMemberAccess on it.");
}

static clang::Expr *handleLookupInsideType(Sema &SemaRef,
                                           clang::ASTContext &CxxAST,
                                           clang::Expr *Prev,
                                           const BinarySyntax *Op,
                                           const Syntax *RHS, bool AddressOf) {
  clang::TypeSourceInfo *TInfo =
                    SemaRef.getTypeSourceInfoFromExpr(Prev, Prev->getExprLoc());

  if (!TInfo)
    return nullptr;

  clang::QualType QT = TInfo->getType();
  const clang::Type *T = QT.getTypePtr();
  const auto *TST = T->getAs<clang::TemplateSpecializationType>();
  if (!(T->isStructureOrClassType() || T->isUnionType()
        || T->isEnumeralType()) && !TST) {
    SemaRef.getCxxSema().Diags.Report(Prev->getExprLoc(),
                                      clang::diag::err_invalid_type_for_name_spec)
                                      << QT;
    return nullptr;
  }

  clang::TagDecl *TD = T->getAsTagDecl();
  // TODO: Implement using statements.
  // if (SemaRef.elaboratingUsingInClassScope() && TST) {
  //   TD = cast<clang::TagDecl>(TST->getTemplateName().getAsTemplateDecl()
  //                             ->getTemplatedDecl());
  // }

  // Fetching declaration to ensure that we actually have the current scope
  // for lookup.
  // Attempthing to fetch the declaration now and popss
  Declaration *DeclForTy = SemaRef.getDeclaration(TD);
  if (!DeclForTy && isa<clang::ClassTemplateSpecializationDecl>(TD)) {
    auto *CTSD = cast<clang::ClassTemplateSpecializationDecl>(TD);
    auto *Primary = CTSD->getSpecializedTemplate();
    DeclForTy = SemaRef.getDeclaration(Primary);
  }
  assert(DeclForTy);

  // TODO: Migrate the ClangToGoldDeclRebuilder into blue. THis is only used to
  //  handle template specializations.

  // ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
  // clang::SourceRange Range = clang::SourceRange(Op->getArgument(0)->getLoc(),
  //                                               RHS->getLoc());
  // if (Rebuilder.finishDecl(DeclForTy, Range))
  //   return nullptr;
  const IdentifierSyntax *Atom = dyn_cast<IdentifierSyntax>(RHS);
  if (!Atom) {
    RHS->dump();
    llvm_unreachable("Nested name specifier syntax not implemented yet.");
    // if (auto NameSpecifierCall = dyn_cast<CallSyntax>(RHS)) {
    //   FusedOpKind FokOp = getFusedOpKind(SemaRef, NameSpecifierCall);
    //   if (FokOp != FOK_Parens) {
    //     llvm_unreachable("as far as I know this can't happen.");
    //   }
    //   Atom = cast<AtomSyntax>(NameSpecifierCall->getArgument(1));
    //   clang::Expr *InnerTy = ExprElaborator(SemaRef.getContext(), SemaRef)
    //                           .elaborateExpr(NameSpecifierCall->getArgument(0));
    //   clang::SourceLocation Loc = NameSpecifierCall->getArgument(0)->getLoc();
    //   if (!InnerTy) {
    //     SemaRef.Diags.Report(Loc, clang::diag::err_not_a_type);
    //     return nullptr;
    //   }

    //   if (!InnerTy->getType()->isTypeOfTypes()) {
    //     SemaRef.Diags.Report(Loc, clang::diag::err_not_a_type);
    //     return nullptr;
    //   }

    //   clang::TypeSourceInfo *InnerTInfo =
    //                             SemaRef.getTypeSourceInfoFromExpr(InnerTy, Loc);
    //   if (!InnerTInfo)
    //     return nullptr;

    //   if (InnerTInfo->getType()->isDependentType()) {
    //     // Handling special case for nested name specifier being a dependent
    //     // expression
    //     clang::DeclarationNameInfo DNI({
    //              &SemaRef.getContext().CxxAST.Idents.get(Atom->getSpelling())},
    //                                    RHS->getLoc());
    //     return clang::CppxDependentMemberAccessExpr::Create(
    //       SemaRef.getContext().CxxAST, Prev,
    //       SemaRef.getContext().CxxAST.DependentTy, Atom->getLoc(), DNI, InnerTy);
    //   }
    //   if (auto RootRD = TInfo->getType()->getAsCXXRecordDecl()) {
    //       if (auto Base = InnerTInfo->getType()->getAsCXXRecordDecl()) {
    //         if (!RootRD->isDerivedFrom(Base)) {
    //           SemaRef.Diags.Report(Prev->getExprLoc(),
    //                                clang::diag::err_nested_namespecifier_not_base)
    //                                <<TInfo->getType() << InnerTInfo->getType();
    //           return nullptr;
    //         }
    //       } else {
    //         SemaRef.Diags.Report(Prev->getExprLoc(),
    //                             clang::diag::err_nested_namespecifier_not_a_class)
    //                             << InnerTInfo->getType();
    //         return nullptr;
    //       }
    //   } else {
    //     SemaRef.Diags.Report(Prev->getExprLoc(),
    //                          clang::diag::err_invalid_type_for_name_spec)
    //                          << InnerTInfo->getType();
    //     return nullptr;
    //   }
    //   return handleLookupInsideType(SemaRef, CxxAST, Op, InnerTy, Atom, AddressOf);
    // }
    // RHS->dump();
    // llvm_unreachable("Invalid AST structure");
  }
  // Processing if we have a single name.
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())},
                                Atom->getLocation());
  // TODO: I may need to transform this into a destructor name eventually?
  // if (Atom->getSpelling() == "destruct") {
  //   clang::DeclarationNameInfo DNI2({
  //     CxxAST.DeclarationNames.getCXXDestructorName(
  //       CxxAST.getCanonicalType(TInfo->getType())
  //     )}, Atom->getLoc());
  //   if (clang::CXXRecordDecl* RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
  //     clang::CXXDestructorDecl *Dtor = RD->getDestructor();
  //     clang::TemplateArgumentListInfo TemplateArgs;
  //     clang::UnresolvedSet<4> USet;
  //     USet.addDecl(Dtor, Dtor->getAccess());
  //     return clang::UnresolvedLookupExpr::Create(CxxAST,
  //                                         RD, clang::NestedNameSpecifierLoc(),
  //                                                 DNI2, /*ADL=*/true,
  //                                                 /*Overloaded*/false,
  //                                                 USet.begin(),
  //                                                 USet.end());
  //   } else {
  //     SemaRef.Diags.Report(Prev->getExprLoc(),
  //                         clang::diag::err_invalid_destructor_call)
  //                         << TInfo->getType();
  //     return nullptr;
  //   }
  // }
  auto R = TD->lookup(DNI.getName());
  clang::NamedDecl *ND = nullptr;
  if (R.size() != 1u) {

    // This could be a template specialization of a member.
    if (!R.empty()) {
      clang::LookupResult Redecls(SemaRef.getCxxSema(), DNI,
                                  clang::Sema::LookupOrdinaryName,
                                  clang::Sema::ForVisibleRedeclaration);
      for (clang::NamedDecl *ND : R)
        Redecls.addDecl(ND);
      Redecls.resolveKind();
      if (Redecls.getResultKind() == clang::LookupResult::FoundOverloaded) {
        clang::TemplateArgumentListInfo TemplateArgs;
        clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(TD);
        assert (RD && "should have avoided this situation");

        clang::CXXScopeSpec SS;
        SS.Extend(CxxAST, TD->getIdentifier(), Prev->getExprLoc(),
                  Op->getLocation());
        return clang::UnresolvedLookupExpr::Create(
          CxxAST, RD, SS.getWithLocInContext(CxxAST), DNI, /*ADL=*/true,
          /*Overloaded*/true, Redecls.asUnresolvedSet().begin(),
          Redecls.asUnresolvedSet().end());
      }

      ND = Redecls.getAcceptableDecl(R.front());
      if (ND && isa<clang::ValueDecl>(ND)) {
        clang::ValueDecl *VD = cast<clang::ValueDecl>(ND);
        // TODO: Nested name specifier not implemented yet.
        // clang::NestedNameSpecifierLoc NNS(SemaRef.CurNNSContext.getBlueScopeRep(),
        //                                   SemaRef.CurNNSContext.location_data());
        // bool UseNNS = SemaRef.CurNNSContext.isSet();
        return clang::DeclRefExpr::Create(
          CxxAST,
          // UseNNS ? NNS : clang::NestedNameSpecifierLoc(),
          clang::NestedNameSpecifierLoc(),
          clang::SourceLocation(), VD, /*Capture=*/false, RHS->getLocation(),
          VD->getType(), AddressOf ? clang::VK_RValue : clang::VK_LValue);
      }
    }

    // This wasn't the name of a member, check if it is the name of a base.
    if (clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
      for (const auto &Base : RD->bases()) {
        clang::CXXRecordDecl *BaseRD = Base.getType()->getAsCXXRecordDecl();
        if (BaseRD->getIdentifier() == DNI.getName().getAsIdentifierInfo())
          ND = BaseRD;
      }
    }

    auto hasUsing = [](clang::NamedDecl const *D) -> bool {
      return isa<clang::UsingDecl>(D);
    };
    unsigned Shadows = 0;
    clang::UnresolvedSet<4> USet;

    // Check if we have any shadows single declarations.
    if (std::find_if(std::begin(R), std::end(R), hasUsing) != std::end(R)) {
      clang::UsingShadowDecl *S = nullptr;
      for (clang::NamedDecl *D : R) {
        if (auto *SD = dyn_cast<clang::UsingShadowDecl>(D)) {
          S = SD;
          ++Shadows;
        }

        USet.addDecl(D, D->getAccess());
      }

      if (Shadows == 1u) {
        ND = S->getTargetDecl();
      }
    }
    // TODO: Shadowed declaration not implemented yet.
    // // Check for a shadowed overload set.
    // if (usingClassLookupIsUnresolved(R, Shadows)) {
    //   // If we're not creating a UsingDecl, these need to be static.
    //   if (!SemaRef.elaboratingUsingInClassScope() && !AddressOf) {
    //     SemaRef.Diags.Report(Prev->getExprLoc(),
    //                           clang::diag::err_ref_non_value) << Prev;
    //     return nullptr;
    //   }

    //   if (!Shadows)
    //     for (clang::NamedDecl *D : R)
    //       USet.addDecl(D, D->getAccess());
    //   clang::Expr *Base = const_cast<clang::Expr *>(Prev);
    //   clang::TemplateArgumentListInfo TemplateArgs;
    //   auto *UME =
    //     clang::UnresolvedMemberExpr::Create(CxxAST,
    //                                         /*UnresolvedUsing=*/true,
    //                                         Base,
    //                                         Base->getType(),
    //                                         Base->getType()->isPointerType(),
    //                                         RHS->getLoc(),
    //                                         clang::NestedNameSpecifierLoc(),
    //                                         clang::SourceLocation(),
    //                                         DNI,
    //                                         &TemplateArgs,
    //                                         USet.begin(),
    //                                         USet.end());
    //   return UME;
    // }

    // This was neither a type nor a shadowed declaration.
    if (!ND) {
      SemaRef.getCxxSema().Diags.Report(RHS->getLocation(), clang::diag::err_no_member)
        << Atom->getSpelling() << TD;
      return nullptr;
    }
  }

  if (!ND)
    ND = R.front();

  if (clang::TypeDecl *TD = dyn_cast<clang::TypeDecl>(ND)) {
    TD->setIsUsed();
    clang::QualType Ty = CxxAST.getTypeDeclType(TD);
    return SemaRef.buildTypeExpr(Ty, RHS->getLocation());
  }

  // This is how we access static member variables, and strangely also fields.
  if (clang::VarDecl *VDecl = dyn_cast<clang::VarDecl>(ND))
    return clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                      clang::SourceLocation(),VDecl,
                                      /*Capture=*/false, RHS->getLocation(),
                                      VDecl->getType(), clang::VK_LValue);

  // access a record from an NNS
  if (isa<clang::CXXRecordDecl>(ND))
    return SemaRef.buildTypeExprFromTypeDecl(TD, RHS->getLocation());

  // FIXME: static methods should be handled here

  // otherwise, we have a FieldDecl from a nested name specifier lookup.
  // In which case, the rhs should be static, called via operator'()',
  // inside a using macro if the lhs was a record type, or as the operand
  // of operator'&'.
  // if (Prev->getType()->isTypeOfTypes() && Op->getOperator().hasKind(tok::Dot)) {
  //   // TODO: Fix this once we have usings implemented.
  //   // clang::QualType Ty =
  //   //   cast<clang::CppxTypeLiteral>(Prev)->getValue()->getType();
  //   // if (!SemaRef.elaboratingUsingInClassScope() && !Ty->isEnumeralType()
  //   //     && !AddressOf) {
  //   //   SemaRef.Diags.Report(Prev->getExprLoc(),
  //   //                         clang::diag::err_ref_non_value) << Prev;
  //   //   return nullptr;
  //   // }
  // }
  if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(ND)) {
    if (clang::CXXRecordDecl* RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
      clang::CXXScopeSpec SS;
      SS.Extend(CxxAST, TD->getIdentifier(), Prev->getExprLoc(),
                Op->getLocation());
      clang::TemplateArgumentListInfo TemplateArgs;
      clang::UnresolvedSet<4> USet;
      USet.addDecl(FD, FD->getAccess());
      return clang::UnresolvedLookupExpr::Create(CxxAST,
                                                  RD,
                                                  SS.getWithLocInContext(CxxAST),
                                                  DNI, /*ADL=*/true,
                                                  /*Overloaded*/true,
                                                  USet.begin(),
                                                  USet.end());
    } else {
      llvm_unreachable("Incorrect tag type.");
    }
  }
  if (clang::ValueDecl *VD = dyn_cast<clang::ValueDecl>(ND)) {
    // TODO: Nested name specifier not implemented yet.
    // clang::NestedNameSpecifierLoc NNS(SemaRef.CurNNSContext.getBlueScopeRep(),
    //                                   SemaRef.CurNNSContext.location_data());
    // bool UseNNS = SemaRef.CurNNSContext.isSet();
    return clang::DeclRefExpr::Create(
      CxxAST,
      // UseNNS ? NNS : clang::NestedNameSpecifierLoc(),
      clang::NestedNameSpecifierLoc(),
      clang::SourceLocation(), VD, /*Capture=*/false, RHS->getLocation(),
      VD->getType(), AddressOf ? clang::VK_RValue : clang::VK_LValue);
  }

  llvm_unreachable("Unknown syntax encountered during nested member lookup.");
}

clang::Expr *Elaborator::elaborateTypeNameAccess(clang::Expr *LHS,
                                                 const InfixSyntax *S) {
    return handleLookupInsideType(SemaRef, getCxxContext(), LHS, S,
                                  S->getOperand(1), false);
}

clang::Expr *Elaborator::elaborateNestedNamespaceAccess(clang::Expr *LHS,
                                                        const InfixSyntax *S) {
  clang::SourceLocation Loc = S->getLocation();
  if (auto NSRef = dyn_cast<clang::CppxNamespaceDecl>(
                                           SemaRef.getDeclFromExpr(LHS, Loc)))
    return elaborateNNS(NSRef, S);
  if (auto NSAliasRef = dyn_cast<clang::NamespaceAliasDecl>(
                                             SemaRef.getDeclFromExpr(LHS, Loc)))
    return elaborateNNS(NSAliasRef, S);
  llvm_unreachable("Invalid namespace type returned.");
}
clang::Expr *Elaborator::elaborateNNS(clang::NamedDecl *NS,
                                      const InfixSyntax *S) {
  const Syntax *RHS = S->getOperand(1);
  // The object type cannot coexist with a set scope-specifier.
  clang::Sema::NestedNameSpecInfo IdInfo(NS->getIdentifier(),
                                         NS->getBeginLoc(),
                                         S->getLocation(),
                                         /*ObjectType=*/clang::QualType());
  // Look this up as an NNS.
  bool EnteringContext = SemaRef.isQualifiedLookupContext();
  bool Failure = SemaRef.getCxxSema().
    ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                EnteringContext, SemaRef.CurNNSContext,
                                /*RecoveryLookup=*/false,
                                /*IsCorrected=*/nullptr,
                                /*OnlyNamespace=*/false);
  if (Failure) {
    SemaRef.CurNNSContext.clear();
    return nullptr;
  }

  OptionalInitScope<Sema::QualifiedLookupRAII> Qual(SemaRef);
  if (auto *CppxNs = dyn_cast<clang::CppxNamespaceDecl>(NS)) {
    Qual.Init(SemaRef.QualifiedLookupContext, CppxNs);
  } else if (auto *Alias = dyn_cast<clang::NamespaceAliasDecl>(NS)) {
    Qual.Init(SemaRef.QualifiedLookupContext, Alias);
  } else {
    NS->dump();
    llvm_unreachable("We hvae a new type of namespace specifier that we've "
                     "never seen before.");
  }
  clang::Expr *RHSExpr = elaborateExpression(RHS);
  if (!RHSExpr) {
    SemaRef.CurNNSContext.clear();
    return nullptr;
  }

  if (RHSExpr->getType()->isNamespaceType())
    return RHSExpr;

  // We've finished lookup and can clear the NNS context.
  if (!SemaRef.isExtendedQualifiedLookupContext())
    SemaRef.CurNNSContext.clear();
  ExprMarker(getCxxContext(), SemaRef).Visit(RHSExpr);
  return RHSExpr;
}
// clang::Expr *ExprElaborator::elaborateNNS(clang::NamedDecl *NS,
//                                           const CallSyntax *Op,
//                                           const Syntax *RHS) {

// }

clang::Expr *Elaborator::elaborateMemberAccessOp(clang::Expr *LHS,
                                                 const InfixSyntax *S) {
  if (auto IdSyntax = dyn_cast<IdentifierSyntax>(S->getOperand(1))) {
    clang::UnqualifiedId Id;
    clang::IdentifierInfo *IdInfo =
      &getCxxContext().Idents.get(IdSyntax->getSpelling());
    // TODO: Implement operator lookup.
    // OpInfoBase const *OpInfo = SemaRef.OpInfo.getOpInfo(IdInfo);
    // if (OpInfo) {
      // clang::OverloadedOperatorKind UnaryOO = OpInfo->getUnaryOverloadKind();
      // clang::OverloadedOperatorKind BinaryOO = OpInfo->getBinaryOverloadKind();
      // if (UnaryOO != BinaryOO) {
      //   clang::CXXScopeSpec TempSS;
      //   clang::Expr *LookedUpCandidates = doDerefAndXOrLookUp(Context, SemaRef,
      //                                                         UnaryOO, BinaryOO,
      //                                                         TempSS,
      //                                                         ElaboratedLHS,
      //                                                         Op, RHS->getLoc(),
      //                                                         OpInfo);
      //   return LookedUpCandidates;
      // }
      // clang::SourceLocation RHSLoc = IdSyntax->getLocation();
      // clang::SourceLocation SymbolLocations[3] = {RHSLoc, RHSLoc, RHSLoc};
      // Id.setOperatorFunctionId(RHSLoc, OpInfo->getUnaryOverloadKind(),
      //                           SymbolLocations);
    // } else {
    Id.setIdentifier(IdInfo, IdSyntax->getLocation());
    // }


    if (LHS->getDependence() != clang::ExprDependence::None) {
      llvm_unreachable("dependent access expression not implemented yet.");
    }

    clang::CXXScopeSpec SS;
    clang::SourceLocation Loc;
    clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
    if (LHS->getType()->isPointerType())
      AccessTokenKind = clang::tok::TokenKind::arrow;

    clang::ExprResult RHSExpr =
      SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                                LHS, S->getLocation(),
                                                AccessTokenKind, SS, Loc, Id,
                                                nullptr);
    if (RHSExpr.isInvalid())
      return nullptr;

    ExprMarker(getCxxContext(), SemaRef).Visit(RHSExpr.get());
    return RHSExpr.get();
  } else {
    llvm_unreachable("Qualified member access not implemented yet.");
  }
}

/// This function extracts the number of bytes argument from integer, character,
/// and real, and evaluates the integer within the constant expression in order
/// to make sure that it's a valid value.
/// @returns true in the event there was an error.
static bool handleBuiltInByteCountEval(Elaborator &Elab, const Syntax *BytesArg,
                                       const char *FnName,
                                       const char *ArgOneText,
                                       const char* ArgTwoErrorText,
                                       int64_t MaxBytesAllowed,
                                       int64_t &ByteCount) {
  auto reportInvalidUse = [&](clang::SourceLocation L, int MsgIdx = 0) -> bool {
    Elab.getCxxSema().Diags.Report(
      L, clang::diag::err_invalid_use_of_built_in_type_function)
      << FnName << ArgOneText << ArgTwoErrorText << MsgIdx;
    return true;
  };
  clang::SourceLocation BytesLoc = BytesArg->getLocation();
  auto ByteCountExpr = Elab.elaborateConstantExpression(BytesArg);
  if (!ByteCountExpr)
    return reportInvalidUse(BytesLoc);
  clang::Expr::EvalResult BCResult;
  clang::Expr::EvalContext EvalCtx(Elab.getCxxContext(),
                               Elab.getCxxSema().GetReflectionCallbackObj());
  if (ByteCountExpr->EvaluateAsConstantExpr(BCResult, EvalCtx)) {
    if (BCResult.Val.isInt()) {
      if (!BCResult.Val.getInt().isPowerOf2())
        return reportInvalidUse(BytesLoc, 1);

      // Max number of bytes allowed.
      ByteCount = BCResult.Val.getInt().getExtValue();
      if (ByteCount > MaxBytesAllowed)
        return reportInvalidUse(BytesLoc, 1);
      return false;
    } else
      return reportInvalidUse(BytesLoc, 2);
  } else
    return reportInvalidUse(BytesLoc, 3);
}

clang::Expr *Elaborator::elaborateIntegerMetaFunction(const BinarySyntax *S) {
  auto LHSSyntax = dyn_cast<LiteralSyntax>(S->getOperand(0));
  clang::SourceLocation Loc = LHSSyntax->getLocation();
  auto ArgEnclosure = dyn_cast<EnclosureSyntax>(S->getOperand(1));
  auto reportInvalidUse = [&](clang::SourceLocation L, int MsgIdx =0) -> clang::Expr * {
    getCxxSema().Diags.Report(L,
                         clang::diag::err_invalid_use_of_built_in_type_function)
                              << "integer" << "number-of-bytes"
                              << "signed-or-unsigned"
                              << MsgIdx;
    return nullptr;
  };
  if (!ArgEnclosure) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }
  if (!ArgEnclosure->isBracketEnclosure()) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }

  if (auto RHSList = dyn_cast<ListSyntax>(ArgEnclosure->getOperand())) {
    if (RHSList->getNumChildren() != 2)
      return reportInvalidUse(RHSList->getLocation());

    if (!RHSList->getOperand(0))
      return reportInvalidUse(Loc);
    int64_t ByteCount = 0;
    if(handleBuiltInByteCountEval(*this, RHSList->getOperand(0), "integer",
                                  "number-of-bytes", "signed-or-unsigned",
                                  16, ByteCount))
      return nullptr;

    // Checking for signedness.
    if (!RHSList->getOperand(1))
      return reportInvalidUse(Loc);
    bool IsSigned = false;
    clang::SourceLocation SignednessLoc = RHSList->getOperand(1)->getLocation();
    if (auto SignnessSyntax = dyn_cast<IdentifierSyntax>(RHSList->getOperand(1))) {
      if (SignnessSyntax->getSpelling() == "signed") {
        IsSigned = true;
      } else if(SignnessSyntax->getSpelling() == "unsigned") {
        IsSigned = false;
      } else {
        return reportInvalidUse(SignednessLoc, 4);
      }
    } else {
      return reportInvalidUse(SignednessLoc, 4);
    }
    // Now creating the type expression.
    auto Ty = getCxxContext().getIntTypeForBitwidth(8*ByteCount, IsSigned);
    return SemaRef.buildTypeExpr(Ty, Loc);

  } else {
    return reportInvalidUse(Loc);
  }
}

clang::Expr *Elaborator::elaborateCharacterMetaFunction(const BinarySyntax *S) {
  auto LHSSyntax = dyn_cast<LiteralSyntax>(S->getOperand(0));
  clang::SourceLocation Loc = LHSSyntax->getLocation();
  auto ArgEnclosure = dyn_cast<EnclosureSyntax>(S->getOperand(1));
  auto reportInvalidUse = [&](clang::SourceLocation L, int MsgIdx = 0) -> clang::Expr * {
    getCxxSema().Diags.Report(L,
                         clang::diag::err_invalid_use_of_built_in_type_function)
                              << "character" << "number-of-bytes"
                              << "ascii-or-utf"
                              << MsgIdx;
    return nullptr;
  };


  if (!ArgEnclosure) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }
  if (!ArgEnclosure->isBracketEnclosure()) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }
  if (auto RHSList = dyn_cast<ListSyntax>(ArgEnclosure->getOperand())) {
    if (RHSList->getNumChildren() != 2)
      return reportInvalidUse(RHSList->getLocation());

    if (!RHSList->getOperand(0))
      return reportInvalidUse(Loc);
    int64_t ByteCount = 0;
    if(handleBuiltInByteCountEval(*this, RHSList->getOperand(0), "character",
                                  "number-of-bytes", "ascii-or-utf",
                                  4, ByteCount))
      return nullptr;

    // Checking for signedness.
    if (!RHSList->getOperand(1))
      return reportInvalidUse(Loc);
    bool IsUTF = false;
    clang::SourceLocation SecondArgLoc = RHSList->getOperand(1)->getLocation();
    if (auto SecondArg = dyn_cast<IdentifierSyntax>(RHSList->getOperand(1))) {
      if (SecondArg->getSpelling() == "ascii") {
        IsUTF = false;
      } else if(SecondArg->getSpelling() == "utf") {
        IsUTF = true;
      } else {
        return reportInvalidUse(SecondArgLoc, 8);
      }
    } else {
      return reportInvalidUse(SecondArgLoc, 4);
    }

    // Now creating the type expression.
    clang::QualType RetTy;
    if(IsUTF) {
      switch(ByteCount) {
      case 1:
        RetTy = getCxxContext().Char8Ty;
        break;
      case 2:
        RetTy = getCxxContext().Char16Ty;
        break;
      case 4:
        RetTy = getCxxContext().Char32Ty;
        break;
      default:
        return reportInvalidUse(SecondArgLoc, 9);
      }
    } else {
      switch(ByteCount) {
      case 1:
        RetTy = getCxxContext().CharTy;
        break;
      default:
        return reportInvalidUse(SecondArgLoc, 8);
      }
    }
    return SemaRef.buildTypeExpr(RetTy, Loc);

  } else {
    return reportInvalidUse(Loc);
  }
}

clang::Expr *Elaborator::elaborateRealMetaFunction(const BinarySyntax *S) {
  auto LHSSyntax = dyn_cast<LiteralSyntax>(S->getOperand(0));
  clang::SourceLocation Loc = LHSSyntax->getLocation();
  auto ArgEnclosure = dyn_cast<EnclosureSyntax>(S->getOperand(1));
  auto reportInvalidUse = [&](clang::SourceLocation L, int MsgIdx =0) -> clang::Expr * {
    getCxxSema().Diags.Report(L,
                         clang::diag::err_invalid_use_of_built_in_type_function)
                              << "real" << "number-of-bytes"
                              << "binary-or-decimal"
                              << MsgIdx;
    return nullptr;
  };


  if (!ArgEnclosure) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }
  if (!ArgEnclosure->isBracketEnclosure()) {
    return reportInvalidUse(ArgEnclosure->getLocation());
  }
  if (auto RHSList = dyn_cast<ListSyntax>(ArgEnclosure->getOperand())) {
    if (RHSList->getNumChildren() != 2)
      return reportInvalidUse(RHSList->getLocation());

    if (!RHSList->getOperand(0))
      return reportInvalidUse(Loc);
    int64_t ByteCount = 0;
    if (handleBuiltInByteCountEval(*this, RHSList->getOperand(0), "real",
                                   "number-of-bytes", "binary-or-decimal",
                                   16, ByteCount))
      return nullptr;
    if (ByteCount <= 2)
      return reportInvalidUse(RHSList->getOperand(0)->getLocation(), 7);

    // Checking for signedness.
    if (!RHSList->getOperand(1))
      return reportInvalidUse(Loc);

    clang::SourceLocation SignednessLoc = RHSList->getOperand(1)->getLocation();
    if (auto SignSyntax = dyn_cast<IdentifierSyntax>(RHSList->getOperand(1))) {
      if (SignSyntax->getSpelling() == "binary") {
        auto Ty = getCxxContext().getRealTypeForBitwidth(8*ByteCount, true);
        return SemaRef.buildTypeExpr(Ty, Loc);
      } else if(SignSyntax->getSpelling() == "decimal")
        return reportInvalidUse(SignednessLoc, 6);
      else
        return reportInvalidUse(SignednessLoc, 5);
    } else {
      return reportInvalidUse(SignednessLoc, 4);
    }
  } else {
    return reportInvalidUse(Loc);
  }
  // llvm_unreachable("elaborateRealMetaFunction");
}

// Diagnostics
void Elaborator::Error(clang::SourceLocation Loc, llvm::StringRef Msg) {
  CxxSema.Diags.Report(Loc, clang::diag::err_blue_elaboration) << Msg;
}



bool Elaborator::delayElaborateDeclType(clang::CXXRecordDecl *RD,
                                        Declaration *D) {
  // Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  // if (!D) {
  //   return false;
  // }

  // Handling a check for possible late elaboration on each declaration.
  if (phaseOf(D) > Phase::Identification)
    return false;


  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  // || D->declaresForwardRecordDecl()
  if (D->declaratorContainsTag()) {
    delayElaborationClassBody(D);
    return true;
  }
  bool WasDelayed = false;
  if (D->declaratorContainsFunction()) {
    if (D->declIsStatic()) {
      elaborateDeclarationTyping(D);
    } else {
      if (RD->isUnion()) {
        elaborateDeclarationTyping(D);
      } else {
        // Attempting to delay method decl/def combos
        delayElaborateMethodDecl(D);
        WasDelayed = true;
      }
    }
    if (D->hasInitializer()) {
      delayElaborateMethodDef(D);
      WasDelayed = true;
    }
    return WasDelayed;
  }
  elaborateDeclarationTyping(D);
  if (!D->isFieldDecl()) {
    elaborateDefinitionInitialization(D);
    return false;
  }

  if (D->declaresInitializedVariable()) {
    delayElaborateMemberInitializer(D);
    return true;
  }
  return false;
}


void Elaborator::delayElaborateMemberInitializer(Declaration *D) {
  SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
    new LateElaborateMemberInitializer(SemaRef, D)
  );
}

void Elaborator::delayElaborateMethodDecl(Declaration *D) {
  SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
    new LateElaboratedMethodDeclaration(SemaRef, D)
  );
}

void Elaborator::delayElaborateMethodDef(Declaration *D) {
  SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
    new LateElaboratedMethodDef(SemaRef, D)
  );
}

void Elaborator::delayElaborationClassBody(Declaration *D) {
  elaborateDeclarationTyping(D);
}

void Elaborator::delayElaborateDefaultParam(Declaration *ParamDecl) {
  assert(SemaRef.CurrentLateMethodDecl && "Late method decl not set");
  SemaRef.CurrentLateMethodDecl->DefaultArgs.emplace_back(ParamDecl);
}



void Elaborator::finishDelayedElaboration(ElaboratingClass &Class) {
  lateElaborateAttributes(Class);
  lateElaborateMethodDecls(Class);
  lateElaborateDefaultParams(Class);
  // We call this because no new declarations can be added after this point.
  // This is only called for the top level class.
  SemaRef.getCxxSema().ActOnFinishCXXMemberDecls();

  lateElaborateMemberInitializers(Class);
  lateElaborateMethodDefs(Class);
}

void Elaborator::lateElaborateAttributes(ElaboratingClass &Class) {
  bool CurrentlyNested = !Class.IsTopLevelClass;
  Sema::ClangScopeRAII AttributeDelayedScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);


  OptionalInitScope<ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
                        Class.TagOrTemplate->Def);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());
  }

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateAttributes();
  }
  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());
}

void Elaborator::lateElaborateMethodDecls(ElaboratingClass &Class) {
  // If the current class is a template re-enter the template before we continue.
  bool HasTemplateScope = !Class.IsTopLevelClass && Class.TemplateScope;
  Sema::ClangScopeRAII ClassTemplateScope(SemaRef,
    clang::Scope::TemplateParamScope, clang::SourceLocation(), HasTemplateScope);

  if (HasTemplateScope)
    SemaRef.getCxxSema().ActOnReenterTemplateScope(Class.TagOrTemplate->getCxx(),
                                                   [&] {
      return SemaRef.getCurClangScope();
    });

  bool CurrentlyNested = !Class.IsTopLevelClass;
  Sema::ClangScopeRAII FunctionDeclScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);

  OptionalInitScope<Sema::DeclContextRAII> DCTracking(SemaRef);
  OptionalInitScope<ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Def, false);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());
    DCTracking.Init(Class.TagOrTemplate, true);
  }

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMethodDeclarations();
  }

  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());
}

void Elaborator::lateElaborateDefaultParams(ElaboratingClass &Class) {
  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateDefaultParams();
  }
}

void Elaborator::lateElaborateMemberInitializers(ElaboratingClass &Class) {
  bool CurrentlyNested = !Class.IsTopLevelClass;

  Sema::ClangScopeRAII MemberInitScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);

  OptionalInitScope<ResumeScopeRAII> OptResumeScope(SemaRef);
  OptionalInitScope<Sema::DeclContextRAII> DCTracking(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Def, false);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());
    DCTracking.Init(Class.TagOrTemplate, true);
  }

  clang::Sema::CXXThisScopeRAII PushThisIntoScope(SemaRef.getCxxSema(),
      Class.TagOrTemplate->getCxx(), clang::Qualifiers());

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMemberInitializers();
  }

  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->getCxx());

  SemaRef.getCxxSema().ActOnFinishDelayedMemberInitializers(
    Class.TagOrTemplate->getCxx());
}

void Elaborator::lateElaborateMethodDefs(ElaboratingClass &Class) {
  bool CurrentlyNested = !Class.IsTopLevelClass;
  Sema::ClangScopeRAII MethodDefScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);

  OptionalInitScope<ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested)
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Def, false);

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMethodDefs();
  }
}

void Elaborator::lateElaborateAttribute(LateElaboratedAttributeDecl &Field) {
  // TODO: find a valid example of how this actually works/when this is used.
  llvm_unreachable("We currently don't have late binding attributes? I don't "
      "really know.");
}

void Elaborator::lateElaborateMethodDef(LateElaboratedMethodDef &Method) {
  if (!Method.D->getCxx())
    return;
  // Finish exception spec before method body?
  {
    if (!Method.D->isFunctionDecl())
      return;
    Declarator *FnDecl = Method.D->getFirstDeclarator(Declarator::Function);
    // // Attempting to push the scope for the current function onto the stack
    // // This helps with lookup during evaluation exception specification.
    ResumeScopeRAII TempScope(SemaRef,
                                    FnDecl->DeclInfo.ParamScope,
                                    FnDecl->DeclInfo.ParamScope->getTerm(),
                                    /*PopOnExit=*/false);
    Sema::OptionalInitClangRAII<clang::Sema::CXXThisScopeRAII> ThisScope(SemaRef);
    if (clang::CXXMethodDecl *MD
                         = dyn_cast<clang::CXXMethodDecl>(Method.D->getCxx())) {
      ThisScope.Init(MD->getParent(), MD->getMethodQualifiers(), true);
    }
    // finishExceptionSpecAttr(getCxxContext(), SemaRef, Method.D);
  }
  elaborateFunctionDef(Method.D);
  if (!Method.D->getCxx())
    return;
  SemaRef.getCxxSema().ActOnFinishInlineFunctionDef(
    cast<clang::FunctionDecl>(Method.D->getCxx()));
}


void Elaborator::lateElaborateMemberInitializer(
    LateElaborateMemberInitializer &MemberInit) {
  assert(MemberInit.D && "Invalid declaration detected.");
  assert(MemberInit.D->isFieldDecl()
         && "Declaration doesn't declare a field.\n");

  // Start delayed member This occurs for each member initializer within a
  // given class.
  elaborateDefinitionInitialization(MemberInit.D);
}

void Elaborator::lateElaborateMethodDecl(
    LateElaboratedMethodDeclaration &Method) {
  Sema::ClangScopeRAII FunctionDeclScope(SemaRef, clang::Scope::DeclScope |
      clang::Scope::FunctionPrototypeScope |
      clang::Scope::FunctionDeclarationScope,
      clang::SourceLocation());
  Sema::LateMethodRAII MethodTracking(SemaRef, &Method);
  elaborateDeclarationTyping(Method.D);
  // This is to check if the method delcaration was a success and in the event
  // that it is we need to finish the exception specifier after the end of the
  // class.
  if (Method.D->getCxx()) {
    if (auto FD = dyn_cast<clang::FunctionDecl>(Method.D->getCxx())) {
      if (FD->getExceptionSpecType() == clang::EST_Unparsed) {
        if (!Method.D->getInitializer()) {
          SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
            new LateElaboratedMethodDef(SemaRef, Method.D)
          );
        }
      }
    }
  }
  SemaRef.getCxxSema().ActOnFinishDelayedCXXMethodDeclaration(
      SemaRef.getCurClangScope(), Method.D->getCxx());
}

void Elaborator::lateElaborateDefaultParams(
    LateElaboratedMethodDeclaration &MethodDecl) {
  for(LateElaboratedDefaultArgument &Arg : MethodDecl.DefaultArgs)
    lateElaborateDefaultParam(Arg);
}

void Elaborator::lateElaborateDefaultParam(
    LateElaboratedDefaultArgument &DefaultParam) {
  elaborateDefinitionInitialization(DefaultParam.Param);
}


} // namespace blue

