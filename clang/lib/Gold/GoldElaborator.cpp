//=== GoldElaborator.cpp - Elaboration for Gold Nodes ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Elaborator interface, which creates clang::Decl*
//  nodes out of Gold Syntax nodes.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/DeclarationName.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/AST/DeclCXX.h"

namespace gold {

/// This extracts the access specifier if one was given, if not it's set to public.
/// Returns false on success, and true if there's an error.
static bool computeAccessSpecifier(Sema& SemaRef, Declaration *D,
    clang::AccessSpecifier& AS) {
  AS = clang::AS_public;
  if (!D->Decl->UnprocessedAttributes)
    return false;
  auto Iter = D->Decl->UnprocessedAttributes->begin();
  auto End = D->Decl->UnprocessedAttributes->end();
  for (;Iter != End; ++Iter) {
    if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(*Iter)){
      if (Atom->getSpelling() == "private") {
        AS = clang::AS_private;
        break;
      } else if (Atom->getSpelling() == "protected") {
        AS = clang::AS_protected;
        break;
      } else if (Atom->getSpelling() == "public") {
        AS = clang::AS_public;
        break;
      }
    }
  }
  const Syntax *AttribSpec = nullptr;
  if(Iter != End) {
    AttribSpec = *Iter;
    D->Decl->UnprocessedAttributes->erase(Iter);
    Iter = D->Decl->UnprocessedAttributes->begin();
    End = D->Decl->UnprocessedAttributes->end();
    for (;Iter != End; ++Iter) {
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(*Iter)){
        if (Atom->getSpelling() == "private"
            || Atom->getSpelling() == "protected"
            || Atom->getSpelling() == "public") {
          break;
        }
      }
    }
    if (Iter != End) {
      // TODO: Create valid error message
      // llvm::outs() << "Duplicate access specifier given\n";
      SemaRef.Diags.Report((*Iter)->getLoc(),
            clang::diag::err_duplicate_access_specifier)
              << AttribSpec->getLoc() << (*Iter)->getLoc(); 
      return true;
    }
  }
  return false;
}
// static void handleInClassAttributeApplication(Sema& SemaRef,SyntaxContext& Context,
//     clang::Decl *Dec, Declaration *D) {
//   if (D->Decl->AttributeNode) {
//     
//     for (auto const& attr : D->Decl->AttributeNode->getAttributes()) {
//       if (const AtomSyntax *AttrName = dyn_cast<AtomSyntax>(attr->getArg())) {

//         // TODO: We need to change in order to prevent multiple access specifiers
//         // from being given. Currently this only works for single access
//         // specifiers.

//         } else {
//           llvm_unreachable("Unsupported attribute given.");
//         }
//       }
//     }

//     // Don't chainge default access in the event the access specifier was never
//     // given.
//     if (AS != clang::AS_none)
//       Dec->setAccess(AS);
//   }
// }
// static bool CheckDeclA

// static bool checkAccessSpecifier(Sema& SemaRef, Declaration *D) {
//   return false;
// } 
Elaborator::Elaborator(SyntaxContext &Context, Sema &SemaRef)
  : Context(Context), SemaRef(SemaRef) {}

clang::Decl *Elaborator::elaborateFile(const Syntax *S) {
  SemaRef.getCxxSema().FieldCollector.reset(new clang::CXXFieldCollector());
  assert(isa<FileSyntax>(S) && "S is not a file");
  // S->dump();
  clang::Scope *Scope = SemaRef.enterClangScope(clang::Scope::DeclScope);
  SemaRef.getCxxSema().ActOnTranslationUnitScope(Scope);
  SemaRef.getCxxSema().Initialize();
  SemaRef.getCxxSema().ActOnStartOfTranslationUnit();
  startFile(S);

  const FileSyntax *File = cast<FileSyntax>(S);

  // Pass 1. identify declarations in scope.
  for (const Syntax *SS : File->children()) {
    identifyDecl(SS);
  }

  // Pass 2: elaborate the types.
  for (const Syntax *SS : File->children()) {
    elaborateDeclType(SS);
  }

  // Pass 3: elaborate definitions.
  for (const Syntax *SS : File->children()) {
    elaborateDeclInit(SS);
  }
  
  finishFile(S);
  SemaRef.getCxxSema().ActOnEndOfTranslationUnit();
  SemaRef.leaveClangScope(S->getLoc());


  return Context.CxxAST.getTranslationUnitDecl();
}

void Elaborator::startFile(const Syntax *S) {
  // Enter the global scope.
  SemaRef.enterScope(SK_Namespace, S);

  /// Build the declaration for the global namespace.
  Declaration *D = new Declaration(S);
  D->SavedScope = SemaRef.getCurrentScope();
  D->Cxx = Context.CxxAST.getTranslationUnitDecl();
  SemaRef.pushDecl(D);
}

void Elaborator::finishFile(const Syntax *S) {
  SemaRef.getCxxSema().ActOnEndOfTranslationUnit();
  SemaRef.popDecl();
  SemaRef.leaveScope(S);

  // TODO: Any pending semantic analysis to do here?
}

clang::Decl *Elaborator::elaborateDeclType(const Syntax *S) {
  // TODO: Can we elaborate top-level statements? What would they do?
  // Would these equivalent to directives?
  //
  // TODO: Look for module-related declarations.
  //
  // TODO: What should we find for a list of declarators?
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D) {
    return nullptr;
  }
  return elaborateDecl(D);
}

static void BuildTemplateParams(SyntaxContext &Ctx, Sema &SemaRef,
                                const Syntax *Params,
                                llvm::SmallVectorImpl<clang::NamedDecl *> &Res);

static clang::Decl*
processCXXRecordDecl(Elaborator& Elab, SyntaxContext& Context, Sema& SemaRef,
                    Declaration *D) {
  using namespace clang;
  bool Template = D->declaresTemplateType();
  const Syntax *TemplParams;
  
  // Checking if we are a nested template decl/class.
  bool WithinClass = SemaRef.getCurrentScope()->getKind() == SK_Class;
  
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  llvm::SmallVector<clang::TemplateParameterList *, 4> TPLStorage;
  MultiTemplateParamsArg MTP;
  Declarator *TemplateDeclarator = nullptr;
  if (Template) {
    // TODO: In the future change this so we can enter multiple template scopes
    // and track their depth
    TemplateDeclarator = D->getFirstTemplateDeclarator();
    TemplParams = TemplateDeclarator->Data.TemplateInfo.Params;
    SemaRef.enterScope(SK_Template, TemplParams);
    BuildTemplateParams(Context, SemaRef, TemplParams, TemplateParamDecls);
    clang::SourceLocation Loc = TemplParams->getLoc();
    TemplateDeclarator->Data.TemplateInfo.ClangScope
      = SemaRef.enterClangScope(clang::Scope::TemplateParamScope);
    clang::TemplateParameterList *TPL
      = SemaRef.getCxxSema().ActOnTemplateParameterList(
      /*unsigned Depth*/0u, /*ExportLoc*/Loc, /*TemplateLoc*/Loc,
      /*LAngleLoc*/Loc, TemplateParamDecls, /*RAngleLoc*/Loc,
      /*RequiresClause*/nullptr);
    TPLStorage.push_back(TPL);
    MTP = TPLStorage;
  }

  bool IsOwned = false;
  bool IsDependent = false;
  CXXScopeSpec SS;
  TypeResult UnderlyingType;
  AccessSpecifier AS = AS_none;
  if (WithinClass) {
    if (Template)
      AS = AS_public;
    // Attempting to check for access specifiers
    if (D->Decl->AttributeNode) {
      for (auto const& attr : D->Decl->AttributeNode->getAttributes()) {
        if (const AtomSyntax *AttrName = dyn_cast<AtomSyntax>(attr->getArg())) {
          if (AttrName->getSpelling() == "private") {
            AS = clang::AS_private;
          } else if (AttrName->getSpelling() == "protected") {
            AS = clang::AS_protected;
          } else if (AttrName->getSpelling() == "public") {
            AS = clang::AS_public;
          }
        }
      }
    }
  }
  clang::SourceLocation IdLoc = D->Decl->getLoc();
  // llvm::outs() << "IdLoc Location\n";
  // IdLoc.dump(SemaRef.getCxxSema().getSourceManager());

  Decl *Declaration = SemaRef.getCxxSema().ActOnTag(SemaRef.getCurClangScope(),
      clang::DeclSpec::TST_struct, /*Metafunction=*/nullptr, clang::Sema::TUK_Definition,
      D->Init->getLoc(), SS, D->getId(), IdLoc, clang::ParsedAttributesView(),
      /*AccessSpecifier=*/AS, /*ModulePrivateLoc=*/SourceLocation(),
      MTP, IsOwned, IsDependent, /*ScopedEnumKWLoc=*/SourceLocation(),
      /*ScopeEnumUsesClassTag=*/false, UnderlyingType, /*IsTypeSpecifier=*/false,
      /*IsTemplateParamOrArg=*/false, /*SkipBody=*/nullptr);
  CXXRecordDecl *ClsDecl = nullptr;
  if(isa<CXXRecordDecl>(Declaration)) {
    ClsDecl = cast<CXXRecordDecl>(Declaration);
  } else if (isa<ClassTemplateDecl>(Declaration)) {
    ClassTemplateDecl *TempTemplateDecl = cast<ClassTemplateDecl>(Declaration);
    ClsDecl = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  }
  D->Cxx = ClsDecl;
  SemaRef.getCurrentScope()->addUserDefinedType(D->Id,
                                      Context.CxxAST.getTypeDeclType(ClsDecl));
  SemaRef.enterScope(ClsDecl, D->Init);
  SemaRef.pushDecl(D);
  Elab.elaborateTypeBody(D, ClsDecl);
  SemaRef.popDecl();
  D->SavedScope = SemaRef.saveScope(D->Init);
  if (Template) {
    TemplateDeclarator->Data.TemplateInfo.DeclScope = SemaRef.saveScope(
      TemplateDeclarator->Data.TemplateInfo.Params);
    // This may not work ver well because the scope may need to be re-entered
    // at a later time and in order to do that correctly we may need to 
    // adjust the scope stack in order to correctly reproduce the
    // correct scope in order to elaborate this type fully. I'm currently
    // not sure how we are going to handle this properly. NOTE:
    // This issue may only occur when we are doing out of order elaborations.
    TemplateDeclarator->Data.TemplateInfo.ClangScope
      = SemaRef.moveToParentScopeNoPop();
  }
    
  return ClsDecl;
}



clang::Decl *Elaborator::elaborateDecl(Declaration *D) {
  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  // clang::Decl *Ret = nullptr;
  if (D->declaresRecord()) 
    return processCXXRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresFunction())
    return elaborateFunctionDecl(D);
  return elaborateVariableDecl(D);
  
  // 
  // TODO: We should be able to elaborate definitions at this point too.
  // We've already loaded salient identifier tables, so it shouldn't any
  // forward references should be resolvable.
}

// The parameter scope of a function declaration is always found in the
// second declarator.
static Declarator *getFunctionDeclarator(Declarator *D) {
  assert(D->isIdentifier());
  assert(D->Next->isFunction());
  return D->Next;
}

// Returns the function declarator part of D.
static Declarator *getFunctionDeclarator(Declaration *D) {
  assert(D->Decl);
  return getFunctionDeclarator(D->Decl);
}

// Get the Clang parameter declarations for D
static void getFunctionParameters(Declaration *D,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  Declarator *FnDecl = getFunctionDeclarator(D);
  const Syntax *ParamList = FnDecl->Data.ParamInfo.Params;
  Scope *ParamScope = FnDecl->Data.ParamInfo.ConstructedScope;
  for (const Syntax *P : ParamList->children()) {
    Declaration *PD = ParamScope->findDecl(P);
    assert(PD->Cxx && "No corresponding declaration");
    Params.push_back(cast<clang::ParmVarDecl>(PD->Cxx));
  }
}

void BuildTemplateParams(SyntaxContext &Ctx, Sema &SemaRef,
                         const Syntax *Params,
                         llvm::SmallVectorImpl<clang::NamedDecl *> &Res)
{
  std::size_t I = 0;
  for (const Syntax *P : Params->children()) {
    Elaborator Elab(Ctx, SemaRef);
    clang::NamedDecl *ND =
      cast_or_null<clang::NamedDecl>(Elab.elaborateDeclSyntax(P));
    // Just skip this on error.
    if (!ND)
      continue;

    // FIXME: set the depth too.
    if (auto *TP = dyn_cast<clang::NonTypeTemplateParmDecl>(ND)) {
      TP->setPosition(I);
    } else if (auto *TP = dyn_cast<clang::TemplateTypeParmDecl>(ND)) {
      // FIXME: Make this a friend of TTPD so we can set this :/
      // TP->setPosition(I);
    }

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");
    Res.push_back(ND);

    ++I;
  }
}

clang::Decl *Elaborator::elaborateFunctionDecl(Declaration *D) {
  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  Declarator *FnDclrtr = getFunctionDeclarator(D);

  // Create the template parameters if they exist.
  const Syntax *TemplParams = D->getTemplateParams();
  bool Template = TemplParams;
  bool InClass = SemaRef.getCurrentScope()->getKind() == SK_Class;
  clang::CXXRecordDecl *RD = nullptr;
  // Before we enter the template scope we need a reference to the containing
  // class.
  if (InClass) 
    RD = cast<clang::CXXRecordDecl>(SemaRef.getCurrentScope()->Record);

  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  if (Template) {
    SemaRef.enterScope(SK_Template, TemplParams);
    BuildTemplateParams(Context, SemaRef, TemplParams, TemplateParamDecls);
  }

  // Elaborate the return type.
  ExprElaborator TypeElab(Context, SemaRef);
  ExprElaborator::Expression TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (TypeExpr.isNull()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::TypeSourceInfo *TInfo = TypeExpr.get<clang::TypeSourceInfo *>();
  clang::DeclarationName Name = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::FunctionDecl *FD = nullptr;
  if(InClass) {
    

    const clang::FunctionType *FT = cast<clang::FunctionType>(TInfo->getType());
    clang::DeclarationNameInfo DNI(Name, D->Op->getLoc());
    if (D->getId()->isStr("constructor")) {
      if (FT->getReturnType() != Context.CxxAST.VoidTy) {
        // TODO: Emit correct diagonstic message here.
        llvm_unreachable("Constructor has incorrect return type.");
      }
      clang::QualType RecordTy = Context.CxxAST.getTypeDeclType(RD);
      clang::CanQualType Ty = Context.CxxAST.getCanonicalType(RecordTy);
      Name = Context.CxxAST.DeclarationNames.getCXXConstructorName(Ty);
      clang::DeclarationNameInfo DNI2(Name, D->Decl->getLoc());
      clang::CXXConstructorDecl* CtorDecl = nullptr;
      clang::ExplicitSpecifier ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
      FD = CtorDecl = clang::CXXConstructorDecl::Create(Context.CxxAST, RD, Loc, DNI2,
          clang::QualType(), nullptr, ES, false, false,
          clang::ConstexprSpecKind::CSK_unspecified);
      CtorDecl->setImplicit(false);
      CtorDecl->setDefaulted(false);
      CtorDecl->setBody(nullptr);
      CtorDecl->setAccess(clang::AS_public);
      clang::FunctionProtoType::ExtProtoInfo EPI;

      // Build an exception specification pointing back at this member.
      EPI.ExceptionSpec.Type = clang::EST_None;
      EPI.ExceptionSpec.SourceDecl = CtorDecl;

      // Set the calling convention to the default for C++ instance methods.
      EPI.ExtInfo = EPI.ExtInfo.withCallingConv(
          Context.CxxAST.getDefaultCallingConvention(/*IsVariadic=*/false,
                                                /*IsCXXMethod=*/true));
      clang::LangAS AS = SemaRef.getCxxSema().getDefaultCXXMethodAddrSpace();
      if (AS != clang::LangAS::Default) {
        EPI.TypeQuals.addAddressSpace(AS);
      }
      const clang::FunctionProtoType *FPT = cast<clang::FunctionProtoType>(
        TInfo->getType().getTypePtr());
      auto QT = Context.CxxAST.getFunctionType(Context.CxxAST.VoidTy,
        FPT->getParamTypes(), EPI);
      CtorDecl->setType(QT);

    } else if(D->getId()->isStr("destructor")) {
      if (FT->getReturnType() != Context.CxxAST.VoidTy) {
        // TODO: Emit correct diagonstic message here.
        assert(false && "Constructor has incorrect return type");
      }
      clang::QualType RecordTy = Context.CxxAST.getTypeDeclType(RD);
      clang::CanQualType Ty = Context.CxxAST.getCanonicalType(RecordTy);
      Name = Context.CxxAST.DeclarationNames.getCXXDestructorName(Ty);
      clang::DeclarationNameInfo DNI2(Name, D->Decl->getLoc());
      clang::CXXDestructorDecl* DtorDecl = nullptr;
      clang::ExplicitSpecifier ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
      FD = DtorDecl = clang::CXXDestructorDecl::Create(Context.CxxAST, RD, Loc,
          DNI2, clang::QualType(), nullptr, false, false,
          clang::ConstexprSpecKind::CSK_unspecified);
      DtorDecl->setImplicit(false);
      DtorDecl->setDefaulted(false);
      DtorDecl->setBody(nullptr);
      DtorDecl->setAccess(clang::AS_public);
      clang::FunctionProtoType::ExtProtoInfo EPI;

      // Build an exception specification pointing back at this member.
      EPI.ExceptionSpec.Type = clang::EST_None;
      EPI.ExceptionSpec.SourceDecl = DtorDecl;

      // Set the calling convention to the default for C++ instance methods.
      EPI.ExtInfo = EPI.ExtInfo.withCallingConv(
          Context.CxxAST.getDefaultCallingConvention(/*IsVariadic=*/false,
                                                /*IsCXXMethod=*/true));
      clang::LangAS AS = SemaRef.getCxxSema().getDefaultCXXMethodAddrSpace();
      if (AS != clang::LangAS::Default) {
        EPI.TypeQuals.addAddressSpace(AS);
      }
      const clang::FunctionProtoType *FPT = cast<clang::FunctionProtoType>(
        TInfo->getType().getTypePtr());
      if (FPT->getNumParams() != 0) {
        // TODO: Figure out how to correctly emit diagnostics here.
        assert(false && "Destructors cannot have any parameters.");
      }
      auto QT = Context.CxxAST.getFunctionType(Context.CxxAST.VoidTy,
        clang::None, EPI);
      DtorDecl->setType(QT);

    } else {
      FD = clang::CXXMethodDecl::Create(Context.CxxAST, RD, Loc, DNI,
                                        TInfo->getType(), TInfo,
                                        clang::StorageClass::SC_None,
                                        /*isInline*/true,
                                      clang::ConstexprSpecKind::CSK_unspecified,
                                        Loc);
    }
    clang::AccessSpecifier AS;
    if (computeAccessSpecifier(SemaRef, D, AS)) {
      return nullptr;
    }
    FD->setAccess(AS);
  } else {
    FD = clang::FunctionDecl::Create(Context.CxxAST, Owner, Loc, Loc, Name,
                                     TInfo->getType(), TInfo, clang::SC_None);
    if (FD->isMain()) {
      clang::AttributeFactory Attrs;
      clang::DeclSpec DS(Attrs);
      SemaRef.getCxxSema().CheckMain(FD, DS);
    }
  }

  // Create a template out for FD, if we have to.
  if (Template) {
    clang::SourceLocation Loc = TemplParams->getLoc();
    auto *TPL =
      clang::TemplateParameterList::Create(Context.CxxAST, Loc, Loc,
                                           TemplateParamDecls, Loc,
                                           /*RequiresClause=*/nullptr);
    auto *FTD = clang::FunctionTemplateDecl::Create(Context.CxxAST, Owner, Loc,
                                                    FD->getDeclName(), TPL, FD);
    FTD->setLexicalDeclContext(Owner);
    FD->setDescribedFunctionTemplate(FTD);
    Owner->addDecl(FTD);
    FnDclrtr->Data.ParamInfo.TemplateScope = SemaRef.saveScope(TemplParams);
  }

  // Update the function parameters.
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getFunctionParameters(D, Params);
  FD->setParams(Params);

  D->Cxx = FD;
  // Add the declaration and update bindings.
  if (!Template && !D->declaresConstructor()) {
    Owner->addDecl(FD);
  }
  if(D->declaresConstructor()) {
    clang::CXXConstructorDecl* CtorDecl = cast<clang::CXXConstructorDecl>(D->Cxx);    
    clang::DeclarationNameInfo DNI2(Name, D->Decl->getLoc());
    SemaRef.getCxxSema().PushOnScopeChains(CtorDecl, SemaRef.getCurClangScope());
    SemaRef.getCxxSema().CheckConstructor(CtorDecl);
    SemaRef.getCxxSema().CheckOverrideControl(CtorDecl);
    // Need to add 
    clang::LookupResult R(SemaRef.getCxxSema(), DNI2, clang::Sema::LookupOrdinaryName);
    if(SemaRef.getCxxSema().CheckFunctionDeclaration(SemaRef.getCurClangScope(),
        CtorDecl, R, true)) {
    }
  }
  return FD;
}

static clang::StorageClass getStorageClass(Elaborator &Elab) {
  // FIXME: What is the storage class for a variable? Computed from scope
  // and specifiers probably. We don't have specifiers yet.
  return Elab.SemaRef.getCurrentScope()->isBlockScope()
    ? clang::SC_Auto
    : clang::SC_Static;
}


clang::Decl *Elaborator::elaborateVariableDecl(Declaration *D) {
  if (SemaRef.getCurrentScope()->isParameterScope())
    return elaborateParameterDecl(D);
  if (SemaRef.getCurrentScope()->isTemplateScope())
    return elaborateTemplateParamDecl(D);
  if (SemaRef.getCurrentScope()->isClassScope())
    return elaborateField(D);

  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  ExprElaborator TypeElab(Context, SemaRef);
  ExprElaborator::Expression TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (TypeExpr.isNull()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = TypeExpr.get<clang::TypeSourceInfo *>();
  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::StorageClass SC = getStorageClass(*this);


  // Create the variable and add it to it's owning context.
  clang::VarDecl *VD = clang::VarDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                              Id, TInfo->getType(), TInfo, SC);
  Owner->addDecl(VD);
  D->Cxx = VD;
  return VD;
}

clang::Decl *Elaborator::elaborateParameterDecl(Declaration *D) {
  // Get type information.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();

  ExprElaborator TypeElab(Context, SemaRef);
  ExprElaborator::Expression TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (TypeExpr.isNull()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = TypeExpr.get<clang::TypeSourceInfo *>();

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();

  // Just return the parameter. We add it to it's function later.
  clang::ParmVarDecl *P = clang::ParmVarDecl::Create(Context.CxxAST, Owner, Loc,
                                                     Loc, Id, TInfo->getType(),
                                                     TInfo, clang::SC_None,
                                                     /*DefaultArg=*/nullptr);
  D->Cxx = P;
  return P;
}

clang::Decl *Elaborator::elaborateTemplateParamDecl(Declaration *D) {
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();

  ExprElaborator TypeElab(Context, SemaRef);
  ExprElaborator::Expression TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (TypeExpr.isNull()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = TypeExpr.get<clang::TypeSourceInfo *>();

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();

  // This is a template type parameter decl.
  if (TInfo->getType()->getAs<clang::CppxKindType>()) {
    auto *TTPD =
      clang::TemplateTypeParmDecl::Create(Context.CxxAST, Owner, Loc, Loc, 0, 0,
                              Id, /*TypenameKW=*/true, /*ParameterPack=*/false);
    D->Cxx = TTPD;
    return TTPD;
  }

  // The depth and position of the parameter will be set later.
  auto *NTTP =
    clang::NonTypeTemplateParmDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                           0, 0, Id, TInfo->getType(),
                                           /*Pack=*/false, TInfo);
  D->Cxx = NTTP;
  return NTTP;
}

clang::Decl *Elaborator::elaborateDeclSyntax(const Syntax *S) {
  // Identify this as a declaration first.
  identifyDecl(S);

  // Elaborate the declaration.
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S); 
  if (D) {
    elaborateDecl(D);
    elaborateDef(D);
    return D->Cxx;
  }

  return nullptr;
}

clang::Decl *Elaborator::elaborateDeclEarly(Declaration *D) {
  assert(D && D->getId() && "Early elaboration of unidentified declaration");

  elaborateDecl(D);
  elaborateDef(D);
  return D->Cxx;
}

void Elaborator::elaborateDeclInit(const Syntax *S) {
  // TODO: See elaborateDeclType. We have the same kinds of concerns.
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    return;
  elaborateDef(D);
}

void Elaborator::elaborateDef(Declaration *D) {
  if (D->declaresRecord())
    return elaborateTypeDefinition(D);
  if (D->declaresFunction())
    return elaborateFunctionDef(D);
  if (SemaRef.getCurrentScope()->isTemplateScope())
    return elaborateTemplateParamInit(D);
  if(D->declaresMemberVariable())
    return elaborateFieldInit(D);  
  return elaborateVariableInit(D);
}

void Elaborator::elaborateFunctionDef(Declaration *D) {
  if (D->declaresConstructor()) {
    llvm::SmallVector<clang::CXXCtorInitializer*, 32> Initializers;
    SemaRef.getCxxSema().ActOnMemInitializers(D->Cxx, clang::SourceLocation(),
        Initializers, false);
  }
  if (!D->Cxx)
    return;

  assert(isa<clang::FunctionDecl>(D->Cxx) && "Bad function declaration.");
  clang::FunctionDecl *FD = cast<clang::FunctionDecl>(D->Cxx);

  if (!D->Init)
    return;

  if (SemaRef.checkForRedefinition<clang::FunctionDecl>(D))
    return;

  SemaRef.pushDecl(D);

  // We saved the parameter scope while elaborating this function's type,
  // so push it on before we enter the function scope.
  Declarator *FnDecl = getFunctionDeclarator(D);
  bool IsTemplate = D->declaresTemplate();

  Scope *TemplateScope = nullptr;
  if (IsTemplate) {
    SemaRef.pushScope(FnDecl->Data.ParamInfo.TemplateScope);
    TemplateScope = SemaRef.getCurrentScope();
  }
  SemaRef.pushScope(FnDecl->Data.ParamInfo.ConstructedScope);
  // The parameter scope is owned by the template scope, but they were
  // constructed out of order.
  if (TemplateScope) {
    SemaRef.getCurrentScope()->setParent(TemplateScope);
  }
  SemaRef.getCxxSema().PushFunctionScope();
  SemaRef.enterScope(SK_Function, D->Init);
  // FIXME: is this necessary for Gold? It enables some more semantic
  // checking, but not all of it is necessarily meaningful to us.
  
  // Elaborate the function body.
  StmtElaborator BodyElaborator(Context, SemaRef);
  clang::Stmt *Body = BodyElaborator.elaborateBlock(D->Init);
  FD->setBody(Body);

  // Leave the function scope.
  SemaRef.leaveScope(D->Init);
  // Leave the parameter scope.
  SemaRef.popScope();
  SemaRef.getCxxSema().PopFunctionScopeInfo();

  // Leave the template scope
  if (IsTemplate)
    SemaRef.popScope();

  SemaRef.popDecl();
}

/// In the case of an automatically deduced array macro, <initalizer_list>
/// needs to be included to perform deduction. In that case, just construct
/// the necessary array type manually.
/// ex:
///
/// \code
///   xs = array{0, 1, 2};
/// \endcode
///
/// Here, we construct array type [sizeof(list)]typeof(list[0])
static clang::QualType buildImplicitArrayType(clang::ASTContext &Ctx,
                                              clang::Sema &SemaRef,
                                              clang::InitListExpr *List) {
  if (!List->getNumInits()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "Cannot create an empty array");
    SemaRef.Diags.Report(List->getBeginLoc(), DiagID);
    return clang::QualType();
  }

  clang::QualType EltTy = List->getInit(0)->getType();
  llvm::APSInt Size = llvm::APSInt::get(List->getNumInits());

  return Ctx.getConstantArrayType(EltTy, Size, /*SizeExpr=*/nullptr,
                                  clang::ArrayType::Normal, 0);
}

void Elaborator::elaborateVariableInit(Declaration *D) {
  if (!D->Cxx){
    return;
  }
  clang::VarDecl *VD = cast<clang::VarDecl>(D->Cxx);

  if (!D->Init) {
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
    SemaRef.getCxxSema().ActOnUninitializedDecl(VD); 
    return;
  }

  // FIXME: If we synthesize initializers, this might need to happen before that
  if (SemaRef.checkForRedefinition<clang::VarDecl>(D))
    return;

  // Elaborate the initializer.
  ExprElaborator ExprElab(Context, SemaRef);
  ExprElaborator::Expression Init = ExprElab.elaborateExpr(D->Init);

  // Make sure the initializer was elaborated as a type. 
  // FIXME: This will need to change in order to properly address how identifiers
  // are being processed. Because we will need to change things so that we can
  // properly identify member types, and member templates, template template
  // parameters etc.
  if (Init.is<clang::TypeSourceInfo *>()) {
    llvm::errs() << "Expected expression.\n";
    return;
  }

  clang::Expr *InitExpr = Init.get<clang::Expr *>();
  // Perform auto deduction.
  if (VD->getType()->isUndeducedType()) {
    clang::QualType Ty;

    // Certain macros must be deduced manually.
    if (const MacroSyntax *InitM = dyn_cast<MacroSyntax>(D->Init)) {
        assert (isa<AtomSyntax>(InitM->getCall()) && "Unexpected macro call");
        assert (isa<clang::InitListExpr>(InitExpr) &&
                "Invalid array macro init");

        const AtomSyntax *Call = cast<AtomSyntax>(InitM->getCall());
        if (Call->getSpelling() == "array") {
          Ty = buildImplicitArrayType(Context.CxxAST, SemaRef.getCxxSema(),
                                      cast<clang::InitListExpr>(InitExpr));

          if (Ty.isNull()) {
            VD->setInvalidDecl();
            return;
          }
        }
    } else {
      clang::Sema &CxxSema = SemaRef.getCxxSema();
      auto Result = CxxSema.DeduceAutoType(VD->getTypeSourceInfo(), InitExpr, Ty);
      if (Result == clang::Sema::DAR_Failed) {
        // FIXME: Make this a real diagnostic.
        llvm::errs() << "Failed to deduce type of expression.\n";
        return;
      }
    }

    VD->setType(Ty);
  }

  // Update the initializer.
  SemaRef.getCxxSema().AddInitializerToDecl(VD, InitExpr, /*DirectInit=*/true);
}

void Elaborator::elaborateTemplateParamInit(Declaration *D) {
  if (!D->Init)
    return;

  assert((isa<clang::NonTypeTemplateParmDecl>(D->Cxx) ||
          isa<clang::TemplateTemplateParmDecl>(D->Cxx) ||
          isa<clang::TemplateTypeParmDecl>(D->Cxx)) &&
         "Initializing non-template parameter.");

  // FIXME: isADeclarationOrDefinition isn't implemented
  //        for template parameters, so we can't check for
  //        redefinition using the template.

  // TODO: these might have default arguments.
}

void Elaborator::elaborateTypeDefinition(Declaration *D) {
  bool Template = D->declaresTemplateType();
  if (Template) {
    // Pushing template scopes onto stack again.
    Declarator *templateArgs = D->getFirstTemplateDeclarator();
    assert(templateArgs && "Failed to get template arguments.\n");
    SemaRef.ReEnterScope(templateArgs->Data.TemplateInfo.ClangScope);
    SemaRef.pushScope(templateArgs->Data.TemplateInfo.DeclScope);
  }

  auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
  auto const* BodyArray = dyn_cast<ArraySyntax>(MacroRoot->getBlock());

  SemaRef.pushScope(D->SavedScope);
  // SemaRef.pushDecl(D);
  // Simply getting the current scope.

  
  clang::CXXRecordDecl *R = dyn_cast<clang::CXXRecordDecl>(D->Cxx);
  clang::Scope *Scope = SemaRef.enterClangScope(clang::Scope::ClassScope
                                                | clang::Scope::DeclScope);
  SemaRef.getCxxSema().ActOnTagStartDefinition(Scope, R);
  SemaRef.getCxxSema().ActOnStartCXXMemberDeclarations(Scope, R,
    clang::SourceLocation(), true, clang::SourceLocation());
  SemaRef.setCurrentDecl(D);
  // Since all declarations have already been added, we don't need to do another
  // Reordering scan. Maybe?

  // TODO:/FIXME: Need to create a means for building member functions/initializers
  for (const Syntax *SS : BodyArray->children()) {
    elaborateDeclType(SS);
  }
  
  // Elaborating types first.
  for (const Syntax *SS : BodyArray->children()) {
    Declaration *LocalDecl = SemaRef.getCurrentScope()->findDecl(SS);
    if (!LocalDecl)
      continue;
    if(LocalDecl->declaresRecord() || LocalDecl->declaresType())
      elaborateDef(LocalDecl);
  }

  // Elaborating members second. TODO: Once we have static methods we need
  // to handle those before we handle the variables.
  for (const Syntax *SS : BodyArray->children()) {
    Declaration *LocalDecl = SemaRef.getCurrentScope()->findDecl(SS);
    if (!LocalDecl)
      continue;
    if (LocalDecl->declaresMemberVariable()){
      elaborateDef(LocalDecl);
    }
  }

  // Elaborating everything else.
  for (const Syntax *SS : BodyArray->children()) {
    Declaration *LocalDecl = SemaRef.getCurrentScope()->findDecl(SS);
    if (!LocalDecl)
      continue;
    if (LocalDecl->declaresMemberFunction() || LocalDecl->declaresConstructor()) 
      elaborateDef(LocalDecl);
    
  }
  SemaRef.getCxxSema().ActOnFinishCXXMemberSpecification(Scope,
    clang::SourceLocation(), R, clang::SourceLocation(), clang::SourceLocation(),
    clang::ParsedAttributesView());
  SemaRef.getCxxSema().ActOnFinishCXXMemberDecls();
  clang::Decl *TempDeclPtr = R;
  SemaRef.getCxxSema().ActOnTagFinishDefinition(Scope, TempDeclPtr,
                                                clang::SourceRange());
  SemaRef.setCurrentDecl(D->getOwner());
  SemaRef.leaveClangScope(D->Op->getLoc());
  SemaRef.popScope();
  if (Template) {
    // Leaving template scopes again.
    SemaRef.popScope();
    SemaRef.leaveClangScope(D->Op->getLoc());
  }

}


clang::Decl *Elaborator::elaborateTypeBody(Declaration* D, clang::CXXRecordDecl* R) {
  if(!D->Init) {
    // FIXME: Handle forward declarations here? I think.
    assert(false && "No implementation for type body not implemented yet.");
    return nullptr;
  }
  auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
  assert(MacroRoot && "Invalid AST structure.");
  auto const* BodyArray = dyn_cast<ArraySyntax>(MacroRoot->getBlock());
  assert(BodyArray && "Invalid AST structure Expected array structure.");

  for (auto const* ChildDecl : BodyArray->children()) {
    identifyDecl(ChildDecl);
  }
  return D->Cxx;
}


clang::Decl *Elaborator::elaborateField(Declaration *D) {
  // Get the type of the entity.
  clang::CXXRecordDecl *Owner = SemaRef.getCurrentScope()->Record;
  if(!Owner) {
    assert(false && "Failed to get owner data from scope.");
  }
  ExprElaborator TypeElab(Context, SemaRef);
  ExprElaborator::Expression TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);

  if (TypeExpr.isNull()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = TypeExpr.get<clang::TypeSourceInfo *>();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::DeclarationName DN = D->getId();
  clang::InClassInitStyle InitStyle = clang::InClassInitStyle::ICIS_NoInit;
  if (D->Init) {
    InitStyle = clang::InClassInitStyle::ICIS_ListInit;
  }
  clang::AccessSpecifier AS;
  if (computeAccessSpecifier(SemaRef, D, AS)) {
    return nullptr;
  }
  clang::FieldDecl *FD = SemaRef.getCxxSema().CheckFieldDecl(DN, TInfo->getType(),
                                          TInfo, /*RecordDecl=*/Owner,
                                          Loc, /*Mutable=*/false,
                                          /*BitWidth=*/nullptr, InitStyle, Loc,
                                          AS, nullptr);
  Owner->addDecl(FD);
  D->Cxx = FD;
  // handleInClassAttributeApplication(SemaRef, Context, FD, D);
  return FD;
}

void Elaborator::elaborateFieldInit(Declaration *D) {
  if (!D->Init)
    return;
  assert(D && "Missing Declaration.");
  ExprElaborator ExprElab(Context, SemaRef);
  ExprElaborator::Expression Init = ExprElab.elaborateExpr(D->Init);

  // Make sure the initializer was not elaborated as a type.
  if (Init.is<clang::TypeSourceInfo*>()) {
    // TODO: Insert diagnostics here?
    llvm::errs() << "Expected expression, not a type.\n";
    return;
  }
  clang::FieldDecl *Field = cast<clang::FieldDecl>(D->Cxx);
  // Checking if the field declaration needs an implicit conversion or not.
  clang::Expr *InitExpr = Init.get<clang::Expr*>();
  if (Field->getType() == InitExpr->getType())
    return Field->setInClassInitializer(InitExpr);
  // Trying an implicit conversion.
  clang::ExprResult ConvertedResult
      = SemaRef.getCxxSema().PerformImplicitConversion(InitExpr,
      Field->getType(), clang::Sema::AssignmentAction::AA_Initializing, false);
  Field->setInClassInitializer(ConvertedResult.get());
}

// Get the clang::QualType described by an operator':' call.
clang::QualType Elaborator::getOperatorColonType(const CallSyntax *S) const {
  // Get the argument list of an operator':' call. This should have
  // two arguments, the entity (argument 1) and its type (argument 2).

  // Right now this has to be an explicitly named type.
  // if (const AtomSyntax *Typename = dyn_cast<AtomSyntax>(S->getArgument(1))) {
  //   auto BuiltinMapIter = BuiltinTypes.find(Typename->Tok.getSpelling());
  //   if (BuiltinMapIter == BuiltinTypes.end())
  //     assert(false && "Only builtin types are supported right now.");

  //   return BuiltinMapIter->second;
  // }

  assert(false && "Working on fixing this.");
}
static Declarator *makeDeclarator(Sema &SemaRef, const Syntax *S);

static Declarator *buildIdDeclarator(const Syntax *S, Declarator *Next) {
  Declarator *D = new Declarator(DK_Identifier, Next);
  D->Data.Id = S;
  D->recordAttributes(S);
  return D;
}

static Declarator *buildTypeDeclarator(const Syntax *S, Declarator *Next) {
  assert((isa<AtomSyntax>(S) || isa<CallSyntax>(S)) &&
         "cannot build type-declarator out of given syntax");
  Declarator *D = new Declarator(DK_Type, Next);

  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    D->Call = Call;
    D->Data.Type = Next ? Next->getType() : Call->getArgument(1);
  } else if (isa<AtomSyntax>(S)) {
    D->Data.Type = S;
  }

  return D;
}

static Declarator *buildTypeExpression(const Syntax *S, Declarator *Next) {
  assert((isa<AtomSyntax>(S) || isa<CallSyntax>(S)) &&
         "cannot build type-declarator out of given syntax");
  Declarator *D = new Declarator(DK_Type, Next);

  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S))
    D->Call = Call;
  D->Data.Type = S;

  return D;
}

static Declarator *buildFunctionDeclarator(const CallSyntax *S, Declarator *Next) {
  // FIXME: Store the parameter list.
  Declarator *D = new Declarator(DK_Function, Next);
  D->Call = S;
  D->Data.ParamInfo.Params = S->getArguments();
  D->Data.ParamInfo.TemplateParams = nullptr;
  D->Data.ParamInfo.TemplateScope = nullptr;
  D->Data.ParamInfo.ConstructedScope = nullptr;
  return D;
}

static Declarator *buildFunctionDeclarator(const CallSyntax *S,
                                           const ElemSyntax *T,
                                           Declarator *Next) {
  Declarator *D = new Declarator(DK_Function, Next);
  D->Call = S;
  D->Data.ParamInfo.Params = S->getArguments();
  D->Data.ParamInfo.TemplateParams = T->getArguments();
  D->Data.ParamInfo.TemplateScope = nullptr;
  D->Data.ParamInfo.ConstructedScope = nullptr;
  return D;
}

static Declarator *buildTemplatedName(const ElemSyntax *Call,
                                      Declarator *Next) {
  Declarator *D = new Declarator(DK_TemplateType, Next);
  D->Call = Call;
  D->Data.TemplateInfo.Params = Call->getArguments();
  D->Data.TemplateInfo.DeclScope = nullptr;
  return D;
}

static Declarator *buildPointerDeclarator(const CallSyntax *S, 
                                          Declarator *Next) {
  Declarator *D = new Declarator(DK_Pointer, Next);
  D->Call = S;
  return D;
}

static Declarator *buildArrayDeclarator(const CallSyntax *S,
                                        Declarator *Next) {
  Declarator *D = new Declarator(DK_Array, Next);
  D->Call = S;
  D->Data.Index = S->getArgument(0);
  return D;
}

/// FIXME: Convert this back to an iterative function, if possible (see
///        disabled iterative version below).
///
/// Analyze and decompose the declarator.
///
/// This is a recursive walk through a series of call nodes. In each step,
/// we build a declarator fragment.
Declarator *makeDeclarator(Sema &SemaRef, const Syntax *S, Declarator *Next) {
  // If we find an atom, then we're done.
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(S)) {

    // This might be a typename, in which case, build a type-declarator.
    clang::DeclarationNameInfo DNI(
      {&SemaRef.Context.CxxAST.Idents.get(Atom->getSpelling()), S->getLoc()});
    clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupTagName);
    if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope()))
      return buildTypeDeclarator(Atom, Next);

    // Otherwise just build an identifier-declarator.
    return buildIdDeclarator(Atom, Next);

  } else if(const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    if (const AtomSyntax *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {

      // Check for "builtin" operators in the declarator.
      if (Callee->getSpelling() == "operator':'") {

        // If the rhs is something complicated, we need to
        // elaborate it recursively.
        if (isa<CallSyntax>(Call->getArgument(1))) {
          // Elaborate rhs, and then elaborate lhs using the completed
          // type-declarator from rhs as the type.
          Next = makeDeclarator(SemaRef, Call->getArgument(1), Next);
          return makeDeclarator(SemaRef, Call->getArgument(0), Next);
        }

        // Otherwise, rhs is just a literal type.
        return makeDeclarator(SemaRef, Call->getArgument(0),
                              buildTypeDeclarator(Call, Next));

      } else if (Callee->getSpelling() == "operator'^'") {
        // We have a pointer operator, so first create a declarator
        // out of its inner type and use that as `Next`.
        Next = makeDeclarator(SemaRef, Call->getArgument(0), Next);

        // Now build a pointer-declarator that owns its inner type and
        // we're done.
        return buildPointerDeclarator(Call, Next);

      } else if (Callee->getSpelling() == "operator'[]'") {
        // This is a prefix operator'[]', meaning we are creating an array type.
        Next = makeDeclarator(SemaRef, Call->getArgument(1), Next);
        return makeDeclarator(SemaRef, Call->getArgument(0),
                              buildArrayDeclarator(Call, Next));
      } else if (Callee->getSpelling() == "operator'.'") {
        // This is also a possible type declaration because it's a nested type
        // declaration.
        return buildTypeExpression(Call, Next);
      } else if (Callee->getSpelling() == "operator'in'") {
        return makeDeclarator(SemaRef, Call->getArgument(0), Next);
      }
      // Otherwise, this appears to be a function declarator.
      Declarator *Temp = makeDeclarator(SemaRef, Callee,
                                        buildFunctionDeclarator(Call, Next));
      Temp->recordAttributes(Call);
      return Temp;


    } else if (const ElemSyntax *Callee = dyn_cast<ElemSyntax>(Call->getCallee())) {
      // We have a template parameter list here, so build the
      // function declarator accordingly.
      Declarator *Temp = makeDeclarator(SemaRef, Callee->getObject(),
                                        buildFunctionDeclarator(Call, Callee,
                                                                Next));
      Temp->recordAttributes(Call);
      return Temp;
    }
  } else if (const ElemSyntax *TemplateType = dyn_cast<ElemSyntax>(S)) {
    // Building type parameters for each element within the list and chaining
    // them together.
    if (const AtomSyntax *TemplateName
        = dyn_cast<AtomSyntax>(TemplateType->getObject())) {
      Declarator *Temp = buildIdDeclarator(TemplateName,
                                        buildTemplatedName(TemplateType, Next));
      Temp->recordAttributes(TemplateType);
      return Temp;
    }
    llvm_unreachable("Invalid templated declarator.");
  }
  return nullptr;
}

Declarator *makeDeclarator(Sema &SemaRef, const Syntax *S) {
  Declarator *D = nullptr;
  return makeDeclarator(SemaRef, S, D);
}

static clang::IdentifierInfo *getIdentifier(Elaborator &Elab,
                                            const Declarator *D) {
  if (const auto *Atom = dyn_cast_or_null<AtomSyntax>(D->getId()))
    return &Elab.Context.CxxAST.Idents.get(Atom->getSpelling());
  return nullptr;
}

void Elaborator::identifyDecl(const Syntax *S) {
  // Keep track of whether or not this is an operator'=' call.
  bool OperatorEquals = false;

  // Declarations only appear in calls.
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    if (const auto *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
      llvm::StringRef Op = Callee->getToken().getSpelling();
      
      // Need to figure out if this is a declaration or expression?
      // Unpack the declarator.
      const Syntax *Decl;
      const Syntax *Init;
      if (Op == "operator'='") {
        // This is to reject t.x as a declaration.
        // Also also reject the delcaration
        const auto *Args = cast<ListSyntax>(Call->getArguments());
        Decl = Args->getChild(0);

        // This is for checking if a declaration already exists in a parent scope
        // for example if this is in a member function and we are accessing a
        // member variable.
        if(const AtomSyntax *LHS = dyn_cast<AtomSyntax>(Decl)) {
          clang::DeclarationNameInfo DNI({
              &Context.CxxAST.Idents.get(LHS->getSpelling())
            }, S->getLoc());
          clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
          if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) 
            return;
        }

        if (const CallSyntax *InnerCallOp = dyn_cast<CallSyntax>(Decl))
          if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(
                                                      InnerCallOp->getCallee()))
            if (Atom->getSpelling() == "operator'.'")
              return;
        Init = Args->getChild(1);
        OperatorEquals = true;

      } else if (Op == "operator'!'") {
        const auto *Args = cast<ListSyntax>(Call->getArguments());

        // Disallow definitions here.
        //
        // FIXME: This should be an error, not an assertion.
        if (SemaRef.getCurrentScope()->isParameterScope())
          assert(false && "Function definition");

        Decl = Args->getChild(0);
        Init = Args->getChild(1);
      } else if (Op == "operator':'") {
        Decl = S;
        Init = nullptr;
      } else if (Op == "operator'in'") {
        Decl = S;
        Init = nullptr;
      } else {
        // Syntactically, this is not a declaration.
        return;
      }


      // FIXME: I think we can filter out some syntactic forms as
      // non-declarations. For example, the following look like definitions
      // but are actually assignments.
      //
      //    f(x) = 4
      //    a[3] = 5
      //
      // The array case might be tricky to disambiguate, and requires
      // a lookup. If it's the first initialization of the variable, then
      // it must be a declaration. See below.

      // Try to build a declarator for the declaration.
      Declarator *Dcl = makeDeclarator(SemaRef, Decl);
      if (!Dcl) {
        return;
      }

      // Parameters can only be declared as x, x:T, or :T. The full range
      // of declarator syntax is not supported.
      //
      // FIXME: Emit an error instead of a diagnostic.
      if (SemaRef.getCurrentScope()->isParameterScope() && !Dcl->isIdentifier()){
        assert(false && "Invalid parameter declaration");
      }

      clang::IdentifierInfo* Id = getIdentifier(*this, Dcl);

      Scope *CurScope = SemaRef.getCurrentScope();
      if (CurScope->isBlockScope()) {
        // If we're assigning to a name that already exist in the current block,
        // then we're not declaring anything. For example:
        // \code
        //    x = 3
        //    x = 4
        // \endcode
        // The first statement is a declaration. The second is an assignment.
        // FIXME: is this the right way to handle the lookup set?
        if (!CurScope->findDecl(Id).empty() && OperatorEquals) 
          return;
      }

      // Create a declaration for this node.
      //
      // FIXME: Do a better job managing memory.
      Declaration *ParentDecl = SemaRef.getCurrentDecl();
      Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, Init);
      TheDecl->Id = Id;

      // If we're in namespace or parameter scope and this identifier already
      // exists, consider it a redeclaration.
      // TODO: distinguish between redefinition, redeclaration, and redeclaration
      // with different type.
      if ((CurScope->isNamespaceScope() || CurScope->isParameterScope()) &&
          !TheDecl->declaresFunction()) {
        // FIXME: rewrite this!!
        auto DeclSet = CurScope->findDecl(Id);

        if (!DeclSet.empty()) {
          assert((DeclSet.size() == 1) && "elaborated redefinition.");
          TheDecl->setPreviousDecl(*DeclSet.begin());
        }
      }

      SemaRef.getCurrentScope()->addDecl(TheDecl);
    }
  }

  // FIXME: What other kinds of things are declarations?
  //
  // TODO: If S is a list, then we might be looking at one of these
  //
  //    x, y : int
  //    x, y = foo()
  //
  // We need to elaborate each declarator in the list, and then propagate
  // type information backwards.

  return;
}

FusedOpKind getFusedOpKind(Sema &SemaRef, llvm::StringRef Spelling) {
  const clang::IdentifierInfo *Tokenization =
    &SemaRef.Context.CxxAST.Idents.get(Spelling);

  if (Tokenization == SemaRef.OperatorColonII)
    return FOK_Colon;
  if (Tokenization == SemaRef.OperatorExclaimII)
    return FOK_Exclaim;
  if (Tokenization == SemaRef.OperatorEqualsII)
    return FOK_Equals;
  if (Tokenization == SemaRef.OperatorIfII)
    return FOK_If;
  if (Tokenization == SemaRef.OperatorElseII)
    return FOK_Else;
  if (Tokenization == SemaRef.OperatorReturnII)
    return FOK_Return;
  if (Tokenization == SemaRef.OperatorReturnsII)
    return FOK_Return;
  if (Tokenization == SemaRef.OperatorDotII)
    return FOK_MemberAccess;
  if (Tokenization == SemaRef.OperatorForII)
    return FOK_For;
  if (Tokenization == SemaRef.OperatorInII)
    return FOK_In;
  return FOK_Unknown;
}

} // namespace gold
