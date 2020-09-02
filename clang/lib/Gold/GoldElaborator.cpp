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

#include "clang/Gold/GoldElaborator.h"

#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/Stmt.h"

#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldDeclarationBuilder.h"


namespace gold {



static void applyESIToFunctionType(SyntaxContext &Context, Sema &SemaRef,
                                   clang::FunctionDecl *FD,
                       const clang::FunctionProtoType::ExceptionSpecInfo &ESI);

AttrFormat checkAttrFormatAndName(const Syntax *Attr, llvm::StringRef &Name) {
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Attr)) {
    Name = Atom->getSpelling();
    return AF_Name;
  } else if (const CallSyntax *Call = dyn_cast<CallSyntax>(Attr)) {
    if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Call->getCallee())) {
      Name = Atom->getSpelling();
      return AF_Call;
    }
  }
  return AF_Invalid;
}



/// Overload for declaration processing.
template<typename OnAttr, typename IsSameAttr, typename OnDuplicate>
static bool locateValidAttribute(Declaration *D, OnAttr OnAttribute,
                                 IsSameAttr CheckAttr, OnDuplicate OnDup) {
  assert(D && "Invalid declaration.");

  if (!D->Decl->UnprocessedAttributes)
    return false;
  return locateValidAttribute(*D->Decl->UnprocessedAttributes, OnAttribute,
      CheckAttr, OnDup);
}


static bool computeAccessSpecifier(Sema& SemaRef, Attributes& attrs,
                                   clang::AccessSpecifier& AS) {
  AS = clang::AS_public;
  return locateValidAttribute(attrs,
    // OnAttr
    [&](const Syntax *Attr) -> bool {
      llvm::StringRef ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        if (ActualName == "private") {
          AS = clang::AS_private;
          return true;
        } else if (ActualName == "protected") {
          AS = clang::AS_protected;
          return true;
        } else if (ActualName == "public") {
          AS = clang::AS_public;
          return true;
        }
        return false;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "private" || ActualName == "protected"
            || ActualName == "public") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          return true;
        }
        return false;
      }
      return false;
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      llvm::StringRef ActualName;
      checkAttrFormatAndName(Attr, ActualName);
      return (ActualName == "private"
              || ActualName == "protected"
              || ActualName == "public");
    },
    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr){
      SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                           clang::diag::err_duplicate_access_specifier)
                           << FirstAttr->getLoc() << DuplicateAttr->getLoc();
    });
}


static bool isVirtualBase(Sema& SemaRef, Attributes& attrs,
                          bool &IsVirtual) {
  IsVirtual = false;
  return locateValidAttribute(attrs,
    // OnAttr
    [&](const Syntax *Attr) -> bool {
      llvm::StringRef ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        if (ActualName == "virtual") {
          IsVirtual = true;
          return true;
        }
        return false;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "virtual") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          IsVirtual = true;
          return true;
        }
        return false;
      }
      return false;
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      llvm::StringRef ActualName;
      checkAttrFormatAndName(Attr, ActualName);
      return ActualName == "virtual";
    },
    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr) {
      SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                           clang::diag::err_duplicate_access_specifier)
                           << FirstAttr->getLoc() << DuplicateAttr->getLoc();
    });
}


/// This should ONLY be used with member variables
bool isStaticMember(Sema& SemaRef, Declaration *D, bool &IsStatic) {
  return locateValidAttribute(D,
    // OnAttr
    [&](const Syntax *Attr) -> bool{
      llvm::StringRef ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        if (ActualName == "static") {
          IsStatic = true;
          return true;
        }
        return false;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "static") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          IsStatic = true;
          return true;
        }
        return false;
      }
      return false;
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Attr)) {
        if (Atom->getSpelling() == "static") {
          return true;
        }
      }
      return false;
    },
    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr) {
      return SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                                  clang::diag::err_conflicting_attributes)
                                  << FirstAttr->getLoc()
                                  << DuplicateAttr->getLoc();
    });
}

/// This should ONLY be used with member variables
bool isMutable(Sema& SemaRef, Declaration *D, bool &IsMutable) {
  IsMutable = false;
  return locateValidAttribute(D,
    // OnAttr
    [&](const Syntax *Attr) -> bool{
      llvm::StringRef ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        if (ActualName == "mutable") {
          IsMutable = true;
          return true;
        }
        return false;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "mutable") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          IsMutable = true;
          return true;
        }
        return false;
      }
      return false;
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      llvm::StringRef ActualName;
      checkAttrFormatAndName(Attr, ActualName);
      return ActualName == "mutable";
    },
    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr) {
      return SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                                  clang::diag::err_conflicting_attributes)
                                  << FirstAttr->getLoc()
                                  << DuplicateAttr->getLoc();
    });
}

Elaborator::Elaborator(SyntaxContext &Context, Sema &SemaRef)
  : Context(Context), SemaRef(SemaRef) {}

clang::Decl *Elaborator::elaborateFile(const Syntax *S) {

  assert(isa<FileSyntax>(S) && "S is not a file");

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

  return Context.CxxAST.getTranslationUnitDecl();
}

void Elaborator::startFile(const Syntax *S) {
  // This is missed during initialziation because of the language setting.
  SemaRef.getCxxSema().FieldCollector.reset(new clang::CXXFieldCollector());
  // Setting up clang scopes.
  clang::Scope *Scope = SemaRef.enterClangScope(clang::Scope::DeclScope);
  Scope->setEntity(Context.CxxAST.getTranslationUnitDecl());
  SemaRef.getCxxSema().ActOnTranslationUnitScope(Scope);
  SemaRef.getCxxSema().Initialize();

  // Count the decls generated by clang::Sema::Initialize().
  clang::TranslationUnitDecl *TU = Context.CxxAST.getTranslationUnitDecl();
  for (auto D = TU->decls_begin(); D != TU->decls_end(); ++D)
    ++ImplicitSemaDecls;

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

static void processBaseSpecifiers(Elaborator& Elab, Sema& SemaRef,
                                  SyntaxContext& Context, Declaration *D,
                                  clang::CXXRecordDecl *R,
                                  const CallSyntax *ClsKwCall) {
  const ListSyntax *Bases = dyn_cast<ListSyntax>(ClsKwCall->getArguments());
  if (!Bases)
    return;

  ExprElaborator TypeElab(Context, SemaRef);

  // Evaluating each individual child expression. Some could be template names.
  // It's also worth noting that these type of bases could have attributes
  // associated with each expression.
  Sema::ClangScopeRAII InheritanceScope(SemaRef, clang::Scope::DeclScope |
      clang::Scope::ClassScope | clang::Scope::ClassInheritanceScope,
      clang::SourceLocation());

  llvm::SmallVector<clang::CXXBaseSpecifier *, 4> GivenBaseClasses;
  // FIXME: This currently doesn't account for dependent names.
  Attributes Attrs;
  bool IsVirtualBase = false;
  for (const Syntax *Base : Bases->children()) {
    clang::Expr *BaseExpr = TypeElab.elaborateExpr(Base);

    if (!BaseExpr) {
      SemaRef.Diags.Report(Base->getLoc(),
                           clang::diag::err_failed_to_translate_expr);
      continue;
    }

    Attrs.clear();
    clang::AccessSpecifier AS = clang::AS_public;
    IsVirtualBase = false;
    if (!Base->getAttributes().empty()) {
      // Gathering all of the attributes from the root node of the expression
      // (Which is technically)
      for (const Attribute *Attr : Base->getAttributes())
        Attrs.emplace_back(Attr->getArg());

      if (computeAccessSpecifier(SemaRef, Attrs, AS))
        return;

      if (isVirtualBase(SemaRef, Attrs, IsVirtualBase))
        return;

      // TODO: Create an error message in the event that the attributes
      // associated with the current type are wrong.
      if (!Attrs.empty()) {
        // TODO: Create an error message for here.
        llvm::errs() << "Invalid base class attribute\n";
        return;
      }
    }

    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                      BaseExpr, Base->getLoc());
    if (!TInfo)
      return;

    clang::ParsedType PT = SemaRef.getCxxSema().CreateParsedType(
                                                       TInfo->getType(), TInfo);
    clang::ParsedAttributes Attributes(SemaRef.AttrFactory);
    auto BaseResult = SemaRef.getCxxSema()
      .ActOnBaseSpecifier(R, clang::SourceRange(Base->getLoc(), Base->getLoc()),
                          Attributes, IsVirtualBase, AS, PT, Base->getLoc(),
                          clang::SourceLocation());

    if (BaseResult.isInvalid())
      continue;
    GivenBaseClasses.emplace_back(BaseResult.get());
  }

  SemaRef.getCxxSema().ActOnBaseSpecifiers(R, GivenBaseClasses);
}

static clang::TypeResult
getUnderlyingEnumType(SyntaxContext& Context, Sema& SemaRef,
                      const Syntax *MS) {
  if (const CallSyntax *Call = dyn_cast<CallSyntax>(MS)) {

    if (Call->getNumArguments() > 1) {
      SemaRef.Diags.Report(Call->getLoc(),
                          clang::diag::err_too_many_underlying_enum_types);
      return clang::TypeResult();
    }

    // We have no underlying type so exit.
    if (Call->getNumArguments() == 0)
      return clang::TypeResult();

    // We must have 1 argument so elaborate it.
    ExprElaborator ExprElab(Context, SemaRef);
    clang::Expr *UnderlyingTy = ExprElab.elaborateExpr(Call->getArgument(0));
    if (!UnderlyingTy)
      return clang::TypeResult();
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                  UnderlyingTy, Call->getLoc());
    if (!TInfo)
      return clang::TypeResult();
    clang::ParsedType PT;
    PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
    return clang::TypeResult(PT);
  }

  // Return an empty expression result.
  return clang::TypeResult();
}

static clang::Decl *
handleClassSpecialization(SyntaxContext &Context,
                          Sema &SemaRef, Declaration *D,
                          clang::TypeSpecifierType TST,
                          clang::CXXScopeSpec &SS,
                          clang::MultiTemplateParamsArg &MTP) {
  SpecializationDeclarator *SD = D->SpecializationArgs->getAsSpecialization();
  assert(SD->ElaboratedArgs && "failed to elaborate specialization");

  clang::Sema &CxxSema = SemaRef.getCxxSema();
  clang::SourceLocation IdLoc = D->Decl->getLoc();

  clang::DeclarationNameInfo DNI;
  DNI.setName(D->getId());
  DNI.setLoc(IdLoc);
  clang::LookupResult Previous(CxxSema, DNI,
                               clang::Sema::LookupOrdinaryName,
                               CxxSema.forRedeclarationInCurContext());
  clang::Scope *FoundScope = SemaRef.getCurClangScope();
  while ((FoundScope->getFlags() & clang::Scope::DeclScope) == 0 ||
         (FoundScope->getFlags() & clang::Scope::TemplateParamScope) != 0)
    FoundScope = FoundScope->getParent();

  SemaRef.getCxxSema().LookupName(Previous, FoundScope, false);
  if (!Previous.isSingleResult()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "specialization of ambiguous template");
    SemaRef.Diags.Report(IdLoc, DiagID);
    return nullptr;
  }

  clang::ClassTemplateDecl *Principal =
    Previous.getAsSingle<clang::ClassTemplateDecl>();
  if (!Principal) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "specialization of undeclared template");
    SemaRef.Diags.Report(IdLoc, DiagID);
    return nullptr;
  }

  clang::TemplateName Template(Principal);
  clang::OpaquePtr<clang::TemplateName> TemplatePtr =
    clang::OpaquePtr<clang::TemplateName>::make(Template);
  clang::UnqualifiedId UnqualId;
  UnqualId.setIdentifier(D->getId(), IdLoc);
  llvm::SmallVector<clang::TemplateIdAnnotation *, 16> TemplateIds;
  llvm::SmallVector<clang::ParsedTemplateArgument, 4> Args;

  const clang::TemplateArgumentLoc *ArgInfo =
    SD->getArgList().getArgumentArray();
  for (unsigned I = 0; I < SD->getArgList().size(); ++I) {
    clang::TemplateArgument Arg = ArgInfo[I].getArgument();

    bool BadArgument = false;
    switch (Arg.getKind()) {
    case clang::TemplateArgument::Type: {
      clang::ParsedTemplateArgument NewArg(
        clang::ParsedTemplateArgument::Type,
        (void *)Arg.getAsType().getTypePtr(),
        ArgInfo[I].getLocation());
      Args.push_back(NewArg);
      break;
    }

    case clang::TemplateArgument::Expression: {
      // FIXME: this might be classified as an expression because it is
      // dependent, but is actually something else.
      clang::ParsedTemplateArgument NewArg(
        clang::ParsedTemplateArgument::NonType, (void *)Arg.getAsExpr(),
        ArgInfo[I].getLocation());
      Args.push_back(NewArg);
      break;
    }

    default: {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "invalid specialization argument");
      SemaRef.Diags.Report(ArgInfo[I].getLocation(), DiagID);
      BadArgument = true;
    }
    }

    if (BadArgument)
      continue;
  }

  auto *TempId =
    clang::TemplateIdAnnotation::Create(
      D->Op->getLoc(), IdLoc, D->getId(), false, clang::OO_None, TemplatePtr,
      clang::TNK_Type_template, IdLoc, IdLoc, Args, false, TemplateIds);
  auto Res = SemaRef.getCxxSema().ActOnClassTemplateSpecialization(
    SemaRef.getCurClangScope(), TST, clang::Sema::TUK_Definition,
    D->Init->getLoc(), /*ModulePrivLoc=*/SourceLocation(), SS,
    *TempId, clang::ParsedAttributesView(), MTP);

  if (Res.isInvalid()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "failed to specialize template");
    SemaRef.Diags.Report(IdLoc, DiagID);
    return nullptr;
  }

  clang::Decl *Ret = Res.get();

  // We need to be sure this has a valid source location, otherwise clang
  // will assume it is a partial specialization.
  using Specialization = clang::ClassTemplateSpecializationDecl;
  cast<Specialization>(Ret)->setTemplateKeywordLoc(IdLoc);
  return Ret;
}

static clang::Decl *
processCXXRecordDecl(Elaborator &Elab, SyntaxContext &Context, Sema &SemaRef,
                     Declaration *D) {
  using namespace clang;
  D->CurrentPhase = Phase::Typing;


  // Checking if we are a nested template decl/class.
  bool WithinClass = D->ScopeForDecl->getKind() == SK_Class;
  MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  bool IsOwned = false;
  bool IsDependent = false;
  CXXScopeSpec SS;
  TypeResult UnderlyingType;
  AccessSpecifier AS = AS_none;
  if (WithinClass)
    AS = AS_public;
  clang::SourceLocation IdLoc = D->Decl->getLoc();
  clang::TypeSpecifierType TST = clang::DeclSpec::TST_struct;
  bool ScopeEnumUsesClassTag = false;
  clang::SourceLocation ScopedEnumClassKW;
  const AtomSyntax *Name = nullptr;
  ScopeKind SK = SK_Class;
  if (D->getTagName(Name)) {
    if (Name->hasToken(tok::ClassKeyword)) {
      TST = clang::DeclSpec::TST_struct;
    } else if (Name->hasToken(tok::UnionKeyword)) {
      TST = clang::DeclSpec::TST_union;
    } else if (Name->hasToken(tok::EnumKeyword)) {
      TST = clang::DeclSpec::TST_enum;
      // I need to extract the underlying type for an enum.
      ScopeEnumUsesClassTag = true;
      if (const MacroSyntax *MS = dyn_cast<MacroSyntax>(D->Init)) {
        UnderlyingType = getUnderlyingEnumType(Context, SemaRef, MS->getCall());
      } else {
        llvm_unreachable("Invalid tree syntax.");
      }
      ScopedEnumClassKW = Name->getLoc();
      SK = SK_Enum;
    } else {
      llvm_unreachable("Incorrectly identified tag type");
    }
  } else {
    llvm_unreachable("Incorrectly identified tag type");
  }

  Decl *Declaration = nullptr;
  if (D->SpecializationArgs) {
    Declaration = handleClassSpecialization(Context, SemaRef, D, TST, SS, MTP);
  } else {
    Declaration = SemaRef.getCxxSema().ActOnTag(
      SemaRef.getCurClangScope(), TST, /*Metafunction=*/nullptr,
      clang::Sema::TUK_Definition, D->Init->getLoc(), SS, D->getId(), IdLoc,
      clang::ParsedAttributesView(), AS, /*ModulePrivateLoc=*/SourceLocation(),
      MTP, IsOwned, IsDependent, ScopedEnumClassKW, ScopeEnumUsesClassTag,
      UnderlyingType, /*IsTypeSpecifier=*/false, /*IsTemplateParamOrArg=*/false);
  }

  TagDecl *Tag = nullptr;
  if (!Declaration) {
    return nullptr;
  }
  if(isa<CXXRecordDecl>(Declaration)) {
    Tag = cast<CXXRecordDecl>(Declaration);
  } else if (isa<ClassTemplateDecl>(Declaration)) {
    ClassTemplateDecl *TempTemplateDecl = cast<ClassTemplateDecl>(Declaration);
    Tag = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  } else if (isa<EnumDecl>(Declaration)) {
    Tag = cast<TagDecl>(Declaration);
  }

  D->Cxx = Tag;
  Elab.elaborateAttributes(D);

  Sema::ScopeRAII ClassBodyScope(SemaRef, SK, D->Op, &D->SavedScope);
  SemaRef.getCurrentScope()->Entity = D;

  Sema::ClangScopeRAII ClangClassScopeBody(SemaRef,
         (Tag->isEnum() ? clang::Scope::EnumScope : clang::Scope::ClassScope)
                                  | clang::Scope::DeclScope, D->Init->getLoc());


  // Need to do this before the next step because this is actually pushed on to
  // the stack a by the next function called.
  SemaRef.getCxxSema().ActOnTagStartDefinition(SemaRef.getCurClangScope(), Tag);

  // This keeps the declContext working correctly.
  Sema::DeclContextRAII DCTracking(SemaRef, D, true);
  if (TST == clang::DeclSpec::TST_enum) {
    Elab.elaborateEnumBody(D, Tag);
    if (Tag->isInvalidDecl()) {
      // Need to make sure that this isn't elaborated as a variable later on.
      D->CurrentPhase = Phase::Initialization;
      return Tag;
    }
  } else {
    // THis handles processing for class, struct, and union bodies.
    // This keeps track of class nesting.
    Sema::ElaboratingClassDefRAII ClsElabState(SemaRef, D,
                                              !SemaRef.isElaboratingClass());
    CXXRecordDecl *ClsDecl = cast<CXXRecordDecl>(Tag);
    Elab.elaborateTypeBody(D, ClsDecl);
    // Attempt to figure out if any nested elaboration is actually required.
    // If not then we can proceed as normal.
    auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
    auto const* BodyArray = MacroRoot->getBlock();

    // Handling possible base classes.
    if (const CallSyntax *ClsKwCall
                          = dyn_cast<CallSyntax>(MacroRoot->getCall())) {
      processBaseSpecifiers(Elab, SemaRef, Context, D, ClsDecl, ClsKwCall);
    }

    SemaRef.getCxxSema().ActOnStartCXXMemberDeclarations(
                                                     SemaRef.getCurClangScope(),
                                                         ClsDecl,
                                                         SourceLocation(), true,
                                                         SourceLocation());
    // Since all declarations have already been added, we don't need to do another
    // Reordering scan.
    // Doing possible delaying of member declaration/initialziation.
    for (const Syntax *SS : BodyArray->children()) {
      Elab.delayElaborateDeclType(ClsDecl, SS);
    }

    SemaRef.getCxxSema().ActOnFinishCXXMemberSpecification(
      SemaRef.getCurClangScope(), SourceLocation(), ClsDecl, SourceLocation(),
      SourceLocation(), ParsedAttributesView());
    D->CurrentPhase = Phase::Initialization;
    if (!WithinClass) {
      ElaboratingClass &LateElabClass = SemaRef.getCurrentElaboratingClass();
      Elab.finishDelayedElaboration(LateElabClass);
      SemaRef.getCxxSema().ActOnFinishCXXNonNestedClass(ClsDecl);
    }
  }

  clang::Decl *TempDeclPtr = Tag;
  SemaRef.getCxxSema().ActOnTagFinishDefinition(SemaRef.getCurClangScope(),
                                                TempDeclPtr, SourceRange());
  return Tag;
}

static clang::Decl*
processCXXForwardRecordDecl(Elaborator& Elab, SyntaxContext& Context,
                            Sema& SemaRef, Declaration *D) {
  using namespace clang;
  D->CurrentPhase = Phase::Typing;

  // bool Template = D->declaresTemplateType();
  // const ListSyntax *TemplParams;

  // Checking if we are a nested template decl/class.
  bool WithinClass = D->ScopeForDecl->getKind() == SK_Class;
  MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  bool IsOwned = false;
  bool IsDependent = false;
  CXXScopeSpec SS;
  TypeResult UnderlyingType;
  bool ScopeEnumUsesClassTag = false;
  SourceLocation ScopedEnumClassKW;
  AccessSpecifier AS = AS_none;
  if (WithinClass)
    AS = AS_public;
  clang::SourceLocation IdLoc = D->Decl->getLoc();
  clang::TypeSpecifierType TST = clang::DeclSpec::TST_struct;
  if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(D->Init)) {
    if (Name->hasToken(tok::ClassKeyword)) {
      TST = clang::DeclSpec::TST_struct;
    } else if (Name->hasToken(tok::UnionKeyword)) {
      TST = clang::DeclSpec::TST_union;
    } else if (Name->hasToken(tok::EnumKeyword)) {
      // This is here because it's used specifically to generate an error from
      // Sema::ActOnTag
      TST = clang::DeclSpec::TST_enum;
      ScopeEnumUsesClassTag = true;
      ScopedEnumClassKW = Name->getLoc();
    } else {
      llvm_unreachable("Incorrectly identified tag type");
    }
  } else if (const CallSyntax *EnumCall = dyn_cast<CallSyntax>(D->Init)) {
    if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(EnumCall->getCallee())) {
      if (Name->hasToken(tok::EnumKeyword)) {
        TST = clang::DeclSpec::TST_enum;
        ScopeEnumUsesClassTag = true;
        UnderlyingType = getUnderlyingEnumType(Context, SemaRef, EnumCall);
        ScopedEnumClassKW = Name->getLoc();
      } else {
        SemaRef.Diags.Report(EnumCall->getLoc(),
                            clang::diag::err_invalid_declaration);
        return nullptr;
      }
    } else {
      SemaRef.Diags.Report(EnumCall->getLoc(),
                          clang::diag::err_invalid_declaration);
      return nullptr;
    }
  } else {
    llvm_unreachable("Incorrectly identified tag type");
  }


  Decl *Declaration = SemaRef.getCxxSema().ActOnTag(SemaRef.getCurClangScope(),
      TST, /*Metafunction=*/nullptr,
      clang::Sema::TUK_Declaration, D->Init->getLoc(), SS, D->getId(), IdLoc,
      clang::ParsedAttributesView(), /*AccessSpecifier=*/AS,
      /*ModulePrivateLoc=*/SourceLocation(),
      MTP, IsOwned, IsDependent,
      /*ScopedEnumKWLoc=*/ScopedEnumClassKW,
      /*ScopeEnumUsesClassTag=*/ScopeEnumUsesClassTag, UnderlyingType,
      /*IsTypeSpecifier=*/false, /*IsTemplateParamOrArg=*/false,
      /*SkipBody=*/nullptr);
  CXXRecordDecl *ClsDecl = nullptr;
  if(isa<CXXRecordDecl>(Declaration)) {
    ClsDecl = cast<CXXRecordDecl>(Declaration);
  } else if (isa<ClassTemplateDecl>(Declaration)) {
    ClassTemplateDecl *TempTemplateDecl = cast<ClassTemplateDecl>(Declaration);
    ClsDecl = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  }
  D->Cxx = ClsDecl;
  Elab.elaborateAttributes(D);
  D->CurrentPhase = Phase::Initialization;
  return ClsDecl;
}

static clang::Decl *processNamespaceDecl(Elaborator& Elab,
                                         SyntaxContext& Context,
                                         Sema& SemaRef, Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  using namespace clang;

  // Create and enter a namespace scope.
  clang::CppxNamespaceDecl *NSDecl = nullptr;
  clang::Scope *NSScope = SemaRef.enterClangScope(clang::Scope::DeclScope);

  // FIXME: keep track of nested namespaces?
  gold::Sema::OptionalScopeRAII NewScope(SemaRef);
  gold::Sema::OptionalResumeScopeRAII ResumedScope(SemaRef);
  clang::UsingDirectiveDecl *UD = nullptr;
  clang::AttributeFactory Attrs;
  clang::ParsedAttributes ParsedAttrs(Attrs);
  NSDecl = SemaRef.ActOnStartNamespaceDef(NSScope,
                                          SourceLocation(),
                                          D->Decl->getLoc(),
                                          D->Decl->getLoc(),
                                          D->getId(),
                                          D->Decl->getLoc(),
                                          ParsedAttrs, UD);
  if (!NSDecl) {
    // Making sure that if something does go wrong that we properly recover
    // from it.
    SemaRef.leaveClangScope(clang::SourceLocation());
    return nullptr;
  }
  // Resume or create a new scope for the current namespace.
  // This is to allow the representations to all share the same scope.
  // This makes it easier to handle lookup for those elements of the scope.
  if (!NSDecl->Rep) {
    NewScope.Init(SK_Namespace, D->Init, &NSDecl->Rep);
    NSDecl->Rep = SemaRef.getCurrentScope();
  } else {
    ResumedScope.Init(NSDecl->Rep, NSDecl->Rep->Term, false);
  }

  D->Cxx = NSDecl;
  Elab.elaborateAttributes(D);

  SemaRef.pushDecl(D);

  const MacroSyntax *NSMacro = cast<MacroSyntax>(D->Init);
  const Syntax *NSBody = NSMacro->getBlock();

  // Keep track of the location of the last syntax, as a closing location.
  clang::SourceLocation LastLoc;
  for (const Syntax *S : NSBody->children()) {
    Elaborator(Context, SemaRef).elaborateDeclSyntax(S);
    LastLoc = S->getLoc();
  }

  NSDecl->Rep = SemaRef.getCurrentScope();
  SemaRef.getCxxSema().ActOnFinishNamespaceDef(NSDecl, LastLoc);
  SemaRef.leaveClangScope(LastLoc);
  SemaRef.popDecl();

  // FIXME: We should be returning a DeclGroupPtr to the NSDecl grouped
  // with the implicit UsingDecl, UD.
  return NSDecl;
}

static void handleTemplateParameters(Sema &SemaRef,
                                     Sema::OptionalScopeRAII &ScopeToInit,
                                     Sema::OptioanlClangScopeRAII &ClangScope,
                                     Declaration *D, Declarator *Dcl) {
  gold::Scope **ScopePtr = nullptr;
  clang::SourceLocation Loc = Dcl->getLoc();
  clang::TemplateParameterList *ParamList = nullptr;
  const Syntax *Params = nullptr;
  TemplateParamsDeclarator *TPD = Dcl->getAsTemplateParams();
  ScopePtr = TPD->getScopePtrPtr();
  Params = TPD->getSyntax();

  // Initializing the scopes we have to deal with.
  ScopeToInit.Init(SK_Template, Params, ScopePtr);
  ClangScope.Init(clang::Scope::TemplateParamScope, Loc);

  // Constructing actual parameters.
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  if (!TPD->isImplicitlyEmpty())
    BuildTemplateParams(SemaRef.getContext(), SemaRef, Params,
                        TemplateParamDecls);

  ParamList = SemaRef.getCxxSema().ActOnTemplateParameterList(
                               /*unsigned Depth*/SemaRef.computeTemplateDepth(),
                                           /*ExportLoc*/clang::SourceLocation(),
                                                            /*TemplateLoc*/Loc,
                                                            /*LAngleLoc*/Loc,
                                                            TemplateParamDecls,
                                                            /*RAngleLoc*/Loc,
                                                     /*RequiresClause*/nullptr);

  TPD->setTemplateParameterList(ParamList);
  // Recording template parameters for use during declaration construction.
  D->TemplateParamStorage.push_back(ParamList);
}

static bool handleSpecializationArgs(Sema &SemaRef, const Syntax *Args,
                                clang::TemplateArgumentListInfo &TemplateArgs) {
  for (auto *SS : Args->children()) {
    clang::Expr *E = ExprElaborator(SemaRef.getContext(), SemaRef).
                                              elaborateExpectedConstantExpr(SS);
    if (!E) {
      return true;
    }
    // TODO: create a list of valid expressions this can or cannot be;
    // for example, namespace or pseudo-destructor is invalid here.
    if (E->getType()->isTypeOfTypes()) {
      auto *TInfo = SemaRef.getTypeSourceInfoFromExpr(E, SS->getLoc());
      clang::TemplateArgument Arg(TInfo->getType());
      clang::TemplateArgumentLoc ArgLoc(Arg, TInfo);
      TemplateArgs.addArgument(ArgLoc);
    } else {
      clang::TemplateArgument Arg(E, clang::TemplateArgument::Expression);
      clang::TemplateArgumentLoc ArgLoc(Arg, E);
      TemplateArgs.addArgument(ArgLoc);
    }
  }

  TemplateArgs.setLAngleLoc(Args->getLoc());
  TemplateArgs.setRAngleLoc(Args->getLoc());
  return false;
}

static bool elaborateSpecializationArgs(Sema &SemaRef,
                                        Declaration *D) {
  SpecializationDeclarator *SD = D->SpecializationArgs->getAsSpecialization();
  assert(!SD->ElaboratedArgs && "specialization arguments already elaborated");

  SD->ElaboratedArgs = true;
  if (handleSpecializationArgs(SemaRef, SD->getArgs(), SD->getArgList())){
    SD->setDidError();
  }
  return SD->getDidError();
}

clang::Decl *Elaborator::elaborateDecl(Declaration *D) {
  if (phaseOf(D) != Phase::Identification)
    return D->Cxx;

  Scope *Sc = SemaRef.getCurrentScope();
  // Resume the current scope because that will allow us to modify the current
  // scope during processing and restore when we pop this off the scope
  // stack.
  Sema::ResumeScopeRAII ScopeTracking(SemaRef, Sc, Sc->getConcreteTerm());
  if (!D->NNSInfo.empty() || D->GlobalNsSpecifier)
    llvm_unreachable("Nested name elaboration not implemented yet.");

  Sema::OptionalScopeRAII TemplateParamScope(SemaRef);
  Sema::OptioanlClangScopeRAII ClangTemplateScope(SemaRef);

  // Checking to see if we are need to enter a name scope for a template
  if (D->TemplateParameters)
    handleTemplateParameters(SemaRef, TemplateParamScope, ClangTemplateScope,
                             D, D->TemplateParameters);

  if (D->SpecializationArgs)
    elaborateSpecializationArgs(SemaRef, D);

  clang::Decl *Ret = elaborateDeclContent(D);

  // Checking the error from the specializaton and marking the decl as invalid.
  if (D->SpecializationArgs) {
    if (D->SpecializationArgs->getAsSpecialization()->getDidError()) {
      Ret->setInvalidDecl();
    }
  }
  return Ret;
  // TODO: We should be able to elaborate definitions at this point too.
  // We've already loaded salient identifier tables, so it shouldn't any
  // forward references should be resolvable.
}
clang::Decl *Elaborator::elaborateDeclContent(Declaration *D) {
  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  if (D->declaresTag())
    return processCXXRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresForwardRecordDecl())
    return processCXXForwardRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresNamespace())
    return processNamespaceDecl(*this, Context, SemaRef, D);
  if (D->declaresFunction())
    return elaborateFunctionDecl(D);
  return elaborateVariableDecl(D);
}

static void BuildTemplateParams(SyntaxContext &Ctx, Sema &SemaRef,
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
      clang::QualType NewTy =
        Ctx.CxxAST.getTemplateTypeParmType(Depth, I,
                                           Ty->isParameterPack(),
                                           Ty->getDecl());
      TP->setTypeForDecl(NewTy.getTypePtr());
    } else {
      llvm_unreachable("Invalid template parameter");
    }

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");
    Res.push_back(ND);

    ++I;
  }
}

// anonymous namespace comprised of subroutines for elaborateFunctionDecl
namespace {

// Get the Clang parameter declarations for D
void getFunctionParameters(Declaration *D,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  FunctionDeclarator *FnDecl = D->FunctionDcl->getAsFunction();
  const ListSyntax *ParamList = FnDecl->getParams();
  Scope *ParamScope = FnDecl->getScope();
  bool Variadic = FnDecl->isVariadic();

  unsigned N = ParamList->getNumChildren();
  for (unsigned I = 0; I < N; ++I) {
    if (I == N - 1 && Variadic)
      break;
    const Syntax *P = ParamList->getChild(I);
    Declaration *PD = ParamScope->findDecl(P);
    assert(PD->Cxx && "No corresponding declaration");
    Params.push_back(cast<clang::ParmVarDecl>(PD->Cxx));
  }
}

bool getOperatorDeclarationName(SyntaxContext &Context, Sema &SemaRef,
                                const OpInfoBase *OpInfo,
                                bool InClass, unsigned ParamCount,
                                clang::SourceLocation NameLoc,
                                clang::DeclarationName &Name) {
  if (OpInfo->isMemberOnly() && !InClass) {
    SemaRef.Diags.Report(NameLoc,
                        clang::diag::err_operator_overload_must_be_member)
                        << OpInfo->getGoldDeclName()->getName();
    return true;
  }

  if (OpInfo->isUnaryAndBinary()) {
    if (InClass) {
      ParamCount += 1;
    }
    if (ParamCount == 0) {
      SemaRef.Diags.Report(NameLoc,
                           clang::diag::err_operator_too_few_parameters)
                           << OpInfo->getGoldDeclName();
        return true;
    }
    if (ParamCount == 1) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(
                                                OpInfo->getUnaryOverloadKind());
    }
    if (ParamCount == 2) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(
                                               OpInfo->getBinaryOverloadKind());
    }
    if (ParamCount > 2) {
      SemaRef.Diags.Report(NameLoc,
                           clang::diag::err_operator_overload_must_be)
                           << OpInfo->getGoldDeclName()
                           << ParamCount
                           << /*unary or binary*/2;
        return true;
    }
  } else if(OpInfo->isBinary()) {
    Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(
                                               OpInfo->getBinaryOverloadKind());
  } else {
    Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(
                                                OpInfo->getUnaryOverloadKind());
  }
  return false;
}

// Get either the canonical name of a function, or its C++ name if it's
// an operator.
clang::DeclarationName getFunctionName(SyntaxContext &Ctx, Sema &SemaRef,
                                       Declaration *D,
                                       clang::TypeSourceInfo *TInfo,
                                       bool InClass,
                                       const clang::RecordDecl *RD) {
  clang::DeclarationName Name;
  if (D->OpInfo) {
    const clang::FunctionProtoType *FPT = cast<clang::FunctionProtoType>(
                                                 TInfo->getType().getTypePtr());
    assert(FPT && "function does not have prototype");
    if (getOperatorDeclarationName(Ctx, SemaRef, D->OpInfo, InClass,
                                   FPT->getNumParams(),
                                   D->IdDcl->getLoc(), Name)) {
      // FIXME: Should this be an error or not?
      return clang::DeclarationName();
    }
  } else {
    Name = D->getId();
  }

  return Name;
}

void setSpecialFunctionName(SyntaxContext &Ctx, clang::CXXRecordDecl *RD,
                            Declaration *D, clang::DeclarationName &Name) {
  clang::QualType RecordTy = Ctx.CxxAST.getTypeDeclType(RD);
  clang::CanQualType Ty = Ctx.CxxAST.getCanonicalType(RecordTy);
  if (D->getId()->isStr("constructor")) {
    Name = Ctx.CxxAST.DeclarationNames.getCXXConstructorName(Ty);
  } else if (D->getId()->isStr("destructor")) {
    Name = Ctx.CxxAST.DeclarationNames.getCXXDestructorName(Ty);
  }
}

void lookupFunctionRedecls(Sema &SemaRef, clang::Scope *FoundScope,
                           clang::LookupResult &Previous) {
  while ((FoundScope->getFlags() & clang::Scope::DeclScope) == 0 ||
         (FoundScope->getFlags() & clang::Scope::TemplateParamScope) != 0)
    FoundScope = FoundScope->getParent();

  SemaRef.getCxxSema().LookupName(Previous, FoundScope, false);
}

bool buildMethod(SyntaxContext &Context, Sema &SemaRef, Declaration *Fn,
                 clang::DeclarationName const &Name, clang::FunctionDecl **FD,
                 clang::TypeSourceInfo *Ty, clang::CXXRecordDecl *RD) {
  clang::SourceLocation ExLoc = Fn->Op->getLoc();
  clang::SourceLocation FnLoc = Fn->Decl->getLoc();
  const clang::FunctionProtoType *FPT =
    Ty->getType()->getAs<clang::FunctionProtoType>();
  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(ExLoc);

  bool Constructor = Fn->getId()->isStr("constructor");
  bool Destructor = Fn->getId()->isStr("destructor");
  if (Constructor || Destructor) {
    if (FPT->getReturnType() == Context.CxxAST.getAutoDeductType()) {
      // double verifying function type.
      if (!Fn->getFirstDeclarator(DK_Type)) {
        // The we set the default type to void instead because we are a
        // constructor.
        auto ParamTys = FPT->getParamTypes();
        llvm::SmallVector<clang::QualType, 10> ParamTypes(ParamTys.begin(),
                                                          ParamTys.end());
        clang::QualType FnTy = SemaRef.getCxxSema().BuildFunctionType(
          Context.CxxAST.VoidTy,
          ParamTypes, FnLoc,
          clang::DeclarationName(),
          FPT->getExtProtoInfo());
        if (FnTy->isFunctionProtoType()) {
          FPT = FnTy->getAs<clang::FunctionProtoType>();
        } else {
          SemaRef.Diags.Report(FnLoc,
                    clang::diag::err_invalid_return_type_for_ctor_or_dtor) << 0;
          return false;
        }
      }
    }

    if (FPT->getReturnType() != Context.CxxAST.VoidTy) {
      SemaRef.Diags.Report(FnLoc,
                           clang::diag::err_invalid_return_type_for_ctor_or_dtor)
        << 0;
      return false;
    }

    clang::ExplicitSpecifier
      ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
    clang::CXXMethodDecl *Method = nullptr;

    if (Constructor)
      *FD = Method =
        clang::CXXConstructorDecl::Create(Context.CxxAST, RD, ExLoc, DNI,
                                          clang::QualType(), nullptr, ES, false,
                              false, clang::ConstexprSpecKind::CSK_unspecified);
    else if (Destructor)
    *FD = Method =
      clang::CXXDestructorDecl::Create(Context.CxxAST, RD, ExLoc, DNI,
                                       clang::QualType(), nullptr, false, false,
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
      Context.CxxAST.getDefaultCallingConvention(/*IsVariadic=*/false,
                                                 /*IsCXXMethod=*/true));
    clang::LangAS AS = SemaRef.getCxxSema().getDefaultCXXMethodAddrSpace();
    if (AS != clang::LangAS::Default)
      EPI.TypeQuals.addAddressSpace(AS);

    const clang::FunctionProtoType *FPT
      = cast<clang::FunctionProtoType>(Ty->getType().getTypePtr());
    if (Destructor && FPT->getNumParams() != 0) {
      SemaRef.Diags.Report(ExLoc,
                           clang::diag::err_destructor_with_params);
      return false;
    }

    auto VoidFnTy =
      Context.CxxAST.getFunctionType(Context.CxxAST.VoidTy,
                                     Constructor ?
                                     FPT->getParamTypes() : clang::None,
                                     EPI);
    Method->setType(VoidFnTy);
  } else {
    clang::StorageClass SC = clang::SC_None;
    *FD = clang::CXXMethodDecl::Create(Context.CxxAST, RD, ExLoc, DNI,
                                       Ty->getType(), Ty,
                                       SC, /*isInline*/true,
                                       clang::ConstexprSpecKind::CSK_unspecified,
                                       ExLoc);
  }

  (*FD)->setAccess(clang::AS_public);
  return true;
}

} // end anonymous namespace

clang::Decl *Elaborator::elaborateFunctionDecl(Declaration *D) {
  clang::Sema &CxxSema = SemaRef.getCxxSema();

  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  FunctionDeclarator *FnDclPtr = D->FunctionDcl->getAsFunction();

  // Get a reference to the containing class if there is one.
  bool InClass = D->ScopeForDecl->getKind() == SK_Class;
  clang::CXXRecordDecl *RD = nullptr;
  if (InClass) {
    clang::Decl *ScopesDecl = SemaRef.getDeclForScope();
    assert(ScopesDecl && "Invalid declaration for scope.");
    RD = dyn_cast<clang::CXXRecordDecl>(ScopesDecl);
    assert(RD && "Class scope doesn't contain declaration.");
  }

  // Create the template parameters if they exist.
  bool Template = D->TemplateParameters;
  TemplateParamsDeclarator *TPD = nullptr;
  bool Specialization = D->SpecializationArgs;
  if (Template) {
    TPD = D->TemplateParameters->getAsTemplateParams();
  }

  // Elaborate the return type.
  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->FunctionDcl);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                        clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                      D->FunctionDcl->getLoc());
  if (!TInfo)
    return nullptr;

  // Get name info for the AST.
  clang::DeclarationName Name =
    getFunctionName(Context, SemaRef, D, TInfo, InClass, RD);
  if (Name.isEmpty())
    return nullptr;

  // Set the declaration info here to help determine if this should have
  // a C++ special name.
  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(D->IdDcl->getLoc());

  if (InClass)
    setSpecialFunctionName(Context, RD, D, Name);

  clang::LookupResult Previous(CxxSema, DNI,
                               clang::Sema::LookupOrdinaryName,
                               CxxSema.forRedeclarationInCurContext());
  clang::Scope *CxxScope = SemaRef.getCurClangScope();
  lookupFunctionRedecls(SemaRef, CxxScope, Previous);

  clang::SourceLocation Loc = D->Op->getLoc();
  clang::FunctionDecl *FD = nullptr;
  if(InClass) {
    if (!buildMethod(Context, SemaRef, D, Name, &FD, TInfo, RD))
      return nullptr;
  } else {
    FD = clang::FunctionDecl::Create(Context.CxxAST, Owner, Loc, Loc, Name,
                                     TInfo->getType(), TInfo, clang::SC_None);
    if (FD->isMain()) {
      clang::AttributeFactory Attrs;
      clang::DeclSpec DS(Attrs);
      CxxSema.CheckMain(FD, DS);
    }
  }

  // If this describes a primary template declaration, create it.
  if (Template && !Specialization) {
    clang::SourceLocation Loc = TPD->getLoc();
    auto *FTD = clang::FunctionTemplateDecl::Create(Context.CxxAST, Owner, Loc,
                                                    FD->getDeclName(),
                                                TPD->getTemplateParameterList(),
                                                    FD);
    FTD->setLexicalDeclContext(Owner);
    FD->setDescribedFunctionTemplate(FTD);
    Owner->addDecl(FTD);
    if (InClass)
      FTD->setAccess(clang::AS_public);

    // An auto return type here is always dependent.
    if (FD->getReturnType()->isUndeducedAutoType()) {
      clang::QualType OldTy = FD->getType();
      const clang::FunctionProtoType *FPT =
        OldTy->getAs<clang::FunctionProtoType>();
      clang::ASTContext &CxxAST = Context.CxxAST;
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

  CxxSema.getImplicitCodeSegOrSectionAttrForFunction(FD, D->Init);

  // Update the function parameters.
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getFunctionParameters(D, Params);
  FD->setParams(Params);
  D->Cxx = FD;
  {
    // We have previously exited this scope that was created during type
    // elaboration.
    Sema::ResumeScopeRAII FuncScope(SemaRef,
                                    FnDclPtr->getScope(),
                                    FnDclPtr->getScope()->getConcreteTerm(),
                                    /*PopOnExit=*/false);
    elaborateAttributes(D);
  } // end anonymous scope

  // Add the declaration and update bindings.
  if ((!Template || Specialization) && !D->declaresConstructor())
    Owner->addDecl(FD);

  if (D->declaresConstructor()) {
    clang::CXXConstructorDecl* CtorDecl =
      cast<clang::CXXConstructorDecl>(D->Cxx);
    CxxSema.PushOnScopeChains(CtorDecl, CxxScope);
    CxxSema.CheckConstructor(CtorDecl);
  }

  CxxSema.FilterLookupForScope(Previous, Owner, CxxScope,
                                            !InClass, !InClass);
  CxxSema.CheckFunctionDeclaration(CxxScope,
                                                FD, Previous, true);

  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(FD)) {

    checkCXXMethodDecl(MD);
    CxxSema.CheckOverrideControl(MD);
  }

  // Handle function template specialization.
  if (!FD->isInvalidDecl() && !Previous.empty() && Specialization) {
    SpecializationDeclarator *SpDcl =
      D->SpecializationArgs->getAsSpecialization();
    clang::TemplateArgumentListInfo *Args =
      SpDcl->HasArguments() ? &SpDcl->getArgList() : nullptr;

    if (CxxSema.CheckFunctionTemplateSpecialization(FD, Args, Previous))
      FD->setInvalidDecl();
  }

  // FIXME: this is not necessarily what should happen.
  if (FD->isInvalidDecl())
    return nullptr;

  D->CurrentPhase = Phase::Typing;
  return FD;
}

void Elaborator::checkCXXMethodDecl(clang::CXXMethodDecl *MD) {
  // We can't check dependent instance methods.
  if (MD && MD->isInstance() &&
      (MD->getParent()->hasAnyDependentBases() ||
       MD->getType()->isDependentType()))
    return;

  // We should delay checking of methods declared inside of a fragment.
  if (MD && MD->isInFragment())
    return;

  // Doing member checking to make sure that we can sew together virtual
  // function overrides.s
  if (MD && !MD->isVirtual()) {
    // If we have a non-virtual method, check if if hides a virtual method.
    // (In that case, it's most likely the method has the wrong type.)
    llvm::SmallVector<clang::CXXMethodDecl *, 8> OverloadedMethods;
    SemaRef.getCxxSema().FindHiddenVirtualMethods(MD, OverloadedMethods);

    if (!OverloadedMethods.empty()) {
      if (clang::OverrideAttr *OA = MD->getAttr<clang::OverrideAttr>()) {
        SemaRef.Diags.Report(OA->getLocation(),
                  clang::diag::override_keyword_hides_virtual_member_function)
                              << "override" << (OverloadedMethods.size() > 1);
      } else if (clang::FinalAttr *FA = MD->getAttr<clang::FinalAttr>()) {
        SemaRef.Diags.Report(FA->getLocation(),
                  clang::diag::override_keyword_hides_virtual_member_function)
                          << (FA->isSpelledAsSealed() ? "sealed" : "final")
                          << (OverloadedMethods.size() > 1);
      }
      SemaRef.getCxxSema().NoteHiddenVirtualMethods(MD, OverloadedMethods);
      MD->setInvalidDecl();
      return;
    }
    // Fall through into the general case diagnostic.
    // FIXME: We might want to attempt typo correction here.
  }

  if (!MD || !MD->isVirtual()) {
    if (clang::OverrideAttr *OA = MD->getAttr<clang::OverrideAttr>()) {
      SemaRef.Diags.Report(OA->getLocation(),
        clang::diag::override_keyword_only_allowed_on_virtual_member_functions)
                            << "override"
                          << clang::FixItHint::CreateRemoval(OA->getLocation());
      MD->dropAttr<clang::OverrideAttr>();
    }
    if (clang::FinalAttr *FA = MD->getAttr<clang::FinalAttr>()) {
      SemaRef.Diags.Report(FA->getLocation(),
        clang::diag::override_keyword_only_allowed_on_virtual_member_functions)
                            << (FA->isSpelledAsSealed() ? "sealed" : "final")
                        << clang::FixItHint::CreateRemoval(FA->getLocation());
      MD->dropAttr<clang::FinalAttr>();
    }
    return;
  }

  // C++11 [class.virtual]p5:
  //   If a function is marked with the virt-specifier override and
  //   does not override a member function of a base class, the program is
  //   ill-formed.
  bool HasOverriddenMethods = MD->size_overridden_methods() != 0;
  if (MD->hasAttr<clang::OverrideAttr>() && !HasOverriddenMethods)
    SemaRef.Diags.Report(MD->getLocation(),
                      clang::diag::err_function_marked_override_not_overriding)
                          << MD->getDeclName();
}

static clang::StorageClass getDefaultVariableStorageClass(Elaborator &Elab) {
  return Elab.SemaRef.getCurrentScope()->isBlockScope() ||
    Elab.SemaRef.getCurrentScope()->isControlScope()
    ? clang::SC_Auto
    : clang::SC_None;
}


clang::Decl *Elaborator::elaborateVariableDecl(Declaration *D) {
  D->SavedScope = SemaRef.getCurrentScope();
  if (D->ScopeForDecl->isParameterScope())
    return elaborateParameterDecl(D);

  if (D->ScopeForDecl->isTemplateScope())
    return elaborateTemplateParamDecl(D);

  // This is specifically for processing a special kind of declarator.
  if (Declarator *TemplateDeclarator = D->TemplateParameters)
    return elaborateTemplateAliasOrVariable(D, TemplateDeclarator);

  // We need to make sure that the type we are elaborating isn't infact a
  // a CppxKindType expression. If it is we may have an issue emitting this
  // as a valid type alias.
  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  if (!TypeExpr->getType()->isTypeOfTypes()) {
    // TODO: template and namespace types need a variable implementation.

    if (TypeExpr->getType()->isTemplateType()) {
      llvm_unreachable("Template variables not implemented yet");
    }
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_declaration_type_not_a_type);
    return nullptr;
  }

  clang::CppxTypeLiteral *TyLitExpr = dyn_cast<clang::CppxTypeLiteral>(TypeExpr);
  if (!TyLitExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_unsupported_unknown_any_decl)
                        << D->getId();
    return nullptr;
  }
  if (TyLitExpr->getValue()->getType()->isTypeOfTypes()) {
    // TODO: This will need to be handled using a CppxPartialDecl.
    return elaborateTypeAlias(D);
  }
  if (TyLitExpr->getValue()->getType()->isNamespaceType()) {
    return elaborateNsAlias(D);
  }

  if (D->ScopeForDecl->isClassScope()) {
    return elaborateField(D);
  }

  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TyLitExpr,
                                                             D->Decl->getLoc());
  if (!TInfo) {
    return nullptr;
  }
  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::StorageClass SC = getDefaultVariableStorageClass(*this);

  // Create the variable and add it to it's owning context.
  clang::VarDecl *VD = clang::VarDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                              Id, TInfo->getType(), TInfo, SC);
  Owner->addDecl(VD);
  D->Cxx = VD;
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  return VD;
}

static clang::Decl *buildTypeAlias(Elaborator &E, Sema &SemaRef,
                                   Declaration *D, clang::Expr *TyExpr) {
  clang::ParsedType PT;
  clang::TypeSourceInfo *TInfo =
    SemaRef.getTypeSourceInfoFromExpr(TyExpr, D->Init->getLoc());
  if (!TInfo)
    return nullptr;

  PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
  D->CurrentPhase = Phase::Initialization;

  clang::IdentifierInfo *IdInfo = D->getId();
  clang::UnqualifiedId Id;
  Id.setIdentifier(IdInfo, D->Decl->getLoc());
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::MultiTemplateParamsArg MTP;

  // Constructing the type alias on the way out because we need to correctly
  // construct its internal type before continuing.
  clang::TypeResult TR(PT);
  clang::Decl *TypeAlias = SemaRef.getCxxSema().ActOnAliasDeclaration(
      SemaRef.getCurClangScope(), clang::AS_public, MTP, Loc, Id,
      clang::ParsedAttributesView(), TR, nullptr);

  D->Cxx = TypeAlias;
  E.elaborateAttributes(D);
  return TypeAlias;
}

clang::Decl *Elaborator::elaborateTypeAlias(Declaration *D) {
  if (!D->Init) {
    SemaRef.Diags.Report(D->Op->getLoc(), clang::diag::err_expected_type);
    return nullptr;
  }

  // Elaborating RHS
  ExprElaborator Elab(Context, SemaRef);
  clang::Expr *InitTyExpr = Elab.elaborateExpr(D->Init);
  if (!InitTyExpr->getType()->isTypeOfTypes()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "initializer of type alias is not a type");
    SemaRef.Diags.Report(D->Init->getLoc(), DiagID);
    return nullptr;
  }

  return buildTypeAlias(*this, SemaRef, D, InitTyExpr);
}

static clang::NamespaceAliasDecl *buildNsAlias(clang::ASTContext &CxxAST,
                                               Sema &SemaRef, Declaration *D,
                                               clang::Expr *NsExpr) {
  clang::Decl *PossibleNs = SemaRef.getDeclFromExpr(NsExpr, D->Init->getLoc());
  if (!PossibleNs)
    return nullptr;
  assert(isa<clang::NamedDecl>(PossibleNs) && "invalid namespace");

  clang::NamedDecl *Ns
    = cast<clang::NamedDecl>(PossibleNs)->getUnderlyingDecl();

  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::SourceLocation TypeDeclLoc = D->TypeDcl ?
    D->TypeDcl->getLoc() : clang::SourceLocation();
  clang::NamespaceAliasDecl *NsAD
    = clang::NamespaceAliasDecl::Create(CxxAST, Owner,
                                        TypeDeclLoc,
                                        D->Decl->getLoc(),
                                        D->getId(),
                                        clang::NestedNameSpecifierLoc(),
                                        D->Init->getLoc(),
                                        Ns);
  Owner->addDecl(NsAD);

  // Nested name specifiers are looked up by clang, so we need to convince
  // the clang lookup that this namespace actually exists.
  SemaRef.getCurClangScope()->AddDecl(NsAD);
  SemaRef.getCxxSema().IdResolver->AddDecl(NsAD);
  D->Cxx = NsAD;
  D->CurrentPhase = Phase::Initialization;
  return NsAD;
}

clang::Decl *Elaborator::elaborateNsAlias(Declaration *D) {
  if (!D->Init) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_namespace_alias_requires_a_name);
    return nullptr;
  }

  // Attempting to elaborate the RHS to get a namespace.
  ExprElaborator Elab(Context, SemaRef);
  clang::Expr *NsExpr = Elab.elaborateExpr(D->Init);
  if (!NsExpr)
    return nullptr;

  if (!NsExpr->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_namespace_alias_non_namespace);
    return nullptr;
  }

  return buildNsAlias(Context.CxxAST, SemaRef, D, NsExpr);
}

clang::Decl *Elaborator::elaborateTemplateAliasOrVariable(Declaration *D,
    Declarator *TemplateParams) {
  bool InClass = D->ScopeForDecl->getKind();
  // Attempting to elaborate template for scope.
  // const ListSyntax *TemplParams;

  // Checking if we are a nested template decl/class.
  clang::MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  // This REQUIRES that we have specified type for now. But in order to do this
  // correctly we can't construct a templated type right off the bat we need
  // to figure out
  // Attempting to get the
  ExprElaborator Elab(Context, SemaRef);
  if (!D->TypeDcl) {
    llvm_unreachable("Improperly identified template type alias, "
                     "or template variable");
  }
  TypeDeclarator *TyDeclarator = D->TypeDcl->getAsType();
  clang::Expr *TypeExpr = Elab.elaborateTypeExpr(TyDeclarator);
  if (!TypeExpr) {
    SemaRef.Diags.Report(TyDeclarator->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  if (!TypeExpr->getType()->isTypeOfTypes()) {
    if (TypeExpr->getType()->isNamespaceType()) {
      SemaRef.Diags.Report(TyDeclarator->getLoc(),
                           clang::diag::err_templated_namespace_type);
      return nullptr;
    }
    if (TypeExpr->getType()->isTemplateType()) {
      llvm_unreachable("Template variables not implemented yet");
    }
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_declaration_type_not_a_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TypeExprInfo
               = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                   TyDeclarator->getLoc());
  if (!TypeExprInfo)
    return nullptr;

  // Constructing the elaboration name.
  clang::IdentifierInfo *IdInfo = D->getId();
  clang::UnqualifiedId Id;
  assert(D->IdDcl && "We are some how missing an identifier declarator?!\n");
  Id.setIdentifier(IdInfo, D->IdDcl->getLoc());
  clang::SourceLocation Loc = D->Op->getLoc();

  if (!TypeExprInfo->getType()->isTypeOfTypes()) {
    bool DeclIsStatic = false;
    if (isStaticMember(SemaRef, D, DeclIsStatic)) {
      return nullptr;
    }

    // Emit an error message here.
    if (InClass && !DeclIsStatic) {
      SemaRef.Diags.Report(D->IdDcl->getLoc(),
                           clang::diag::err_template_member)
                           << D->getId()->getName();
      return nullptr;
    }
    clang::StorageClass TS = clang::SC_None;
    if (DeclIsStatic) {
      TS = clang::SC_Static;
    }
    clang::VarDecl *VDecl = clang::VarDecl::Create(Context.CxxAST,
                                               SemaRef.getCurClangDeclContext(),
                                                   Loc, Loc, IdInfo,
                                                   TypeExprInfo->getType(),
                                                   TypeExprInfo, TS);
    clang::DeclarationName DeclName = IdInfo;
    clang::VarTemplateDecl *VTD = clang::VarTemplateDecl::Create(
                                                      Context.CxxAST,
                                                      VDecl->getDeclContext(),
                                                      Loc, DeclName, MTP.back(),
                                                      VDecl);
    SemaRef.getCxxSema().PushOnScopeChains(VTD, SemaRef.getCurClangScope(),
                                           true);
    D->CurrentPhase = Phase::Typing;
    D->Cxx = VTD;
  } else {
    if (!D->Init) {
      SemaRef.Diags.Report(D->Op->getLoc(),
                          clang::diag::err_templated_namespace_type);
      return nullptr;
    }
    // Attempting to elaborate the type expression.
    clang::Expr *InitTyExpr = Elab.elaborateExpr(D->Init);
    if (!InitTyExpr)
      return nullptr;
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InitTyExpr,
                                                              D->Init->getLoc());
    if (!TInfo)
      return nullptr;
    clang::ParsedType PT;
    PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
    clang::TypeResult TR(PT);
    clang::Decl *TypeAlias = SemaRef.getCxxSema().ActOnAliasDeclaration(
        SemaRef.getCurClangScope(), clang::AS_public, MTP, Loc, Id,
        clang::ParsedAttributesView(), TR, nullptr);
    D->Cxx = TypeAlias;
    // Only the type alias is fully elaborated at this point in time.
    D->CurrentPhase = Phase::Initialization;
  }
  if (D->Cxx) {
    elaborateAttributes(D);
  }
  return D->Cxx;
}

clang::Decl *Elaborator::elaborateParameterDecl(Declaration *D) {
  // Get type information.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();

  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                             D->Decl->getLoc());
  if (!TInfo) {
    return nullptr;
  }

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::DeclarationNameInfo DNI({Id, Loc});
  // This may help attribute elaboration.
  Sema::ClangScopeRAII FunctionDeclScope(SemaRef, clang::Scope::DeclScope |
      clang::Scope::FunctionPrototypeScope |
      clang::Scope::FunctionDeclarationScope,
      clang::SourceLocation());
  // Just return the parameter. We add it to it's function later.
  clang::ParmVarDecl *P = SemaRef.getCxxSema().CheckParameter(Owner, Loc, DNI,
                                                              TInfo->getType(),
                                                              TInfo,
                                                              clang::SC_None);
  D->Cxx = P;
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  return P;
}

clang::Decl *Elaborator::elaborateTemplateParamDecl(Declaration *D) {
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();

  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->IdDcl->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                            D->IdDcl->getLoc());
  if (!TInfo)
    return nullptr;

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->IdDcl->getLoc();

  // This is a template type or template template parameter decl.
  if (TInfo->getType()->getAs<clang::CppxKindType>()) {
    using TemplateTemplate = clang::TemplateTemplateParmDecl;
    using TemplateType = clang::TemplateTypeParmDecl;

    if (D->TemplateParameters)
      D->Cxx = TemplateTemplate::Create(Context.CxxAST, Owner, Loc, 0,
                                        0, /*Pack=*/false, Id,
                                        D->TemplateParamStorage.front());
    else
      D->Cxx = TemplateType::Create(Context.CxxAST, Owner, Loc, Loc, 0, 0,
                                    Id, /*TypenameKW=*/true, /*Pack=*/false);

    D->CurrentPhase = Phase::Typing;
    return D->Cxx;
  }

  // The depth and position of the parameter will be set later.
  auto *NTTP =
    clang::NonTypeTemplateParmDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                           0, 0, Id, TInfo->getType(),
                                           /*Pack=*/false, TInfo);
  D->Cxx = NTTP;
  D->CurrentPhase = Phase::Typing;
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

clang::Decl *Elaborator::elaborateParmDeclSyntax(const Syntax *S) {
// Identify this as a declaration first.
  identifyDecl(S);

  // Elaborate the declaration.
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (D) {
    elaborateDecl(D);
    if (!D->Cxx)
      return nullptr;
    if (D->Init) {

      if (SemaRef.isElaboratingClass()) {
        delayElaborateDefaultParam(D);
      } else {
        // In the event we are not a member declaration then process the
        // default argument right off the bat.
        elaborateDef(D);
      }
    }
    return D->Cxx;
  }

  return nullptr;
}

clang::Decl *Elaborator::elaborateDeclEarly(Declaration *D) {
  assert(D && D->getId() && "Early elaboration of unidentified declaration");
  Sema::OptionalInitScope<Sema::EnterNonNestedClassEarlyElaboration>
    ENNCEE(SemaRef);
  if (SemaRef.isElaboratingClass() && !D->isDeclaredWithinClass()) {
    ENNCEE.Init(D);
  }

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
  if (phaseOf(D) != Phase::Typing)
    return;

  if (D->declaresFunction())
    return elaborateFunctionDef(D);
  if (D->ScopeForDecl->isTemplateScope())
    return elaborateTemplateParamInit(D);
  if (D->declaresMemberVariable())
    return;
  if (D->declaresTypeAlias())
    return;
  return elaborateVariableInit(D);
}

void Elaborator::elaborateFunctionDef(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  if (!D->Cxx)
    return;
  if (!D->Init)
    return;
  if (SemaRef.checkForRedefinition<clang::FunctionDecl>(D))
    return;
  if (D->declaresConstructor()) {
    llvm::SmallVector<clang::CXXCtorInitializer*, 32> Initializers;
    SemaRef.getCxxSema().ActOnMemInitializers(D->Cxx, clang::SourceLocation(),
                                              Initializers, false);
  }

  if (D->Op) {
    if (D->declaresFunctionWithImplicitReturn()) {
      // Checking to see if the declaration is actually what we are expected
      // in order to be consider implicitly virtual or not.
      if (clang::CXXMethodDecl *MD = D->getAs<clang::CXXMethodDecl>()) {
        if (MD->isVirtual() && D->declaresPossiblePureVirtualFunction()
            && !D->defines<clang::CXXConstructorDecl>()) {
          // Declaring this as an abstract function declaration.
          SemaRef.getCxxSema().ActOnPureSpecifier(D->Cxx, D->Init->getLoc());
          return;
        }
      }
      // Checking other kinds of functions.
      if (D->declaresDefaultedFunction()) {
        SemaRef.getCxxSema().SetDeclDefaulted(D->Cxx, D->Init->getLoc());
        return;
      }
      if (D->declaresDeletedFunction()) {
        SemaRef.getCxxSema().SetDeclDeleted(D->Cxx, D->Init->getLoc());
        return;
      }
    }
  }



  // We saved the parameter scope while elaborating this function's type,
  // so push it on before we enter the function scope.
  FunctionDeclarator *FnDecl = D->FunctionDcl->getAsFunction();
  Sema::ResumeScopeRAII FnDclScope(SemaRef, FnDecl->getScope(),
                                   FnDecl->getScope()->getConcreteTerm());

  Declaration *CurrentDeclaration = SemaRef.getCurrentDecl();
  // Entering clang scope. for function definition.
  clang::Scope *FnClangScope = SemaRef.enterClangScope(clang::Scope::FnScope |
                                                       clang::Scope::DeclScope |
                                               clang::Scope::CompoundStmtScope);

  clang::Decl *FuncDecl = SemaRef.getCxxSema().ActOnStartOfFunctionDef(
                                                          FnClangScope, D->Cxx);


  SemaRef.enterScope(SK_Function, D->Init, D);
  SemaRef.setCurrentDecl(D);
  // FIXME: is this necessary for Gold? It enables some more semantic
  // checking, but not all of it is necessarily meaningful to us.
  // clang::Scope *Scope = SemaRef.enterClangScope(clang::Scope::ClassScope
  //                                               | clang::Scope::DeclScope);
  // Elaborate the function body.
  StmtElaborator BodyElaborator(Context, SemaRef);
  clang::Stmt *Body = BodyElaborator.elaborateBlock(D->Init);
  SemaRef.getCxxSema().ActOnFinishFunctionBody(FuncDecl, Body);

  // Leave the function scope.
  SemaRef.leaveScope(D->Init);

  SemaRef.setCurrentDecl(CurrentDeclaration);
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
  D->CurrentPhase = Phase::Initialization;
  if (!D->Cxx)
    return;

  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);
  bool NeedsConstEvaluation = false;
  clang::VarDecl *VD = nullptr;
  if (D->defines<clang::VarTemplateDecl>()) {
    if (SemaRef.checkForRedefinition<clang::VarTemplateDecl>(D))
      return;

    // We need to attempt to re-enter the template context for this variable.
    OptResumeScope.Init(D->SavedScope, D->Op);
    clang::VarTemplateDecl *VTD = cast<clang::VarTemplateDecl>(D->Cxx);
    VD = VTD->getTemplatedDecl();
  } else {
    if (SemaRef.checkForRedefinition<clang::VarDecl>(D))
      return;
    VD = cast<clang::VarDecl>(D->Cxx);
  }
  if (VD->isConstexpr()) {
    NeedsConstEvaluation = true;
  }
  if (!D->Init) {
    if (isa<clang::ParmVarDecl>(VD))
      return;
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
    SemaRef.getCxxSema().FinalizeDeclaration(VD);
    return;
  }

  // If we are inside of a variable template we need to re-enter the scope
  // for the variable.
  clang::Expr *InitExpr = nullptr;
  ExprElaborator ExprElab(Context, SemaRef);
  if (D->declaresInlineInitializedStaticVarDecl() && !NeedsConstEvaluation) {
    Sema::ExprEvalRAII EvalScope(SemaRef,
      clang::Sema::ExpressionEvaluationContext::PotentiallyEvaluated);
    InitExpr = ExprElab.elaborateExpr(D->Init);
  } else {
    if (NeedsConstEvaluation)
      InitExpr = ExprElab.elaborateExpectedConstantExpr(D->Init);
    else
      InitExpr = ExprElab.elaborateExpr(D->Init);
  }

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
      if (!InitExpr) {
        SemaRef.Diags.Report(VD->getLocation(), clang::diag::err_auto_no_init);
        return;
      }

      if (InitExpr->getType()->isNamespaceType()) {
        D->Cxx->getDeclContext()->removeDecl(D->Cxx);
        D->Cxx = buildNsAlias(Context.CxxAST, SemaRef, D, InitExpr);
        return;
      }

      if (InitExpr->getType()->isTypeOfTypes()) {
        D->Cxx->getDeclContext()->removeDecl(D->Cxx);
        D->Cxx = buildTypeAlias(*this, SemaRef, D, InitExpr);
        return;
      }

      clang::Sema &CxxSema = SemaRef.getCxxSema();
      auto Result = CxxSema.DeduceAutoType(VD->getTypeSourceInfo(), InitExpr, Ty);
      if (Result == clang::Sema::DAR_Failed) {
        SemaRef.Diags.Report(VD->getLocation(), clang::diag::err_auto_failed);
        return;
      }
    }

    VD->setType(Ty);
  }

  if (D->Init && !InitExpr) {
    SemaRef.Diags.Report(VD->getLocation(),
                         clang::diag::err_failed_to_translate_expr);
    return;
  }
  if (D->defines<clang::VarTemplateDecl>()) {
    // I may need to revisit this in the furture becaus this might not be
    // the right thing to do in this case.
    VD->setInit(InitExpr);
  } else {
    // Update the initializer.
    SemaRef.getCxxSema().AddInitializerToDecl(VD, InitExpr, /*DirectInit=*/true);
  }
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

  clang::Expr *Init =
    ExprElaborator(Context, SemaRef).elaborateExpr(D->Init);
  if (!Init) {
    D->CurrentPhase = Phase::Initialization;
    return;
  }

  clang::SourceLocation Loc = Init->getExprLoc();
  if (auto *TD = dyn_cast<clang::NonTypeTemplateParmDecl>(D->Cxx)) {
    TD->setDefaultArgument(Init);
  } else if (auto *TD = dyn_cast<clang::TemplateTypeParmDecl>(D->Cxx)) {
    clang::QualType InitTy = Init->getType();
    if (!InitTy->isTypeOfTypes())
      SemaRef.Diags.Report(Loc, clang::diag::err_expected_type);
    else
      TD->setDefaultArgument(SemaRef.getTypeSourceInfoFromExpr(Init, Loc));
  } else if (auto *TD = dyn_cast<clang::TemplateTemplateParmDecl>(D->Cxx)) {
    clang::TemplateArgument Arg(Init, clang::TemplateArgument::Expression);
    clang::TemplateArgumentLoc ArgLoc(Arg, Init);
    TD->setDefaultArgument(Context.CxxAST, ArgLoc);
  }

  D->CurrentPhase = Phase::Initialization;
}

clang::Decl *Elaborator::elaborateTypeBody(Declaration* D, clang::CXXRecordDecl* R) {
  if(!D->Init) {
    // FIXME: Handle forward declarations here? I think.
    llvm_unreachable("Type forward declarations are not implemented yet.");
    return nullptr;
  }
  auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
  assert(MacroRoot && "Invalid AST structure.");
  auto const* BodyArray = MacroRoot->getBlock();
  D->CurrentPhase = Phase::Typing;

  for (auto const* ChildDecl : BodyArray->children()) {
    identifyDecl(ChildDecl);
  }
  return D->Cxx;
}

clang::Decl *Elaborator::elaborateField(Declaration *D) {
  clang::Decl *Ctxt = SemaRef.getCurrentDecl()->Cxx;
  clang::CXXRecordDecl *Owner = dyn_cast<clang::CXXRecordDecl>(Ctxt);
  // Get the type of the entity.
  if(!Owner) {
    llvm_unreachable("Invalid field declaration. Declaration is not within a class.");
  }
  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);

  if (!TypeExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                             D->Decl->getLoc());
  if (!TInfo)
    return nullptr;

  clang::SourceLocation Loc = D->Decl->getLoc();
  clang::SourceLocation LocEnd = D->getEndOfDecl();
  clang::DeclarationName DN = D->getId();
  clang::InClassInitStyle InitStyle = clang::InClassInitStyle::ICIS_NoInit;
  if (D->Init)
    InitStyle = clang::InClassInitStyle::ICIS_ListInit;

  bool DeclIsStatic = false;
  if (isStaticMember(SemaRef, D, DeclIsStatic)) {
    return nullptr;
  }
  clang::Decl *Field = nullptr;
  if (DeclIsStatic) {
    // In this case we are creating a static member variable.
    clang::VarDecl *VDecl= clang::VarDecl::Create(Context.CxxAST, Owner, Loc,
                                                  LocEnd, D->getId(),
                                                  TInfo->getType(), TInfo,
                                                  clang::SC_Static);
    VDecl->setInlineSpecified();
    VDecl->setAccess(clang::AS_public);
    Field = VDecl;
  } else {
    bool Mutable = false;
    if (isMutable(SemaRef, D, Mutable))
      return nullptr;
    // We are create field within a class.
    Field = SemaRef.getCxxSema().CheckFieldDecl(DN, TInfo->getType(),
                                                TInfo, /*RecordDecl=*/Owner,
                                                Loc, Mutable,
                                                /*BitWidth=*/nullptr, InitStyle,
                                                Loc, clang::AS_public, nullptr);
  }
  Owner->addDecl(Field);
  D->Cxx = Field;
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  return Field;
}

void Elaborator::elaborateFieldInit(Declaration *D) {
  assert(D && "Missing Declaration.");
  if (!D->Init)
    return;
  D->CurrentPhase = Phase::Initialization;
  SemaRef.getCxxSema().ActOnStartCXXInClassMemberInitializer();

  using EEC = clang::Sema::ExpressionEvaluationContext;
  clang::EnterExpressionEvaluationContext EEContext(SemaRef.getCxxSema(),
                                             EEC::PotentiallyEvaluated, D->Cxx);

  ExprElaborator ExprElab(Context, SemaRef);
  clang::Expr *Init = ExprElab.elaborateExpr(D->Init);
  if (!Init) {
    return;
  }

  SemaRef.getCxxSema().ActOnFinishCXXInClassMemberInitializer(D->Cxx,
                                                         D->Op->getLoc(), Init);
}

clang::Decl *
Elaborator::elaborateEnumBody(Declaration* D,
                              clang::Decl *EnumD) {
  llvm::SmallVector<clang::Decl *, 32> EnumConstantDecls;
  clang::EnumConstantDecl *OriginalLastEnumConst
                                       = SemaRef.getCxxSema().LastEnumConstDecl;
  SemaRef.getCxxSema().LastEnumConstDecl = nullptr;

  auto const* MacroRoot = cast<MacroSyntax>(D->Init);
  auto const* BodyArray = MacroRoot->getBlock();
  // Phase 1
  for (const Syntax *EnumMember : BodyArray->children()) {
    auto *D = identifyDecl(EnumMember);
    if (!D) {
      EnumD->setInvalidDecl();
      SemaRef.Diags.Report(EnumMember->getLoc(),
                           clang::diag::err_invalid_enum_member_decl);
      return nullptr;
    }
  }
  D->CurrentPhase = Phase::Identification;
  // Elaborating only part of the declaration.
  // Phase 2
  for (const Syntax* EnumMember : BodyArray->children()) {
    clang::Decl *Temp = elaborateEnumMemberDecl(EnumMember, EnumD);
    if (Temp)
      EnumConstantDecls.push_back(Temp);
    else
      EnumD->setInvalidDecl();
  }
  D->CurrentPhase = Phase::Typing;

  for (const Syntax* EnumMember : BodyArray->children()) {
    if (elaborateEnumMemberInit(EnumMember)) {
      EnumD->setInvalidDecl();
    }
  }


  // Attempting to complete elaboration of the body of an enumeration.
  D->CurrentPhase = Phase::Initialization;

  // Restoring previous decl.
  SemaRef.getCxxSema().LastEnumConstDecl = OriginalLastEnumConst;
  clang::ParsedAttributes attrs(SemaRef.AttrFactory);

  SemaRef.getCxxSema().ActOnEnumBody(D->IdDcl->getLoc(),
                                     clang::SourceRange(),
                                     EnumD, EnumConstantDecls,
                                     SemaRef.getCurClangScope(), attrs);
  return D->Cxx;
}

clang::Decl *Elaborator::elaborateEnumMemberDecl(const Syntax *S,
                                                 clang::Decl *EnumD) {
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    // This should indicate that some kind of error occurred prior to this and
    // we need to bail because we can't properly elaborate this.
    return nullptr;

  if (phaseOf(D) > Phase::Identification)
    return D->Cxx;

  if (phaseOf(D) != Phase::Identification)
    return D->Cxx;
  assert(D->IdDcl);
  // Attempting to construct enum name.
  clang::DeclarationNameInfo DNI({D->Id}, D->IdDcl->getLoc());
  clang::ParsedAttributes Attributes(SemaRef.AttrFactory);
  // We do this now and we add the value later on.
  clang::Decl *ECD = SemaRef.getCxxSema().ActOnEnumConstant(
      SemaRef.getCurClangScope(), EnumD, SemaRef.getCxxSema().LastEnumConstDecl,
      DNI, Attributes, clang::SourceLocation(), nullptr);

  D->Cxx = ECD;
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  SemaRef.getCxxSema().LastEnumConstDecl
                                   = cast_or_null<clang::EnumConstantDecl>(ECD);
  return ECD;
}

bool Elaborator::elaborateEnumMemberInit(const Syntax *S) {
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    // This should indicate that some kind of error occurred prior to this and
    // we need to bail because we can't properly elaborate this.
    return true;
  if (phaseOf(D) > Phase::Initialization)
    return false;

  if (phaseOf(D) != Phase::Typing)
    return true;
  if (!D->Cxx)
    return true;

  ExprElaborator Elab(Context, SemaRef);
  if (D->Init) {
    clang::Expr *ConstExpr = Elab.elaborateExpectedConstantExpr(D->Init);
    if (ConstExpr) {
      clang::EnumConstantDecl *ECD = cast<clang::EnumConstantDecl>(D->Cxx);
      ECD->setInitExpr(ConstExpr);
    } else {
      return true;
    }
  }
  D->CurrentPhase = Phase::Initialization;
  return false;
}


Declaration *Elaborator::identifyDecl(const Syntax *S) {
  /// Moving the declaration/declarator construction into another class.
  // This was created to prevent duplicate elaboration failure which could
  // previously result in an error.
  if (SemaRef.getCurrentScope()->hasDeclaration(S)) {
    return nullptr;
  }
  return DeclarationBuilder(SemaRef).build(S);
}

bool Elaborator::delayElaborateDeclType(clang::CXXRecordDecl *RD,
                                        const Syntax *S) {
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D) {
    return false;
  }

  // Handling a check for possible late elaboration on each declaration.
  if (phaseOf(D) > Phase::Identification)
    return false;


  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  if (D->declaresTag()) {
    delayElaborationClassBody(D);
    return true;
  }
  bool WasDelayed = false;
  if (D->declaresFunction()) {
    if (D->declIsStatic()) {
      // TODO: If this reaches across class then we may need to change how look up
      // is handled in this set of circumstances. Because this could
      // reach into the scope of another class and get data from there
      // but the late elaboration from there might not exist yet.
      // So we might need to do partial elaboration of a few things in order to
      // correctly define them.
      elaborateDecl(D);
    } else {
      if (RD->isUnion()) {
        elaborateDecl(D);
      } else {
        // Attempting to delay method decl/def combos
        delayElaborateMethodDecl(D);
        WasDelayed = true;
      }
    }
    if (D->declaresFunctionDef()) {
      delayElaborateMethodDef(D);
      WasDelayed = true;
    }
    return WasDelayed;
  }

  // The final portion of this handles field declaration processing.
  // I will need to check for initializers and if the thing is static or not.
  elaborateVariableDecl(D);
  if (!D->declaresMemberVariable()) {
    elaborateDeclEarly(D);
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
    new LateElaborateMemberInitializer(SemaRef, Context, D)
  );
}

void Elaborator::delayElaborateMethodDecl(Declaration *D) {
  SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
    new LateElaboratedMethodDeclaration(SemaRef, Context, D)
  );
}

void Elaborator::delayElaborateMethodDef(Declaration *D) {
  SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
    new LateElaboratedMethodDef(SemaRef, Context, D)
  );
}

void Elaborator::delayElaborationClassBody(Declaration *D) {
  elaborateDecl(D);
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


  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope, Class.TagOrTemplate->Op);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);
  }

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateAttributes();
  }
  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);
}

void Elaborator::lateElaborateMethodDecls(ElaboratingClass &Class) {
  // If the current class is a template re-enter the template before we continue.
  bool HasTemplateScope = !Class.IsTopLevelClass && Class.TemplateScope;
  Sema::ClangScopeRAII ClassTemplateScope(SemaRef,
    clang::Scope::TemplateParamScope, clang::SourceLocation(), HasTemplateScope);

  if (HasTemplateScope)
    SemaRef.getCxxSema().ActOnReenterTemplateScope(Class.TagOrTemplate->Cxx,
                                                   [&] {
      return SemaRef.getCurClangScope();
    });

  bool CurrentlyNested = !Class.IsTopLevelClass;
  Sema::ClangScopeRAII FunctionDeclScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);

  Sema::OptionalInitScope<Sema::DeclContextRAII> DCTracking(SemaRef);
  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Op, false);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);
    DCTracking.Init(Class.TagOrTemplate, true);
  }

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMethodDeclarations();
  }

  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);
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

  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);
  Sema::OptionalInitScope<Sema::DeclContextRAII> DCTracking(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested) {
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Op, false);
    SemaRef.getCxxSema().ActOnStartDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);
    DCTracking.Init(Class.TagOrTemplate, true);
  }

  clang::Sema::CXXThisScopeRAII PushThisIntoScope(SemaRef.getCxxSema(),
      Class.TagOrTemplate->Cxx, clang::Qualifiers());

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMemberInitializers();
  }

  if (CurrentlyNested)
    SemaRef.getCxxSema().ActOnFinishDelayedMemberDeclarations(
      SemaRef.getCurClangScope(), Class.TagOrTemplate->Cxx);

  SemaRef.getCxxSema().ActOnFinishDelayedMemberInitializers(
    Class.TagOrTemplate->Cxx);
}


void Elaborator::lateElaborateMethodDefs(ElaboratingClass &Class) {
  bool CurrentlyNested = !Class.IsTopLevelClass;
  Sema::ClangScopeRAII MethodDefScope(SemaRef, clang::Scope::DeclScope |
    clang::Scope::ClassScope, clang::SourceLocation(), CurrentlyNested);

  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);

  // This may need to be moved to somewhere else.
  if (CurrentlyNested)
    OptResumeScope.Init(Class.TagOrTemplate->SavedScope,
      Class.TagOrTemplate->Op, false);

  for (size_t i = 0; i < Class.LateElaborations.size(); ++i) {
    Class.LateElaborations[i]->ElaborateMethodDefs();
  }
}

void Elaborator::lateElaborateAttribute(LateElaboratedAttributeDecl &Field) {
  // TODO: find a valid example of how this actually works/when this is used.
  llvm_unreachable("We currently don't have late binding attributes? I don't "
      "really know.");
}

void Elaborator::lateElaborateMemberInitializer(
    LateElaborateMemberInitializer &MemberInit) {
  assert(MemberInit.D && "Invalid declaration detected.");
  assert(MemberInit.D->declaresMemberVariable()
         && "Declaration doesn't declare a field.\n");

  // Start delayed member This occurs for each member initializer within a
  // given class.
  elaborateFieldInit(MemberInit.D);
}

void Elaborator::lateElaborateMethodDecl(
    LateElaboratedMethodDeclaration &Method) {
  Sema::ClangScopeRAII FunctionDeclScope(SemaRef, clang::Scope::DeclScope |
      clang::Scope::FunctionPrototypeScope |
      clang::Scope::FunctionDeclarationScope,
      clang::SourceLocation());
  Sema::LateMethodRAII MethodTracking(SemaRef, &Method);
  elaborateDecl(Method.D);
  // This is to check if the method delcaration was a success and in the event
  // that it is we need to finish the exception specifier after the end of the
  // class.
  if (Method.D->Cxx) {
    if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(Method.D->Cxx)) {
      if (FD->getExceptionSpecType() == clang::EST_Unparsed) {
        if (!Method.D->Init) {
          SemaRef.getCurrentElaboratingClass().LateElaborations.push_back(
            new LateElaboratedMethodDef(SemaRef, Context, Method.D)
          );
        }
      }
    }
  }
  SemaRef.getCxxSema().ActOnFinishDelayedCXXMethodDeclaration(
      SemaRef.getCurClangScope(), Method.D->Cxx);
}

void Elaborator::lateElaborateDefaultParams(
    LateElaboratedMethodDeclaration &MethodDecl) {
  for(LateElaboratedDefaultArgument &Arg : MethodDecl.DefaultArgs)
    lateElaborateDefaultParam(Arg);
}

void Elaborator::lateElaborateDefaultParam(
    LateElaboratedDefaultArgument &DefaultParam) {
  elaborateDef(DefaultParam.Param);
}

void applyESIToFunctionType(SyntaxContext &Context, Sema &SemaRef,
                            clang::FunctionDecl *FD,
                       const clang::FunctionProtoType::ExceptionSpecInfo &ESI) {
  const clang::FunctionProtoType *FPT
                             = FD->getType()->getAs<clang::FunctionProtoType>();
  SemaRef.rebuildFunctionType(FD, FD->getBeginLoc(), FPT, FPT->getExtInfo(),
                              FPT->getExtProtoInfo(), ESI);
}

static void applyCallExceptionSpecAttr(SyntaxContext &Context, Sema &SemaRef,
                                       clang::FunctionDecl *FD,
                                       const gold::CallSyntax *Call,
                                       const gold::AtomSyntax *Name) {
  clang::FunctionProtoType::ExceptionSpecInfo ESI;
  ESI.SourceDecl = FD;
  clang::ExprResult ESIResultExpr;
  clang::ExceptionSpecificationType EST;
  if (Name->getSpelling() == "noexcept") {
    if (Call->getNumArguments() != 1) {
      SemaRef.Diags.Report(Call->getLoc(),
                           clang::diag::err_incorrect_number_of_arguments)
                           << Name->getSpelling();
      return;
    }
    clang::Expr *ExceptionSpecExpr = nullptr;
    {
      clang::EnterExpressionEvaluationContext ConstantEvaluated(
          SemaRef.getCxxSema(),
          clang::Sema::ExpressionEvaluationContext::Unevaluated);
      ExprElaborator ExprElab(Context, SemaRef);
      ExceptionSpecExpr = ExprElab.elaborateExpr(Call->getArgument(0));
    }
    if (!ExceptionSpecExpr)
      return;

    if (ExceptionSpecExpr->getType()->isTypeOfTypes()
        || ExceptionSpecExpr->getType()->isTemplateType()
        || ExceptionSpecExpr->getType()->isNamespaceType()) {
      SemaRef.Diags.Report(Call->getArgument(0)->getLoc(),
                           clang::diag::err_invalid_result_type);
      return;
    }
    ESIResultExpr = SemaRef.getCxxSema().ActOnNoexceptSpec(Name->getLoc(),
                                                           ExceptionSpecExpr,
                                                           EST);
    if (ESIResultExpr.isInvalid())
      return;

    ESI.NoexceptExpr = ESIResultExpr.get();
    ESI.Type = EST;

  } else if (Name->getSpelling() == "throw") {
    // EST_DynamicNone case

    clang::EnterExpressionEvaluationContext ConstantEvaluated(
                                                           SemaRef.getCxxSema(),
                         clang::Sema::ExpressionEvaluationContext::Unevaluated);
    ExprElaborator ExprElab(Context, SemaRef);
    llvm::SmallVector<clang::QualType, 8> ExceptionTypes;
    const ListSyntax *ExceptionList
                              = dyn_cast<ListSyntax>(Call->getArguments());
    for (const Syntax *TySyntax : ExceptionList->children()) {
      clang::Expr *ExceptionSpecExpr = ExprElab.elaborateExpr(TySyntax);
      if (!ExceptionSpecExpr)
        continue;

      if (!ExceptionSpecExpr->getType()->isTypeOfTypes()) {
        SemaRef.Diags.Report(TySyntax->getLoc(),
                             clang::diag::err_invalid_dyn_exception_type);
        continue;
      }
      clang::QualType ExceptionTy = SemaRef.getQualTypeFromTypeExpr(
                                                             ExceptionSpecExpr);
      if (!ExceptionTy.isNull())
        continue;
      ExceptionTypes.emplace_back(ExceptionTy);
    }

    ESI.Exceptions = ExceptionTypes;
    ESI.Type = ESI.Exceptions.size() ?
               clang::EST_Dynamic : clang::EST_DynamicNone;
  } else {
    llvm_unreachable("There are no other valid attributes for exception "
                     "specficiaton.");
  }

  // Updating the function type with exception specification info correctly.
  applyESIToFunctionType(Context, SemaRef, FD, ESI);
}

// This does almost the same thing as the previous
static void finishExceptionSpecAttr(SyntaxContext &Context, Sema &SemaRef,
                                    Declaration *D) {
  assert(D && "Invalid declaration.");
  clang::FunctionDecl *FD = cast<clang::FunctionDecl>(D->Cxx);
  if (FD->getExceptionSpecType() != clang::EST_Unparsed)
    return;
  assert(D->ES_Name && "Didn't set exception spec name");
  assert(D->ES_Call && "Didn't set exception spec call");
  applyCallExceptionSpecAttr(Context, SemaRef, FD, D->ES_Call, D->ES_Name);
}

void Elaborator::lateElaborateMethodDef(LateElaboratedMethodDef &Method) {
  if (!Method.D->Cxx)
    return;
  // Finish exception spec before method body?
  {
    if (!Method.D->FunctionDcl)
      return;
    FunctionDeclarator *FnDecl = Method.D->FunctionDcl->getAsFunction();
    // Attempting to push the scope for the current function onto the stack
    // This helps with lookup during evaluation exception specification.
    Sema::ResumeScopeRAII TempScope(SemaRef,
                                    FnDecl->getScope(),
                                    FnDecl->getScope()->getConcreteTerm(),
                                    /*PopOnExit=*/false);
    Sema::OptionalInitClangRAII<clang::Sema::CXXThisScopeRAII> ThisScope(
                                                                       SemaRef);
    if (clang::CXXMethodDecl *MD
                              = dyn_cast<clang::CXXMethodDecl>(Method.D->Cxx)) {
      ThisScope.Init(MD->getParent(), MD->getMethodQualifiers(), true);
    }
    finishExceptionSpecAttr(Context, SemaRef, Method.D);
  }
  elaborateFunctionDef(Method.D);
  if (!Method.D->Cxx)
    return;
  SemaRef.getCxxSema().ActOnFinishInlineFunctionDef(
    cast<clang::FunctionDecl>(Method.D->Cxx));
}

void Elaborator::elaborateAttributes(Declaration *D) {
  assert(D && "Missing declaration.");
  llvm::SmallVector<Attributes::iterator, 16> ToRemoved;
  AttrStatus Status;
  if (!D->Cxx)
    return;
  Declarator *DeclaratorWithAttrs = D->getIdDeclarator();
  if (!DeclaratorWithAttrs || !DeclaratorWithAttrs->UnprocessedAttributes)
    return;
  clang::ParsedAttributes AttrList(SemaRef.AttrFactory);
  auto Iter = DeclaratorWithAttrs->UnprocessedAttributes->begin();
  auto End = DeclaratorWithAttrs->UnprocessedAttributes->end();
  for (; Iter != End; ++Iter) {
    const AtomSyntax *Name;
    if ((Name = dyn_cast<AtomSyntax>(*Iter))) {
      auto Handler = SemaRef.AttrHandlerMap.find(Name->getSpelling());
      if (Handler == SemaRef.AttrHandlerMap.end()) {
        elaborateSystemAttribute(D->Cxx, *Iter, Status, AttrList);
      } else {
        Handler->second(*this, D, *Iter, Status);
      }
      continue;
    }
    if (const CallSyntax* Call = dyn_cast<CallSyntax>(*Iter)) {
      if ((Name = dyn_cast<AtomSyntax>(Call->getCallee()))) {
        auto Handler = SemaRef.AttrHandlerMap.find(Name->getSpelling());
        if (Handler == SemaRef.AttrHandlerMap.end()) {
          elaborateSystemAttribute(D->Cxx, *Iter, Status, AttrList);
        } else {
          Handler->second(*this, D, *Iter, Status);
        }
        continue;
      }
    }
    elaborateSystemAttribute(D->Cxx, *Iter, Status, AttrList);
  }
  // Attempting to handle the remaining declarations.
  SemaRef.getCxxSema().ProcessDeclAttributeList(SemaRef.getCurClangScope(),
                                                D->Cxx, AttrList, true);
}

void Elaborator::elaborateConstExprAttr(Declaration *D, const Syntax *S,
                                        AttrStatus &Status) {
  if (Status.HasConstExpr) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_not_valid_as_call)
                         << "constexpr";
    return;
  }
  Status.HasConstExpr = true;
  // Applying constant expression kind to the FunctionDecl.
  if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx)) {
    FD->setImplicitlyInline();
    if (isa<clang::CXXDestructorDecl>(D->Cxx)) {
      SemaRef.Diags.Report(S->getLoc(), clang::diag::err_constexpr_dtor)
                           << clang::ConstexprSpecKind::CSK_constexpr;
      return;
    }
    FD->setConstexprKind(clang::ConstexprSpecKind::CSK_constexpr);
  } else if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
    VD->setConstexpr(true);
  } else {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_attribute_for_decl)
                         << "constexpr"
                         << "function, static member variable, or "
                            "non-member variable";
    return;
  }
}

void Elaborator::elaborateInlineAttr(Declaration *D, const Syntax *S,
                                     AttrStatus &Status) {
  if (Status.HasInLine) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_not_valid_as_call)
                         << "inline";
    return;
  }
  Status.HasInLine = true;
  if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx)) {
    FD->setInlineSpecified(true);
  } else if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
    VD->setInlineSpecified();
  } else if (clang::NamespaceDecl *NsD = dyn_cast<clang::NamespaceDecl>(D->Cxx)) {
    NsD->setInline(true);
  } else {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_attribute_for_decl)
                         << "inline" << "function, variable, or namespace";
  }
}

void Elaborator::elaborateExternAttr(Declaration *D, const Syntax *S,
                                     AttrStatus &Status) {
  if (isa<clang::CXXRecordDecl>(D->Cxx->getDeclContext())) {
    SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_invalid_attribute_for_decl)
                        << "extern" << "free function or non-member variable.";
    return;
  }
  if (Status.HasExtern) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (Status.HasStatic) {
    SemaRef.Diags.Report(S->getLoc(),
              clang::diag::err_cannot_applied_to_function_with_storage_class)
                          << "static";
    return;
  }
  // Checking a few things.
  if (isa<CallSyntax>(S)) {
    llvm_unreachable("extern(\"C\") not implemented yet");
  } else if (isa<AtomSyntax>(S)) {
    if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx)) {
      FD->setStorageClass(clang::StorageClass::SC_Extern);
    } else if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
      VD->setStorageClass(clang::StorageClass::SC_Extern);
    } else {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << "extern"
                           << "free function or non-member variable.";
      return;
    }

  } else {
    llvm_unreachable("Invalid attribute format");
  }
  Status.HasExtern = true;
}

void Elaborator::elaborateAccessSpecifierAttr(Declaration *D, const Syntax *S,
                                              AttrStatus &Status) {
  if (!isa<clang::CXXRecordDecl>(D->Cxx->getDeclContext())) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_cannot_have_acces_specifier);
    return;
  }

  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_attribute_not_valid_as_call)
                        << cast<AtomSyntax>(Call->getCallee())->getSpelling();
    return;
  }
  const AtomSyntax *ASName = cast<AtomSyntax>(S);
  clang::AccessSpecifier AS = clang::AccessSpecifier::AS_public;
  if (ASName->getSpelling() == "private") {
    AS = clang::AccessSpecifier::AS_private;
  } else if (ASName->getSpelling() == "public") {
    AS = clang::AccessSpecifier::AS_public;
  } else if (ASName->getSpelling() == "protected") {
    AS = clang::AccessSpecifier::AS_protected;
  } else {
    llvm_unreachable("Unknown access specifier.");
  }
  if (clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(D->Cxx)) {
    if(RD->isTemplated()) {
      clang::ClassTemplateDecl *CTD= RD->getDescribedClassTemplate();
      CTD->setAccess(AS);
    }
  } else if (clang::FunctionDecl *FD = D->Cxx->getAsFunction()) {
    if (clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate())
      FTD->setAccess(AS);
  }
  D->Cxx->setAccess(AS);
}


void Elaborator::elaborateExceptionSpecAttr(Declaration *D, const Syntax *S,
                                            AttrStatus &Status) {
  bool isWithinClass = isa<clang::CXXRecordDecl>(D->Cxx->getDeclContext());

  if (Status.HasExceptionSpec) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx);
  if (!FD) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_attribute_for_decl)
                         << "exception specifier" << "a function";
    return;
  }

  clang::FunctionProtoType::ExceptionSpecInfo ESI;
  if (const AtomSyntax* Name = dyn_cast<AtomSyntax>(S)) {
    if (Name->getSpelling() != "noexcept") {
      SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_attribute_requires_call_syntax)
                          << Name->getSpelling();
      return;
    }
    ESI.Type = clang::EST_BasicNoexcept;
    Status.HasExceptionSpec = true;
  }

  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    if (const AtomSyntax* Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
      Status.HasExceptionSpec = true;
      if (isWithinClass) {
        ESI.Type = clang::EST_Unparsed;
        D->ES_Call = Call;
        D->ES_Name = Name;
      } else {
        applyCallExceptionSpecAttr(Context, SemaRef, FD, Call, Name);
        return;
      }
    }
  }
  // Updating the function type with exception specification info.
  applyESIToFunctionType(Context, SemaRef, FD, ESI);
}

void Elaborator::elaborateStaticAttr(Declaration *D, const Syntax *S,
                                     AttrStatus &Status) {
  if (Status.HasStatic) {
    SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_duplicate_attribute);
    return;
  }

  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_not_valid_as_call)
                         << "static";
    return;
  }

  clang::DeclContext *DC = D->Cxx->getDeclContext();
  if (!DC) {
    llvm_unreachable("AST not constructed/elaborated correctly");
  }

  // If we are a record Decl then we know how to handle static.
  if (DC->isRecord()) {
    if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(D->Cxx)) {
      if (isa<clang::CXXConstructorDecl>(D->Cxx)
          || isa<clang::CXXDestructorDecl>(D->Cxx)) {
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_invalid_attribute_for_decl)
                             << "static"
                             << "function, variable, or class member";
        return;
      }
      MD->setStorageClass(clang::StorageClass::SC_Static);
    } else if (isa<clang::FieldDecl>(D->Cxx) || isa<clang::VarDecl>(D->Cxx)) {
      // This can only be reached in the even that static is provided 2x or more
      // times and an error should have already been given in that case.
      return;
    } else {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << "static" << "function, variable, or class member";
      return;
    }
  } else {
    if (Status.HasExtern) {
      SemaRef.Diags.Report(S->getLoc(),
                clang::diag::err_cannot_applied_to_function_with_storage_class)
                            << "static";
      return;
    }
    if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx)) {
      FD->setStorageClass(clang::StorageClass::SC_Static);
    } else if(clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
      VD->setStorageClass(clang::StorageClass::SC_Static);
    }
  }
  Status.HasStatic = true;
}

void Elaborator::elaborateThreadLocalAttr(Declaration *D, const Syntax *S,
                                          AttrStatus &Status) {
  if (Status.HasThreadLocal) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_attribute_not_valid_as_call)
                        << "override";
    return;
  }
  if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
    Status.HasThreadLocal = true;
    VD->setTSCSpec(clang::TSCS_thread_local);
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_invalid_attribute_for_decl)
                       << "thread_local" << "variable declaration";
}

void Elaborator::elaborateExplicitAttr(Declaration *D, const Syntax *S,
                                       AttrStatus &Status) {
  if (Status.HasExplicit) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_attribute_not_valid_as_call)
                        << "override";
    return;
  }

  if (clang::CXXConstructorDecl *CTorDecl
                                = dyn_cast<clang::CXXConstructorDecl>(D->Cxx)) {
    clang::ExplicitSpecifier ES(nullptr,
                                clang::ExplicitSpecKind::ResolvedTrue);
    CTorDecl->setExplicitSpecifier(ES);

  } else if (isa<clang::CXXConversionDecl>(D->Cxx)) {
    llvm_unreachable("ConversionDecl not implemented yet.");
  } else {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_attribute_for_decl)
                         << "explicit" << "constructor or conversion operator";
    return;
  }
  Status.HasExplicit = true;
}

void Elaborator::elaborateVirtualAttr(Declaration *D, const Syntax *S,
                                      AttrStatus &Status) {
  if (Status.HasVirtual) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<AtomSyntax>(S)) {
    if (isa<clang::CXXConstructorDecl>(D->Cxx)) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << "virtual" << "member function";
      return;
    }
    if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(D->Cxx)) {
      if (MD->getStorageClass() != clang::SC_None) {
        SemaRef.Diags.Report(S->getLoc(),
                 clang::diag::err_cannot_applied_to_function_with_storage_class)
                             << "virtual";
        return;
      }
      if (MD->getReturnType() == Context.CxxAST.getAutoDeductType()) {
        SemaRef.Diags.Report(S->getLoc(), clang::diag::err_auto_fn_virtual);
        return;
      }
      MD->setVirtualAsWritten(true);
      Status.HasVirtual = true;
      return;
    } else {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << "virtual" << "member function";
      return;
    }
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_attribute_not_valid_as_call)
                       << "virtual";
}

void Elaborator::elaborateOverrideAttr(Declaration *D, const Syntax *S,
                                       AttrStatus &Status) {
  if (Status.HasOverride) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }

  if (!isa<AtomSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_attribute_not_valid_as_call)
                        << "overide";
    return;
  }
  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(D->Cxx)) {
    if (isa<clang::CXXConstructorDecl>(D->Cxx)
        || isa<clang::CXXDestructorDecl>(D->Cxx))
    {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << "override" << "member function";
      return;
    }

    // Adding override attribute.
    MD->addAttr(clang::OverrideAttr::Create(Context.CxxAST, S->getLoc(),
                                      clang::AttributeCommonInfo::AS_Keyword));
    Status.HasOverride = true;
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_invalid_attribute_for_decl)
                        << "override" << "member function";
}

void Elaborator::elaborateFinalAttr(Declaration *D, const Syntax *S,
                                    AttrStatus &Status) {
  if (Status.HasFinal) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_not_valid_as_call)
                         << "final";
    return;
  }
  Status.HasFinal = true;
  if (clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(D->Cxx)) {
    RD->addAttr(clang::FinalAttr::Create(Context.CxxAST, S->getLoc(),
                                         clang::AttributeCommonInfo::AS_Keyword,
                               static_cast<clang::FinalAttr::Spelling>(false)));
    return;
  }

  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(D->Cxx)) {
    MD->addAttr(clang::FinalAttr::Create(Context.CxxAST, S->getLoc(),
                                         clang::AttributeCommonInfo::AS_Keyword,
                               static_cast<clang::FinalAttr::Spelling>(false)));
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_invalid_attribute_for_decl)
                       << "final" << "virtual member function or class";
}

void Elaborator::elaborateConstAttr(Declaration *D, const Syntax *S,
                                    AttrStatus &Status) {
  if (Status.HasConst) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_not_valid_as_call)
                         << "const";
    return;
  }
  Status.HasConst = true;

  if (isa<clang::CXXMethodDecl>(D->Cxx) &&
      !(isa<clang::CXXConstructorDecl>(D->Cxx)
      || isa<clang::CXXDestructorDecl>(D->Cxx))) {
    clang::CXXMethodDecl *MD = cast<clang::CXXMethodDecl>(D->Cxx);
    if (MD->getStorageClass() == clang::SC_Static) {
      SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_invalid_attribute_for_decl)
                          << "const" << "member function";
      return;
    }
    clang::QualType MemberFuncTy = MD->getType();
    const clang::FunctionProtoType *FPT = MemberFuncTy
                                            ->getAs<clang::FunctionProtoType>();
    auto EPI = FPT->getExtProtoInfo();
    EPI.TypeQuals.addConst();
    SemaRef.rebuildFunctionType(MD, MD->getBeginLoc(), FPT, FPT->getExtInfo(),
                                EPI, FPT->getExceptionSpecInfo());
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                      clang::diag::err_invalid_attribute_for_decl)
                      << "const" << "member function";
}

void Elaborator::elaborateBitsAttr(Declaration *D, const Syntax *S,
                                   AttrStatus &Status) {
  if (Status.HasBits) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (!isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_only_valid_as_call)
                         << "bits";
    return;
  }
  if (!isa<clang::FieldDecl>(D->Cxx)) {
    SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_invalid_attribute_for_decl)
                          << "bits(expr)" << "class member variable";
    return;
  }
  const CallSyntax *BitsCall = cast<CallSyntax>(S);
  const ListSyntax *BitsArguments = cast<ListSyntax>(BitsCall->getArguments());

  if (BitsArguments->getNumChildren() == 0
      || BitsArguments->getNumChildren() > 1) {
    SemaRef.Diags.Report(BitsCall->getLoc(),
                         clang::diag::err_incorrect_number_of_arguments)
                         << "bits";
    return;
  }
  Status.HasBits = true;
  if (Status.HasAlignAs) {
    SemaRef.Diags.Report(BitsCall->getLoc(),
                         clang::diag::err_alignas_attribute_wrong_decl_type)
                         << "alignas" << 3;
    return;
  }
  ExprElaborator Elab(Context, SemaRef);
  clang::Expr *BitsExpr = Elab.elaborateExpectedConstantExpr(
                                                      BitsCall->getArgument(0));

  clang::FieldDecl *Field = cast<clang::FieldDecl>(D->Cxx);
  bool IsZeroWidth = false;
  auto ExprResult = SemaRef.getCxxSema().VerifyBitField(BitsCall->getLoc(),
                                                        Field->getDeclName(),
                                                        Field->getType(),
                                                        /*IsMsStruct=*/false,
                                                        BitsExpr,
                                                        &IsZeroWidth);
  if (ExprResult.isInvalid()) {
    return;
  }
  Field->setBitWidth(ExprResult.get());
}

void Elaborator::elaborateAlignAsAttr(Declaration *D, const Syntax *S,
                                      AttrStatus &Status) {
  if (Status.HasAlignAs) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }
  if (!isa<CallSyntax>(S)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_only_valid_as_call)
                         << "alignas";
    return;
  }

  const CallSyntax *AlignAsCall = cast<CallSyntax>(S);
  const ListSyntax *AlignAsArgs = cast<ListSyntax>(AlignAsCall->getArguments());

  if (AlignAsArgs->getNumChildren() == 0
      || AlignAsArgs->getNumChildren() > 1) {
    SemaRef.Diags.Report(AlignAsCall->getLoc(),
                         clang::diag::err_incorrect_number_of_arguments)
                         << "alignas";
    return;
  }

  if (isa<clang::VarDecl>(D->Cxx) || isa<clang::TagDecl>(D->Cxx)
      || isa<clang::FieldDecl>(D->Cxx)) {
    Status.HasAlignAs = true;
    ExprElaborator Elab(Context, SemaRef);
    clang::Expr *AlignmentExpr = Elab.elaborateExpectedConstantExpr(
                                                   AlignAsCall->getArgument(0));
    clang::QualType AlignmentExprTy = AlignmentExpr->getType();
    if (AlignmentExprTy->isNamespaceType()
        || AlignmentExprTy->isTemplateType()) {
      SemaRef.Diags.Report(AlignAsCall->getArgument(0)->getLoc(),
                           clang::diag::err_invalid_result_type);
      return;
    }
    clang::IdentifierInfo *AttrName = &Context.CxxAST.Idents.get({"alignas"});

    clang::AttributeCommonInfo::Syntax SyntaxKind
                               = clang::AttributeCommonInfo::Syntax::AS_Keyword;
    clang::SourceRange SR(AlignAsCall->getLoc(),
                          AlignAsCall->getArgument(0)->getLoc());
    clang::AttributeCommonInfo AttrInfo(AttrName, nullptr, SR,
                                        AlignAsCall->getLoc(),
                                   clang::AttributeCommonInfo::Kind::AT_Aligned,
                                        SyntaxKind);
    if (AlignmentExprTy->isTypeOfTypes()) {
      clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                        AlignmentExpr, AlignAsCall->getArgument(0)->getLoc());
      if (!TInfo)
        return;
      SemaRef.getCxxSema().AddAlignedAttr(D->Cxx, AttrInfo, TInfo,
                                          /*IsPackExpansion=*/false);
    } else {
      SemaRef.getCxxSema().AddAlignedAttr(D->Cxx, AttrInfo, AlignmentExpr,
                                          /*IsPackExpansion=*/false);
    }
    if (D->Cxx->hasAttrs())
      SemaRef.getCxxSema().CheckAlignasUnderalignment(D->Cxx);
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_invalid_attribute_for_decl)
                       << "alignas(expr)" << "class or variable declaration";
}

void Elaborator::elaborateRefQualifierAttr(Declaration *D, const Syntax *S,
                                           AttrStatus &Status) {
  if (Status.HasRefQualifier) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_duplicate_attribute);
    return;
  }

  const AtomSyntax *Name;
  if (const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    Name = cast<AtomSyntax>(Call->getCallee());
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_attribute_only_valid_as_call)
                         << Name->getSpelling();
    return;
  }

  Name = cast<AtomSyntax>(S);
  Status.HasRefQualifier = true;
  if (isa<clang::CXXMethodDecl>(D->Cxx) &&
     !(isa<clang::CXXConstructorDecl>(D->Cxx)
      || isa<clang::CXXDestructorDecl>(D->Cxx))) {

    clang::CXXMethodDecl *MD = cast<clang::CXXMethodDecl>(D->Cxx);
    if (MD->getStorageClass() == clang::SC_Static) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_attribute_for_decl)
                           << Name->getSpelling() << "method";
      return;
    }
    clang::QualType MemberFuncTy = MD->getType();
    const clang::FunctionProtoType *FPT = MemberFuncTy
                                            ->getAs<clang::FunctionProtoType>();
    auto EPI = FPT->getExtProtoInfo();
    if (Name->getToken().getKind() == tok::RefKeyword) {
      EPI.RefQualifier = clang::RQ_LValue;
    } else if (Name->getToken().getKind() == tok::RValueRefKeyword){
      EPI.RefQualifier = clang::RQ_RValue;
    } else {
      llvm_unreachable("Invalid reference qualifier given.");
    }
    SemaRef.rebuildFunctionType(MD, MD->getBeginLoc(), FPT, FPT->getExtInfo(),
                                EPI, FPT->getExceptionSpecInfo());
    return;
  }
  SemaRef.Diags.Report(S->getLoc(),
                      clang::diag::err_invalid_attribute_for_decl)
                      << "const" << "member function";
}

namespace {

enum SysAttrFormatKind {
  SAFK_Invalid,
  SAFK_Name,
  SAFK_NameCall,
  SAFK_ScopeName,
  SAFK_ScopeNameCall
};
/// This stores attribute information for a single attribute.
struct SysAttrInfo {
  SysAttrFormatKind Kind = SAFK_Invalid;
  const AtomSyntax *ScopeName = nullptr;
  const AtomSyntax *AttrId = nullptr;
  const CallSyntax *CallNode = nullptr;
  const ListSyntax *Args = nullptr;
};

void getSysAttrInfo(const Syntax *Attr, SysAttrInfo &Info) {
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Attr)) {
    Info.Kind = SAFK_Name;
    Info.AttrId = Atom;
    return;
  } else if (const CallSyntax *Call = dyn_cast<CallSyntax>(Attr)) {
    if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Call->getCallee())) {
      if (Atom->getSpelling() == "operator'.'") {
        if (const AtomSyntax *Name
                                 = dyn_cast<AtomSyntax>(Call->getArgument(1))) {
          Info.AttrId = Name;
        } else {
          Info.Kind = SAFK_Invalid;
          return;
        }
        if ((Info.ScopeName = dyn_cast<AtomSyntax>(Call->getArgument(0)))) {
          Info.Kind = SAFK_ScopeName;
        } else {
          Info.Kind = SAFK_Invalid;
        }
        return;
      }
      Info.Kind = SAFK_NameCall;
      Info.AttrId = Atom;
      Info.CallNode = Call;
      Info.Args = cast<ListSyntax>(Call->getArguments());
      return;
    }
    // Checking for nested name in combination with something.
    if (const CallSyntax *InnerCall = dyn_cast<CallSyntax>(Call->getCallee())) {
      if (const AtomSyntax *Dot
                               = dyn_cast<AtomSyntax>(InnerCall->getCallee())) {
        if (Dot->getSpelling() == "operator'.'") {
          if (const AtomSyntax *Name
                            = dyn_cast<AtomSyntax>(InnerCall->getArgument(1))) {
            Info.AttrId = Name;
          } else {
            Info.Kind = SAFK_Invalid;
            return;
          }
          if ((Info.ScopeName
                           = dyn_cast<AtomSyntax>(InnerCall->getArgument(0)))) {
            Info.Kind = SAFK_ScopeNameCall;
          } else {
            Info.Kind = SAFK_Invalid;
            return;
          }
          Info.Args = cast<ListSyntax>(Call->getArguments());
          return;
        }
      }
    }
  }
  Info.Kind = SysAttrFormatKind::SAFK_Invalid;
}
}
static bool processAttributeArgs(SyntaxContext &Context, Sema &SemaRef,
                                 const Syntax *CallArgs,
                                 clang::ArgsVector &Args) {
  ExprElaborator Elab(Context, SemaRef);
  for (const Syntax *ArgOrId : CallArgs->children()) {
    if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(ArgOrId)) {
      if (Name->hasToken(tok::Identifier)) {
        Args.emplace_back(
          clang::IdentifierLoc::create(Context.CxxAST, Name->getLoc(),
                                       &Context.CxxAST.Idents.get(
                                                         Name->getSpelling())));
        continue;
      }
    }

    // Just going a head and assuming that we expect an expression here.
    clang::Expr *Res = Elab.elaborateExpr(ArgOrId);
    if (!Res)
      return true;
    if (Res->getType()->isTypeOfTypes()
        || Res->getType()->isNamespaceType()
        || Res->getType()->isTemplateType()) {
      SemaRef.Diags.Report(ArgOrId->getLoc(),
                           clang::diag::err_invalid_attribute_argument);
      return true;
    }
    Args.emplace_back(Res);
  }
  return false;
}
void Elaborator::elaborateSystemAttribute(clang::Decl *D, const Syntax *S,
                                          AttrStatus &Status,
                                          clang::ParsedAttributes &Attrs) {
  SysAttrInfo Info;
  getSysAttrInfo(S, Info);
  switch(Info.Kind) {
  case SAFK_Invalid: {
    // FIXME: I'm not sure how to handle this properly,
    // I might have to create some kind of warning here or something?
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::warn_invalid_attribute_format);
    return;
  }

  case SAFK_Name: {
    Attrs.addNew(&Context.CxxAST.Idents.get(Info.AttrId->getSpelling()),
                 clang::SourceRange(Info.AttrId->getLoc(),
                                    Info.AttrId->getLoc()),
                 nullptr, clang::SourceLocation(), nullptr, 0u,
                 clang::ParsedAttr::Syntax::AS_CXX11);

    break;
  }

  case SAFK_NameCall: {
    clang::ArgsVector Args;
    if (processAttributeArgs(Context, SemaRef, Info.Args, Args)) {
      return;
    }
    // Handling simple call style attributes.
    Attrs.addNew(&Context.CxxAST.Idents.get(Info.AttrId->getSpelling()),
                 clang::SourceRange(Info.AttrId->getLoc(),
                                    Info.AttrId->getLoc()),
                 nullptr, clang::SourceLocation(), Args.data(),
                 Args.size(), clang::ParsedAttr::Syntax::AS_CXX11);

    break;
  }

  case SAFK_ScopeName: {
    Attrs.addNew(&Context.CxxAST.Idents.get(Info.AttrId->getSpelling()),
                 clang::SourceRange(Info.AttrId->getLoc(),
                                    Info.AttrId->getLoc()),
                 &Context.CxxAST.Idents.get(Info.ScopeName->getSpelling()),
                 Info.ScopeName->getLoc(), nullptr, 0u,
                 clang::ParsedAttr::Syntax::AS_CXX11);

    break;
  }

  case SAFK_ScopeNameCall: {
    clang::ArgsVector Args;
    if (processAttributeArgs(Context, SemaRef, Info.Args, Args)) {
      return;
    }
    Attrs.addNew(&Context.CxxAST.Idents.get(Info.AttrId->getSpelling()),
                 clang::SourceRange(Info.AttrId->getLoc(),
                                    Info.AttrId->getLoc()),
                 &Context.CxxAST.Idents.get(Info.ScopeName->getSpelling()),
                 Info.ScopeName->getLoc(), Args.data(),
                 Args.size(), clang::ParsedAttr::Syntax::AS_CXX11);

    break;
  }
  }
}

void Elaborator::elaborateAttributeError(Declaration *D, const Syntax *S,
                                         AttrStatus &Status) {
  llvm::StringRef AttrName;
  checkAttrFormatAndName(S, AttrName);
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_invalid_attribute_for_decl)
                       << AttrName << "different declaration expression";
}



FusedOpKind getFusedOpKind(Sema &SemaRef, llvm::StringRef Spelling) {
  const clang::IdentifierInfo *Tokenization =
    &SemaRef.Context.CxxAST.Idents.get(Spelling);

  if (Tokenization == SemaRef.OperatorColonII)
    return FOK_Colon;
  if (Tokenization == SemaRef.OperatorArrowII)
    return FOK_Arrow;
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
  if (Tokenization == SemaRef.OperatorWhileII)
    return FOK_While;
  if (Tokenization == SemaRef.OperatorInII)
    return FOK_In;
  if (Tokenization == SemaRef.OperatorDotDotII)
    return FOK_DotDot;
  if (Tokenization == SemaRef.OperatorConstII)
    return FOK_Const;
  if (Tokenization == SemaRef.OperatorRefII)
    return FOK_Ref;
  if (Tokenization == SemaRef.OperatorRRefII)
    return FOK_RRef;
  if (Tokenization == SemaRef.OperatorBracketsII)
    return FOK_Brackets;
  if (Tokenization == SemaRef.OperatorParensII)
    return FOK_Parens;
  return FOK_Unknown;
}

FusedOpKind getFusedOpKind(Sema &SemaRef, const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->getCallee()));
  return getFusedOpKind(SemaRef, cast<AtomSyntax>(S->getCallee())->getSpelling());
}

} // namespace gold
