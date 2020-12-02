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
#include "clang/Basic/TargetInfo.h"

#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Gold/ClangToGoldDeclBuilder.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldDeclarationBuilder.h"


namespace gold {

static void applyESIToFunctionType(SyntaxContext &Context, Sema &SemaRef,
                                   clang::FunctionDecl *FD,
                       const clang::FunctionProtoType::ExceptionSpecInfo &ESI);

AttrFormat checkAttrFormatAndName(const Syntax *Attr, std::string &Name) {
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


static bool computeAccessSpecifier(Sema &SemaRef, Attributes &attrs,
                                   clang::AccessSpecifier &AS) {
  AS = clang::AS_public;
  return locateValidAttribute(attrs,
    // OnAttr
    [&](const Syntax *Attr) -> bool {
      std::string ActualName;
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
      std::string ActualName;
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
      std::string ActualName;
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
      std::string ActualName;
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
      std::string ActualName;
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
static inline bool isMutable(Sema& SemaRef, Declaration *D, bool &IsMutable) {
  IsMutable = false;
  return locateValidAttribute(D,
    // OnAttr
    [&](const Syntax *Attr) -> bool{
      std::string ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Name:
        IsMutable = ActualName == "mutable";
        return IsMutable;
      case AF_Invalid:
        return false;
      case AF_Call:
        if (ActualName == "mutable") {
          SemaRef.Diags.Report(Attr->getLoc(),
                               clang::diag::err_attribute_not_valid_as_call)
                               << ActualName;
          IsMutable = true;
          return IsMutable;
        }

        return false;
      }
    },
    // CheckAttr
    [](const Syntax *Attr) -> bool{
      std::string ActualName;
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
  if (!S)
    return nullptr;
  assert(isa<FileSyntax>(S) && "S is not a file");

  startFile(S);
  SemaRef.createInPlaceNew();
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
  SemaRef.setDeclForDeclaration(D, Context.CxxAST.getTranslationUnitDecl());
  SemaRef.setGlobalClangScope(Scope);
  SemaRef.setTranslationUnit(D);
  SemaRef.setTranslationUnitScope(D->SavedScope);

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
                          clang::MultiTemplateParamsArg &MTP) {
  SpecializationDeclarator *SD = D->SpecializationArgs;
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
    D->Init->getLoc(), /*ModulePrivLoc=*/SourceLocation(), D->ScopeSpec,
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
  ScopeKind SK = SK_Class;

  switch(D->getKind()) {
  case UDK_Class:
    TST = clang::DeclSpec::TST_struct;
    break;
  case UDK_Union:
    TST = clang::DeclSpec::TST_union;
    break;
  case UDK_Enum:
    TST = clang::DeclSpec::TST_enum;
    ScopeEnumUsesClassTag = true;
    if (const MacroSyntax *MS = dyn_cast<MacroSyntax>(D->Init)) {
      UnderlyingType = getUnderlyingEnumType(Context, SemaRef, MS->getCall());
    } else {
      llvm_unreachable("Invalid tree syntax.");
    }
    ScopedEnumClassKW = D->IdDcl->getLoc();
    SK = SK_Enum;
    break;
  default:
    llvm_unreachable("Incorrectly identified tag type");
  }

  Decl *Declaration = nullptr;
  if (D->SpecializationArgs) {
    Declaration = handleClassSpecialization(Context, SemaRef, D, TST, MTP);
  } else {
    Declaration = SemaRef.getCxxSema().ActOnTag(
      SemaRef.getCurClangScope(), TST, /*Metafunction=*/nullptr,
      clang::Sema::TUK_Definition, D->Init->getLoc(), D->ScopeSpec, D->getId(), IdLoc,
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
    // First we need to save the declaration for later because we need to be able
    // specifically locate the declaration at a later time
    SemaRef.setDeclForDeclaration(D, TempTemplateDecl);
    Tag = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  } else if (isa<EnumDecl>(Declaration)) {
    Tag = cast<TagDecl>(Declaration);
  }

  SemaRef.setDeclForDeclaration(D, Tag);
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
  switch(D->getKind()) {
  case UDK_Class:
    TST = clang::DeclSpec::TST_struct;
    break;
  case UDK_Union:
    TST = clang::DeclSpec::TST_union;
    break;
  case UDK_Enum:
    // This is here because it's used specifically to generate an error from
    // Sema::ActOnTag
    TST = clang::DeclSpec::TST_enum;
    ScopeEnumUsesClassTag = true;
    if (const CallSyntax *EnumCall = dyn_cast<CallSyntax>(D->Init)) {
      UnderlyingType = getUnderlyingEnumType(Context, SemaRef, EnumCall);
    }
    ScopedEnumClassKW = D->IdDcl->getLoc();
    break;
  default:
    llvm_unreachable("Incorrectly identified tag type");
  }

  Decl *Declaration = SemaRef.getCxxSema().ActOnTag(SemaRef.getCurClangScope(),
      TST, /*Metafunction=*/nullptr,
      clang::Sema::TUK_Declaration, D->Init->getLoc(), D->ScopeSpec, D->getId(),
      IdLoc, clang::ParsedAttributesView(), /*AccessSpecifier=*/AS,
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
    // First we need to save the declaration for later because we need to be able
    // to locate this later.
    SemaRef.setDeclForDeclaration(D, TempTemplateDecl);
    ClsDecl = cast<CXXRecordDecl>(TempTemplateDecl->getTemplatedDecl());
  }
  SemaRef.setDeclForDeclaration(D, ClsDecl);
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
                                          D->IdDcl->getLoc(),
                                          D->IdDcl->getLoc(),
                                          D->getId(),
                                          D->IdDcl->getLoc(),
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
  if (!NSDecl->Rep) {
    NewScope.Init(SK_Namespace, D->Init, &NSDecl->Rep);
    NSDecl->Rep = SemaRef.getCurrentScope();
  } else {
    ResumedScope.Init(NSDecl->Rep, NSDecl->Rep->Term, false);
  }
  SemaRef.setDeclForDeclaration(D, NSDecl);
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
                                     Declaration *D,
                                     TemplateParamsDeclarator *TPD) {
  clang::TemplateParameterList *ParamList = nullptr;

  // Initializing the scopes we have to deal with.
  ScopeToInit.Init(SK_Template, TPD->getSyntax(), TPD->getScopePtrPtr());
  ClangScope.Init(clang::Scope::TemplateParamScope, TPD->getLoc());

  // Constructing actual parameters.
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  if (!TPD->isImplicitlyEmpty()) {
    Elaborator El(SemaRef.getContext(), SemaRef);
    El.buildTemplateParams(TPD->getSyntax(), TemplateParamDecls);
  }

  ParamList = SemaRef.getCxxSema().ActOnTemplateParameterList(
                               /*unsigned Depth*/SemaRef.computeTemplateDepth(),
                                           /*ExportLoc*/clang::SourceLocation(),
                                                   /*TemplateLoc*/TPD->getLoc(),
                                                     /*LAngleLoc*/TPD->getLoc(),
                                                             TemplateParamDecls,
                                                     /*RAngleLoc*/TPD->getLoc(),
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

static clang::TemplateParameterList *
buildNNSTemplateParam(Sema &SemaRef, Declaration *D,
                      TemplateParamsDeclarator *TemplateDcl) {
  assert(TemplateDcl && "Invalid template declarator.");
  // We may need to exit the clang scope? instead of saving it for later.
  SemaRef.enterClangScope(clang::Scope::TemplateParamScope);
  SemaRef.enterScope(SK_Template, TemplateDcl->getSyntax());
  TemplateDcl->setScope(SemaRef.getCurrentScope());
  // Constructing actual parameters.
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  if (!TemplateDcl->isImplicitlyEmpty()) {
    Elaborator El(SemaRef.getContext(), SemaRef);
    El.buildTemplateParams(TemplateDcl->getSyntax(), TemplateParamDecls);
  }

  auto ParamList = SemaRef.getCxxSema().ActOnTemplateParameterList(
                               /*unsigned Depth*/SemaRef.computeTemplateDepth(),
                                           /*ExportLoc*/clang::SourceLocation(),
                                           /*TemplateLoc*/TemplateDcl->getLoc(),
                                             /*LAngleLoc*/TemplateDcl->getLoc(),
                                                             TemplateParamDecls,
                                             /*RAngleLoc*/TemplateDcl->getLoc(),
                                                     /*RequiresClause*/nullptr);
  TemplateDcl->setTemplateParameterList(ParamList);
  // Recording template parameters for use during declaration construction.
  D->TemplateParamStorage.push_back(ParamList);
  return ParamList;
}

static bool elaborateSpecializationArgs(Sema &SemaRef,
                                        Declaration *D) {
  SpecializationDeclarator *SD = D->SpecializationArgs->getAsSpecialization();
  assert(!SD->ElaboratedArgs && "specialization arguments already elaborated");

  SD->ElaboratedArgs = true;
  if (handleSpecializationArgs(SemaRef, SD->getArgs(), SD->getArgList())) {
    SD->setDidError();
  }
  return SD->getDidError();
}

// used to generate list syntax for our elemenb.
static Syntax **createArray(const SyntaxContext &Ctx,
                            const llvm::SmallVectorImpl<Syntax *> &Vec) {
  Syntax **Array = new (Ctx) Syntax *[Vec.size()];
  std::copy(Vec.begin(), Vec.end(), Array);
  return Array;
}

// Creating a fake token operator.
static AtomSyntax *RebuildAtom(const SyntaxContext &Ctx,
                               const AtomSyntax *Name) {
  return new (Ctx) AtomSyntax(Name->getToken());
}

static const ListSyntax *buildImplicitTemplateElemSyntax(Sema &SemaRef,
                                                     NNSDeclaratorInfo &DInfo) {
  assert(DInfo.Template && "Missing template arguments");
  SyntaxContext &Ctx = SemaRef.getContext();
  llvm::SmallVector<Syntax *, 16> Args;
  gold::Scope *TmpltScope = DInfo.Template->getScope();
  for (const Syntax *ParamToArg : DInfo.Template->getParams()->children()) {
    Declaration *D = TmpltScope->findDecl(ParamToArg);
    if (!D) {
      // FIXME: In the event we have a failed template parameter declaration
      // then we need to emit the proper error message and return a nullptr,
      llvm_unreachable("Fix invalid template parameter declaration.");
    }
    // Reusing the existing name in order to recreate implicit arguments.
    Args.emplace_back(RebuildAtom(Ctx, D->IdDcl->getIdentifier()));
  }
  return new (Ctx) ListSyntax(createArray(Ctx, Args), Args.size());
}



static bool handleNestedName(Sema &SemaRef, Declaration *D,
                             NNSDeclaratorInfo &DInfo) {

  SyntaxContext &Context = SemaRef.getContext();
  ExprElaborator ExprElab(Context, SemaRef);
  clang::Expr *NestedName = nullptr;
  // Attempting to elaborate a given expression context
  NestedName = ExprElab.elaborateExpectedConstantExpr(
                                                   DInfo.Name->getNestedName());
  if (!NestedName) {
    // FIXME: I need to see if this works without having an error message here.
    return true;
  }

  // Handling template parameters.
  clang::TemplateParameterList *TemplateParams = nullptr;
  if (DInfo.Template) {
    Sema::NewNameSpecifierRAII NestedNameStack(SemaRef);
    // Enter a new template scope.
    TemplateParams = buildNNSTemplateParam(SemaRef, D, DInfo.Template);
    // Something went wrong here and we should try and exit?
    if (!TemplateParams)
      return true;
  }

  // Suspend the current qualified lookup because some of the template parameters
  // may have a references to things that are within a different namespace.
  bool EnteringContext = SemaRef.isQualifiedLookupContext();
  clang::QualType ResultTy = NestedName->getType();
  if (ResultTy->isTypeOfTypes()) {
    if (DInfo.Template || DInfo.SpecializationArgs) {
      SemaRef.Diags.Report(DInfo.Name->getLoc(), clang::diag::err_no_template)
                          << DInfo.Name->getNestedName()->getSpelling();
      return true;
    }
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(NestedName,
                                                          DInfo.Name->getLoc());
    if (!TInfo)
      return true;

    clang::QualType Ty(TInfo->getType());
    clang::IdentifierInfo *II = &Context.CxxAST.Idents.get({
                                   DInfo.Name->getNestedName()->getSpelling()});
    clang::SourceLocation BeginLoc = DInfo.Name->getLoc();
    clang::SourceLocation CCLoc = DInfo.Name->getLoc();
    if (const auto *TypeDef = dyn_cast<clang::TypedefType>(&*Ty)) {
      // Resolving typedef to actual type.
      clang::TypedefNameDecl* TND = TypeDef->getDecl();
      Ty = TND->getUnderlyingType();
    }

    clang::QualType Qt;
    clang::Sema::NestedNameSpecInfo IdInfo(II,BeginLoc, CCLoc, Qt);

    if(SemaRef.getCxxSema().
      ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                  EnteringContext, SemaRef.CurNNSContext,
                                  /*RecoveryLookup=*/false,
                                  /*IsCorrected=*/nullptr,
                                  /*OnlyNamespace=*/false))
      return true;

    clang::CXXRecordDecl *RD = Ty->getAsCXXRecordDecl();
    if (!RD) {
      SemaRef.Diags.Report(DInfo.Name->getLoc(),
                           clang::diag::err_invalid_decl_spec_combination)
                          << DInfo.Name->getNestedName()->getSpelling();
      return true;
    }

    Sema::DeclaratorScopeObj DclScope(SemaRef, SemaRef.CurNNSContext,
                                      D->IdDcl->getLoc());
    if (SemaRef.CurNNSContext.isValid() && SemaRef.CurNNSContext.isSet()) {
      if (SemaRef.getCxxSema().ShouldEnterDeclaratorScope(
                             SemaRef.getCurClangScope(), SemaRef.CurNNSContext))
        DclScope.enterDeclaratorScope();
    }
    // Moving to a new declaration context to continue.
    if (SemaRef.setLookupScope(RD)) {
      // FIXME: this should never fail... I think.
      llvm_unreachable("We failed to duplicate the new scope.");
    }
    DInfo.Name->setScope(SemaRef.getLookupScope());
    return false;


  } else if (ResultTy->isCppxNamespaceType()) {
    if (DInfo.Template || DInfo.SpecializationArgs) {
      SemaRef.Diags.Report(DInfo.Name->getLoc(), clang::diag::err_no_template)
                          << DInfo.Name->getNestedName()->getSpelling();
      return true;
    }
    clang::Decl *TempNs = SemaRef.getDeclFromExpr(NestedName,
                                                  DInfo.Name->getLoc());
    clang::CppxNamespaceDecl *NS;
    // Setting the namespace alias for lookup.
    if (auto *CppxNs = dyn_cast<clang::CppxNamespaceDecl>(TempNs)) {
      SemaRef.setLookupScope(CppxNs);
      NS = CppxNs;
    } else if (auto *Alias = dyn_cast<clang::NamespaceAliasDecl>(TempNs)) {
      // Switching to the aliased namespace.
      SemaRef.setLookupScope(NS = cast<clang::CppxNamespaceDecl>(
                                                 Alias->getAliasedNamespace()));
    } else {
      TempNs->dump();
      llvm_unreachable("We hvae a new type of namespace specifier that we've "
                       "never seen before.");
    }

    clang::Sema::NestedNameSpecInfo IdInfo(NS->getIdentifier(),
                                           NS->getBeginLoc(),
                                           DInfo.Name->getLoc(),
                                           clang::QualType());
    if(SemaRef.getCxxSema().
      ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                  EnteringContext, SemaRef.CurNNSContext,
                                  /*RecoveryLookup=*/false,
                                  /*IsCorrected=*/nullptr,
                                  /*OnlyNamespace=*/false))
      return true;
    Sema::DeclaratorScopeObj DclScope(SemaRef, SemaRef.CurNNSContext,
                                      D->IdDcl->getLoc());
    if (SemaRef.CurNNSContext.isValid() && SemaRef.CurNNSContext.isSet())
      if (SemaRef.getCxxSema().ShouldEnterDeclaratorScope(
                           SemaRef.getCurClangScope(), SemaRef.CurNNSContext))
        DclScope.enterDeclaratorScope();

    SemaRef.pushScope(SemaRef.getLookupScope());
    DInfo.Name->setScope(SemaRef.getLookupScope());
    return false;
  } else if (ResultTy->isTemplateType()) {
    clang::Decl *TmpltDecl = SemaRef.getDeclFromExpr(NestedName,
                                                     DInfo.Name->getLoc());
    clang::TemplateDecl *TemplateDeclaration
                                     = dyn_cast<clang::TemplateDecl>(TmpltDecl);
    clang::TemplateName TmpltName(TemplateDeclaration);
    clang::Sema::TemplateTy TmpltTy = clang::Sema::TemplateTy::make(TmpltName);

    const ListSyntax *ArgsList = nullptr;

    // This is for when we have a specialization for our template parameters
    // we don't handle this yet.
    if (DInfo.SpecializationArgs) {
      ArgsList = DInfo.SpecializationArgs->getArgs();
    } else {
      // This is the special case where the template parameter are directly
      // translated into template arguments implicitly.
      // DInfo.Template->getTemplateParameterList();
      ArgsList = buildImplicitTemplateElemSyntax(SemaRef, DInfo);
    }
    clang::TemplateArgumentListInfo ArgInfo;
    llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArgs;
    if (!ArgsList) {
      // FIXME: this needs an error message.
      llvm_unreachable("Unable to create specialization for NNS.");
    }

    clang::ASTTemplateArgsPtr TemplateArgs;
    {
      // We need to temporarily ditch lookup stack so we don't
      // include things that are not in scope.
      Sema::NewNameSpecifierRAII NestedNameStack(SemaRef);
      ExprElaborator ExprElab(SemaRef.getContext(), SemaRef);
      if (ExprElab.elaborateTemplateArugments(ArgsList, ArgInfo, ParsedArgs)) {
        llvm_unreachable("This needs an error message.");
        // return true;
      }
      TemplateArgs = ParsedArgs;
    }

    // Filling template argument.
    if (SemaRef.getCxxSema().ActOnCXXNestedNameSpecifier(
                                                     SemaRef.getCurClangScope(),
                                                         SemaRef.CurNNSContext,
                                          /*TemplateKWLoc*/DInfo.Name->getLoc(),
                                                         TmpltTy,
                                        /*TemplateNameLoc*/DInfo.Name->getLoc(),
                                          /*LAngleLoc*/DInfo.Template->getLoc(),
                                                         TemplateArgs,
                                          /*RAngleLoc*/DInfo.Template->getLoc(),
                                              /*CCLoc*/DInfo.Template->getLoc(),
                                                         EnteringContext))
      return true;



    Sema::DeclaratorScopeObj DclScope(SemaRef,
                                     SemaRef.CurNNSContext, D->IdDcl->getLoc());
    if (SemaRef.CurNNSContext.isValid() && SemaRef.CurNNSContext.isSet()) {
      if (SemaRef.getCxxSema().ShouldEnterDeclaratorScope(
                              SemaRef.getCurClangScope(), SemaRef.CurNNSContext))
        DclScope.enterDeclaratorScope();
    }
    if (SemaRef.CurNNSContext.isSet()) {

      // Computing the current context based on the current scope spec.
      clang::DeclContext *NewDC = SemaRef.getCxxSema().computeDeclContext(
                                                    SemaRef.CurNNSContext, true);

      // FIXME: It may be possible for this to be a type alias, or a template
      // aliase in which case we may need to change things slightly to check
      // for that
      if (auto *RD = dyn_cast<clang::CXXRecordDecl>(NewDC)) {
        if (SemaRef.setLookupScope(RD)) {
          // FIXME: this should never fail... I think.
          llvm_unreachable("We failed to duplicate the new scope.");
        }

      } else {
        // FIXME: This needs a valid error message.
        llvm_unreachable("Invalid kind of template.");
      }
      DInfo.Name->setScope(SemaRef.getLookupScope());
      return false;
    }
    return true;
  }

  SemaRef.Diags.Report(DInfo.Name->getLoc(),
                       clang::diag::err_expected_class_or_namespace)
                       << DInfo.Name->getNestedName()->getSpelling() << 0;
  return true;
}

bool Elaborator::elaborateNestedNameForDecl(Declaration *D) {
  Sema::OptionalInitScope<Sema::QualifiedLookupRAII> GlobalNNS(SemaRef);
  if (D->GlobalNsSpecifier) {
    if (SemaRef.getCxxSema().ActOnCXXGlobalScopeSpecifier(
                         D->GlobalNsSpecifier->getLoc(), SemaRef.CurNNSContext))
      return true;

    // Gathering global namespace into our current scope.
    gold::Scope *GlobalScope = SemaRef.getCurrentScope();
    while (GlobalScope->getParent())
      GlobalScope = GlobalScope->getParent();

    // Initialzing the global namespace tracking for lookup.
    GlobalNNS.Init(SemaRef.QualifiedLookupContext, GlobalScope,
                   Context.CxxAST.getTranslationUnitDecl());
  }

  // For each nested name specifier
  for(auto &NNS : D->NNSInfo)
    if (handleNestedName(SemaRef, D, NNS))
      return true;

  // Copying the constructed scope into it's correct location.
  D->ScopeSpec = SemaRef.CurNNSContext;
  return false;
}


/// Returns true in the event of an error. If located The Located node is set.
static bool hasLinkageSpecDecl(Sema& SemaRef, Declaration *D,
                               const Syntax **LocatedNode) {
  if (!D->IdDcl)
    return false;
  if (!D->IdDcl->UnprocessedAttributes)
    return false;
  return locateValidAttribute(*D->IdDcl->UnprocessedAttributes,

    // OnAttr
    [&](const Syntax *Attr) -> bool {
      std::string ActualName;
      switch(checkAttrFormatAndName(Attr, ActualName)) {
      case AF_Call:
        if (ActualName == "extern") {
          *LocatedNode = Attr;
          return true;
        }
        return false;
      default:
        return false;
      }
    },

    // CheckAttr
    [](const Syntax *Attr) -> bool {
      std::string ActualName;
      checkAttrFormatAndName(Attr, ActualName);
      return ActualName == "extern" || ActualName == "static";
    },

    // OnDup
    [&](const Syntax *FirstAttr, const Syntax *DuplicateAttr) {
      std::string FirstAttrName;
      checkAttrFormatAndName(FirstAttr, FirstAttrName);
      std::string DupAttrName;
      checkAttrFormatAndName(DuplicateAttr, DupAttrName);
      SemaRef.Diags.Report(DuplicateAttr->getLoc(),
                           clang::diag::err_duplicate_access_specifier)
                           << FirstAttrName << DupAttrName;
    });
}

static bool enterLinkageLanguageSpec(Sema &SemaRef,
                                     Declaration *D,
                                     const Syntax *S) {
  assert(S && "Invalid syntax node");
  SemaRef.enterClangScope(clang::Scope::DeclScope);
  SyntaxContext &Context = SemaRef.getContext();
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    if (Call->getNumArguments() != 1) {
      SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_incorrect_number_of_arguments)
                          << "extern";
      return true;
    }
    ExprElaborator Elab(Context, SemaRef);
    clang::Expr *Expr = Elab.elaborateConstexprAttrExpr(Call->getArgument(0));
    if (!Expr) {
      SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_failed_to_translate_expr);
      SemaRef.leaveClangScope(S->getLoc());
      return true;
    }

    clang::Decl *LinkageDecl = SemaRef.getCxxSema().ActOnStartLinkageSpecification(
        SemaRef.getCurClangScope(), Call->getCallee()->getLoc(), Expr,
        clang::SourceLocation()
      );

    // Assuming that we already got an error message for this.
    if (!LinkageDecl){
      SemaRef.leaveClangScope(S->getLoc());
      return true;
    }

    // We do this because we are technically chaning decl contexts.
    D->DeclaringContext = cast<clang::DeclContext>(LinkageDecl);
    ClangToGoldDeclRebuilder rebuilder(Context, SemaRef);
    Declaration *GDecl = rebuilder.generateDeclForDeclContext(
                                                        D->DeclaringContext, S);
    if (!GDecl)
      // Any error reporting will be handled the the rebuilder.
      return true;
    // Changing owner ship to the new decl context.
    D->ParentDecl = GDecl;
    D->Cxt = GDecl;
    SemaRef.pushDecl(GDecl);
  } else {
    llvm_unreachable("Invalid tree format");
  }
  return false;
}

static bool exitLinkageLanguageSpec(Sema &SemaRef,
                                    Declaration *D) {
  clang::Decl *CompleteDecl
      = SemaRef.getCxxSema().ActOnFinishLinkageSpecification(
        SemaRef.getCurClangScope(), cast<clang::Decl>(D->DeclaringContext),
        clang::SourceLocation());

  // Assuming that if we encounter an error that this will return nullptr.
  if (!CompleteDecl)
    return true;

  // Leaving DeclScope
  SemaRef.leaveClangScope(D->Op->getLoc());
  SemaRef.popDecl();
  SemaRef.verifyMatchingDeclarationAndDeclContext();
  return false;
}


static void exitToCorrectScope(Sema &SemaRef, gold::Scope *ExpectedScope,
                               Declaration *D, const Syntax *LinkageAttr) {
  // We need to exit any previous template scopes we entered as part of the nested
  // name specifier. Make sure to leave in revese order otherwise we won't have
  // a valid terms during exit.
  for (auto RIter = D->NNSInfo.rbegin(); RIter != D->NNSInfo.rend(); ++RIter) {
    // Trying to restore the current clang scope spec.
    if (RIter->Name->didEnterScope()) {
      if (RIter->Name->getScopeSpec().isSet()) {
        SemaRef.getCxxSema().ActOnCXXExitDeclaratorScope(
                        SemaRef.getCurClangScope(), RIter->Name->getScopeSpec());
      }
    }
    if (RIter->Name->getScope())
      SemaRef.leaveScope(RIter->Name->getScope()->getConcreteTerm());

    if (RIter->Template && RIter->Template->getScope())
      SemaRef.leaveScope(RIter->Template->getScope()->getConcreteTerm());
  }
  assert(ExpectedScope == SemaRef.getCurrentScope() && "Scope imbalance.");
  if (LinkageAttr) {
    exitLinkageLanguageSpec(SemaRef, D);
  }
}

clang::Decl *handleUsing(SyntaxContext &Ctx, Sema &SemaRef,
                         const Syntax *Arg, clang::SourceLocation UsingLoc) {
  Sema::ExtendQualifiedLookupRAII ExQual(SemaRef);
  clang::SourceLocation ArgLoc = Arg->getLoc();
  clang::Expr *E = ExprElaborator(Ctx, SemaRef).elaborateExpr(Arg);
  if (!E)
    return nullptr;

  clang::Scope *CxxScope = SemaRef.getCurClangScope();
  clang::CXXScopeSpec SS;
  clang::ParsedAttributesView AttrView;
  clang::UnqualifiedId Name;
  clang::AccessSpecifier AS = SemaRef.scopeIsWithinClass() ?
    clang::AS_public : clang::AS_none;

  if (clang::CppxDeclRefExpr *CDRE = dyn_cast<clang::CppxDeclRefExpr>(E)) {
    // using namespace declaration
    if (CDRE->getType()->isNamespaceType()) {
      if (SemaRef.scopeIsWithinClass()) {
        SemaRef.Diags.Report(Arg->getLoc(),
                             clang::diag::err_using_namespace_in_class);
        return nullptr;
      }

      clang::CppxNamespaceDecl *NS =
        cast<clang::CppxNamespaceDecl>(CDRE->getValue());

      clang::Decl *UD = SemaRef.getCxxSema().ActOnUsingDirective(
        CxxScope, UsingLoc, Arg->getLoc(), SS, Arg->getLoc(),
        NS->getIdentifier(), AttrView);
      if (!UD)
        return nullptr;

      SemaRef.getCurrentScope()->UsingDirectives.insert(
        cast<clang::UsingDirectiveDecl>(UD));
      return UD;
    }
  } else if (clang::DeclRefExpr *DRE = dyn_cast<clang::DeclRefExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    gold::Declaration *D = SemaRef.getDeclaration(DRE->getDecl());
    Name.setIdentifier(D->getId(), D->getEndOfDecl());
    Name.StartLocation = Name.EndLocation = Arg->getLoc();
  } else if (auto *ULE = dyn_cast<clang::UnresolvedLookupExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    Name.setIdentifier(ULE->getName().getAsIdentifierInfo(),
                       ULE->getNameLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLoc();
  } else if (auto *UME = dyn_cast<clang::UnresolvedMemberExpr>(E)) {
    // using directive of a declaration in a namespace or base class.
    Name.setIdentifier(UME->getName().getAsIdentifierInfo(),
                       UME->getNameLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLoc();
  } else if (auto *TyLit = dyn_cast<clang::CppxTypeLiteral>(E)) {
    clang::TypeSourceInfo *TInfo = TyLit->getValue();
    if (!TInfo)
      return nullptr;
    Name.setIdentifier(TInfo->getType().getBaseTypeIdentifier(),
                       TyLit->getExprLoc());
    Name.StartLocation = Name.EndLocation = Arg->getLoc();
  } else {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "invalid using macro");
    SemaRef.Diags.Report(ArgLoc, DiagID);
    return nullptr;
  }

  clang::Decl *D = SemaRef.getCxxSema().ActOnUsingDeclaration(
    CxxScope, AS, UsingLoc, clang::SourceLocation(),
    SemaRef.CurNNSContext, Name, clang::SourceLocation(), AttrView);
  if (!D)
    return nullptr;
  // FIXME: if this comes from an operator'.', elaborate lhs to
  // differentiate classes and namespaces.
  if (clang::UsingDecl *UD = dyn_cast<clang::UsingDecl>(D)) {
    for (auto Shadow : cast<clang::UsingDecl>(UD)->shadows())
      SemaRef.getCurrentScope()->Shadows.insert(Shadow);
  } else if (auto *UUVD = dyn_cast<clang::UnresolvedUsingValueDecl>(D)) {
  }

  return D;
}

static void handleUsingBlockList(SyntaxContext &Ctx, Sema &SemaRef,
                                 const ListSyntax *List,
                                 clang::SourceLocation UsingLoc) {
  for (const Syntax *Item : List->children()) {
    clang::Decl *D = handleUsing(Ctx, SemaRef, Item, UsingLoc);
    if (!D)
      continue;
  }
}

static void handleUsingBlock(SyntaxContext &Ctx, Sema &SemaRef,
                             const ArraySyntax *Block,
                             clang::SourceLocation UsingLoc) {
  for (const Syntax *Item : Block->children()) {
    if (const ArraySyntax *II = dyn_cast<ArraySyntax>(Item))
      handleUsingBlock(Ctx, SemaRef, II, UsingLoc);
    else if (const ListSyntax *II = dyn_cast<ListSyntax>(Item))
      handleUsingBlockList(Ctx, SemaRef, II, UsingLoc);
    else {
      clang::Decl *D = handleUsing(Ctx, SemaRef, Item, UsingLoc);
      if (!D)
        continue;
    }
  }
}

static void handleUsingDirectiveDecl(SyntaxContext &Ctx, Sema &SemaRef,
                                     Declaration *D) {
  assert(D->declaresUsingDirective());
  Sema::DeclContextRAII CtxRAII(SemaRef, D);
  const MacroSyntax *S = cast<MacroSyntax>(D->Init);
  const ArraySyntax *Block = cast<ArraySyntax>(S->getBlock());
  handleUsingBlock(Ctx, SemaRef, Block, S->getCall()->getLoc());
  D->CurrentPhase = Phase::Initialization;
}

static clang::Decl *handleNNSNamespaceElab(Elaborator &Elab, Sema &SemaRef,
                                           std::size_t NNSIndex,
                                           Declaration *D);

static clang::Decl *handleBuildNNSNamespace(Elaborator &Elab, Sema &SemaRef,
                                            std::size_t NNSIndex,
                                            Declaration *D) {
  assert(NNSIndex < D->NNSInfo.size() && "Invalid index");
  SyntaxContext &Context = SemaRef.getContext();
  gold::Scope *CurScope = SemaRef.getCurrentScope();

  NNSDeclaratorInfo CurInfo = D->NNSInfo[NNSIndex];
  assert(!CurInfo.Template && "Namespace cannot have template parameters");
  const AtomSyntax *Name = CurInfo.Name->getNestedName();

  clang::IdentifierInfo *NSId = &Context.CxxAST.Idents.get({Name->getSpelling()});

  // Create and enter a namespace scope.
  clang::CppxNamespaceDecl *NSDecl = nullptr;
  Sema::ClangScopeRAII NewNSClangScope(SemaRef, clang::Scope::DeclScope,
                                       Name->getLoc());
  clang::Scope *NSScope = SemaRef.getCurClangScope();
  gold::Sema::OptionalScopeRAII NewScope(SemaRef);
  gold::Sema::OptionalResumeScopeRAII ResumedScope(SemaRef);
  clang::UsingDirectiveDecl *UD = nullptr;
  clang::AttributeFactory Attrs;
  clang::ParsedAttributes ParsedAttrs(Attrs);
  NSDecl = SemaRef.ActOnStartNamespaceDef(NSScope, clang::SourceLocation(),
                                          Name->getLoc(), Name->getLoc(),
                                          NSId, Name->getLoc(), ParsedAttrs,
                                          UD);
  if (!NSDecl)
    return nullptr;

  if (NSDecl->isInvalidDecl())
    return nullptr;

  // Resume or create a new scope for the current namespace.
  // This is to allow the representations to all share the same scope.
  // This makes it easier to handle lookup for those elements of the scope.
  if (!NSDecl->Rep) {
    NewScope.Init(SK_Namespace, D->Init, &NSDecl->Rep);
    NSDecl->Rep = SemaRef.getCurrentScope();
  } else {
    ResumedScope.Init(NSDecl->Rep, NSDecl->Rep->Term, false);
  }

  // Reconstructing a namespace declaration
  ClangToGoldDeclRebuilder Rebuilder(Context, SemaRef);
  Declaration *NewDcl = Rebuilder.generateDeclForNNS(NSDecl, Name);
  // Recording the declaration as part of the previous parent scope.
  CurScope->addDecl(NewDcl);
  SemaRef.pushDecl(NewDcl);


  // Recursing to previous namespace handling.
  clang::Decl *Ret = handleNNSNamespaceElab(Elab, SemaRef, NNSIndex + 1, D);

  // even if there is an error that occurs here we must do this anyway.
  SemaRef.getCxxSema().ActOnFinishNamespaceDef(NSDecl, Name->getLoc());
  SemaRef.popDecl();
  // We must return the inner most namespace.
  return Ret;

}

static clang::Decl *handleNNSNamespaceElab(Elaborator &Elab, Sema &SemaRef,
                                           std::size_t NNSIndex,
                                           Declaration *D) {
  bool ReachedFinalDeclaration = NNSIndex == D->NNSInfo.size();
  if (ReachedFinalDeclaration) {
    // Handling the inner most namespace's creation.
    // The one that's meant by declaration D.
    // Changing to corrected decl context, because technically we moved.
    Declaration *NewDecl = new Declaration(*D);
    NewDecl->Cxt = SemaRef.getCurrentDecl();
    NewDecl->ScopeSpec.clear();
    NewDecl->NNSInfo.clear();
    SemaRef.getCurrentScope()->addDecl(NewDecl);
    // Attempting to correctly apply linkage specification this.
    const Syntax *LinkageAttr = nullptr;
    if (hasLinkageSpecDecl(SemaRef, NewDecl, &LinkageAttr))
      return nullptr;

    if (LinkageAttr)
      if (enterLinkageLanguageSpec(SemaRef, NewDecl, LinkageAttr))
        return nullptr;

    clang::Decl *Ret = processNamespaceDecl(Elab, SemaRef.getContext(),
                                           SemaRef, NewDecl);
    if (LinkageAttr) {
      exitLinkageLanguageSpec(SemaRef, NewDecl);
    }
    return Ret;
  } else {
    return handleBuildNNSNamespace(Elab, SemaRef, NNSIndex, D);
  }
}

clang::Decl *Elaborator::elaborateNestedNameNamespace(Declaration *D) {
  if (D->GlobalNsSpecifier) {
    llvm_unreachable("namespace with a globally qualified nns not valid.");
  }

  // Handling implicit namespace creation recursively
  return handleNNSNamespaceElab(*this, SemaRef, 0, D);
}

clang::Decl *Elaborator::elaborateDecl(Declaration *D) {
  if (phaseOf(D) != Phase::Identification)
    return D->Cxx;
  clang::Scope *OriginalClangScope = SemaRef.getCurClangScope();
  Scope *Sc = SemaRef.getCurrentScope();

  if (D->declaresNamespaceWithNestedName()) {
    auto *Ret = elaborateNestedNameNamespace(D);
    return Ret;
  }

  const Syntax *LinkageAttr = nullptr;
  if (hasLinkageSpecDecl(SemaRef, D, &LinkageAttr))
    return nullptr;

  if (LinkageAttr)
    if (enterLinkageLanguageSpec(SemaRef, D, LinkageAttr))
      return nullptr;


  // This clears any previous lookups and restores them once we reach the end.
  // This might be useful for elaborating parameters with default values that
  // are being assigned as a default value.
  Sema::NewNameSpecifierRAII NestedNameStack(SemaRef);
  if (!D->NNSInfo.empty() || D->GlobalNsSpecifier) {
    if (elaborateNestedNameForDecl(D)) {
      exitToCorrectScope(SemaRef, Sc, D, LinkageAttr);
      return nullptr;
    }
  }

  // In order to maintain correct entry and exit order we have to exit any
  // other scopes before we attempt to leave the template scopes constructed
  // as part of the nested name specifier. So they are destructed before we
  // actually leave their parent scopes.
  clang::Decl *Ret = nullptr;
  {
    Sema::OptionalResumeScopeRAII OriginalDeclScope(SemaRef);
    if (D->ScopeSpec.isSet()) {
      // Re-enter thre scope used to create the initial declaration.
      Scope *DeclScope = SemaRef.getLookupScope();
      if (DeclScope)
        OriginalDeclScope.Init(DeclScope, DeclScope->getConcreteTerm());
      else
        // FIXME: this may need to be an internal compiler error.
        llvm_unreachable("We have an invalid scope to resume!");
    }

    if (D->declaresUsingDirective()) {
      handleUsingDirectiveDecl(Context, SemaRef, D);
      return nullptr;
    }

    Sema::OptionalScopeRAII TemplateParamScope(SemaRef);
    Sema::OptioanlClangScopeRAII ClangTemplateScope(SemaRef);

    // Checking to see if we are need to enter a name scope for a template
    if (D->Template) {
      handleTemplateParameters(SemaRef, TemplateParamScope, ClangTemplateScope,
                               D, D->Template);
    }
    if (D->SpecializationArgs) {
      if (D->SpecializationArgs->ElaboratedArgs)
        // This already failed somewhere else!
        return nullptr;
      elaborateSpecializationArgs(SemaRef, D);
    }


    Ret = elaborateDeclContent(OriginalClangScope, D);
    // Checking the error from the specializaton and marking the decl as invalid.
    if (D->SpecializationArgs) {
      if (D->SpecializationArgs->getAsSpecialization()->getDidError()) {
        Ret->setInvalidDecl();
      }
    }
  }

  exitToCorrectScope(SemaRef, Sc, D, LinkageAttr);
  return Ret;
}

clang::Decl *Elaborator::elaborateDeclContent(clang::Scope *InitialScope,
                                              Declaration *D) {
  assert(D && "missing declaration");
  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  if (D->declaresTagDef())
    return processCXXRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresForwardRecordDecl())
    return processCXXForwardRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresNamespace())
    return processNamespaceDecl(*this, Context, SemaRef, D);
  if (D->declaresFunction())
    return elaborateFunctionDecl(D);
  return elaborateVariableDecl(InitialScope, D);
}

void Elaborator::buildTemplateParams(const Syntax *Params,
                               llvm::SmallVectorImpl<clang::NamedDecl *> &Res) {
  std::size_t I = 0;
  for (const Syntax *P : Params->children()) {
    Elaborator Elab(Context, SemaRef);
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
        Context.CxxAST.getTemplateTypeParmType(Depth, I,
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
void getFunctionParameters(Sema &SemaRef, Declaration *D,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  assert (D->declaresFunction() && "cannot get params for non-function");
  FunctionDeclarator *FnDecl = D->FunctionDcl->getAsFunction();
  const ListSyntax *ParamList = FnDecl->getParams();
  Scope *ParamScope = FnDecl->getScope();

  unsigned N = ParamList->getNumChildren();
  for (unsigned I = 0; I < N; ++I) {
    bool ArgsParam = false;
    const Syntax *P = ParamList->getChild(I);
    Declaration *PD = ParamScope->findDecl(P);
    if (!PD->Cxx)
      continue;

    if (cast<clang::ParmVarDecl>(PD->Cxx)->getType()->isVariadicType()) {
      if (I != N - 1) {
        SemaRef.Diags.Report(PD->getEndOfDecl(),
                             clang::diag::err_expected) << clang::tok::r_paren;
        continue;
      }

      ArgsParam = D->IsVariadic = true;
    }

    if (!ArgsParam)
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
    if (D->getId() == SemaRef.OpInfo.GoldDecl_OpNew
        || D->getId() == SemaRef.OpInfo.GoldDecl_OpDelete
        || D->getId() == SemaRef.OpInfo.GoldDecl_OpArray_New
        || D->getId() == SemaRef.OpInfo.GoldDecl_OpArray_Delete)
      SemaRef.createBuiltinOperatorNewDeleteDecls();
    assert(FPT && "function does not have prototype");
    if (getOperatorDeclarationName(Ctx, SemaRef, D->OpInfo, InClass,
                                   FPT->getNumParams(),
                                   D->IdDcl->getLoc(), Name)) {
      // FIXME: Should this be an error or not?
      return clang::DeclarationName();
    }
  } else if (D->declaresUserDefinedLiteral()) {
    // Attempting to correctly get the literal operator name
    Name = Ctx.CxxAST.DeclarationNames.getCXXLiteralOperatorName(D->UDLSuffixId);
  } else {
    Name = D->getId();
  }

  return Name;
}

void setSpecialFunctionName(SyntaxContext &Ctx, clang::CXXRecordDecl *RD,
                            Declaration *D, clang::DeclarationName &Name,
                            clang::QualType ConversionResultTy) {
  clang::QualType RecordTy = Ctx.CxxAST.getTypeDeclType(RD);
  clang::CanQualType Ty = Ctx.CxxAST.getCanonicalType(RecordTy);
  if (D->getId()->isStr("constructor")) {
    Name = Ctx.CxxAST.DeclarationNames.getCXXConstructorName(Ty);
  } else if (D->getId()->isStr("destructor")) {
    Name = Ctx.CxxAST.DeclarationNames.getCXXDestructorName(Ty);
  } else if (D->getKind() == UDK_ConversionOperator) {
    Name = Ctx.CxxAST.DeclarationNames.getCXXConversionFunctionName(
                               Ctx.CxxAST.getCanonicalType(ConversionResultTy));
  }
}
void lookupFunctionRedecls(Sema &SemaRef, clang::Scope *FoundScope,
                           clang::LookupResult &Previous) {
  while ((FoundScope->getFlags() & clang::Scope::DeclScope) == 0 ||
         (FoundScope->getFlags() & clang::Scope::TemplateParamScope) != 0)
    FoundScope = FoundScope->getParent();

  assert(FoundScope && "Scope not found");
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
      if (!Fn->TypeDcl) {
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
                                          Ty->getType(), Ty, ES, false,
                              false, clang::ConstexprSpecKind::CSK_unspecified);
    else if (Destructor)
      *FD = Method =
        clang::CXXDestructorDecl::Create(Context.CxxAST, RD, ExLoc, DNI,
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
  } else if (Fn->declaresConversionOperator()) {
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
      SemaRef.Diags.Report(Fn->IdDcl->getLoc(),
                         clang::diag::err_conv_function_with_params);
      return false;
    }
    clang::QualType ConvType = Proto->getReturnType();
    // FIXME: I need to enforce this.
    // C++ [class.conv.fct]p4:
    //   The conversion-type-id shall not represent a function type nor
    //   an array type.
    if (ConvType->isArrayType()) {
      SemaRef.Diags.Report(Fn->IdDcl->getLoc(),
                           clang::diag::err_conv_function_to_array);
      return false;
    } else if (ConvType->isFunctionType()) {
      SemaRef.Diags.Report(Fn->IdDcl->getLoc(),
                           clang::diag::err_conv_function_to_function);
      return false;
    }

    *FD = clang::CXXConversionDecl::Create(Context.CxxAST, RD, ExLoc, DNI,
                                          Ty->getType(), Ty,
                                          /*isinline*/false, ES,
                                      clang::ConstexprSpecKind::CSK_unspecified,
                                          ExLoc);
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

// If we have an auto return type that is dependent, 'deduce' it as DependentTy.
void deduceDependentAutoReturn(SyntaxContext &Context,
                               Sema &SemaRef,
                               clang::FunctionDecl *FD) {
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

} // end anonymous namespace

clang::Decl *Elaborator::elaborateFunctionDecl(Declaration *D) {
  clang::Sema &CxxSema = SemaRef.getCxxSema();
  clang::DeclContext *Owner = D->getOwningDeclContext();
  if (auto *Linkage = dyn_cast<clang::LinkageSpecDecl>(Owner)) {
    if (Linkage->getParent()->isRecord()) {
      SemaRef.Diags.Report(D->Op->getLoc(),
                          clang::diag::err_invalid_extern_c)
                          << 0;
      return nullptr;
    }
  }
  clang::DeclContext *ResolvedCtx = Owner;
  if (D->hasNestedNameSpecifier()) {
    ResolvedCtx = SemaRef.getCxxSema().computeDeclContext(D->ScopeSpec, true);
  }
  FunctionDeclarator *FnDclPtr = D->FunctionDcl;

  // Get a reference to the containing class if there is one.
  bool InClass = isa<clang::TagDecl>(ResolvedCtx);
  clang::CXXRecordDecl *RD = nullptr;
  if (InClass) {
    clang::Decl *ScopesDecl = SemaRef.getDeclForScope();
    assert(ScopesDecl && "Invalid declaration for scope.");
    RD = dyn_cast<clang::CXXRecordDecl>(ScopesDecl);
    assert(RD && "Class scope doesn't contain declaration.");
  }

  // Create the template parameters if they exist.
  bool Template = D->Template;
  TemplateParamsDeclarator *TPD = D->Template;
  bool Specialization = D->SpecializationArgs;

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
  // FIXME: Create make sure I can make this work some how.
  if (D->declaresUserDefinedLiteral()) {
    clang::UnqualifiedId UnqualId;
    UnqualId.setLiteralOperatorId(D->UDLSuffixId,
                                  D->IdDcl->getLoc(),
                           D->IdDcl->getIdentifier()->getFusionArg()->getLoc());
    if (SemaRef.getCxxSema().checkLiteralOperatorId(D->ScopeSpec, UnqualId)) {
      return nullptr;
    }
  }
  if (Name.isEmpty())
    return nullptr;

  // Set the declaration info here to help determine if this should have
  // a C++ special name.
  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(D->IdDcl->getLoc());

  if (InClass)
    setSpecialFunctionName(Context, RD, D, Name, TInfo->getType());

  clang::LookupResult Previous(CxxSema, DNI,
                               clang::Sema::LookupOrdinaryName,
                               CxxSema.forRedeclarationInCurContext());
  clang::Scope *CxxScope = SemaRef.getCurClangScope();
  if (D->hasNestedNameSpecifier()) {
    // Attempting to use the previously located decl context in order to
    // correctly identify any previous declarations.
    CxxSema.LookupQualifiedName(Previous, ResolvedCtx);
  } else {
    lookupFunctionRedecls(SemaRef, CxxScope, Previous);
  }

  clang::SourceLocation Loc = D->Op->getLoc();
  clang::FunctionDecl *FD = nullptr;
  if(InClass) {
    if (!buildMethod(Context, SemaRef, D, Name, &FD, TInfo, RD))
      return nullptr;

    // Fixing the lexical context, because this can change in the face of
    // a nested name specifier.
    FD->setLexicalDeclContext(Owner);

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
    auto *FTD = clang::FunctionTemplateDecl::Create(Context.CxxAST,
                                                    Owner, Loc,
                                                    FD->getDeclName(),
                                                TPD->getTemplateParameterList(),
                                                    FD);
    FTD->setLexicalDeclContext(Owner);
    FD->setDescribedFunctionTemplate(FTD);
    Owner->addDecl(FTD);
    if (InClass)
      FTD->setAccess(clang::AS_public);

    // An auto return type here is always dependent.
    if (FD->getReturnType()->isUndeducedAutoType())
      deduceDependentAutoReturn(Context, SemaRef, FD);
  }

  // An auto return type here is always dependent.
  if (InClass && SemaRef.getCurClangDeclContext()->isDependentContext())
    deduceDependentAutoReturn(Context, SemaRef, FD);


  CxxSema.getImplicitCodeSegOrSectionAttrForFunction(FD, D->Init);

  // Update the function parameters.
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getFunctionParameters(SemaRef, D, Params);
  FD->setParams(Params);
  for (auto *D : Params)
    D->setDeclContext(FD);

  D->CurrentPhase = Phase::Typing;
  SemaRef.setDeclForDeclaration(D, FD);
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
  CxxSema.FilterLookupForScope(Previous, ResolvedCtx,
                               CxxScope, !InClass, !InClass);

  CxxSema.CheckFunctionDeclaration(CxxScope, FD, Previous, false);
  bool IsMethod = false;
  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(FD)) {
    checkCXXMethodDecl(MD);
    CxxSema.CheckOverrideControl(MD);
    IsMethod = true;
  }

  // Handle function template specialization.
  if (!FD->isInvalidDecl() && !Previous.empty() && Specialization && !IsMethod) {
    clang::TemplateArgumentListInfo *Args =
      D->SpecializationArgs->HasArguments() ?
           &D->SpecializationArgs->getArgList() : nullptr;
    if (CxxSema.CheckFunctionTemplateSpecialization(FD, Args, Previous))
      FD->setInvalidDecl();
  }

  // FIXME: this is not necessarily what should happen.
  if (FD->isInvalidDecl())
    return nullptr;
  // Attempting to create an set nested name specifier as part of the declaration
  if (D->ScopeSpec.isSet())
    FD->setQualifierInfo(D->ScopeSpec.getWithLocInContext(Context.CxxAST));
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

static clang::StorageClass getDefaultVariableStorageClass(Sema &SemaRef) {
  return SemaRef.getCurrentScope()->isBlockScope() ||
    SemaRef.getCurrentScope()->isControlScope()
    ? clang::SC_Auto
    : clang::SC_None;
}

/// This returns the suspected storage class by searching attributes and
/// returning the first match. This function doesn't detect errors, its only
/// used to help determine the DeclContext.
static clang::StorageClass getSuspectedStorageClass(Sema &SemaRef,
                                                    Declaration *D) {
  if (!D->IdDcl->UnprocessedAttributes)
    return getDefaultVariableStorageClass(SemaRef);

  std::string ActualName;
  auto Iter = std::find_if(
      D->IdDcl->UnprocessedAttributes->begin(),
      D->IdDcl->UnprocessedAttributes->end(),
      [&](const Syntax *Attr) -> bool {
        auto Ret = checkAttrFormatAndName(Attr, ActualName);
        if (Ret == AF_Invalid) {
          return false;
        }
        return ActualName == "extern" || ActualName == "static";
      });
  if (Iter != D->IdDcl->UnprocessedAttributes->end()) {
    if (ActualName == "extern")
      return clang::SC_Extern;
    if (ActualName == "static")
      return clang::SC_Static;
    llvm_unreachable("Invalid attribute located.");
  }
  return getDefaultVariableStorageClass(SemaRef);
}

static bool shouldConsiderLinkage(const clang::VarDecl *VD) {
  if (VD->isInFragment())
    return false;

  const clang::DeclContext *DC = VD->getDeclContext()->getRedeclContext();
  if (DC->isFunctionOrMethod())
    return VD->hasExternalStorage();
  if (DC->isFileContext())
    return true;
  if (DC->isRecord())
    return false;
  if (isa<clang::RequiresExprBodyDecl>(DC))
    return false;
  llvm_unreachable("Unexpected context");
}

// static bool isMemberSpecialization(Declaration *D) {
//   // We need to determine if the current variable declaration could be a member
//   // specialization.
//   for(const auto &Dcltr : D->NNSInfo)
//       if (Dcltr.Template || Dcltr.SpecializationArgs)
//         return true;
//   return false;
// }
static bool isFunctionDefinitionDiscarded(Sema &S, clang::FunctionDecl *FD) {
  // Try to avoid calling GetGVALinkageForFunction.

  // All cases of this require the 'inline' keyword.
  if (!FD->isInlined()) return false;

  // This is only possible in C++ with the gnu_inline attribute.
  if (!FD->hasAttr<clang::GNUInlineAttr>())
    return false;

  // Okay, go ahead and call the relatively-more-expensive function.
  return S.getContext().CxxAST.GetGVALinkageForFunction(FD)
        == clang::GVA_AvailableExternally;
}

/// Determine whether a variable is extern "C" prior to attaching
/// an initializer. We can't just call isExternC() here, because that
/// will also compute and cache whether the declaration is externally
/// visible, which might change when we attach the initializer.
///
/// This can only be used if the declaration is known to not be a
/// redeclaration of an internal linkage declaration.
///
/// For instance:
///
///   auto x = []{};
///
/// Attaching the initializer here makes this declaration not externally
/// visible, because its type has internal linkage.
///
/// FIXME: This is a hack.
template<typename T>
static bool isIncompleteDeclExternC(Sema &S, const T *D) {
  // In C++, the overloadable attribute negates the effects of extern "C".
  if (!D->isInExternCContext() || D->template hasAttr<clang::OverloadableAttr>())
    return false;

  // So do CUDA's host/device attributes.
  if (S.getCxxSema().getLangOpts().CUDA &&
      (D->template hasAttr<clang::CUDADeviceAttr>() ||
      D->template hasAttr<clang::CUDAHostAttr>()))
    return false;

  return D->isExternC();
}

static unsigned getMSManglingNumber(const clang::LangOptions &LO,
                                    clang::Scope *S) {
  return LO.isCompatibleWithMSVC(clang::LangOptions::MSVC2015)
             ? S->getMSCurManglingNumber()
             : S->getMSLastManglingNumber();
}
static void checkDLLAttributeRedeclaration(clang::Sema &S, clang::NamedDecl *OldDecl,
                                           clang::NamedDecl *NewDecl,
                                           bool IsSpecialization,
                                           bool IsDefinition) {
  using namespace clang;
  if (OldDecl->isInvalidDecl() || NewDecl->isInvalidDecl())
    return;

  bool IsTemplate = false;
  if (TemplateDecl *OldTD = dyn_cast<TemplateDecl>(OldDecl)) {
    OldDecl = OldTD->getTemplatedDecl();
    IsTemplate = true;
    if (!IsSpecialization)
      IsDefinition = false;
  }
  if (TemplateDecl *NewTD = dyn_cast<TemplateDecl>(NewDecl)) {
    NewDecl = NewTD->getTemplatedDecl();
    IsTemplate = true;
  }

  if (!OldDecl || !NewDecl)
    return;

  const DLLImportAttr *OldImportAttr = OldDecl->getAttr<DLLImportAttr>();
  const DLLExportAttr *OldExportAttr = OldDecl->getAttr<DLLExportAttr>();
  const DLLImportAttr *NewImportAttr = NewDecl->getAttr<DLLImportAttr>();
  const DLLExportAttr *NewExportAttr = NewDecl->getAttr<DLLExportAttr>();

  // dllimport and dllexport are inheritable attributes so we have to exclude
  // inherited attribute instances.
  bool HasNewAttr = (NewImportAttr && !NewImportAttr->isInherited()) ||
                    (NewExportAttr && !NewExportAttr->isInherited());

  // A redeclaration is not allowed to add a dllimport or dllexport attribute,
  // the only exception being explicit specializations.
  // Implicitly generated declarations are also excluded for now because there
  // is no other way to switch these to use dllimport or dllexport.
  bool AddsAttr = !(OldImportAttr || OldExportAttr) && HasNewAttr;

  if (AddsAttr && !IsSpecialization && !OldDecl->isImplicit()) {
    // Allow with a warning for free functions and global variables.
    bool JustWarn = false;
    if (!OldDecl->isCXXClassMember()) {
      auto *VD = dyn_cast<VarDecl>(OldDecl);
      if (VD && !VD->getDescribedVarTemplate())
        JustWarn = true;
      auto *FD = dyn_cast<FunctionDecl>(OldDecl);
      if (FD && FD->getTemplatedKind() == FunctionDecl::TK_NonTemplate)
        JustWarn = true;
    }

    // We cannot change a declaration that's been used because IR has already
    // been emitted. Dllimported functions will still work though (modulo
    // address equality) as they can use the thunk.
    if (OldDecl->isUsed())
      if (!isa<FunctionDecl>(OldDecl) || !NewImportAttr)
        JustWarn = false;

    unsigned DiagID = JustWarn ? diag::warn_attribute_dll_redeclaration
                               : diag::err_attribute_dll_redeclaration;
    S.Diag(NewDecl->getLocation(), DiagID)
        << NewDecl
        << (NewImportAttr ? (const Attr *)NewImportAttr : NewExportAttr);
    S.Diag(OldDecl->getLocation(), diag::note_previous_declaration);
    if (!JustWarn) {
      NewDecl->setInvalidDecl();
      return;
    }
  }

  // A redeclaration is not allowed to drop a dllimport attribute, the only
  // exceptions being inline function definitions (except for function
  // templates), local extern declarations, qualified friend declarations or
  // special MSVC extension: in the last case, the declaration is treated as if
  // it were marked dllexport.
  bool IsInline = false, IsStaticDataMember = false, IsQualifiedFriend = false;
  bool IsMicrosoft = S.Context.getTargetInfo().getCXXABI().isMicrosoft();
  if (const auto *VD = dyn_cast<VarDecl>(NewDecl)) {
    // Ignore static data because out-of-line definitions are diagnosed
    // separately.
    IsStaticDataMember = VD->isStaticDataMember();
    IsDefinition = VD->isThisDeclarationADefinition(S.Context) !=
                   VarDecl::DeclarationOnly;
  } else if (const auto *FD = dyn_cast<FunctionDecl>(NewDecl)) {
    IsInline = FD->isInlined();
    IsQualifiedFriend = FD->getQualifier() &&
                        FD->getFriendObjectKind() == Decl::FOK_Declared;
  }

  if (OldImportAttr && !HasNewAttr &&
      (!IsInline || (IsMicrosoft && IsTemplate)) && !IsStaticDataMember &&
      !NewDecl->isLocalExternDecl() && !IsQualifiedFriend) {
    if (IsMicrosoft && IsDefinition) {
      S.Diag(NewDecl->getLocation(),
             diag::warn_redeclaration_without_import_attribute)
          << NewDecl;
      S.Diag(OldDecl->getLocation(), diag::note_previous_declaration);
      NewDecl->dropAttr<DLLImportAttr>();
      NewDecl->addAttr(
          DLLExportAttr::CreateImplicit(S.Context, NewImportAttr->getRange()));
    } else {
      S.Diag(NewDecl->getLocation(),
             diag::warn_redeclaration_without_attribute_prev_attribute_ignored)
          << NewDecl << OldImportAttr;
      S.Diag(OldDecl->getLocation(), diag::note_previous_declaration);
      S.Diag(OldImportAttr->getLocation(), diag::note_previous_attribute);
      OldDecl->dropAttr<DLLImportAttr>();
      NewDecl->dropAttr<DLLImportAttr>();
    }
  } else if (IsInline && OldImportAttr && !IsMicrosoft) {
    // In MinGW, seeing a function declared inline drops the dllimport
    // attribute.
    OldDecl->dropAttr<DLLImportAttr>();
    NewDecl->dropAttr<DLLImportAttr>();
    S.Diag(NewDecl->getLocation(),
           diag::warn_dllimport_dropped_from_inline_function)
        << NewDecl << OldImportAttr;
  }

  // A specialization of a class template member function is processed here
  // since it's a redeclaration. If the parent class is dllexport, the
  // specialization inherits that attribute. This doesn't happen automatically
  // since the parent class isn't instantiated until later.
  if (const CXXMethodDecl *MD = dyn_cast<CXXMethodDecl>(NewDecl)) {
    if (MD->getTemplatedKind() == FunctionDecl::TK_MemberSpecialization &&
        !NewImportAttr && !NewExportAttr) {
      if (const DLLExportAttr *ParentExportAttr =
              MD->getParent()->getAttr<DLLExportAttr>()) {
        DLLExportAttr *NewAttr = ParentExportAttr->clone(S.Context);
        NewAttr->setInherited(true);
        NewDecl->addAttr(NewAttr);
      }
    }
  }
}

static clang::TemplateIdAnnotation *buildTemplateIdAnnotation(SyntaxContext &Context,
                                                       Sema &SemaRef,
                                                       clang::LookupResult& R,
                                                       clang::TemplateNameKind TNK,
                                                       Declaration *D) {
  if (!D->Template)
    return nullptr;
  SpecializationDeclarator *SD = D->SpecializationArgs;
  if (!SD) {
    return nullptr;
  }
  clang::SourceLocation IdLoc = D->Decl->getLoc();

  if (!R.isSingleResult()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "specialization of ambiguous template");
    SemaRef.Diags.Report(IdLoc, DiagID);
    return nullptr;
  }

  clang::TemplateDecl *Principal = R.getAsSingle<clang::TemplateDecl>();
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
  bool ArgsAreInvalid = false;
  if (SD) {
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

      if (BadArgument) {
        ArgsAreInvalid = true;
        continue;
      }
    }
  }
  return clang::TemplateIdAnnotation::Create(
      D->Op->getLoc(), IdLoc, D->getId(), /*NameSpliced=*/false,
      clang::OO_None, TemplatePtr,
      TNK, IdLoc, IdLoc, Args, /*ArgsInvalid*/ArgsAreInvalid,
      TemplateIds);
}


static bool isTemplateArgumentTemplateParameter(
    const clang::TemplateArgument &Arg, unsigned Depth, unsigned Index) {

  using namespace clang;

  switch (Arg.getKind()) {
  case TemplateArgument::Null:
  case TemplateArgument::NullPtr:
  case TemplateArgument::Integral:
  case TemplateArgument::Declaration:
  case TemplateArgument::Pack:
  case TemplateArgument::TemplateExpansion:
    return false;

  case TemplateArgument::Type: {
    QualType Type = Arg.getAsType();
    const TemplateTypeParmType *TPT =
        Arg.getAsType()->getAs<TemplateTypeParmType>();
    return TPT && !Type.hasQualifiers() &&
           TPT->getDepth() == Depth && TPT->getIndex() == Index;
  }

  case TemplateArgument::Reflected:
  case TemplateArgument::Expression: {
    DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(Arg.getAsExpr());
    if (!DRE || !DRE->getDecl())
      return false;
    const NonTypeTemplateParmDecl *NTTP =
        dyn_cast<NonTypeTemplateParmDecl>(DRE->getDecl());
    return NTTP && NTTP->getDepth() == Depth && NTTP->getIndex() == Index;
  }

  case TemplateArgument::Template:
    const TemplateTemplateParmDecl *TTP =
        dyn_cast_or_null<TemplateTemplateParmDecl>(
            Arg.getAsTemplateOrTemplatePattern().getAsTemplateDecl());
    return TTP && TTP->getDepth() == Depth && TTP->getIndex() == Index;
  }
  llvm_unreachable("unexpected kind of template argument");
}

static bool isSameAsPrimaryTemplate(clang::TemplateParameterList *Params,
                                clang::ArrayRef<clang::TemplateArgument> Args) {
  using namespace clang;
  if (Params->size() != Args.size())
    return false;

  unsigned Depth = Params->getDepth();

  for (unsigned I = 0, N = Args.size(); I != N; ++I) {
    TemplateArgument Arg = Args[I];

    // If the parameter is a pack expansion, the argument must be a pack
    // whose only element is a pack expansion.
    if (Params->getParam(I)->isParameterPack()) {
      if (Arg.getKind() != TemplateArgument::Pack || Arg.pack_size() != 1 ||
          !Arg.pack_begin()->isPackExpansion())
        return false;
      Arg = Arg.pack_begin()->getPackExpansionPattern();
    }

    if (!isTemplateArgumentTemplateParameter(Arg, Depth, I))
      return false;
  }
  return true;
}

/// Convert the parser's template argument list representation into our form.
static clang::TemplateArgumentListInfo
makeTemplateArgumentListInfo(Sema &S, clang::TemplateIdAnnotation &TemplateId) {
  clang::TemplateArgumentListInfo TemplateArgs(TemplateId.LAngleLoc,
                                               TemplateId.RAngleLoc);
  clang::ASTTemplateArgsPtr TemplateArgsPtr(TemplateId.getTemplateArgs(),
                                            TemplateId.NumArgs);
  S.getCxxSema().translateTemplateArguments(TemplateArgsPtr, TemplateArgs);
  return TemplateArgs;
}


/// Check whether a specialization is well-formed in the current
/// context.
///
/// This routine determines whether a template specialization can be declared
/// in the current context (C++ [temp.expl.spec]p2).
///
/// \param S the semantic analysis object for which this check is being
/// performed.
///
/// \param Specialized the entity being specialized or instantiated, which
/// may be a kind of template (class template, function template, etc.) or
/// a member of a class template (member function, static data member,
/// member class).
///
/// \param PrevDecl the previous declaration of this entity, if any.
///
/// \param Loc the location of the explicit specialization or instantiation of
/// this entity.
///
/// \param IsPartialSpecialization whether this is a partial specialization of
/// a class template.
///
/// \returns true if there was an error that we cannot recover from, false
/// otherwise.
static bool checkTemplateSpecializationScope(clang::Sema &S,
                                             clang::NamedDecl *Specialized,
                                             clang::NamedDecl *PrevDecl,
                                             clang::SourceLocation Loc,
                                             bool IsPartialSpecialization) {
  using namespace clang;
  // Keep these "kind" numbers in sync with the %select statements in the
  // various diagnostics emitted by this routine.
  int EntityKind = 0;
  if (isa<ClassTemplateDecl>(Specialized))
    EntityKind = IsPartialSpecialization? 1 : 0;
  else if (isa<VarTemplateDecl>(Specialized))
    EntityKind = IsPartialSpecialization ? 3 : 2;
  else if (isa<FunctionTemplateDecl>(Specialized))
    EntityKind = 4;
  else if (isa<CXXMethodDecl>(Specialized))
    EntityKind = 5;
  else if (isa<VarDecl>(Specialized))
    EntityKind = 6;
  else if (isa<RecordDecl>(Specialized))
    EntityKind = 7;
  else if (isa<EnumDecl>(Specialized) && S.getLangOpts().CPlusPlus11)
    EntityKind = 8;
  else {
    S.Diag(Loc, diag::err_template_spec_unknown_kind)
      << S.getLangOpts().CPlusPlus11;
    S.Diag(Specialized->getLocation(), diag::note_specialized_entity);
    return true;
  }

  // C++ [temp.expl.spec]p2:
  //   An explicit specialization may be declared in any scope in which
  //   the corresponding primary template may be defined.
  if (S.CurContext->getRedeclContext()->isFunctionOrMethod()) {
    S.Diag(Loc, diag::err_template_spec_decl_function_scope)
      << Specialized;
    return true;
  }

  // C++ [temp.class.spec]p6:
  //   A class template partial specialization may be declared in any
  //   scope in which the primary template may be defined.
  DeclContext *SpecializedContext =
      Specialized->getDeclContext()->getRedeclContext();
  DeclContext *DC = S.CurContext->getRedeclContext();

  // Make sure that this redeclaration (or definition) occurs in the same
  // scope or an enclosing namespace.
  if (!(DC->isFileContext() ? DC->Encloses(SpecializedContext)
                            : DC->Equals(SpecializedContext))) {
    if (isa<TranslationUnitDecl>(SpecializedContext))
      S.Diag(Loc, diag::err_template_spec_redecl_global_scope)
        << EntityKind << Specialized;
    else {
      auto *ND = cast<NamedDecl>(SpecializedContext);
      int Diag = diag::err_template_spec_redecl_out_of_scope;
      if (S.getLangOpts().MicrosoftExt && !DC->isRecord())
        Diag = diag::ext_ms_template_spec_redecl_out_of_scope;
      S.Diag(Loc, Diag) << EntityKind << Specialized
                        << ND << isa<CXXRecordDecl>(ND);
    }

    S.Diag(Specialized->getLocation(), diag::note_specialized_entity);

    // Don't allow specializing in the wrong class during error recovery.
    // Otherwise, things can go horribly wrong.
    if (DC->isRecord())
      return true;
  }

  return false;
}

static clang::TemplateSpecializationKind
getTemplateSpecializationKind(clang::Decl *D) {
  using namespace clang;
  if (!D)
    return TSK_Undeclared;

  if (CXXRecordDecl *Record = dyn_cast<CXXRecordDecl>(D))
    return Record->getTemplateSpecializationKind();
  if (FunctionDecl *Function = dyn_cast<FunctionDecl>(D))
    return Function->getTemplateSpecializationKind();
  if (VarDecl *Var = dyn_cast<VarDecl>(D))
    return Var->getTemplateSpecializationKind();

  return TSK_Undeclared;
}

static bool actOnVarTemplateSpecialziation(Sema &SemaRef,
                                           clang::Scope *InitialScope,
                                           Declaration *D,
                                           clang::DeclContext *Owner,
                                           clang::DeclContext *PreviousDC,
                                        clang::TemplateIdAnnotation *TemplateId,
                                           clang::TypeSourceInfo *TInfo,
                                   clang::TemplateParameterList *TemplateParams,
                                      clang::StorageClass SuspectedStorageClass,
                                           bool IsPartialSpecialization,
                                           bool IsClassMember) {
  using namespace clang;
  auto &CxxSema = SemaRef.getCxxSema();
  SyntaxContext &Context = SemaRef.getContext();
  auto &CxxAST = Context.CxxAST;

  TemplateArgumentListInfo TemplateArgs =
      makeTemplateArgumentListInfo(SemaRef, *TemplateId);
  SourceLocation TemplateNameLoc = D->IdDcl->getLoc();
  SourceLocation LAngleLoc = TemplateId->LAngleLoc;
  SourceLocation RAngleLoc = TemplateId->RAngleLoc;
  SourceLocation TemplateKWLoc = TemplateNameLoc;

  TemplateName Name = TemplateId->Template.get();

  // The template-id must name a variable template.
  VarTemplateDecl *VarTemplate =
      dyn_cast_or_null<VarTemplateDecl>(Name.getAsTemplateDecl());
  if (!VarTemplate) {
    NamedDecl *FnTemplate;
    if (auto *OTS = Name.getAsOverloadedTemplate())
      FnTemplate = *OTS->begin();
    else
      FnTemplate
       = dyn_cast_or_null<FunctionTemplateDecl>(Name.getAsTemplateDecl());
    if (FnTemplate)
      return CxxSema.Diag(D->IdDcl->getLoc(),
                          diag::err_var_spec_no_template_but_method)
                          << FnTemplate->getDeclName();
    return CxxSema.Diag(D->IdDcl->getLoc(),
                        diag::err_var_spec_no_template)
                        << IsPartialSpecialization;
  }

  // Check for unexpanded parameter packs in any of the template arguments.
  for (unsigned I = 0, N = TemplateArgs.size(); I != N; ++I)
    if (CxxSema.DiagnoseUnexpandedParameterPack(TemplateArgs[I],
                                       clang::Sema::UPPC_PartialSpecialization))
      return true;

  // Check that the template argument list is well-formed for this
  // template.
  SmallVector<TemplateArgument, 4> Converted;
  if (CxxSema.CheckTemplateArgumentList(VarTemplate, TemplateNameLoc, TemplateArgs,
                                        false, Converted,
                                        /*UpdateArgsWithConversion=*/true))
    return true;

  // Find the variable template (partial) specialization declaration that
  // corresponds to these arguments.
  if (IsPartialSpecialization) {
    if (CxxSema.CheckTemplatePartialSpecializationArgs(TemplateNameLoc,
                                                       VarTemplate,
                                                       TemplateArgs.size(),
                                                       Converted))
      return true;

    // FIXME: Move these checks to CheckTemplatePartialSpecializationArgs so we
    // also do them during instantiation.
    bool InstantiationDependent;
    if (!Name.isDependent() &&
        !TemplateSpecializationType::anyDependentTemplateArguments(
            TemplateArgs.arguments(),
            InstantiationDependent)) {
      CxxSema.Diag(TemplateNameLoc,
                   diag::err_partial_spec_fully_specialized)
                   << VarTemplate->getDeclName();
      IsPartialSpecialization = false;
    }

    if (isSameAsPrimaryTemplate(VarTemplate->getTemplateParameters(),
                                Converted) &&
        (!CxxAST.getLangOpts().CPlusPlus20 ||
         !TemplateParams->hasAssociatedConstraints())) {
      // C++ [temp.class.spec]p9b3:
      //
      //   -- The argument list of the specialization shall not be identical
      //      to the implicit argument list of the primary template.
      CxxSema.Diag(TemplateNameLoc,
                   diag::err_partial_spec_args_match_primary_template)
                   << /* variable template */1
                   << /* is definition */
                 (SuspectedStorageClass != SC_Extern && !PreviousDC->isRecord())
                 << FixItHint::CreateRemoval(SourceRange(LAngleLoc, RAngleLoc));
      // FIXME: Recover from this by treating the declaration as a redeclaration
      // of the primary template.
      return true;
    }
  }

  void *InsertPos = nullptr;
  VarTemplateSpecializationDecl *PrevDecl = nullptr;

  if (IsPartialSpecialization)
    PrevDecl = VarTemplate->findPartialSpecialization(Converted, TemplateParams,
                                                      InsertPos);
  else
    PrevDecl = VarTemplate->findSpecialization(Converted, InsertPos);

  VarTemplateSpecializationDecl *Specialization = nullptr;

  // Check whether we can declare a variable template specialization in
  // the current scope.
  if (checkTemplateSpecializationScope(CxxSema, VarTemplate, PrevDecl,
                                       TemplateNameLoc,
                                       IsPartialSpecialization))
    return true;

  if (PrevDecl && PrevDecl->getSpecializationKind() == TSK_Undeclared) {
    // Since the only prior variable template specialization with these
    // arguments was referenced but not declared,  reuse that
    // declaration node as our own, updating its source location and
    // the list of outer template parameters to reflect our new declaration.
    Specialization = PrevDecl;
    Specialization->setLocation(TemplateNameLoc);
    PrevDecl = nullptr;
  } else if (IsPartialSpecialization) {
    // Create a new class template partial specialization declaration node.
    VarTemplatePartialSpecializationDecl *PrevPartial =
        cast_or_null<VarTemplatePartialSpecializationDecl>(PrevDecl);
    VarTemplatePartialSpecializationDecl *Partial =
        VarTemplatePartialSpecializationDecl::Create(
            CxxAST, VarTemplate->getDeclContext(), TemplateKWLoc,
            TemplateNameLoc, TemplateParams, VarTemplate, TInfo->getType(),
            TInfo, SuspectedStorageClass, Converted, TemplateArgs);
    if (!PrevPartial)
      VarTemplate->AddPartialSpecialization(Partial, InsertPos);
    Specialization = Partial;

    // If we are providing an explicit specialization of a member variable
    // template specialization, make a note of that.
    if (PrevPartial && PrevPartial->getInstantiatedFromMember())
      PrevPartial->setMemberSpecialization();

    CxxSema.CheckTemplatePartialSpecialization(Partial);
  } else {
    // Create a new class template specialization declaration node for
    // this explicit specialization or friend declaration.
    Specialization = VarTemplateSpecializationDecl::Create(
        CxxAST, VarTemplate->getDeclContext(), TemplateKWLoc, TemplateNameLoc,
        VarTemplate, TInfo->getType(), TInfo, SuspectedStorageClass,
        Converted);
    Specialization->setTemplateArgsInfo(TemplateArgs);
    if (!PrevDecl)
      VarTemplate->AddSpecialization(Specialization, InsertPos);
  }

  // C++ [temp.expl.spec]p6:
  //   If a template, a member template or the member of a class template is
  //   explicitly specialized then that specialization shall be declared
  //   before the first use of that specialization that would cause an implicit
  //   instantiation to take place, in every translation unit in which such a
  //   use occurs; no diagnostic is required.
  if (PrevDecl && PrevDecl->getPointOfInstantiation().isValid()) {
    bool Okay = false;
    for (Decl *Prev = PrevDecl; Prev; Prev = Prev->getPreviousDecl()) {
      // Is there any previous explicit specialization declaration?
      if (getTemplateSpecializationKind(Prev) == TSK_ExplicitSpecialization) {
        Okay = true;
        break;
      }
    }

    if (!Okay) {
      SourceRange Range(TemplateNameLoc, RAngleLoc);
      CxxSema.Diag(TemplateNameLoc, diag::err_specialization_after_instantiation)
                   << Name << Range;

      CxxSema.Diag(PrevDecl->getPointOfInstantiation(),
                   diag::note_instantiation_required_here)
                   << (PrevDecl->getTemplateSpecializationKind() !=
                  TSK_ImplicitInstantiation);
      return true;
    }
  }

  Specialization->setTemplateKeywordLoc(TemplateKWLoc);
  Specialization->setLexicalDeclContext(Owner);

  // Add the specialization into its lexical context, so that it can
  // be seen when iterating through the list of declarations in that
  // context. However, specializations are not found by name lookup.
  if (IsClassMember)
    Specialization->setAccess(clang::AS_public);


  // Note that this is an explicit specialization.
  Specialization->setSpecializationKind(TSK_ExplicitSpecialization);

  if (PrevDecl) {
    // Check that this isn't a redefinition of this specialization,
    // merging with previous declarations.
    clang::DeclarationName DName(D->getId());
    clang::DeclarationNameInfo DNI;
    DNI.setName(DName);
    DNI.setLoc(D->IdDcl->getLoc());
    LookupResult PrevSpec(CxxSema, DNI, clang::Sema::LookupOrdinaryName,
                          CxxSema.forRedeclarationInCurContext());
    PrevSpec.addDecl(PrevDecl);
    D->IsRedeclaration
                   = CxxSema.CheckVariableDeclaration(Specialization, PrevSpec);
  } else if (Specialization->isStaticDataMember() &&
             Specialization->isOutOfLine()) {
    Specialization->setAccess(VarTemplate->getAccess());
  }

  Owner->addDecl(Specialization);
  Specialization->setDeclContext(PreviousDC);
  Specialization->setLexicalDeclContext(Owner);
  SemaRef.setDeclForDeclaration(D, Specialization);
  return false;
}

clang::Decl *Elaborator::elaborateVariableDecl(clang::Scope *InitialScope,
                                               Declaration *D) {
  assert(InitialScope && "Invalid owning scope\n");
  D->SavedScope = SemaRef.getCurrentScope();
  if (D->ScopeForDecl->isParameterScope())
    return elaborateParameterDecl(D);

  if (D->ScopeForDecl->isTemplateScope())
    return elaborateTemplateParamDecl(D);

  // We need to make sure that the type we are elaborating isn't infact a
  // a CppxKindType expression. If it is we may have an issue emitting this
  // as a valid type alias.
  clang::Expr *TypeExpr = nullptr;
  clang::SourceLocation TypeLocation;
  if (D->TypeDcl) {
    ExprElaborator TypeElab(Context, SemaRef);
    TypeExpr = TypeElab.elaborateExplicitType(D->TypeDcl, nullptr);
  } else {
    TypeExpr = SemaRef.buildTypeExpr(Context.CxxAST.getAutoDeductType(),
                                     D->Op->getLoc());
  }

  if (!TypeExpr) {
    SemaRef.Diags.Report(TypeLocation,
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                                  TypeLocation);
  if (!TInfo) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_unsupported_unknown_any_decl)
                         << D->getId();
    return nullptr;
  }
  // This is where we differentiate between type alias template,
  // variable template, namespace aliases, members, etc.
  clang::QualType VarType = TInfo->getType();
  if (VarType->isTypeOfTypes()) {
    if (D->Template)
      return elaborateTemplateAliasOrVariable(D);
    return elaborateTypeAlias(D);
  }

  if (VarType->isNamespaceType()) {
    return elaborateNsAlias(D);
  }

  bool IsClassMember = D->isDeclaredWithinClass();

  // Cannot have a local extern variable with linkage?
  if (auto *Linkage
                = dyn_cast<clang::LinkageSpecDecl>(D->getOwningDeclContext())) {
    clang::DeclContext *Cur = Linkage;
    while(Cur) {
      if (Cur->isFunctionOrMethod()) {
        SemaRef.Diags.Report(D->Op->getLoc(),
                              clang::diag::err_invalid_extern_c)
                              << 3;
        return nullptr;
      }
      Cur = Cur->getParent();
    }
  }
  // Templated variables cannot be a field
  if (IsClassMember && !D->Template) {
    return elaborateField(D, TInfo);
  }
  clang::Sema &CxxSema = SemaRef.getCxxSema();

  clang::DeclarationName Name(D->getId());
  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(D->IdDcl->getLoc());

  clang::LookupResult Previous(CxxSema, DNI,
                               clang::Sema::LookupOrdinaryName,
                           CxxSema.forRedeclarationInCurContext());
  clang::DeclContext *PreviousDC = SemaRef.getCurClangDeclContext();
  clang::Scope *CxxScope = SemaRef.getCurClangScope();

  if (D->ScopeSpec.isSet()) {
    // bool EnteringContext = !D.getDeclSpec().isFriendSpecified();
    PreviousDC = CxxSema.computeDeclContext(D->ScopeSpec, true);
    if (!PreviousDC || isa<clang::EnumDecl>(PreviousDC)) {
      // If we could not compute the declaration context, it's because the
      // declaration context is dependent but does not refer to a class,
      // class template, or class template partial specialization. Complain
      // and return early, to avoid the coming semantic disaster.
      CxxSema.Diag(D->IdDcl->getLoc(),
            clang::diag::err_template_qualified_declarator_no_match)
        << D->ScopeSpec.getScopeRep()
        << D->ScopeSpec.getRange();
      return nullptr;
    }

    bool IsDependentContext = PreviousDC->isDependentContext();

    if (!IsDependentContext &&
        CxxSema.RequireCompleteDeclContext(D->ScopeSpec, PreviousDC))
      return nullptr;

    // If a class is incomplete, do not parse entities inside it.
    if (isa<clang::CXXRecordDecl>(PreviousDC)
        && !cast<clang::CXXRecordDecl>(PreviousDC)->hasDefinition()) {
      CxxSema.Diag(D->IdDcl->getLoc(),
            clang::diag::err_member_def_undefined_record)
        << Name << PreviousDC << D->ScopeSpec.getRange();
      return nullptr;
    }
  }

  if (D->hasNestedNameSpecifier()) {
    // Attempting to use the previously located decl context in order to
    // correctly identify any previous declarations.
    CxxSema.LookupQualifiedName(Previous, PreviousDC);
  } else
    lookupFunctionRedecls(SemaRef, CxxScope, Previous);

  clang::DeclContext *Owner = D->getOwningDeclContext();
  if (CxxSema.DiagnoseClassNameShadow(Owner, DNI)) {
    // Forget that the previous declaration is the injected-class-name.
    Previous.clear();
  }

  // Get the type of the entity.
  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::StorageClass SC = getSuspectedStorageClass(SemaRef, D);

  // Adjusting context
  clang::DeclContext *OriginalDC = Owner;
  bool IsLocalExternDecl = SC == clang::SC_Extern &&
                            clang::Sema::adjustContextForLocalExternDecl(Owner);

  bool IsMemberSpecialization = false;
  bool IsVariableTemplateSpecialization = false;
  bool IsPartialSpecialization = false;
  bool IsVariableTemplate = false;
  clang::VarDecl *NewVD = nullptr;
  clang::VarTemplateDecl *NewTemplate = nullptr;
  clang::TemplateParameterList *TemplateParams = nullptr;
  clang::TemplateIdAnnotation *TemplateId = nullptr;
  // Attempting to detect an invalid specialization, or one that doesn't have
  // a default template yet.
  if (D->Template && D->SpecializationArgs && Previous.empty()) {
    return nullptr;
  }

  if (D->Template)
    TemplateId = buildTemplateIdAnnotation(Context, SemaRef, Previous,
                                           clang::TNK_Var_template, D);

  clang::MultiTemplateParamsArg TemplateParamLists = D->TemplateParamStorage;
  bool Invalid = false;
  // Match up the template parameter lists with the scope specifier, then
  // determine whether we have a template or a template specialization.
  bool InvalidScope = false;
  TemplateParams = CxxSema.MatchTemplateParametersToScopeSpecifier(
      D->IdDcl->getLoc(), D->IdDcl->getLoc(), D->ScopeSpec,
      /*TemplateIdAnnotation *TemplateId=*/TemplateId,
      TemplateParamLists,
      /*never a friend*/false, IsMemberSpecialization, InvalidScope);
  Invalid |= InvalidScope;

  if (TemplateParams) {
    if (!TemplateParams->size() && !TemplateId) {
      // There is an extraneous 'template<>' for this variable. Complain
      // about it, but allow the declaration of the variable.
      SemaRef.getCxxSema().Diag(TemplateParams->getTemplateLoc(),
            clang::diag::err_template_variable_noparams)
        << Name.getAsIdentifierInfo()
        << clang::SourceRange(TemplateParams->getTemplateLoc(),
                        TemplateParams->getRAngleLoc());
      TemplateParams = nullptr;
    } else {
      // Check that we can declare a template here.
      if (CxxSema.CheckTemplateDeclScope(CxxScope, TemplateParams))
        return nullptr;

      if (D->SpecializationArgs) {
        // This is an explicit specialization or a partial specialization.
        IsVariableTemplateSpecialization = true;
        IsPartialSpecialization = TemplateParams->size() > 0;
      } else {
        // This is a template declaration.
        IsVariableTemplate = true;

        // Only C++1y supports variable templates (N3651).
        CxxSema.Diag(D->IdDcl->getLoc(),
                     clang::diag::warn_cxx11_compat_variable_template);
      }
    }
  } else {
    // Check that we can declare a member specialization here.
    if (!TemplateParamLists.empty() && IsMemberSpecialization &&
        CxxSema.CheckTemplateDeclScope(CxxScope, TemplateParamLists.back()))
      return nullptr;
    assert((Invalid || !D->Template) && "should have a '[]' for this decl");
  }

  if (IsVariableTemplateSpecialization) {
    if (actOnVarTemplateSpecialziation(SemaRef, InitialScope, D, Owner,
                                       PreviousDC, TemplateId, TInfo,
                                       TemplateParams, SC,
                                       IsPartialSpecialization,
                                       IsClassMember)) {
      return nullptr;
    }

    NewVD = cast<clang::VarDecl>(D->Cxx);
  } else {
    NewVD = clang::VarDecl::Create(Context.CxxAST, Owner,
                                    Loc, Loc, Id, TInfo->getType(),
                                    TInfo,
                                    getDefaultVariableStorageClass(SemaRef));
    if (IsClassMember)
      NewVD->setAccess(clang::AS_public);
  }



  // If this is supposed to be a variable template, create it as such.
  if (IsVariableTemplate) {
    NewTemplate =
        clang::VarTemplateDecl::Create(Context.CxxAST, Owner, D->IdDcl->getLoc(),
                                        Name, TemplateParams, NewVD);
    if (IsClassMember) {
      NewTemplate->setAccess(clang::AS_public);
    }
    Owner->addDecl(NewTemplate);
    NewVD->setDescribedVarTemplate(NewTemplate);
    NewTemplate->setDeclContext(PreviousDC);
    NewTemplate->setLexicalDeclContext(Owner);
    NewVD->setLexicalDeclContext(Owner);
    SemaRef.setDeclForDeclaration(D, NewTemplate);
  }

  if (D->ScopeSpec.isSet())
    NewVD->setQualifierInfo(
        D->ScopeSpec.getWithLocInContext(Context.CxxAST));
  if (!NewTemplate && !IsVariableTemplateSpecialization)
    SemaRef.setDeclForDeclaration(D, NewVD);

  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);

  if (!IsVariableTemplateSpecialization) {
    if (!isa<clang::LinkageSpecDecl>(D->Cxx)) {
      if (!NewTemplate) {
        Owner->addDecl(NewVD);
      }
      NewVD->setDeclContext(PreviousDC);
      NewVD->setLexicalDeclContext(Owner);
    }
  }

  SC = NewVD->getStorageClass();
  if (Invalid) {
    NewVD->setInvalidDecl();
    if (NewTemplate)
      NewTemplate->setInvalidDecl();
  }

  if (Owner->isRecord() && !SemaRef.getCurClangDeclContext()->isRecord()) {
    // This is an out-of-line definition of a static data member.
    switch (SC) {
    case clang::SC_None:
      break;
    case clang::SC_Static:
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
        clang::diag::err_static_out_of_line)
        // FIXME: this may need a better error message here.
        << clang::FixItHint::CreateRemoval(
              clang::SourceRange(NewVD->getSourceRange()));
      break;
    case clang::SC_Auto:
    case clang::SC_Register:
    case clang::SC_Extern:
      // [dcl.stc] p2: The auto or register specifiers shall be applied only
      // to names of variables declared in a block or to function parameters.
      // [dcl.stc] p6: The extern specifier cannot be used in the declaration
      // of class members

      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
            clang::diag::err_storage_class_for_static_member)
        << clang::FixItHint::CreateRemoval(NewVD->getSourceRange());
      break;
    case clang::SC_PrivateExtern:
      llvm_unreachable("C storage class in gold!");
    }
  }

  if (SC == clang::SC_Static && SemaRef.getCurClangDeclContext()->isRecord()) {
    if (const clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(Owner)) {
      // Walk up the enclosing DeclContexts to check for any that are
      // incompatible with static data members.
      const clang::DeclContext *FunctionOrMethod = nullptr;
      const clang::CXXRecordDecl *AnonStruct = nullptr;

      // Ignore these checks for fragments
      // FIXME: Duplicated in SemaInject for CheckInjectedVarDecl
      const clang::DeclContext *ImmediateParent = RD->getParent();
      if (!ImmediateParent || !ImmediateParent->isFragment()) {
        for (clang::DeclContext *Ctxt = Owner; Ctxt; Ctxt = Ctxt->getParent()) {
          if (Ctxt->isFunctionOrMethod()) {
            FunctionOrMethod = Ctxt;
            break;
          }
          const clang::CXXRecordDecl *ParentDecl = dyn_cast<clang::CXXRecordDecl>(Ctxt);
          if (ParentDecl && !ParentDecl->getDeclName()) {
            AnonStruct = ParentDecl;
            break;
          }
        }
      }

      if (FunctionOrMethod) {
        // C++ [class.static.data]p5: A local class shall not have static data
        // members.
        SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                 clang::diag::err_static_data_member_not_allowed_in_local_class)
              << Name << RD->getDeclName() << RD->getTagKind();
      } else if (AnonStruct) {
        // C++ [class.static.data]p4: Unnamed classes and classes contained
        // directly or indirectly within unnamed classes shall not contain
        // static data members.
        SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                 clang::diag::err_static_data_member_not_allowed_in_anon_struct)
          << Name << AnonStruct->getTagKind();
        Invalid = true;
      } else if (RD->isUnion()) {
        // C++98 [class.union]p1: If a union contains a static data member,
        // the program is ill-formed. C++11 drops this restriction.
        SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                     clang::diag::warn_cxx98_compat_static_data_member_in_union)
                                  << Name;
      }
    }
  }

  if (IsLocalExternDecl)
    NewVD->setLocalExternDecl();

  // Checking Thread storage class specification.
  auto TSCSpec = NewVD->getTSCSpec();
  if (TSCSpec != clang::TSCS_unspecified) {
    // C++11 [dcl.stc]p4:
    //   When thread_local is applied to a variable of block scope the
    //   storage-class-specifier static is implied if it does not appear
    //   explicitly.
    // Core issue: 'static' is not implied if the variable is declared
    //   'extern'.
    if (NewVD->hasLocalStorage() &&
        !Owner->isFunctionOrMethod())
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
           clang::diag::err_thread_non_global)
        << "thread_local";
    else if (!Context.CxxAST.getTargetInfo().isTLSSupported()) {
      llvm_unreachable("Invalid context.");
    }
  }


  // C99 6.7.4p3
  //   An inline definition of a function with external linkage shall
  //   not contain a definition of a modifiable object with static or
  //   thread storage duration...
  // We only apply this when the function is required to be defined
  // elsewhere, i.e. when the function is not 'extern inline'.  Note
  // that a local variable with thread storage duration still has to
  // be marked 'static'.  Also note that it's possible to get these
  // semantics in C++ using __attribute__((gnu_inline)).
  if (SC == clang::SC_Static
      && SemaRef.getCurClangScope()->getFnParent() != nullptr &&
      !NewVD->getType().isConstQualified()) {
    clang::FunctionDecl *CurFD = SemaRef.getCxxSema().getCurFunctionDecl();
    if (CurFD && isFunctionDefinitionDiscarded(SemaRef, CurFD)) {
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                               clang::diag::warn_static_local_in_extern_inline);
      SemaRef.getCxxSema().MaybeSuggestAddingStaticToDecl(CurFD);
    }
  }

  // Doing additional verification.
  if (!Owner->isRecord()
       && CxxScope->getFnParent() == nullptr) {
    // C99 6.9p2: The storage-class specifiers auto and register shall not
    // appear in the declaration specifiers in an external declaration.
    // Global Register+Asm is a GNU extension we support.
    if (SC == clang::SC_Auto || (SC == clang::SC_Register)) {
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                                clang::diag::err_typecheck_sclass_fscope);
      NewVD->setInvalidDecl();
      return nullptr;
    }
  }

  // Ensure that dllimport globals without explicit storage class are treated as
  // extern. The storage class is set above using parsed attributes. Now we can
  // check the VarDecl itself.
  assert(!NewVD->hasAttr<clang::DLLImportAttr>() ||
         NewVD->getAttr<clang::DLLImportAttr>()->isInherited() ||
         NewVD->isStaticDataMember() ||
         NewVD->getStorageClass() != clang::SC_None);

  // Find the shadowed declaration before filtering for scope.
  clang::NamedDecl *ShadowedDecl = D->ScopeSpec.isEmpty()
                  ? SemaRef.getCxxSema().getShadowedDeclaration(NewVD, Previous)
                  : nullptr;

  CxxSema.FilterLookupForScope(Previous, PreviousDC, CxxScope,
                               shouldConsiderLinkage(NewVD),
                               D->ScopeSpec.isNotEmpty() ||
                               IsMemberSpecialization ||
                               IsVariableTemplateSpecialization);

  // Check whether the previous declaration is in the same block scope. This
  // affects whether we merge types with it, per C++11 [dcl.array]p3.
  if (NewVD->isLocalVarDecl() && NewVD->hasExternalStorage())
    NewVD->setPreviousDeclInSameBlockScope(
        Previous.isSingleResult() && !Previous.isShadowed() &&
        CxxSema.isDeclInScope(Previous.getFoundDecl(), OriginalDC,
                              SemaRef.getCurClangScope(), false));

  // If this is an explicit specialization of a static data member, check it.
  if (IsMemberSpecialization && !NewVD->isInvalidDecl() &&
      SemaRef.getCxxSema().CheckMemberSpecialization(NewVD, Previous))
    NewVD->setInvalidDecl();

  // Merge the decl with the existing one if appropriate.
  if (!Previous.empty()) {
    if (Previous.isSingleResult() &&
        isa<clang::FieldDecl>(Previous.getFoundDecl()) &&
        D->ScopeSpec.isSet()) {
      // The user tried to define a non-static data member
      // out-of-line (C++ [dcl.meaning]p1).
      CxxSema.Diag(NewVD->getLocation(),
                   clang::diag::err_nonstatic_member_out_of_line)
                   << D->ScopeSpec.getRange();
      Previous.clear();
      NewVD->setInvalidDecl();
    }
  } else if (D->ScopeSpec.isSet()) {
    // No previous declaration in the qualifying scope.
    CxxSema.Diag(D->IdDcl->getLoc(), clang::diag::err_no_member)
                 << Name
                 << SemaRef.getCxxSema().computeDeclContext(D->ScopeSpec, true)
                 << D->ScopeSpec.getRange();
    NewVD->setInvalidDecl();
  }

  if (!IsVariableTemplateSpecialization)
    D->IsRedeclaration = CxxSema.CheckVariableDeclaration(NewVD, Previous);

  if (NewTemplate) {
    clang::VarTemplateDecl *PrevVarTemplate =
        NewVD->getPreviousDecl()
            ? NewVD->getPreviousDecl()->getDescribedVarTemplate()
            : nullptr;

    // Check the template parameter list of this declaration, possibly
    // merging in the template parameter list from the previous variable
    // template declaration.
    if (CxxSema.CheckTemplateParameterList(
            TemplateParams,
            PrevVarTemplate ? PrevVarTemplate->getTemplateParameters()
                            : nullptr,
            // TODO: Figure out if I'm usnig the right DC here.
            (D->ScopeSpec.isSet() && PreviousDC && PreviousDC->isRecord() &&
              PreviousDC->isDependentContext())
                ? clang::Sema::TPC_ClassTemplateMember
                : clang::Sema::TPC_VarTemplate))
      NewVD->setInvalidDecl();

    // If we are providing an explicit specialization of a static variable
    // template, make a note of that.
    if (PrevVarTemplate &&
        PrevVarTemplate->getInstantiatedFromMemberTemplate())
      PrevVarTemplate->setMemberSpecialization();
  }

  // Diagnose shadowed variables iff this isn't a redeclaration.
  if (ShadowedDecl && !D->IsRedeclaration)
    CxxSema.CheckShadow(NewVD, ShadowedDecl, Previous);

  // If this is the first declaration of an extern C variable, update
  // the map of such variables.
  if (NewVD->isFirstDecl() && !NewVD->isInvalidDecl() &&
      isIncompleteDeclExternC(SemaRef, NewVD))
   CxxSema.RegisterLocallyScopedExternCDecl(NewVD, SemaRef.getCurClangScope());

  if (NewVD->isStaticLocal()) {
    clang::MangleNumberingContext *MCtx;
    clang::Decl *ManglingContextDecl;
    std::tie(MCtx, ManglingContextDecl) =
                 CxxSema.getCurrentMangleNumberContext(NewVD->getDeclContext());
    if (MCtx) {
      Context.CxxAST.setManglingNumber(NewVD, MCtx->getManglingNumber(
                     NewVD,
                     getMSManglingNumber(CxxSema.getLangOpts(),
                                         SemaRef.getCurClangScope())));
      Context.CxxAST.setStaticLocalNumber(NewVD,
                                          MCtx->getStaticLocalNumber(NewVD));
    }
  }

  // Special handling of variable named 'main'.
  if (Name.getAsIdentifierInfo() && Name.getAsIdentifierInfo()->isStr("main") &&
      NewVD->getDeclContext()->getRedeclContext()->isTranslationUnit() &&
      !CxxSema.getLangOpts().Freestanding
      && !NewVD->getDescribedVarTemplate()) {

    // C++ [basic.start.main]p3
    // A program that declares a variable main at global scope is ill-formed.
    CxxSema.Diag(D->IdDcl->getLoc(),
                 clang::diag::err_main_global_variable);
  }

  if (Invalid) {
    NewVD->setInvalidDecl();
    if (NewTemplate)
      NewTemplate->setInvalidDecl();
  }


  // If we have any template parameter lists that don't directly belong to
  // the variable (matching the scope specifier), store them.
  unsigned VDTemplateParamLists = TemplateParams ? 1 : 0;
  if (TemplateParamLists.size() > VDTemplateParamLists)
    NewVD->setTemplateParameterListsInfo(
      Context.CxxAST, TemplateParamLists.drop_back(VDTemplateParamLists));

  if (NewVD->isInlineSpecified()) {
    if (SemaRef.getCurClangDeclContext()->isFunctionOrMethod()) {
      // 'inline' is not allowed on block scope variable declaration.
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
          clang::diag::err_inline_declaration_block_scope) << Name
          << clang::FixItHint::CreateRemoval(NewVD->getSourceRange());
    } else {
      SemaRef.getCxxSema().Diag(D->IdDcl->getLoc(),
                                clang::diag::warn_cxx14_compat_inline_variable);
      NewVD->setInlineSpecified();
    }
  }


  if (D->IsRedeclaration && !Previous.empty()) {
    clang::NamedDecl *Prev = Previous.getRepresentativeDecl();
    checkDLLAttributeRedeclaration(CxxSema, Prev, NewVD, IsMemberSpecialization,
                                   D->declaresFunction() && !D->IsDeclOnly);
  }

  if (NewTemplate) {
    if (NewVD->isInvalidDecl())
      NewTemplate->setInvalidDecl();
    CxxSema.ActOnDocumentableDecl(NewTemplate);
  }

  if (IsMemberSpecialization && !NewVD->isInvalidDecl())
    CxxSema.CompleteMemberSpecialization(NewVD, Previous);
  // Labeling our catch variable.
  if (D->declaresCatchVariable())
    NewVD->setExceptionVariable(true);

  return NewVD;
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

  SemaRef.setDeclForDeclaration(D, TypeAlias);
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
  if (D->isDeclaredWithinClass()) {
    SemaRef.Diags.Report(D->IdDcl->getLoc(),
                         clang::diag::err_namespace_alias_within_class);
    return nullptr;
  }

  clang::Decl *PossibleNs = SemaRef.getDeclFromExpr(NsExpr, D->Init->getLoc());
  if (!PossibleNs)
    return nullptr;
  assert(isa<clang::NamedDecl>(PossibleNs) && "invalid namespace");

  clang::NamedDecl *Ns
    = cast<clang::NamedDecl>(PossibleNs)->getUnderlyingDecl();

  clang::DeclContext *Owner = D->getOwningDeclContext();
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
  SemaRef.setDeclForDeclaration(D, NsAD);

  // Nested name specifiers are looked up by clang, so we need to convince
  // the clang lookup that this namespace actually exists.
  SemaRef.getCurClangScope()->AddDecl(NsAD);
  SemaRef.getCxxSema().IdResolver->AddDecl(NsAD);
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

clang::Decl *Elaborator::elaborateTemplateAliasOrVariable(Declaration *D) {
  bool InClass = D->isDeclaredWithinClass();

  // Checking if we are a nested template decl/class.
  clang::MultiTemplateParamsArg MTP = D->TemplateParamStorage;

  // This REQUIRES that we have specified type for now. But in order to do this
  // correctly we can't construct a templated type right off the bat we need
  // to figure out
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
    VDecl->setImplicitlyInline();
    clang::DeclarationName DeclName = IdInfo;
    clang::VarTemplateDecl *VTD = clang::VarTemplateDecl::Create(
                                                      Context.CxxAST,
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
    SemaRef.setDeclForDeclaration(D, VTD);
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

    SemaRef.setDeclForDeclaration(D, TypeAlias);
    // Only the type alias is fully elaborated at this point in time.
    if (InClass) {
      D->Cxx->setAccess(clang::AS_public);
    }
    D->CurrentPhase = Phase::Initialization;
  }
  // Making sure that if we are in side of a class/record we explicitly set the
  // current access to public.
  if (D->Cxx) {
    elaborateAttributes(D);
  }
  return D->Cxx;
}

clang::Decl *Elaborator::elaborateParameterDecl(Declaration *D) {
  // Get type information.
  clang::DeclContext *Owner = D->getOwningDeclContext();
  if (isa<clang::LinkageSpecDecl>(Owner)) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_invalid_extern_c)
                         << /*a parameter*/1;
    return nullptr;
  }
  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->TypeDcl);
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
  SemaRef.setDeclForDeclaration(D, P);
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  return P;
}

clang::Decl *Elaborator::elaborateTemplateParamDecl(Declaration *D) {

  clang::DeclContext *Owner = D->getOwningDeclContext();
  if (isa<clang::LinkageSpecDecl>(Owner)) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_invalid_extern_c)
                         << /*a template parameter*/2;
    return nullptr;
  }

  ExprElaborator TypeElab(Context, SemaRef);
  const Syntax *TySyntax = D->TypeDcl->getTyExpr();
  bool IsPack = false;

  // Checking to see if we are a parameter pack
  // This technically doesn't have a spot in the AST only as a boolean
  // associated with template parameters.
  if (auto Call = dyn_cast<CallSyntax>(TySyntax)) {
    if (auto AtomName = dyn_cast<AtomSyntax>(Call->getCallee())) {
      if (AtomName->hasToken(tok::Ellipsis)) {
        assert(Call->getNumArguments() == 1
               && "Invalid number of arguments to ellipsis within AST");
        TySyntax = Call->getArgument(0);
        IsPack = true;
        D->EllipsisLoc = AtomName->getLoc();
      }
    }
  }

  clang::Expr *TypeExpr = TypeElab.elaborateExpr(TySyntax);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->IdDcl->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                          D->TypeDcl->getLoc());
  if (!TInfo)
    return nullptr;

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->IdDcl->getLoc();

  // This is a template type or template template parameter decl.
  if (TInfo->getType()->getAs<clang::CppxKindType>()) {
    using TemplateTemplate = clang::TemplateTemplateParmDecl;
    using TemplateType = clang::TemplateTypeParmDecl;
    clang::Decl *ReturnedDecl = nullptr;
    if (D->Template)
      ReturnedDecl = TemplateTemplate::Create(Context.CxxAST, Owner, Loc, 0,
                                        0, /*Pack=*/IsPack, Id,
                                        D->TemplateParamStorage.front());
    else
      ReturnedDecl = TemplateType::Create(Context.CxxAST, Owner, Loc, Loc, 0, 0,
                                    Id, /*TypenameKW=*/true, /*Pack=*/IsPack);

    D->CurrentPhase = Phase::Typing;
    SemaRef.setDeclForDeclaration(D, ReturnedDecl);
    return ReturnedDecl;
  }

  // The depth and position of the parameter will be set later.
  auto *NTTP =
    clang::NonTypeTemplateParmDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                           0, 0, Id, TInfo->getType(),
                                           /*Pack=*/IsPack, TInfo);
  SemaRef.setDeclForDeclaration(D, NTTP);
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
  assert(D && (D->getId() || D->declaresUsingDirective())
         && "Early elaboration of unidentified declaration");
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
  // Sema::SaveAndRestoreClangDCAndScopeRAII DCAndScopeSaver(SemaRef);
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



  // We saved the parameter scope while elaborating this function's type,
  // so push it on before we enter the function scope.
  FunctionDeclarator *FnDecl = D->FunctionDcl;
  Sema::ResumeScopeRAII FnDclScope(SemaRef, FnDecl->getScope(),
                                   FnDecl->getScope()->getConcreteTerm());

  Declaration *CurrentDeclaration = SemaRef.getCurrentDecl();
  // Entering clang scope. for function definition.
  SemaRef.enterClangScope(clang::Scope::FnScope |clang::Scope::DeclScope |
                          clang::Scope::CompoundStmtScope);

  clang::Decl *FuncDecl = SemaRef.getCxxSema().ActOnStartOfFunctionDef(
                                            SemaRef.getCurClangScope(), D->Cxx);


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

  llvm::APSInt Size = llvm::APSInt::get(List->getNumInits());
  clang::Expr *Init = List->getInit(0);
  if (clang::InitListExpr *Sublist = dyn_cast<clang::InitListExpr>(Init)) {
    return Ctx.getConstantArrayType(
      buildImplicitArrayType(Ctx, SemaRef, Sublist), Size, /*SizeExpr=*/nullptr,
      clang::ArrayType::Normal, 0);
  }

  clang::QualType EltTy = Init->getType();
  return Ctx.getConstantArrayType(EltTy, Size, /*SizeExpr=*/nullptr,
                                  clang::ArrayType::Normal, 0);
}

void Elaborator::elaborateVariableInit(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  if (!D->Cxx)
    return;

  Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);
  clang::Expr *InitExpr = nullptr;
  clang::VarDecl *VD = nullptr;
  Sema::DeclInitializationScope ClangInitScope(SemaRef, D);
  bool NeedsConstEvaluation = false;
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
  if (!InitExpr) {
    SemaRef.Diags.Report(VD->getLocation(),
                        clang::diag::err_failed_to_translate_expr);
    return;
  }

  // Perform auto deduction.
  if (VD->getType()->isUndeducedType()) {
    clang::QualType Ty;

    // Certain macros must be deduced manually.
    if (const MacroSyntax *InitM = dyn_cast<MacroSyntax>(D->Init)) {
      if (!isa<CallSyntax>(InitM->getCall())) {

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
    // FIXME: This needs to verify that if we are a static member that we don't
    // have an initializer, unless we are a static constant, or inline.
    if (D->isDeclaredWithinClass() && !VD->isInlineSpecified()
        && (!VD->getType().isConstant(Context.CxxAST) && !VD->isConstexpr())) {
      SemaRef.Diags.Report(D->IdDcl->getLoc(),
                          clang::diag::err_in_class_initializer_non_const);
      return;
    }
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

clang::Decl *Elaborator::elaborateField(Declaration *D,
                                        clang::TypeSourceInfo *TInfo) {
  clang::Decl *Ctxt = SemaRef.getCurrentDecl()->Cxx;
  clang::CXXRecordDecl *Owner = dyn_cast<clang::CXXRecordDecl>(Ctxt);
  // Get the type of the entity.
  if(!Owner) {
    // This occurs when we are within an extern "C" decl
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_invalid_extern_c)
                         << /* a member */0;
    return nullptr;
  }

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
  SemaRef.setDeclForDeclaration(D, Field);
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
  SemaRef.setDeclForDeclaration(D, ECD);
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
  if (SemaRef.getCurrentScope()->hasDeclaration(S))
    return nullptr;

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
  if (D->declaresTagDef() || D->declaresForwardRecordDecl()) {
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
  elaborateDecl(D);
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
      ExceptionSpecExpr = ExprElab.elaborateAttrExpr(Call->getArgument(0));
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
      clang::Expr *ExceptionSpecExpr = ExprElab.elaborateAttrExpr(TySyntax);
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
  assert(!isa<CallSyntax>(S) && "extern attribute in invalid syntax");
  if (isa<AtomSyntax>(S)) {
    if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(D->Cxx)) {
      FD->setStorageClass(clang::StorageClass::SC_Extern);
    } else if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(D->Cxx)) {
      VD->setStorageClass(clang::StorageClass::SC_Extern);
    } else if (clang::VarTemplateDecl *VTD = dyn_cast<clang::VarTemplateDecl>(D->Cxx)) {
      VTD->getTemplatedDecl()->setStorageClass(clang::StorageClass::SC_Extern);
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
  } else if (auto *VTD = dyn_cast<clang::VarTemplateDecl>(D->Cxx)) {
    VTD->getTemplatedDecl()->setAccess(AS);
  } else if (auto *TATD = dyn_cast<clang::TypeAliasTemplateDecl>(D->Cxx))
    TATD->getTemplatedDecl()->setAccess(AS);
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
      if (isa<clang::CXXConversionDecl>(D->Cxx)) {
        // TODO: Verify that this displays something meaningful
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_conv_function_not_member)
                            << clang::SourceRange(S->getLoc(), S->getLoc())
                  << clang::SourceRange(D->IdDcl->getLoc(), D->IdDcl->getLoc());
        D->Cxx->setInvalidDecl();
        return;
      }
      if (isa<clang::CXXConstructorDecl>(D->Cxx)
          || isa<clang::CXXDestructorDecl>(D->Cxx)) {
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_invalid_attribute_for_decl)
                             << "static"
                             << "function, variable, or class member";
        return;
      }
      MD->setStorageClass(clang::StorageClass::SC_Static);
    } else if(auto *VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(D->Cxx)) {
      VTSD->setStorageClass(clang::StorageClass::SC_Static);
    } else if(auto *VD = dyn_cast<clang::VarTemplateDecl>(D->Cxx)) {
      VD->getTemplatedDecl()->setStorageClass(clang::StorageClass::SC_Static);
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
    } else if(auto *VD = dyn_cast<clang::VarTemplateDecl>(D->Cxx)) {
      VD->getTemplatedDecl()->setStorageClass(clang::StorageClass::SC_Static);
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
  clang::Expr *BitsExpr = Elab.elaborateConstexprAttrExpr(
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
    clang::Expr *AlignmentExpr = Elab.elaborateConstexprAttrExpr(
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
    clang::Expr *Res = Elab.elaborateConstexprAttrExpr(ArgOrId);
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
  std::string AttrName;
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
  if (Tokenization == SemaRef.OperatorThrowII)
    return FOK_Throw;
  if (Tokenization == SemaRef.OperatorCaretII)
    return FOK_Caret;
  if (Tokenization == SemaRef.OperatorDotCaretII)
    return FOK_DotCaret;
  return FOK_Unknown;
}

FusedOpKind getFusedOpKind(Sema &SemaRef, const CallSyntax *S) {
  if (!isa<AtomSyntax>(S->getCallee()))
    return FOK_Unknown;
  return getFusedOpKind(SemaRef, cast<AtomSyntax>(S->getCallee())->getSpelling());
}

} // namespace gold
