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


#include "clang/AST/DeclarationName.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/ParsedAttr.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Sema/CXXFieldCollector.h"

#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/AST/DeclCXX.h"

namespace gold {

enum AttrFormat {
  AF_Invalid,
  AF_Call,
  AF_Name
};

static AttrFormat checkAttrFormatAndName(const Syntax *Attr, llvm::StringRef &Name) {

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

/// locateValidAttribute
/// This is a privately used template function, basucally taking the place of me
/// duplicating the same thing over and over again for each type of attribute
/// I need to find.
///
/// OnAttr is a callable with signature
///       bool (const Syntax *)
///   Returing true means you've located one of the possible attributes.
///   This should also be used to set the correct value of any possible return
///   value.
///     true = Matched an attribute.
///     false = didn't match an attribute.
///
/// IsSameAttr is a callable used to detect duplicates of the same attribute class.
///   This is the same as OnAttr with the exception that it doesn't set
///   the current desired attribute when it's true.
///   Signature:
///       bool (const Syntax *)
///     true = we hvae a failure (meaning that we have a duplicate attribute)
///     false = we don't have an attribute of the same class.
///
/// OnDuplicate is a callable with signature
///       void (const syntax *first, const Syntax *Duplicate)
///   This is used to create error messages.
template<typename OnAttr, typename IsSameAttr, typename OnDuplicate>
static bool locateValidAttribute(Attributes& UnprocessedAttributes,
    OnAttr OnAttribute, IsSameAttr CheckAttr, OnDuplicate OnDup) {

  auto Iter = UnprocessedAttributes.begin();
  auto End = UnprocessedAttributes.end();
  for (;Iter != End; ++Iter) {
    if (OnAttribute(*Iter)) {
      break;
    }
  }
  const Syntax *AttribSpec = nullptr;
  bool didFail = false;
  if(Iter != End) {
    AttribSpec = *Iter;
    UnprocessedAttributes.erase(Iter);
    Iter = UnprocessedAttributes.begin();
    End = UnprocessedAttributes.end();
    for (;Iter != End; ++Iter) {
      if (CheckAttr(*Iter)) {
        OnDup(AttribSpec, *Iter);
        didFail = true;
      }
    }
  }
  return didFail;
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
  SemaRef.getCxxSema().ActOnTranslationUnitScope(Scope);
  SemaRef.getCxxSema().Initialize();

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

static clang::Decl*
processCXXRecordDecl(Elaborator& Elab, SyntaxContext& Context, Sema& SemaRef,
                     Declaration *D) {
  using namespace clang;
  D->CurrentPhase = Phase::Typing;

  bool Template = D->declaresTemplateType();
  const Syntax *TemplParams;
  
  // Checking if we are a nested template decl/class.
  bool WithinClass = SemaRef.getCurrentScope()->getKind() == SK_Class;
  
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  llvm::SmallVector<clang::TemplateParameterList *, 4> TPLStorage;
  MultiTemplateParamsArg MTP;
  Declarator *TemplateDeclarator = nullptr;
  Sema::OptionalScopeRAII TemplateScope(SemaRef);
  Sema::OptioanlClangScopeRAII ClangTemplateScope(SemaRef);
  if (Template) {
    // TODO: In the future change this so we can enter multiple template scopes
    // and track their depth
    TemplateDeclarator = D->getFirstTemplateDeclarator();
    TemplParams = TemplateDeclarator->Data.TemplateInfo.Params;
    
    // Entering initial template scope.
    TemplateScope.Init(SK_Template, TemplParams);
    clang::SourceLocation Loc = TemplParams->getLoc();
    ClangTemplateScope.Init(clang::Scope::TemplateParamScope, Loc);
    BuildTemplateParams(Context, SemaRef, TemplParams, TemplateParamDecls);
    clang::TemplateParameterList *TPL
      = SemaRef.getCxxSema().ActOnTemplateParameterList(
      /*unsigned Depth*/SemaRef.computeTemplateDepth(), /*ExportLoc*/Loc,
      /*TemplateLoc*/Loc, /*LAngleLoc*/Loc, TemplateParamDecls,
      /*RAngleLoc*/Loc, /*RequiresClause*/nullptr);
    TPLStorage.push_back(TPL);
    MTP = TPLStorage;
  }

  bool IsOwned = false;
  bool IsDependent = false;
  CXXScopeSpec SS;
  TypeResult UnderlyingType;
  AccessSpecifier AS = AS_none;
  if (WithinClass)
    AS = AS_public;
  clang::SourceLocation IdLoc = D->Decl->getLoc();

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
  Elab.elaborateAttributes(D);
  Sema::ScopeRAII ClassBodyScope(SemaRef, SK_Class, D->Op, &D->SavedScope);
  SemaRef.getCurrentScope()->Entity = D;
  

  Sema::ClangScopeRAII ClangClassScopeBody(SemaRef,
    clang::Scope::ClassScope | clang::Scope::DeclScope, D->Init->getLoc());
    

  // Need to do this before the next step because this is actually pushed on to
  // the stack a by the next function called.
  SemaRef.getCxxSema().ActOnTagStartDefinition(
    SemaRef.getCurClangScope(), D->Cxx);

  // This keeps the declContext working correctly.
  Sema::DeclContextRAII DCTracking(SemaRef, D, true);

  // This keeps track of class nesting.
  Sema::ElaboratingClassDefRAII ClsElabState(SemaRef, D,
    !SemaRef.isElaboratingClass());

  Elab.elaborateTypeBody(D, ClsDecl);
  // Attempt to figure out if any nested elaboration is actually required.
  // If not then we can proceed as normal.  
  auto const* MacroRoot = dyn_cast<MacroSyntax>(D->Init);
  auto const* BodyArray = dyn_cast<ArraySyntax>(MacroRoot->getBlock());

  // Handling possible base classes.
  if (const CallSyntax *ClsKwCall
                        = dyn_cast<CallSyntax>(MacroRoot->getCall())) {
    processBaseSpecifiers(Elab, SemaRef, Context, D, ClsDecl, ClsKwCall);
  }

  SemaRef.getCxxSema().ActOnStartCXXMemberDeclarations(
    SemaRef.getCurClangScope(), ClsDecl, SourceLocation(), true,
    SourceLocation());
  
  // Since all declarations have already been added, we don't need to do another
  // Reordering scan.
  // Doing possible delaying of member declaration/initialziation.
  for (const Syntax *SS : BodyArray->children()) {
    Elab.delayElaborateDeclType(SS);
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

  // TODO: Insert any late elaboration of members goes here!
  clang::Decl *TempDeclPtr = ClsDecl;
  SemaRef.getCxxSema().ActOnTagFinishDefinition(SemaRef.getCurClangScope(),
    TempDeclPtr, SourceRange());
  
  return ClsDecl;
}

static clang::Decl *processNamespaceDecl(Elaborator& Elab,
                                         SyntaxContext& Context,
                                         Sema& SemaRef, Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  using namespace clang;

  // Create and enter a namespace scope.
  clang::Scope *NSScope = SemaRef.enterClangScope(clang::Scope::DeclScope);

  gold::Sema::ScopeRAII GoldScopeRAII(SemaRef, SK_Namespace, D->Init);

  // FIXME: keep track of nested namespaces?

  clang::UsingDirectiveDecl *UD = nullptr;
  clang::AttributeFactory Attrs;
  clang::ParsedAttributes ParsedAttrs(Attrs);
  clang::Decl *NSDecl = SemaRef.getCxxSema().ActOnStartNamespaceDef(
    NSScope, SourceLocation(), D->Decl->getLoc(),
    D->Decl->getLoc(), D->getId(), D->Decl->getLoc(), ParsedAttrs, UD);
  D->Cxx = NSDecl;
  Elab.elaborateAttributes(D);
  SemaRef.pushDecl(D);

  const MacroSyntax *NSMacro = cast<MacroSyntax>(D->Init);
  const ArraySyntax *NSBody = cast<ArraySyntax>(NSMacro->getBlock());

  // Keep track of the location of the last syntax, as a closing location.
  clang::SourceLocation LastLoc;
  for (const Syntax *S : NSBody->children()) {
    Elaborator(Context, SemaRef).elaborateDeclSyntax(S);
    LastLoc = S->getLoc();
  }

  gold::Scope *NamespaceRep = SemaRef.getCurrentScope();
  SemaRef.getCxxSema().ActOnFinishNamespaceDef(NSDecl, LastLoc);
  SemaRef.leaveClangScope(LastLoc);
  SemaRef.popDecl();

  DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  CppxNamespaceDecl *Wrapper =
    CppxNamespaceDecl::Create(Context.CxxAST, Owner, D->Decl->getLoc(),
                              D->getId(), cast<clang::NamespaceDecl>(NSDecl),
                              NamespaceRep);
  D->Cxx = Wrapper;

  // FIXME: We should be returning a DeclGroupPtr to the NSDecl grouped
  // with the implicit UsingDecl, UD.
  return NSDecl;
}

clang::Decl *Elaborator::elaborateDecl(Declaration *D) {
  if (phaseOf(D) > Phase::Identification)
    return D->Cxx;

  if (phaseOf(D) != Phase::Identification)
    // Adding this here skips some errors.
    return D->Cxx;

  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  if (D->declaresRecord())
    return processCXXRecordDecl(*this, Context, SemaRef, D);
  if (D->declaresNamespace())
    return processNamespaceDecl(*this, Context, SemaRef, D);
  if (D->declaresFunction())
    return elaborateFunctionDecl(D);
  return elaborateVariableDecl(D);

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
  const ListSyntax *ParamList = cast<ListSyntax>(FnDecl->Data.ParamInfo.Params);
  Scope *ParamScope = FnDecl->Data.ParamInfo.ConstructedScope;
  bool Variadic = FnDecl->Data.ParamInfo.VariadicParam;

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
static bool getOperatorDeclarationName(SyntaxContext &Context, Sema &SemaRef,
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
  if (InClass) {
    clang::Decl *ScopesDecl = SemaRef.getDeclForScope();
    if (!ScopesDecl)
      llvm_unreachable("Invalid declaration for scope.");
    RD = dyn_cast<clang::CXXRecordDecl>(ScopesDecl);
    if(!RD)
      llvm_unreachable("Improperly created scope. Class scope doesn't contain "
          "declaration.");
  }

  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParamDecls;
  if (Template) {
    SemaRef.enterScope(SK_Template, TemplParams);
    BuildTemplateParams(Context, SemaRef, TemplParams, TemplateParamDecls);
  }

  // Elaborate the return type.
  ExprElaborator TypeElab(Context, SemaRef);
  clang::Expr *TypeExpr = TypeElab.elaborateTypeExpr(D->Decl);
  if (!TypeExpr) {
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                   TypeExpr, D->Decl->getLoc());
  if (!TInfo)
    return nullptr;

  // Changing the name to use the clang name if it's an operator.
  clang::DeclarationName Name;
  if (D->OpInfo) {
    const clang::FunctionProtoType *FPT = cast<clang::FunctionProtoType>(
                                                 TInfo->getType().getTypePtr());
    if (!FPT) {
      return nullptr;
    }

    if (getOperatorDeclarationName(Context, SemaRef, D->OpInfo, InClass,
                                   FPT->getNumParams(), 
                                   D->Decl->Data.Id->getLoc(), Name)) {
      return nullptr;
    }
  } else {
    Name = D->getId();
  }
  clang::SourceLocation Loc = D->Op->getLoc();
  clang::FunctionDecl *FD = nullptr;
  clang::DeclarationNameInfo DNI;
  DNI.setName(Name);
  DNI.setLoc(D->Decl->Data.Id->getLoc());
  // Doing additional name computations.
  if (InClass) {
    clang::QualType RecordTy = Context.CxxAST.getTypeDeclType(RD);
    clang::CanQualType Ty = Context.CxxAST.getCanonicalType(RecordTy);
    if (D->getId()->isStr("constructor")) {
      Name = Context.CxxAST.DeclarationNames.getCXXConstructorName(Ty);
    } else if (D->getId()->isStr("destructor")) {
      Name = Context.CxxAST.DeclarationNames.getCXXDestructorName(Ty);
    }
  } 
  clang::Scope *ClangScope = SemaRef.getCurClangScope();
  while ((ClangScope->getFlags() & clang::Scope::DeclScope) == 0 ||
         (ClangScope->getFlags() & clang::Scope::TemplateParamScope) != 0)
    ClangScope = ClangScope->getParent();

  clang::LookupResult Previous(SemaRef.getCxxSema(), DNI,
                               clang::Sema::LookupOrdinaryName,
                           SemaRef.getCxxSema().forRedeclarationInCurContext());
  SemaRef.getCxxSema().LookupName(Previous, ClangScope, false);
  
  if(InClass) {
    const clang::FunctionType *FT = cast<clang::FunctionType>(TInfo->getType());
    DNI.setName(Name);
    DNI.setLoc(D->Op->getLoc());
    if (D->getId()->isStr("constructor")) {
      if (FT->getReturnType() != Context.CxxAST.VoidTy) {
        SemaRef.Diags.Report(D->Decl->getLoc(),
                          clang::diag::err_invalid_return_type_for_ctor_or_dtor)
                          << 0;
        return nullptr;
      }

      DNI.setName(Name);
      clang::CXXConstructorDecl* CtorDecl = nullptr;
      clang::ExplicitSpecifier ES(nullptr,
                                        clang::ExplicitSpecKind::ResolvedFalse);
      FD = CtorDecl = clang::CXXConstructorDecl::Create(Context.CxxAST, RD,
                                                        Loc, DNI, 
                                                        clang::QualType(),
                                                        nullptr, ES, false,
                                                        false,
                                     clang::ConstexprSpecKind::CSK_unspecified);
      CtorDecl->setImplicit(false);
      CtorDecl->setDefaulted(false);
      CtorDecl->setBody(nullptr);
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
        SemaRef.Diags.Report(D->Decl->getLoc(),
                          clang::diag::err_invalid_return_type_for_ctor_or_dtor)
                             << 1;
        return nullptr;
      }
      clang::DeclarationNameInfo DNI2(Name, D->Decl->getLoc());
      clang::CXXDestructorDecl* DtorDecl = nullptr;
      clang::ExplicitSpecifier ES(nullptr, clang::ExplicitSpecKind::ResolvedFalse);
      FD = DtorDecl = clang::CXXDestructorDecl::Create(Context.CxxAST, RD, Loc,
          DNI2, clang::QualType(), nullptr, false, false,
          clang::ConstexprSpecKind::CSK_unspecified);
      DtorDecl->setImplicit(false);
      DtorDecl->setDefaulted(false);
      DtorDecl->setBody(nullptr);
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
        SemaRef.Diags.Report(D->Op->getLoc(),
                             clang::diag::err_destructor_with_params);
        return nullptr;
      }
      auto QT = Context.CxxAST.getFunctionType(Context.CxxAST.VoidTy,
        clang::None, EPI);
      DtorDecl->setType(QT);

    } else {
      clang::StorageClass SC = clang::SC_None;
      FD = clang::CXXMethodDecl::Create(Context.CxxAST, RD, Loc, DNI,
                                        TInfo->getType(), TInfo,
                                        SC, /*isInline*/true,
                                      clang::ConstexprSpecKind::CSK_unspecified,
                                        Loc);
    }
    FD->setAccess(clang::AS_public);

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
    if (InClass)
      FTD->setAccess(clang::AS_public);
  }
  SemaRef.getCxxSema().getImplicitCodeSegOrSectionAttrForFunction(FD, D->Init);
  // Update the function parameters.
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getFunctionParameters(D, Params);
  FD->setParams(Params);
  D->Cxx = FD;
  {
    // We have previously exited this scope that was created during type
    // elaboration.
    Declarator *FnDecl = getFunctionDeclarator(D->Decl);
    Sema::ResumeScopeRAII FuncScope(SemaRef,
                                    FnDecl->Data.ParamInfo.ConstructedScope,
                     FnDecl->Data.ParamInfo.ConstructedScope->getConcreteTerm(), 
                                    /*PopOnExit=*/false);
    elaborateAttributes(D);
  }
  // Add the declaration and update bindings.
  if (!Template && !D->declaresConstructor()) {
    Owner->addDecl(FD);
  }
  if (D->declaresConstructor()) {
    clang::CXXConstructorDecl* CtorDecl = cast<clang::CXXConstructorDecl>(D->Cxx);
    SemaRef.getCxxSema().PushOnScopeChains(CtorDecl, SemaRef.getCurClangScope());
    SemaRef.getCxxSema().CheckConstructor(CtorDecl);

  }
  if (InClass) {
    SemaRef.getCxxSema().FilterLookupForScope(Previous, Owner, ClangScope,
                                              false, false);
  } else {
    SemaRef.getCxxSema().FilterLookupForScope(Previous, Owner, ClangScope,
                                              true, true);
  }
  SemaRef.getCxxSema().CheckFunctionDeclaration(SemaRef.getCurClangScope(),
                                                FD, Previous, true);
  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(FD)) {

    checkCXXMethodDecl(MD);
    SemaRef.getCxxSema().CheckOverrideControl(MD);
  }
  if (FD->isInvalidDecl()) {
    return nullptr;
  }
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
  if (SemaRef.getCurrentScope()->isParameterScope())
    return elaborateParameterDecl(D);

  if (SemaRef.getCurrentScope()->isTemplateScope())
    return elaborateTemplateParamDecl(D);
  
  // This is specifically for processing a special kind of declarator.
  if (Declarator *TemplateDeclarator = D->getFirstTemplateDeclarator()) {
    return elaborateTemplateAliasOrVariable(D, TemplateDeclarator);
  }

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
    if (TypeExpr->getType()->isNamespaceType()) {
      llvm_unreachable("Namespace variables not implemented yet");
    }
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

  if (SemaRef.getCurrentScope()->isClassScope()) {
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
  // if (compluteVariableStorageClassSpec(SemaRef, D, SC)) {
  //   return nullptr;
  // }
  // Create the variable and add it to it's owning context.
  clang::VarDecl *VD = clang::VarDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                              Id, TInfo->getType(), TInfo, SC);
  Owner->addDecl(VD);
  D->Cxx = VD;
  D->CurrentPhase = Phase::Typing;
  elaborateAttributes(D);
  return VD;
}

clang::Decl *Elaborator::elaborateTypeAlias(Declaration *D) {
  if (!D->Init) {
    SemaRef.Diags.Report(D->Op->getLoc(), clang::diag::err_expected_type);
    return nullptr;
  }

  // Elaborating RHS
  ExprElaborator Elab(Context, SemaRef);
  clang::Expr *InitTyExpr = Elab.elaborateExpr(D->Init);
  // TODO: Create an error message and verify that the result type of the expression
  // is cppx kind type.
  
  // We are doing complete evaluation at this point because all types need to be made
  // available by phase 3.
  // if (clang::NamespaceDecl *NsD = InitExpr.dyn_cast<clang::NamespaceDecl *>()) {
  //   SemaRef.Diags.Report(D->Decl->getLoc(), clang::diag::err_unknown_typename)
  //         << NsD->getName();
  //   return nullptr;
  // }
  // if (!InitExpr.is<clang::Expr *>()) {
  //   // FIXME: This needs an error message indicating that we have a nemespace
  //   // instead of a type or a value instead of a type? something like that.
  //   // However this could be easilly repaired in the event that the resulting
  //   // expression had a derived namespace type of some kind.
  //   llvm_unreachable("Received part of an expression that isn't an alias.");
  // }
  clang::ParsedType PT;
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InitTyExpr,
                                                             D->Init->getLoc());
  if (!TInfo) {
    return nullptr;
  }
  PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
  D->CurrentPhase = Phase::Initialization;



  clang::IdentifierInfo *IdInfo = D->getId();
  clang::UnqualifiedId Id;
  Id.setIdentifier(IdInfo, D->Decl->getLoc());
  clang::SourceLocation Loc = D->Op->getLoc();


  // TODO: In future create a means for us to correctly construct any template
  // parameters.
  clang::MultiTemplateParamsArg MTP;

  // Constructing the type alias on the way out because I need to correctly
  // construct it's internal type, before continuing oward.
  clang::TypeResult TR(PT);
  clang::Decl *TypeAlias = SemaRef.getCxxSema().ActOnAliasDeclaration(
      SemaRef.getCurClangScope(), clang::AS_public, MTP, Loc, Id,
      clang::ParsedAttributesView(), TR, nullptr);
  // Attempting to add attribute support for this kind of declaration.
  
  bool InClass = SemaRef.getCurrentScope()->getKind() == SK_Class;
  if (InClass) {
    TypeAlias->setAccess(clang::AS_public);
  }

  D->Cxx = TypeAlias;
  elaborateAttributes(D);
  return TypeAlias;
}

clang::Decl *Elaborator::elaborateTemplateAliasOrVariable(Declaration *D,
    Declarator *TemplateParams) {
  llvm_unreachable("This isn't implemented yet!");
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
    SemaRef.Diags.Report(D->Op->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  // clang::Expr *TemplateTy = TypeExpr.get<clang::Expr*>();
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(TypeExpr,
                                                             D->Decl->getLoc());
  if (!TInfo) {
    return nullptr;
  }

  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc = D->Op->getLoc();

  // This is a template type parameter decl.
  if (TInfo->getType()->getAs<clang::CppxKindType>()) {
    auto *TTPD =
      clang::TemplateTypeParmDecl::Create(Context.CxxAST, Owner, Loc, Loc, 0, 0,
                              Id, /*TypenameKW=*/true, /*ParameterPack=*/false);
    D->Cxx = TTPD;
    D->CurrentPhase = Phase::Typing;
    return TTPD;
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
  if (SemaRef.getCurrentScope()->isTemplateScope())
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

  if (D->declaresConstructor()) {
    llvm::SmallVector<clang::CXXCtorInitializer*, 32> Initializers;
    SemaRef.getCxxSema().ActOnMemInitializers(D->Cxx, clang::SourceLocation(),
        Initializers, false);
  }

  if (SemaRef.checkForRedefinition<clang::FunctionDecl>(D))
    return;

  // We saved the parameter scope while elaborating this function's type,
  // so push it on before we enter the function scope.
  Declarator *FnDecl = getFunctionDeclarator(D);
  bool IsTemplate = D->declaresFunctionTemplate();

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

  // Leave the parameter scope.
  SemaRef.popScope();
  
  // Leave the template scope
  if (IsTemplate)
    SemaRef.popScope();
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

  // FIXME: If we synthesize initializers, this might need to happen before that
  if (SemaRef.checkForRedefinition<clang::VarDecl>(D))
    return;

  clang::VarDecl *VD = cast<clang::VarDecl>(D->Cxx);
  
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

  // Doing
  // ExprElaborator::Expression Init;
  clang::Expr *InitExpr = nullptr;
  ExprElaborator ExprElab(Context, SemaRef);
  if (D->declaresInlineInitializedStaticVarDecl()) {
    Sema::ExprEvalRAII Ctxt(SemaRef,
      clang::Sema::ExpressionEvaluationContext::PotentiallyEvaluated);
    InitExpr = ExprElab.elaborateExpr(D->Init);
  } else {
    InitExpr = ExprElab.elaborateExpr(D->Init);
  }

  // Make sure the initializer was elaborated as a type. 
  // FIXME: This will need to change in order to properly address how identifiers
  // are being processed. Because we will need to change things so that we can
  // properly identify member types, and member templates, template template
  // parameters etc.
  // if (Init.is<clang::TypeSourceInfo *>()) {
  //   llvm::errs() << "Expected expression.\n";
  //   return;
  // }


  
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
  auto const* BodyArray = dyn_cast<ArraySyntax>(MacroRoot->getBlock());
  assert(BodyArray && "Invalid AST structure Expected array structure.");
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

static Declarator *buildTypeRoot(const Syntax *S, Declarator *Next) {
  Declarator *D = new Declarator(DK_Type, Next);
  D->Call = S;
  D->Data.Type = S;
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

// Check if the last parameter in a function is of type 'args'.
static bool lastParamIsVarArgs(Sema &SemaRef, const CallSyntax *S) {
  if (!S->getNumArguments())
    return false;
  std::size_t N = S->getNumArguments() - 1;
  const CallSyntax *LastParam = dyn_cast_or_null<CallSyntax>(S->getArgument(N));
  if (!LastParam)
    return false;

  FusedOpKind Op = getFusedOpKind(SemaRef, LastParam);
  if (Op != FOK_Colon)
    return false;

  // We might have something like `varargs : args` or just `:args`
  N = LastParam->getNumArguments() - 1;
  if (N > 1)
    return false;

  if (const AtomSyntax *Ty = dyn_cast<AtomSyntax>(LastParam->getArgument(N)))
    return Ty->Tok.hasKind(tok::ArgsKeyword);

  return false;
}

static Declarator *buildFunctionDeclarator(Sema &SemaRef, const CallSyntax *S,
                                           Declarator *Next) {
  // FIXME: Store the parameter list.
  Declarator *D = new Declarator(DK_Function, Next);
  D->Call = S;
  D->Data.ParamInfo.Params = S->getArguments();
  D->Data.ParamInfo.TemplateParams = nullptr;
  D->Data.ParamInfo.TemplateScope = nullptr;
  D->Data.ParamInfo.ConstructedScope = nullptr;
  D->Data.ParamInfo.VariadicParam = lastParamIsVarArgs(SemaRef, S);
  return D;
}

static Declarator *buildFunctionDeclarator(Sema &SemaRef,
                                           const CallSyntax *S,
                                           const ElemSyntax *T,
                                           Declarator *Next) {
  Declarator *D = new Declarator(DK_Function, Next);
  D->Call = S;
  D->Data.ParamInfo.Params = S->getArguments();
  D->Data.ParamInfo.TemplateParams = T->getArguments();
  D->Data.ParamInfo.TemplateScope = nullptr;
  D->Data.ParamInfo.ConstructedScope = nullptr;
  D->Data.ParamInfo.VariadicParam = lastParamIsVarArgs(SemaRef, S);
  return D;
}

static Declarator *buildTemplatedName(const ElemSyntax *Call,
                                      Declarator *Next) {
  Declarator *D = new Declarator(DK_TemplateType, Next);
  D->Call = Call;
  D->Data.TemplateInfo.Params = Call->getArguments();
  return D;
}

static Declarator *buildErrorDeclarator(const ErrorSyntax *S, Declarator *Next) {
  Declarator *D = new Declarator(DK_Error, Next);
  D->Call = S;
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
    if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
      return buildTypeDeclarator(Atom, Next);
    }
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
          Next = buildTypeRoot(Call->getArgument(1), Next);
          return makeDeclarator(SemaRef, Call->getArgument(0), Next);
        }

        // Otherwise, rhs is just a literal type.
        return makeDeclarator(SemaRef, Call->getArgument(0),
                              buildTypeDeclarator(Call, Next));

      } else if (Callee->getSpelling() == "operator'.'") {
        // This is also a possible type declaration because it's a nested type
        // declaration.
        return buildTypeExpression(Call, Next);
      } else if (Callee->getSpelling() == "operator'in'") {
        return makeDeclarator(SemaRef, Call->getArgument(0), Next);
      }

      // Otherwise, this appears to be a function declarator.
      Declarator *Temp = makeDeclarator(SemaRef, Callee,
                                        buildFunctionDeclarator(SemaRef,
                                                                Call, Next));
      Temp->recordAttributes(Call);
      return Temp;

    } else if (const ElemSyntax *Callee
               = dyn_cast<ElemSyntax>(Call->getCallee())) {
      // We have a template parameter list here, so build the
      // function declarator accordingly.
      Declarator *Temp = makeDeclarator(SemaRef, Callee->getObject(),
                                        buildFunctionDeclarator(SemaRef,
                                                                Call,
                                                                Callee,
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
  } else if(const ErrorSyntax *Err = dyn_cast<ErrorSyntax>(S)) {
    return buildErrorDeclarator(Err, nullptr);
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

Declaration *Elaborator::identifyDecl(const Syntax *S) {
  // Keep track of whether or not this is an operator'=' call.
  bool OperatorEquals = false;
  // if (decomposeElaboratableDecl(S, OperatorEquals, Decl, Init)) {
  // Declarations only appear in calls.
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    const Syntax *Decl;
    const Syntax *Init;
    if (const auto *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
      llvm::StringRef Op = Callee->getToken().getSpelling();
      // Need to figure out if this is a declaration or expression?
      // Unpack the declarator.
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
            return nullptr;
        }

        // Explicilty ignoring declarations that use x.y.
        if (const CallSyntax *InnerCallOp = dyn_cast<CallSyntax>(Decl))
          if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(
                                                      InnerCallOp->getCallee()))
            if (Atom->getSpelling() == "operator'.'")
              return nullptr;
        // Attempting to verify if this is an ElemSyntax.
        if (isa<ElemSyntax>(Decl))
          // This can't be a declaration, because would need to say":type" after
          // the name to be considered a template type.
          return nullptr;

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
      } else if (Op == "operator'[]'") {

        // We always return false here because any type alias must indicate
        // have a ": type" after it or it's not a template alias.
        return nullptr;
      } else {
        // Syntactically, this is not a declaration.
        return nullptr;
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
      
      // FIXME: Handling the subscript operator is actually much more dificult 
      // then previously stated. It might be best to simply label this as a 
      // possible declaration, and wait until later to figure if it is. In the
      // event that it isn't a declaration then we can consider it an expression
      // and try and evaluate it as such. 

      // There is an alternative, and that is that we check for declarations
      // within the argument lists.
      // 

      // Try to build a declarator for the declaration.
      Declarator *Dcl = makeDeclarator(SemaRef, Decl);
      if (!Dcl)
        return nullptr;

      // Parameters can only be declared as x, x:T, or :T. The full range
      // of declarator syntax is not supported.
      //
      // FIXME: Emit an error instead of a diagnostic.
      if (SemaRef.getCurrentScope()->isParameterScope() && !Dcl->isIdentifier()) {
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

        if (OperatorEquals && !CurScope->findDecl(Id).empty())
          return nullptr;
      }
        
      // Create a declaration for this node.
      //
      // FIXME: Do a better job managing memory.
      Declaration *ParentDecl = SemaRef.getCurrentDecl();
      
      // This was created to prevent duplicate elaboration failure which could
      // previously result in an error.
      if (SemaRef.getCurrentScope()->hasDeclaration(S)) {
        return nullptr;
      }

      if (Dcl->getKind() != DK_Identifier) {
        llvm_unreachable("Invalid declarator structure.");
      }
      OpInfoBase const *OpInfo = nullptr;
      if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Dcl->Data.Id)) {
        clang::StringRef Nm = Name->getSpelling();
        if (Nm.find('"') != llvm::StringRef::npos) {
          if (Nm.startswith("operator\"")) {
            OpInfo = SemaRef.OpInfo.getOpInfo(Nm);
            if (!OpInfo) {
              SemaRef.Diags.Report(Name->getLoc(),
                                  clang::diag::err_operator_cannot_be_overloaded)
                                  << Nm;
              return nullptr;
            }
          } else if (Nm.startswith("literal\"")) {
            llvm_unreachable("User defined literal declarations not "
                              "imeplemented yet.");
          } else if (Nm.startswith("conversion\"")) {
            llvm_unreachable("User defined conversion declarations not "
                              "imeplemented yet.");
          }
        }
      } else {
        llvm_unreachable("Unknown declarator name declaration!\n");
      }
      Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, Init);
      TheDecl->Id = Id;
      TheDecl->OpInfo = OpInfo;

      if (OpInfo && !TheDecl->declaresFunction()) {
        llvm_unreachable("INvalid operator id not implemented yet!\n");
      }

      // Getting information that's necessary in order to correctly restore
      // a declaration's context during early elaboration.
      TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
      TheDecl->ParentDecl = SemaRef.getCurrentDecl();
      TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
      TheDecl->ScopeForDecl = SemaRef.getCurrentScope();

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
      TheDecl->CurrentPhase = Phase::Identification;
      return TheDecl;
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

  return nullptr;
}

bool Elaborator::delayElaborateDeclType(const Syntax *S) {
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
  if (D->declaresRecord()) {
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
      elaborateFunctionDecl(D);
    } else {
      // Attempting to delay method decl/def combos
      delayElaborateMethodDecl(D);
      WasDelayed = true;
    }
    if (D->decalaresFunctionDef()) {
      delayElaborateMethodDef(D);
      WasDelayed = true;
    }
    return WasDelayed;
  }
  // TODO: Nested nested types are going to be kind of confusing, but I just
  // need to better understand how they are created, and what scope they are
  // located within.

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
  processCXXRecordDecl(*this, Context, SemaRef, D);
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
    SemaRef.getCxxSema().ActOnReenterTemplateScope(SemaRef.getCurClangScope(),
        Class.TagOrTemplate->Cxx);

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
  elaborateFunctionDecl(Method.D);
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

static void applyESIToFunctionType(SyntaxContext &Context, Sema &SemaRef,
                                   clang::FunctionDecl *FD,
                       const clang::FunctionProtoType::ExceptionSpecInfo &ESI) {
  clang::QualType ExceptionAdjustedTy
      = Context.CxxAST.getFunctionTypeWithExceptionSpec(FD->getType(), ESI);
  llvm::SmallVector<clang::ParmVarDecl *, 10> Parameters(
                              FD->parameters().begin(), FD->parameters().end());
  clang::TypeSourceInfo *TInfo = BuildFunctionTypeLoc(Context.CxxAST,
                                                      ExceptionAdjustedTy,
                                                      FD->getBeginLoc(),
                                                      clang::SourceLocation(),
                                                      clang::SourceLocation(),
                                                      clang::SourceRange(),
                                                      FD->getEndLoc(),
                                                      Parameters);

  FD->setType(ExceptionAdjustedTy);
  FD->setTypeSourceInfo(TInfo);
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
    Declarator *FnDecl = getFunctionDeclarator(Method.D);
    if (!FnDecl)
      return;
    // Attempting to push the scope for the current function onto the stack
    // This helps with lookup during evaluation exception specification.
    Sema::ResumeScopeRAII TempScope(SemaRef,
                                    FnDecl->Data.ParamInfo.ConstructedScope,
                     FnDecl->Data.ParamInfo.ConstructedScope->getConcreteTerm(),
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
  auto Iter = DeclaratorWithAttrs->UnprocessedAttributes->begin();
  auto End = DeclaratorWithAttrs->UnprocessedAttributes->end();
  for (; Iter != End; ++Iter) {
    const AtomSyntax *Name;
    if ((Name = dyn_cast<AtomSyntax>(*Iter))) {
      auto Handler = SemaRef.AttrHandlerMap.find(Name->getSpelling());
      if (Handler == SemaRef.AttrHandlerMap.end()) {
        elaborateUnknownAttr(D, *Iter, Status);
      } else {
        Handler->second(*this, D, *Iter, Status);
      }
      continue;
    }
    if (const CallSyntax* Call = dyn_cast<CallSyntax>(*Iter)) {
      if ((Name = dyn_cast<AtomSyntax>(Call->getCallee()))) {
        auto Handler = SemaRef.AttrHandlerMap.find(Name->getSpelling());
        if (Handler == SemaRef.AttrHandlerMap.end()) {
          elaborateUnknownAttr(D, *Iter, Status);
        } else {
          Handler->second(*this, D, *Iter, Status);
        }
        continue;
      }
    }
    elaborateUnknownAttr(D, *Iter, Status);
  }
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
  llvm_unreachable(" not implemented");
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
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateCarriesDependencyAttr(Declaration *D, const Syntax *S,
                                                AttrStatus &Status) {
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateDeprecatedAttr(Declaration *D, const Syntax *S,
                                         AttrStatus &Status) {
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateMaybeUnusedAttr(Declaration *D, const Syntax *S,
                                          AttrStatus &Status) {
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateNoDiscardAttr(Declaration *D, const Syntax *S,
                                        AttrStatus &Status) {
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateNoReturnAttr(Declaration *D, const Syntax *S,
                                       AttrStatus &Status) {
  llvm_unreachable(" not implemented");
}

void Elaborator::elaborateUnknownAttr(Declaration *D, const Syntax *S,
                                      AttrStatus &Status) {
  // TODO: I may need to stick some kind of warning here possibly.
  // llvm_unreachable("Processing of unknown attributes isn't implemented yet.");
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
  if (Tokenization == SemaRef.OperatorArrayBracketsII) 
    return FOK_Array;
  return FOK_Unknown;
}

FusedOpKind getFusedOpKind(Sema &SemaRef, const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->getCallee()));
  return getFusedOpKind(SemaRef, cast<AtomSyntax>(S->getCallee())->getSpelling());
}

} // namespace gold
