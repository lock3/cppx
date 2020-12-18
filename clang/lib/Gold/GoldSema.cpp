//===- GoldSema.cpp - Semantic Analysis of Gold ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the gold::Sema class, which performs semantic analysis
//  for the Gold language.
//
//===----------------------------------------------------------------------===//
#include "clang/Gold/GoldSema.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/ClangToGoldDeclBuilder.h"
#include "clang/Gold/GoldConstexprASTElaborator.h"
#include "clang/Gold/GoldDependentExprTransformer.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldIdentifierResolver.h"
#include "clang/Gold/GoldPartialExpr.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldTemplateCallback.h"

#include <algorithm>

namespace gold {

using namespace llvm;

const llvm::StringMap<clang::QualType> Sema::createBuiltinTypeList() {
  return {
    {"void", Context.CxxAST.VoidTy},
    {"bool", Context.CxxAST.BoolTy},
    {"null_t", Context.CxxAST.NullPtrTy},
    { "auto", Context.CxxAST.getAutoDeductType()},

    // character
    {"cchar", Context.CxxAST.CharTy},
    {"char", Context.CxxAST.getIntTypeForBitwidth(8, false)},
    {"char8", Context.CxxAST.getIntTypeForBitwidth(8, false)},
    {"char16", Context.CxxAST.getIntTypeForBitwidth(16, false)},
    {"char32", Context.CxxAST.getIntTypeForBitwidth(32, false)},

    // Signed integers
    {"int", Context.CxxAST.IntTy},
    {"int8", Context.CxxAST.getIntTypeForBitwidth(8, true)},
    {"int16", Context.CxxAST.getIntTypeForBitwidth(16, true)},
    {"int32", Context.CxxAST.getIntTypeForBitwidth(32, true)},
    {"int64", Context.CxxAST.getIntTypeForBitwidth(64, true)},
    {"int128", Context.CxxAST.getIntTypeForBitwidth(128, true)},

    // unsigned integers.
    {"uint", Context.CxxAST.UnsignedIntTy},
    {"uint8", Context.CxxAST.getIntTypeForBitwidth(8, false)},
    {"uint16", Context.CxxAST.getIntTypeForBitwidth(16, false)},
    {"uint32", Context.CxxAST.getIntTypeForBitwidth(32, false)},
    {"uint64", Context.CxxAST.getIntTypeForBitwidth(64, false)},
    {"uint128", Context.CxxAST.getIntTypeForBitwidth(128, false)},

    // Floating point numbers
    {"float", Context.CxxAST.FloatTy},
    {"float16", Context.CxxAST.HalfTy},
    {"float32", Context.CxxAST.getRealTypeForBitwidth(32, /*IEEE=*/false)},
    {"float64", Context.CxxAST.getRealTypeForBitwidth(64, /*IEEE=*/false)},
    {"float128", Context.CxxAST.getRealTypeForBitwidth(128, /*IEEE=*/true)},
    {"double", Context.CxxAST.DoubleTy},

    // type of a type.
    { "type", Context.CxxAST.CppxKindTy },
    { "namespace", Context.CxxAST.CppxNamespaceTy },
    { "args", Context.CxxAST.CppxArgsTy },
    { "__builtin_va_list", createVaListType() },
  };
}

static Sema::StringToAttrHandlerMap buildAttributeMapping() {
#define ATTR_HANDLER_LAMBDA(METHOD_NAME)\
    [](Elaborator &E, Declaration *D, const Syntax *Attr,\
       AttrStatus &Status) {\
      E.METHOD_NAME(D, Attr, Status);\
    }
  return {
    { "constexpr", ATTR_HANDLER_LAMBDA(elaborateConstExprAttr) },

    { "inline", ATTR_HANDLER_LAMBDA(elaborateInlineAttr) },


    { "public", ATTR_HANDLER_LAMBDA(elaborateAccessSpecifierAttr) },
    { "private", ATTR_HANDLER_LAMBDA(elaborateAccessSpecifierAttr) },
    { "protected", ATTR_HANDLER_LAMBDA(elaborateAccessSpecifierAttr) },

    { "noexcept", ATTR_HANDLER_LAMBDA(elaborateExceptionSpecAttr) },
    { "throw", ATTR_HANDLER_LAMBDA(elaborateExceptionSpecAttr) },

    // Should auto be allowed here?
    { "static", ATTR_HANDLER_LAMBDA(elaborateStaticAttr) },

    { "thread_local", ATTR_HANDLER_LAMBDA(elaborateThreadLocalAttr) },

    { "extern", ATTR_HANDLER_LAMBDA(elaborateExternAttr) },

    { "explicit", ATTR_HANDLER_LAMBDA(elaborateExplicitAttr) },

    // Poylmorphic attributes.
    { "virtual", ATTR_HANDLER_LAMBDA(elaborateVirtualAttr) },
    { "override", ATTR_HANDLER_LAMBDA(elaborateOverrideAttr) },
    { "final", ATTR_HANDLER_LAMBDA(elaborateFinalAttr) },

    // This only apply to methods.
    { "const", ATTR_HANDLER_LAMBDA(elaborateConstAttr) },
    { "ref", ATTR_HANDLER_LAMBDA(elaborateRefQualifierAttr) },
    { "rref", ATTR_HANDLER_LAMBDA(elaborateRefQualifierAttr) },

    { "bits", ATTR_HANDLER_LAMBDA(elaborateBitsAttr)},
    { "alignas", ATTR_HANDLER_LAMBDA(elaborateAlignAsAttr)},

    // Error Attributes.
    { "mutable", ATTR_HANDLER_LAMBDA(elaborateAttributeError)},
    // NOTE: All other attributes are handled by elaborateSystemAttributes
  };
#undef ATTR_HANDLER_LAMBDA
}

static clang::IdentifierInfo *makeOpII(clang::ASTContext &Ctx, const std::string &OpText) {
  return &Ctx.Idents.get("operator'" + OpText + "'");
}
static llvm::DenseMap<clang::IdentifierInfo *, clang::tok::TokenKind>
buildFoldOpLookup(clang::ASTContext &Ctx) {
  return {
    { makeOpII(Ctx, "&& ..."), clang::tok::ampamp },
    { makeOpII(Ctx, "... &&"), clang::tok::ampamp },
    { makeOpII(Ctx, "... ,"),  clang::tok::comma },
    { makeOpII(Ctx, "|| ..."), clang::tok::pipepipe },
    { makeOpII(Ctx, "... ||"), clang::tok::pipepipe },
    { makeOpII(Ctx, ", ..."),  clang::tok::comma },

    // Binary fold operators.
    { makeOpII(Ctx, "+ ... +"), clang::tok::plus },
    { makeOpII(Ctx, "- ... -"), clang::tok::minus },
    { makeOpII(Ctx, "* ... *"), clang::tok::star },
    { makeOpII(Ctx, "/ ... /"), clang::tok::slash },
    { makeOpII(Ctx, "% ... %"), clang::tok::percent },
    { makeOpII(Ctx, "^ ... ^"), clang::tok::caret },
    { makeOpII(Ctx, "& ... &"), clang::tok::amp },
    { makeOpII(Ctx, "| ... |"), clang::tok::pipe },
    { makeOpII(Ctx, "= ... ="), clang::tok::equal },
    { makeOpII(Ctx, "< ... <"), clang::tok::less },
    { makeOpII(Ctx, "> ... >"), clang::tok::greater },
    { makeOpII(Ctx, "<< ... <<"), clang::tok::lessless },
    { makeOpII(Ctx, ">> ... >>"), clang::tok::greatergreater },

    { makeOpII(Ctx, "+= ... +="), clang::tok::plusequal },
    { makeOpII(Ctx, "-= ... -="), clang::tok::minusequal },
    { makeOpII(Ctx, "*= ... *="), clang::tok::starequal },
    { makeOpII(Ctx, "/= ... /="), clang::tok::slashequal },
    { makeOpII(Ctx, "%= ... %="), clang::tok::percentequal },
    { makeOpII(Ctx, "^= ... ^="), clang::tok::caretequal },
    { makeOpII(Ctx, "&= ... &="), clang::tok::ampequal },
    { makeOpII(Ctx, "|= ... |="), clang::tok::pipeequal },
    { makeOpII(Ctx, "<= ... <="), clang::tok::lessequal },
    { makeOpII(Ctx, ">= ... >="), clang::tok::greaterequal },
    { makeOpII(Ctx, "<<= ... <<="), clang::tok::lesslessequal },
    { makeOpII(Ctx, ">>= ... >>="), clang::tok::greatergreaterequal },
    { makeOpII(Ctx, "== ... =="), clang::tok::equalequal },
    { makeOpII(Ctx, "<> ... <>"), clang::tok::exclaimequal },
    { makeOpII(Ctx, "&& ... &&"), clang::tok::ampamp },
    { makeOpII(Ctx, "|| ... ||"), clang::tok::pipepipe },

    // Things we don't support for fold operators.
    // , .* ->*
  };
}
template<typename Derived, typename Base, typename Del>
static std::unique_ptr<Derived, Del> 
static_unique_ptr_cast(std::unique_ptr<Base, Del>&& p )
{
    auto d = static_cast<Derived *>(p.release());
    return std::unique_ptr<Derived, Del>(d, std::move(p.get_deleter()));
}

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : CxxSema(CxxSema), CurrentDecl(), Context(Context),
    Diags(Context.CxxAST.getSourceManager().getDiagnostics()),
    IdResolver(new (Context) IdentifierResolver(*this)),
    OperatorColonII(&Context.CxxAST.Idents.get("operator':'")),
    OperatorArrowII(&Context.CxxAST.Idents.get("operator'->'")),
    OperatorExclaimII(&Context.CxxAST.Idents.get("operator'!'")),
    OperatorEqualsII(&Context.CxxAST.Idents.get("operator'='")),
    OperatorIfII(&Context.CxxAST.Idents.get("operator'if'")),
    OperatorElseII(&Context.CxxAST.Idents.get("operator'else'")),
    OperatorReturnII(&Context.CxxAST.Idents.get("operator'return'")),
    OperatorReturnsII(&Context.CxxAST.Idents.get("operator'returns'")),
    OperatorDotII(&Context.CxxAST.Idents.get("operator'.'")),
    OperatorForII(&Context.CxxAST.Idents.get("operator'for'")),
    OperatorWhileII(&Context.CxxAST.Idents.get("operator'while'")),
    OperatorInII(&Context.CxxAST.Idents.get("operator'in'")),
    OperatorDotDotII(&Context.CxxAST.Idents.get("operator'..'")),
    OperatorConstII(&Context.CxxAST.Idents.get("operator'const'")),
    OperatorRefII(&Context.CxxAST.Idents.get("operator'ref'")),
    OperatorRRefII(&Context.CxxAST.Idents.get("operator'rref'")),
    OperatorBracketsII(&Context.CxxAST.Idents.get("operator'[]'")),
    OperatorParensII(&Context.CxxAST.Idents.get("operator'()'")),
    OperatorThrowII(&Context.CxxAST.Idents.get("operator'throw'")),
    OperatorCaretII(&Context.CxxAST.Idents.get("operator'^'")),
    OperatorDotCaretII(&Context.CxxAST.Idents.get("operator'.^'")),
    OperatorAmpersandII(&Context.CxxAST.Idents.get("operator'&'")),
    ConstructorII(&Context.CxxAST.Idents.get("constructor")),
    DestructorII(&Context.CxxAST.Idents.get("destructor")),
    VaStartII(&Context.CxxAST.Idents.get("__builtin_va_start")),
    VaEndII(&Context.CxxAST.Idents.get("__builtin_va_end")),
    VaCopyII(&Context.CxxAST.Idents.get("__builtin_va_copy")),
    VaArgII(&Context.CxxAST.Idents.get("__builtin_va_arg")),
    DefaultCharTy(Context.CxxAST.getIntTypeForBitwidth(8, false)),
    BuiltinTypes(createBuiltinTypeList()),
    OpInfo(Context.CxxAST),
    AttrHandlerMap(buildAttributeMapping()),
    FoldOpToClangKind(buildFoldOpLookup(Context.CxxAST))
{
  CxxSema.CurScope = nullptr;
}

Sema::~Sema() {

  assert(ScopeStack.empty() && "Scope stack is not empty.");
  delete getCurClangScope();
  CxxSema.CurScope = nullptr;
}

clang::QualType Sema::createVaListType() {
  return Context.CxxAST.getBuiltinVaListType();
}

bool Sema::accessSpecifierIsValidInScope() {
  return ScopeStack.back() && ScopeStack.back()->getKind() == SK_Class;
}

Scope *Sema::getCurrentScope() const {
  if (ScopeStack.empty())
    return nullptr;
  return ScopeStack.back();
}

void Sema::pushScope(Scope *S) {
  assert(S && "Invalid scope");
  // FIXME: The scope should self-describe itself. We can't rely on
  // the existence of Clang structures at the time we push a scope.
  // if (S->isDeclarationScope())
  //   CxxSema.PushFunctionScope();
  ScopeStack.push_back(S);
}

Scope *Sema::popScope() {
  Scope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

void Sema::enterScope(ScopeKind K, const Syntax *S, Declaration *D) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new Scope(K, S, getCurrentScope(), D));
}

void Sema::leaveScope(const Syntax *S) {
  if (getCurrentScope()->getConcreteTerm() != S) {
    llvm::outs() << "Actual Expected term = ";
    getCurrentScope()->getConcreteTerm()->dump();
    llvm::outs() << "Given Expected term = ";
    S->dump();
  }
  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

Scope *Sema::saveScope(const Syntax *S) {

  assert(getCurrentScope()->getConcreteTerm() == S);
  // FIXME: Queue the scope for subsequent deletion?
  Scope *Scope = getCurrentScope();
  popScope();
  return Scope;
}

clang::DeclContext *Sema::getCurrentCxxDeclContext() {
  return CurrentDecl->getCxxContext();
}

clang::DeclContext *Sema::getCurClangDeclContext() const {
  return CxxSema.CurContext;
}

void Sema::setClangDeclContext(clang::DeclContext *DC) {
  CxxSema.CurContext = DC;
}

void Sema::verifyMatchingDeclarationAndDeclContext() const {
#ifndef NDEBUG
  assert(CurrentDecl && "gold decl context not set");
  assert(CxxSema.CurContext && "clang decl context not set");
  assert(CurrentDecl->Cxx && "Decl context not set.");
  clang::DeclContext *GDC = dyn_cast<clang::DeclContext>(CurrentDecl->Cxx);
  if (!GDC) {
    llvm::errs() << "Non decl context declaration\n";
    CurrentDecl->Cxx->dump();
  }
  assert(GDC && "declaration context cannot be a decl context");
  if (CxxSema.CurContext != GDC) {
    llvm::errs() << "Current clang Decl context = \n";
    CxxSema.CurContext->dumpDeclContext();
    llvm::errs() << "Current Gold Decl Context = \n";
    GDC->dumpDeclContext();
  }
  assert((CxxSema.CurContext == GDC)
         && "Declaration context doesn't match.");
#endif
}

void Sema::restoreDeclContext(Declaration *D) {
  CurrentDecl = D;
  getCxxSema().CurContext = clang::Decl::castToDeclContext(D->Cxx);
}

void Sema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);
  CurrentDecl = D;
  if (D->Cxx)
    getCxxSema().CurContext = clang::Decl::castToDeclContext(D->Cxx);
}

void Sema::setCurrentDecl(Declaration *D) {
  CurrentDecl = D;
}

void Sema::popDecl() {
  CurrentDecl = CurrentDecl->getOwner();
  getCxxSema().CurContext = CurrentDecl ?
    clang::Decl::castToDeclContext(CurrentDecl->Cxx) : nullptr;
}

void Sema::addDeclToDecl(clang::Decl *CDecl, gold::Declaration *GDecl) {
  assert(CDecl && "Invalid clang declaration");
  assert(GDecl && "Invalid gold declaration");

  auto Ret = DeclToDecl.try_emplace(CDecl, GDecl);
  if (!Ret.second) {
    llvm::errs() << "Duplicate declaration Id = "
                 << GDecl->getId()->getName() <<"\n";
    llvm_unreachable("we should never add something to the decl map 2x.");
  }
}

gold::Declaration *Sema::getDeclaration(clang::Decl *CDecl) {
  assert(CDecl && "Invalid declaration.");
  auto Iter = DeclToDecl.find(CDecl);
  if (Iter == DeclToDecl.end()) {
    if (!isa<clang::CXXRecordDecl>(CDecl)) {
      std::string Name = "";
      if (auto *ND = dyn_cast<clang::NamedDecl>(CDecl)) {
        Name = ND->getName().str();
      }
      Diags.Report(CDecl->getLocation(),
                   clang::diag::err_unknown_template_declaration)
                   << Name;
      return nullptr;
    }
    ClangToGoldDeclRebuilder rebuilder(Context, *this);
    Declaration *GDecl = rebuilder.rebuild(cast<clang::CXXRecordDecl>(CDecl));
    if (!GDecl)
      // Any error reporting will be handled the the rebuilder.
      return nullptr;
    return GDecl;
  }
  return Iter->second;
}

void Sema::setDeclForDeclaration(gold::Declaration *GDecl, clang::Decl *CDecl) {
  assert(GDecl && "Invalid gold declaration");
  GDecl->Cxx = CDecl;
  if (CDecl)
    addDeclToDecl(CDecl, GDecl);
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R,
                                 Declaration *NotThisOne) {
  return lookupUnqualifiedName(R, getCurrentScope(), NotThisOne);
}



bool Sema::lookupQualifiedName(clang::LookupResult &R,
                               Declaration *NotThisOne) {
  gold::Scope *LookupScope = nullptr;
  switch (CurNNSKind) {
  case NNSK_Empty:
    // TODO: This may need an assertion or an error message.
    llvm_unreachable("Cannot do qualified name lookup without a nested name "
                     "specifier.");
    break;
  case NNSK_Global:
    LookupScope = CurNNSLookupDecl.Global.Scope;
    break;
  case NNSK_Namespace:
    LookupScope = CurNNSLookupDecl.NNS->getScopeRep();
    break;
  case NNSK_NamespaceAlias: {
    if (auto *Ns = dyn_cast<clang::CppxNamespaceDecl>(
                                      CurNNSLookupDecl.Alias->getNamespace())) {
      LookupScope = Ns->getScopeRep();
    } else {
      Diags.Report(Ns->getLocation(), clang::diag::err_expected_namespace);
      return false;
    }
    break;
  }
  case NNSK_Record:{
    LookupScope = CurNNSLookupDecl.RebuiltClassScope;
  }
  break;
  }
  return lookupUnqualifiedName(R, LookupScope, NotThisOne);
}

static bool findOrdinaryMember(clang::RecordDecl *BaseRecord, clang::CXXBasePath &Path,
                               clang::DeclarationName Name) {
  const unsigned IDNS = clang::Decl::IDNS_Ordinary | clang::Decl::IDNS_Tag |
                        clang::Decl::IDNS_Member;
  for (Path.Decls = BaseRecord->lookup(Name);
       !Path.Decls.empty();
       Path.Decls = Path.Decls.slice(1)) {
    if (Path.Decls.front()->isInIdentifierNamespace(IDNS))
      return true;
  }

  return false;
}
// Taken from SemaLookup.cpp:2184
template<typename InputIterator>
static bool HasOnlyStaticMembers(InputIterator First, InputIterator Last) {
  using namespace clang;
  Decl *D = (*First)->getUnderlyingDecl();
  if (isa<VarDecl>(D) || isa<TypeDecl>(D) || isa<EnumConstantDecl>(D))
    return true;

  if (isa<CXXMethodDecl>(D)) {
    // Determine whether all of the methods are static.
    bool AllMethodsAreStatic = true;
    for(; First != Last; ++First) {
      D = (*First)->getUnderlyingDecl();

      if (!isa<CXXMethodDecl>(D)) {
        assert(isa<TagDecl>(D) && "Non-function must be a tag decl");
        break;
      }

      if (!cast<CXXMethodDecl>(D)->isStatic()) {
        AllMethodsAreStatic = false;
        break;
      }
    }

    if (AllMethodsAreStatic)
      return true;
  }

  return false;
}

static bool lookupInSideOfRecordBases(Sema &SemaRef, clang::ASTContext &Context,
    clang::LookupResult &R, clang::CXXRecordDecl *RD, clang::DeclarationName Name) {
  using namespace clang;
  CXXBasePaths Paths;
  if(!RD->lookupInBases(
      [=](const clang::CXXBaseSpecifier *Specifier, clang::CXXBasePath &P) ->bool {
        clang::RecordDecl *BaseRecord =
            Specifier->getType()->castAs<clang::RecordType>()->getDecl();
        return findOrdinaryMember(BaseRecord, P, Name);
      }, Paths, /*LookupInDependent=*/false)) {
    return false;
  }

  // This code taken almost directly from Sema::LookupQualifiedName in
  // SemaLookup.cpp:2236

  // Need to finish gathering all of the necessary decls?
  R.setNamingClass(RD);

  // C++ [class.member.lookup]p2:
  //   [...] If the resulting set of declarations are not all from
  //   sub-objects of the same type, or the set has a nonstatic member
  //   and includes members from distinct sub-objects, there is an
  //   ambiguity and the program is ill-formed. Otherwise that set is
  //   the result of the lookup.
  QualType SubobjectType;
  int SubobjectNumber = 0;
  AccessSpecifier SubobjectAccess = AS_none;

  for (CXXBasePaths::paths_iterator Path = Paths.begin(), PathEnd = Paths.end();
       Path != PathEnd; ++Path) {
    const CXXBasePathElement &PathElement = Path->back();

    // Pick the best (i.e. most permissive i.e. numerically lowest) access
    // across all paths.
    SubobjectAccess = std::min(SubobjectAccess, Path->Access);

    // Determine whether we're looking at a distinct sub-object or not.
    if (SubobjectType.isNull()) {
      // This is the first subobject we've looked at. Record its type.
      SubobjectType = SemaRef.getContext().CxxAST.getCanonicalType(PathElement.Base->getType());
      SubobjectNumber = PathElement.SubobjectNumber;
      continue;
    }

    if (SubobjectType
                 != Context.getCanonicalType(PathElement.Base->getType())) {
      // We found members of the given name in two subobjects of
      // different types. If the declaration sets aren't the same, this
      // lookup is ambiguous.
      if (HasOnlyStaticMembers(Path->Decls.begin(), Path->Decls.end())) {
        CXXBasePaths::paths_iterator FirstPath = Paths.begin();
        DeclContext::lookup_iterator FirstD = FirstPath->Decls.begin();
        DeclContext::lookup_iterator CurrentD = Path->Decls.begin();

        // Get the decl that we should use for deduplicating this lookup.
        auto GetRepresentativeDecl = [&](NamedDecl *D) -> Decl * {
          // C++ [temp.local]p3:
          //   A lookup that finds an injected-class-name (10.2) can result in
          //   an ambiguity in certain cases (for example, if it is found in
          //   more than one base class). If all of the injected-class-names
          //   that are found refer to specializations of the same class
          //   template, and if the name is used as a template-name, the
          //   reference refers to the class template itself and not a
          //   specialization thereof, and is not ambiguous.
          if (R.isTemplateNameLookup())
            if (auto *TD = SemaRef.getCxxSema().getAsTemplateNameDecl(D))
              D = TD;
          return D->getUnderlyingDecl()->getCanonicalDecl();
        };

        while (FirstD != FirstPath->Decls.end() &&
               CurrentD != Path->Decls.end()) {
          if (GetRepresentativeDecl(*FirstD) !=
              GetRepresentativeDecl(*CurrentD))
            break;

          ++FirstD;
          ++CurrentD;
        }

        if (FirstD == FirstPath->Decls.end() &&
            CurrentD == Path->Decls.end())
          continue;
      }

      R.setAmbiguousBaseSubobjectTypes(Paths);
      return true;
    }

    if (SubobjectNumber != PathElement.SubobjectNumber) {
      // We have a different subobject of the same type.

      // C++ [class.member.lookup]p5:
      //   A static member, a nested type or an enumerator defined in
      //   a base class T can unambiguously be found even if an object
      //   has more than one base class subobject of type T.
      if (HasOnlyStaticMembers(Path->Decls.begin(), Path->Decls.end()))
        continue;

      // We have found a nonstatic member name in multiple, distinct
      // subobjects. Name lookup is ambiguous.
      R.setAmbiguousBaseSubobjects(Paths);
      return true;
    }
  }

  // Lookup in a base class succeeded; return these results.
  for (auto *D : Paths.front().Decls) {
    AccessSpecifier AS = CXXRecordDecl::MergeAccess(SubobjectAccess,
                                                    D->getAccess());
    R.addDecl(D, AS);
  }
  R.resolveKind();
  return true;
}

static void addIfNotDuplicate(clang::LookupResult &R, clang::NamedDecl *ND) {
  for (clang::Decl *D : R) {
    if (D == ND) {
      return;
    }
  }
  R.addDecl(ND);
}

bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S,
                                 Declaration *NotThisOne) {
  assert(S);

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "Invalid id");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  // See if this is a builtin type, if we care about those.
  if (LookupKind == clang::Sema::LookupTagName ||
      LookupKind == clang::Sema::LookupAnyName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end()) {
      if (BuiltinMapIter->second.isNull()) {
        Diags.Report(clang::SourceLocation(),
                     clang::diag::err_invalid_builtin_type) << Id;
        return false;
      }

      return true;
    }
  }

  clang::IdentifierResolver::iterator
    I = getCxxSema().IdResolver->begin(Name),
    IEnd = getCxxSema().IdResolver->end();
  auto addShadows = [&R](Scope *S, clang::NamedDecl *D) -> bool {
    bool Shadowed = false;
    clang::UsingDecl *UD = dyn_cast<clang::UsingDecl>(D);
    if (!UD) {
      clang::UsingShadowDecl *Shadow = dyn_cast<clang::UsingShadowDecl>(D);
      if (Shadow) {
        R.addDecl(Shadow);
        Shadowed = true;
        return true;
      }

      return false;
    }

    for (auto *Shadow : UD->shadows()) {
      auto It = std::find(std::begin(S->Shadows), std::end(S->Shadows), Shadow);
      if (It != std::end(S->Shadows)) {
        R.addDecl(Shadow);
        Shadowed = true;
      }
    }

    return true;
  };

  // This is done based on how CppLookUpName is handled, with a few exceptions,
  // this will return uninstantiated template declarations, namespaces,
  // and other kinds of declarations. This also handles some early elaboration
  // of some types.
  bool FoundFirstClassScope = false;
  for(; S; S = S->getParent()) {
    std::set<Declaration *> Found = S->findDecl(Id);

    // Look through any using directives, but only if we didn't already find
    // something acceptable. However, we always check the shadows in a lambda
    // block.
    if (Found.empty() || S->isLambdaScope()) {
      // See if Clang has anything in the identifier resolver.
      bool Shadowed = false;
      for (; I != IEnd; ++I)
        Shadowed |= addShadows(S, *I);
      if (Shadowed)
        return true;

      bool FoundInNamespace = false;
      for (clang::UsingDirectiveDecl *UD : S->UsingDirectives) {
        assert(isa<clang::CppxNamespaceDecl>(UD->getNominatedNamespace()));

        clang::CppxNamespaceDecl *NS =
          cast<clang::CppxNamespaceDecl>(UD->getNominatedNamespace());
        std::set<Declaration *> NSFound = NS->Rep->findDecl(Id);

        // We found the name in more than one namespace.
        if (FoundInNamespace && !NSFound.empty()) {
          Diags.Report(R.getNameLoc(), clang::diag::err_ambiguous_reference)
            << Name;
          return false;
        }

        FoundInNamespace = !NSFound.empty();
        Found = NSFound;
      }
    }

    if (!Found.empty()) {
      for (auto *FoundDecl : Found) {
        // Skipping this particular declaration to avoid triggering
        // double early elaboration.
        if (FoundDecl == NotThisOne)
          continue;
        // If we find a name that hasn't been elaborated,
        // then we actually need to elaborate it.

        // Attempting to add special processing of declarations being elaborated
        // during a constant expression, and require full elaboration before
        // use.
        if (CxxSema.isConstantEvaluated() || isInDeepElaborationMode()) {
          // If we aren't 100% completed then do complete elaboration.
          if ((phaseOf(FoundDecl) < Phase::Initialization)) {
            EnterDeepElabRAII DeepElab(*this);
            // change the elaboration context back to PotentiallyEvaluated.
            clang::EnterExpressionEvaluationContext ConstantEvaluated(CxxSema,
                clang::Sema::ExpressionEvaluationContext::PotentiallyEvaluated);
            AttrElabRAII Attr(*this, false);
            Elaborator(Context, *this).elaborateDeclEarly(FoundDecl);
          }
        }

        if (FoundDecl->IsElaborating && !FoundDecl->declaresNamespace()) {
          // This might be allowed in some scenarios Specifically when we
          // reference a class type within
          diagnoseElabCycleError(FoundDecl);
          return false;
        }

        // Skip early elaboration of declarations with nested name specifiers.
        if (FoundDecl->hasNestedNameSpecifier())
          continue;

        if (!FoundDecl->Cxx) {
          AttrElabRAII Attr(*this, false);
          Elaborator(Context, *this).elaborateDeclTypeEarly(FoundDecl);
        }

        if (!FoundDecl->Cxx)
          return false;

        clang::NamedDecl *ND = cast<clang::NamedDecl>(FoundDecl->Cxx);

        // FIXME: check if this is a tag decl, not a type decl!
        if (LookupKind == clang::Sema::LookupTagName &&
            !isa<clang::TypeDecl>(ND)) {
          // FIXME: Give a proper diagnostic once we implement hiding.
          // unsigned DiagID = Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
          //                                         "Tag is hidden.");
          // Diags.Report(clang::SourceLocation(), DiagID);
          return false;
        }

        // If there is a described template, add that to the result instead
        // of the bare declaration.
        if (FoundDecl->declaresFunctionTemplate()) {
          if (auto *FD = dyn_cast<clang::FunctionDecl>(ND))
            ND = FD->isFunctionTemplateSpecialization() ?
              FD->getPrimaryTemplate() : FD->getDescribedFunctionTemplate();
          else if (auto *VD = dyn_cast<clang::VarDecl>(ND))
            ND = VD->getDescribedVarTemplate();
          else
            llvm_unreachable("Unknown template function type");
        } else if (FoundDecl->declaresTemplateType()) {
          // We want the canonical declaration of a template unless it is
          // a specialization.
          using Specialization = clang::ClassTemplateSpecializationDecl;
          using Record = clang::CXXRecordDecl;
          if (auto *CD = dyn_cast<Specialization>(FoundDecl->Cxx)) {
            ND = CD->getSpecializedTemplate();
          } else if (auto *RD = dyn_cast<Record>(FoundDecl->Cxx)) {
            ND = RD->getDescribedClassTemplate();
            // FIXME: if ND is null, this is not recoverable.
            if (ND)
              ND = cast<clang::NamedDecl>(ND->getCanonicalDecl());
            else
              ND = RD;
          }
        } else {
          // Getting the cannonical declaration so hopefully this will prevent
          // us from returning the same thing more then once.
          if (auto *RD = dyn_cast<clang::CXXRecordDecl>(FoundDecl->Cxx)) {
            ND = cast<clang::NamedDecl>(RD->getCanonicalDecl());
          }
        }
        if (auto *VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(
                                                               FoundDecl->Cxx)) {
          ND = VTSD->getSpecializedTemplate();
        }
        addIfNotDuplicate(R, ND);
      }
      break;
    }

    // This only triggers one time because it's difficult to figure out what kind
    // of scope we are actually processing when we run into these issues.
    // There will be more problems like this. That's because scopes are confusing.
    if (S->getKind() == SK_Class && !FoundFirstClassScope) {
      FoundFirstClassScope = true;
      // Checking that if we are in side of a record and within that record has base classes.
      Declaration *DeclEntity = S->Entity;
      if (DeclEntity) {
        if (DeclEntity->declaresTagDef()) {
          if (DeclEntity->Cxx) {
            clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(DeclEntity->Cxx);
            // We do this because if for whatever reason if this hasn't been initially
            // elaborated yet but if we are some how in side of it then there is a
            // really big problem
            if (!RD)
              llvm_unreachable("Cyclic depdency detected unable to continue.");
            // Basically, if this is true we found something then exit the loop.
            if (lookupInSideOfRecordBases(*this, getCxxSema().getASTContext(),
                R, RD, Name)) {
              break;
            }
          }
        }
      }
    }
  }
  return !R.empty();
}

void Sema::lookupRedecls(Declaration *D, clang::LookupResult &Previous,
                         Scope *Scope) {
  // I may need to do something slightly different here so that
  // We only seach the current scope? Because we don't want to get into
  // conflict with any other functions that are in different namespaces?
  if (D->hasNestedNameSpecifier())
    lookupQualifiedName(Previous, D);
  else
    lookupUnqualifiedName(Previous, getCurrentScope(), D);
}

bool Sema::unqualifiedMemberAccessLookup(clang::LookupResult &R,
                                         const clang::Expr *LHSResultExpr) {
  // clang::QualType LHSTy= LHSResultExpr->getType();
  // if (LHSTy->isRecordDecl()) {

  // }
  llvm_unreachable("Working on it.");
}

bool Sema::checkUnqualifiedNameIsDecl(const clang::DeclarationNameInfo& DNI) {
  return checkUnqualifiedNameIsDecl(DNI, getCurrentScope());
}

bool Sema::checkUnqualifiedNameIsDecl(const clang::DeclarationNameInfo& DNI,
                                      Scope *S) {
  assert(S);

  clang::DeclarationName Name = DNI.getName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();

  assert(Id && "Invalid id");

  auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
  if (BuiltinMapIter != BuiltinTypes.end())
    return false;

  // This is done based on how CppLookUpName is handled, with a few exceptions,
  // this will return uninstantiated template declarations, namespaces,
  // and other kinds of declarations. This also handles some early elaboration
  // of some types.
  bool FoundFirstClassScope = false;
  // if (S->getKind() == SK_Class) {
  //   // Don't go into the base classes for this because this could just be
  //   // overrides.
  // }
  for(;S; S = S->getParent()) {
    std::set<Declaration *> Found = S->findDecl(Id);
    if (!Found.empty()) {
      return false;
    }

    // This only triggers one time because it's difficult to figure out what kind
    // of scope we are actually processing when we run into these issues.
    // There will be more problems like this. That's because scopes are confusing.
    if (S->getKind() == SK_Class && !FoundFirstClassScope) {
      FoundFirstClassScope = true;
      // Checking that if we are in side of a record and within that record has base classes.
      Declaration *DeclEntity = S->Entity;
      if (DeclEntity) {
        if (DeclEntity->declaresTagDef()) {
          if (DeclEntity->Cxx) {
            clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(DeclEntity->Cxx);
            // We do this because if for whatever reason if this hasn't been initially
            // elaborated yet but if we are some how in side of it then there is a
            // really big problem
            if (!RD)
              llvm_unreachable("Cyclic depdency detected unable to continue.");
            clang::LookupResult R(CxxSema, DNI, clang::Sema::LookupOrdinaryName);
            // Basically, if this is true we found something then exit the loop.
            if (lookupInSideOfRecordBases(*this, getCxxSema().getASTContext(),
                                          R, RD, Name)) {
              return false;
            }
          }
        }
      }
    }
  }
  return true;
}

bool Sema::scopeIsWithinClass() const {
  return getCurrentScope()->getKind() & SK_Class;
}

bool Sema::scopeIsWithinClass(Scope *S) const {
  assert(S && "Invalid scope.");
  return S->getKind() & SK_Class;
}

clang::Decl *Sema::getDeclForScope() {
  return getDeclForScope(getCurrentScope());
}

clang::Decl *Sema::getDeclForScope(Scope *S) {
  assert(S && "Invalid scope given.");
  for(;S; S = S->getParent()) {
    if (!S->Entity)
      continue;
    if (!S->Entity->Cxx) {
      llvm_unreachable("Entity not elaborated correctly before getDeclForScope was called.");
    }
    return S->Entity->Cxx;
  }
  return nullptr;
}


clang::Scope *Sema::getCurClangScope() {
  return CxxSema.CurScope;
}

clang::Scope *Sema::enterClangScope(unsigned int ScopeFlags) {
  CxxSema.CurScope = new clang::Scope(getCurClangScope(), ScopeFlags, Diags);
  // Only do this if we are not a template scope to avoid an assertion inside
  // of setEntity.
  if (!CxxSema.CurScope->isTemplateParamScope())
    CxxSema.CurScope->setEntity(nullptr);
  return CxxSema.CurScope;
}

clang::Scope *Sema::moveToParentScopeNoPop() {
  clang::Scope* S = CxxSema.CurScope;
  CxxSema.CurScope = CxxSema.CurScope->getParent();
  return S;
}

void Sema::reEnterClangScope(clang::Scope* Scope) {
  assert(Scope && "Invalid scope.");
  CxxSema.CurScope = Scope;
}

void Sema::leaveClangScope(clang::SourceLocation Loc) {
  assert(getCurClangScope() && "Clang scope imbalance!");

  // Inform the actions module that this scope is going away if there are any
  // decls in it.
  CxxSema.ActOnPopScope(Loc, getCurClangScope());

  clang::Scope *OldScope = getCurClangScope();
  CxxSema.CurScope = OldScope->getParent();

  delete OldScope;
}

clang::Scope* Sema::saveCurrentClangScope() {
  assert(getCurClangScope() && "Clang scope imbalance!");
  clang::Scope *OldScope = getCurClangScope();
  CxxSema.CurScope = OldScope->getParent();
  return OldScope;
}

static void VisitScope(Scope *S, llvm::raw_ostream &Out) {
  Out << "Scope Status = \n";
  S->dump(Out);
  Out << "=================================\n";
  if (S->getParent()) {
    VisitScope(S->Parent, Out);
  }
}

static void VisitClangScope(clang::Scope *S, llvm::raw_ostream &Out) {
  Out << "Clang Scope:\n";
  clang::DeclContext *DC = S->getEntity();
  if (DC) {
    if (clang::FunctionDecl *Fn = dyn_cast<clang::FunctionDecl>(DC)) {
      Out << "We have a function Decl\n";
      Fn->dump(Out);
    } else if (clang::RecordDecl *RD = dyn_cast<clang::RecordDecl>(DC)) {
      Out << "We have a Record Decl\n";
      RD->dump(Out);
    } else if (isa<clang::TranslationUnitDecl>(DC)) {
      Out << "We are a translation unit decl.\n";
    } else
      Out << "Unexpected DeclContext type\n";
  } else
    Out << "Current Scope has no entity.\n";
  S->dumpImpl(Out);
  Out << "==================================\n";
  if (S->getParent()) {
    VisitClangScope(S->getParent(), Out);
  }
}

static void VisitDeclContext(clang::DeclContext *DC, llvm::raw_ostream &Out) {
  Out << "DeclContext = \n";
  if (clang::FunctionDecl *Fn = dyn_cast<clang::FunctionDecl>(DC)) {
    Out << "We have a function Decl\n";
    Fn->dump(Out);
  } else if (clang::RecordDecl *RD = dyn_cast<clang::RecordDecl>(DC)) {
    Out << "We have a Record Decl\n";
    RD->dump(Out);
  } else if (isa<clang::TranslationUnitDecl>(DC)) {
    Out << "We are a translation unit decl.\n";
  } else
    Out << "Unexpected DeclContext type\n";
  DC->dumpDeclContext();
  Out << "=================================\n";
  // if (DC->getParent()) {
  //   VisitDeclContext(DC->getParent(), Out);
  // }
}

void Sema::dumpState(llvm::raw_ostream &Out) {
  Out << "Current Sema State\n";

  Out << "Current gold::Scope Status: \n";
  Out << "=================================\n";
  VisitScope(getCurrentScope(), Out);
  Out << "\n";
  Out << "clang scope and decl contexts\n";
  Out << "=================================\n";
  VisitClangScope(getCurClangScope(), Out);
  Out << "\n";
  Out << "=================================\n";
  Out << "Current DeclContext = \n";
  Out << "=================================\n";
  if (CxxSema.CurContext) {
    VisitDeclContext(CxxSema.CurContext, Out);
  } else
    Out << "Current Context invalid.\n";

  Out.flush();
}

bool Sema::isElaboratingClass() const {
  return !ClassStack.empty();
}

Sema::ClassElaborationState
Sema::pushElaboratingClass(Declaration *D, bool TopLevelClass) {
  assert((TopLevelClass || !ClassStack.empty())
      && "Nestd class without outer class.");
  ClassStack.push_back(new ElaboratingClass(D, TopLevelClass));
  return CxxSema.PushParsingClass();
}

void Sema::deallocateElaboratingClass(ElaboratingClass *D) {
  for (unsigned I = 0, N = D->LateElaborations.size(); I != N; ++I)
    delete D->LateElaborations[I];
  delete D;
}

void Sema::popElaboratingClass(ClassElaborationState State) {
  assert(!ClassStack.empty() && "Mismatched push/pop for class parsing");

  CxxSema.PopParsingClass(State);

  ElaboratingClass *Victim = ClassStack.back();
  ClassStack.pop_back();
  if (Victim->IsTopLevelClass) {
    // Deallocate all of the nested classes of this class,
    // recursively: we don't need to keep any of this information.
    deallocateElaboratingClass(Victim);
    return;
  }
  assert(!ClassStack.empty() && "Missing top-level class?");

  if (Victim->LateElaborations.empty()) {
    // The victim is a nested class, but we will not need to perform
    // any processing after the definition of this class since it has
    // no members whose handling was delayed. Therefore, we can just
    // remove this nested class.
    deallocateElaboratingClass(Victim);
    return;
  }

  // This nested class has some members that will need to be processed
  // after the top-level class is completely defined. Therefore, add
  // it to the list of nested classes within its parent.
  assert(CxxSema.getCurScope()->isClassScope()
      && "Nested class outside of class scope?");
  ClassStack.back()->LateElaborations.push_back(
      new LateElaboratedClass(*this, Context, Victim));
  Victim->TemplateScope
                   = CxxSema.getCurScope()->getParent()->isTemplateParamScope();
}

bool Sema::declNeedsDelayed(Declaration *D) {
  // This may need some special processing to see if it interacts with
  // itself at all.
  if (D->declaresTypeAlias())
    return false;

  if (D->declaresTemplateType() || D->declaresTagDef())
    return true;

  // I haven't found an reason that these would need to be delayed any more
  // then normal so long as they are done in the approriate order
  if (D->declaresFunctionTemplate() || D->declaresFunction())
    // TODO:/FIXME: Once we have default parameters implemented we
    // will need to change how this function works, because when we encounter
    // a type that uses something not elaborated yet like the type associated
    // with a class, then we need to make sure that we do late elaboration
    // of that when it's required.
    return false;

  if (D->declaresInitializedVariable())
    return true;
  return false;
}

unsigned Sema::computeTemplateDepth() const {
  unsigned Count = 0;
  for(auto Iter = ClassStack.rbegin(); Iter != ClassStack.rend(); ++Iter) {
    Count += (*Iter)->TagOrTemplate->declaresTemplateType();
  }
  return Count;
}

// True when we are elaborating a using macro within a class.
bool Sema::elaboratingUsingInClassScope() const {
  if(CurrentDecl)
    if (!CurrentDecl->Decl)
      return false;
  return scopeIsWithinClass() && CurrentDecl->Decl->isUsingDirective();
}

clang::CppxTypeLiteral *Sema::buildTypeExpr(clang::QualType Ty,
                                            clang::SourceLocation Loc) {
  return buildAnyTypeExpr(Context.CxxAST.CppxKindTy, Ty, Loc);
}

clang::CppxTypeLiteral *Sema::buildNsTypeExpr(clang::SourceLocation Loc) {
  return buildAnyTypeExpr(Context.CxxAST.CppxNamespaceTy,
                          Context.CxxAST.CppxNamespaceTy, Loc);
}

clang::CppxTypeLiteral *Sema::buildTypeExpr(clang::TypeSourceInfo *TInfo) {
  assert(TInfo && "Invalid type information.");
  return buildAnyTypeExpr(Context.CxxAST.CppxKindTy, TInfo);
}

clang::CppxTypeLiteral *Sema::buildAnyTypeExpr(clang::QualType KindTy,
    clang::TypeSourceInfo *TInfo) {
  assert(TInfo && "Invalid type information.");
  return clang::CppxTypeLiteral::create(Context.CxxAST, KindTy, TInfo);
}

clang::CppxTypeLiteral *Sema::buildAnyTypeExpr(clang::QualType KindTy,
    clang::QualType Ty, clang::SourceLocation Loc) {
  return buildAnyTypeExpr(KindTy, BuildAnyTypeLoc(Context.CxxAST, Ty, Loc));
}

clang::CppxTypeLiteral *Sema::buildTypeExprTypeFromExpr(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                    bool IsConstruct) {
  return buildAnyTypeExpr(Context.CxxAST.CppxKindTy,
                          BuildAnyTypeLoc(Context.CxxAST,
                                          Context.CxxAST.getCppxTypeExprTy(
                                            E, IsConstruct),
                                          Loc));
}
clang::CppxTypeLiteral *Sema::buildTypeExprTypeFromExprLiteral(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                         bool IsConstructExpr){
  clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST,
                  Context.CxxAST.getCppxTypeExprTy(E, IsConstructExpr), Loc);
  return buildTypeExpr(TInfo);
}

clang::QualType Sema::buildQualTypeExprTypeFromExpr(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                    bool IsConstruct) {
  clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST,
                  Context.CxxAST.getCppxTypeExprTy(E, IsConstruct), Loc);
  return TInfo->getType();
}

clang::CppxTypeLiteral *
Sema::buildFunctionTypeExpr(clang::QualType FnTy, SourceLocation BeginLoc,
                            clang::SourceLocation LParenLoc,
                            clang::SourceLocation RParenLoc,
                            clang::SourceRange ExceptionSpecRange,
                            clang::SourceLocation EndLoc,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  return buildTypeExpr(BuildFunctionTypeLoc(Context.CxxAST, FnTy,
                                            BeginLoc, LParenLoc, RParenLoc,
                                            ExceptionSpecRange, EndLoc,
                                            Params));
}

clang::CppxTypeLiteral *
Sema::buildTypeExprFromTypeDecl(const clang::TypeDecl *TyDecl,
                                clang::SourceLocation Loc) {
  // FIXME: May need to handle template types differently in the future.
  return buildTypeExpr(Context.CxxAST.getTypeDeclType(TyDecl), Loc);
}

clang::CppxDeclRefExpr *Sema::buildTemplateType(clang::TemplateDecl *TD,
                                                clang::SourceLocation Loc) {
  clang::QualType TT = Context.CxxAST.getTemplateType(TD);
  return buildAnyDeclRef(TT, TD, Loc);
}

clang::Expr *Sema::addConstToTypeExpr(const clang::Expr *TyExpr,
                                      clang::SourceLocation Loc) {
  llvm_unreachable("Working on it!");
  // EvaluatedTy.addConst();
}

clang::Expr *Sema::addRefToTypeExpr(const clang::Expr *TyExpr,
                                    clang::SourceLocation Loc) {
  llvm_unreachable("Working on it!");
  // CxxAST.getLValueReferenceType(Inner),

}

clang::Expr *Sema::addRRefToTypeExpr(const clang::Expr *TyExpr,
                                     clang::SourceLocation Loc) {
  llvm_unreachable("Working on it!");
  // CxxAST.getRValueReferenceType(Inner),
}

clang::QualType Sema::getQualTypeFromTypeExpr(const clang::Expr *TyExpr) {
  if (!TyExpr) {
    return clang::QualType();
  }
  if (!TyExpr->getType()->isTypeOfTypes()) {
    Diags.Report(TyExpr->getExprLoc(), clang::diag::err_not_a_type);
    return clang::QualType();
  }
  if (const clang::CppxTypeLiteral *Ty
                                   = dyn_cast<clang::CppxTypeLiteral>(TyExpr)) {

    return Ty->getValue()->getType();
  }
  llvm_unreachable("Invaild type expression evaluates to type of types.");

}

clang::TypeSourceInfo *
Sema::getTypeSourceInfoFromExpr(const clang::Expr *TyExpr,
                                clang::SourceLocation Loc) {
  if (!TyExpr) {
    return nullptr;
  }

  if (!TyExpr->getType()->isTypeOfTypes()) {
    Diags.Report(Loc, clang::diag::err_not_a_type);
    return nullptr;
  }

  if (const clang::CppxTypeLiteral *Ty
                                   = dyn_cast<clang::CppxTypeLiteral>(TyExpr)) {

    return Ty->getValue();
  }

  llvm_unreachable("Invaild type expression evaluates to type of types.");
}


clang::ParsedType Sema::getParsedTypeFromExpr(const clang::Expr *TyExpr,
                                              clang::SourceLocation Loc) {
  clang::TypeSourceInfo *TInfo = getTypeSourceInfoFromExpr(TyExpr, Loc);
  if(!TInfo)
    return nullptr;

  return CxxSema.CreateParsedType(TInfo->getType(), TInfo);
}

clang::CppxDeclRefExpr *Sema::buildNSDeclRef(clang::CppxNamespaceDecl *D,
                                             clang::SourceLocation Loc) {
  return buildAnyDeclRef(Context.CxxAST.CppxNamespaceTy, D, Loc);
}

clang::CppxDeclRefExpr *Sema::buildNSDeclRef(clang::NamespaceAliasDecl *D,
                                             clang::SourceLocation Loc) {
  return buildAnyDeclRef(Context.CxxAST.CppxNamespaceTy, D, Loc);
}

clang::CppxDeclRefExpr *
Sema::buildAnyDeclRef(clang::QualType KindTy, clang::Decl *D,
                      clang::SourceLocation Loc) {
  assert(D && "Invalid declaration to reference.");
  return clang::CppxDeclRefExpr::Create(Context.CxxAST, KindTy, D, Loc);
}

clang::Decl *Sema::getDeclFromExpr(const clang::Expr *DeclExpr,
                                   clang::SourceLocation Loc) {
  assert(DeclExpr && "Invalid expression");

  if (const clang::CppxDeclRefExpr *DecRef
                          = dyn_cast<clang::CppxDeclRefExpr>(DeclExpr)) {
    return DecRef->getValue();
  }
  llvm_unreachable("Unable to get declaration from expression.");
  // TODO: Change this error message to say that the expression doesn't contain
  // a declaration or something like that.
}

clang::CppxNamespaceDecl *Sema::getNSDeclFromExpr(const clang::Expr *DeclExpr,
                                                  clang::SourceLocation Loc) {
  assert(DeclExpr && "Invalid expression");
  if (const clang::CppxDeclRefExpr *DecRef
                                 = dyn_cast<clang::CppxDeclRefExpr>(DeclExpr)) {
    if (clang::CppxNamespaceDecl *NsDecl
                     = dyn_cast<clang::CppxNamespaceDecl>(DecRef->getValue())) {
      return NsDecl;
    }
    Diags.Report(Loc, clang::diag::err_expected_namespace);
    return nullptr;
  }
  Diags.Report(Loc, clang::diag::err_expected_namespace);
  return nullptr;
}

Scope *Sema::duplicateScopeForNestedNameContext(Declaration *D) {
  assert(D && "invalid declaration");
  assert(D->Cxx && "Declaration hasn't been elaborated yet");
  assert(D->SavedScope && "Declaration has no scope to duplicate");
  // Entering a new scope that we can use for lookup.
  enterScope(D->SavedScope->getKind(), D->SavedScope->getConcreteTerm());
  Scope *NextScope = getCurrentScope();
  for (const auto &DeclPair : D->SavedScope->DeclMap) {
    NextScope->addDecl(DeclPair.second);
  }
  // Copying the current entity into the new scope.
  NextScope->Entity = D->SavedScope->Entity;
  return NextScope;
}

bool Sema::setLookupScope(clang::CXXRecordDecl *Record) {
  assert(Record && "Invalid lookup context");
  Declaration *D = getDeclaration(Record);
  if (!D) {
    return true;
  }
  CurNNSLookupDecl.RebuiltClassScope = duplicateScopeForNestedNameContext(D);
  CurNNSKind = NNSK_Record;
  return false;
}

Scope *Sema::getLookupScope() {
  switch (CurNNSKind) {
  case NNSK_Empty:
    return nullptr;
  case NNSK_Global:
    return CurNNSLookupDecl.Global.Scope;
  case NNSK_Namespace:
    return CurNNSLookupDecl.NNS->getScopeRep();
  case NNSK_NamespaceAlias: {
    clang::Decl *AliasedNS = CurNNSLookupDecl.Alias->getAliasedNamespace();
    if (auto *NNS = dyn_cast<clang::CppxNamespaceDecl>(AliasedNS))
      return NNS->getScopeRep();
    // FIXME: The namespace alias doesn't contain a CppxNamespaceDecl
    llvm_unreachable("Invalid namespace alias");
  }

  case NNSK_Record:{
    return CurNNSLookupDecl.RebuiltClassScope;
  }
  } // switch (CurNNSKind)

  llvm_unreachable("Invalid or unknown nested name specifier type");
}

bool Sema::isInDeepElaborationMode() const {
  return ForceDeepElaborationDuringLookup;
}

bool Sema::setDeepElaborationMode(bool EnableDisable) {
  bool Res = ForceDeepElaborationDuringLookup;
  ForceDeepElaborationDuringLookup = EnableDisable;
  return Res;
}

bool Sema::rebuildFunctionType(clang::FunctionDecl *FD,
                               clang::SourceLocation Loc,
                               const clang::FunctionProtoType *FPT,
                               const FunctionExtInfo &EI,
                               const FunctionExtProtoInfo &EPI,
                               const FunctionExceptionSpec &ESI) {
  assert(FD && "Invalid function declaration");
  assert(FPT && "Invalid function proto type");
  auto ParamTys = FPT->getParamTypes();
  llvm::SmallVector<clang::QualType, 10> ParamTypes(ParamTys.begin(),
                                                    ParamTys.end());
  clang::QualType NewFuncTy;
  if (clang::CXXMethodDecl *MD = dyn_cast<clang::CXXMethodDecl>(FD)) {
    NewFuncTy = CxxSema.BuildFunctionType(FPT->getReturnType(), ParamTypes, Loc,
                                          MD->getParent()->getDeclName(), EPI);
  } else {
    NewFuncTy = CxxSema.BuildFunctionType(FPT->getReturnType(), ParamTypes, Loc,
                                          clang::DeclarationName(), EPI);
  }
  if (NewFuncTy.isNull()) {
    Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  const clang::FunctionProtoType *NewFPT
                                 = NewFuncTy->getAs<clang::FunctionProtoType>();
  if (!NewFPT) {
    Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  clang::QualType ExtInfoAdjustedTy(
               Context.CxxAST.adjustFunctionType(NewFPT, EI), /*Qualifiers=*/0);
  if (ExtInfoAdjustedTy.isNull()) {
    Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  clang::QualType ExceptionAdjustedTy
      = Context.CxxAST.getFunctionTypeWithExceptionSpec(ExtInfoAdjustedTy, ESI);
  if (ExceptionAdjustedTy.isNull()) {
    Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  auto ParmVarDecls = FD->parameters();
  llvm::SmallVector<clang::ParmVarDecl *, 32> Parmeters(ParmVarDecls.begin(),
                                                         ParmVarDecls.end());
  clang::TypeSourceInfo *TInfo = BuildFunctionTypeLoc(Context.CxxAST,
                                                      ExceptionAdjustedTy,
                                                      FD->getBeginLoc(),
                                                      clang::SourceLocation(),
                                                      clang::SourceLocation(),
                                                      clang::SourceRange(),
                                                      FD->getEndLoc(),
                                                      Parmeters);
  if (!TInfo) {
    Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  FD->setType(ExceptionAdjustedTy);
  FD->setTypeSourceInfo(TInfo);
  return false;
}

clang::CppxNamespaceDecl *
Sema::ActOnStartNamespaceDef(clang::Scope *NamespcScope,
                             clang::SourceLocation InlineLoc,
                             clang::SourceLocation NamespaceLoc,
                             clang::SourceLocation IdentLoc,
                             clang::IdentifierInfo *II,
                             clang::SourceLocation LBrace,
                             const clang::ParsedAttributesView &AttrList,
                             clang::UsingDirectiveDecl *&UD) {
  using namespace clang;
  SourceLocation StartLoc = InlineLoc.isValid() ? InlineLoc : NamespaceLoc;

  // For anonymous namespace, take the location of the left brace.
  SourceLocation Loc = II ? IdentLoc : LBrace;
  bool IsInline = InlineLoc.isValid();
  bool IsInvalid = false;
  bool IsStd = false;
  bool AddToKnown = false;
  clang::Scope *DeclRegionScope = NamespcScope->getParent();

  NamespaceDecl *PrevNS = nullptr;
  CxxSema.CheckNamespaceDeclaration(II, StartLoc, Loc, IsInline, IsInvalid,
                                    IsStd, AddToKnown, PrevNS);
  gold::Scope *PrevScope = nullptr;
  if (CppxNamespaceDecl *Prev = dyn_cast_or_null<CppxNamespaceDecl>(PrevNS))
    PrevScope = Prev->Rep;


  CppxNamespaceDecl *Namespc = CppxNamespaceDecl::Create(Context.CxxAST,
                                                         CxxSema.CurContext,
                                                         IsInline, StartLoc,
                                                         Loc, II, PrevNS,
                                                         PrevScope);
  if (IsInvalid)
    Namespc->setInvalidDecl();

  CxxSema.ProcessDeclAttributeList(DeclRegionScope, Namespc, AttrList);
  CxxSema.AddPragmaAttributes(DeclRegionScope, Namespc);

  // FIXME: Should we be merging attributes?
  if (const VisibilityAttr *Attr = Namespc->getAttr<VisibilityAttr>())
    CxxSema.PushNamespaceVisibilityAttr(Attr, Loc);

  if (IsStd)
    CxxSema.StdNamespace = Namespc;
  if (AddToKnown)
    CxxSema.KnownNamespaces[Namespc] = false;

  if (II) {
    CxxSema.PushOnScopeChains(Namespc, DeclRegionScope);
  } else {
    // Link the anonymous namespace into its parent.
    DeclContext *Parent = CxxSema.CurContext->getRedeclContext();
    if (TranslationUnitDecl *TU = dyn_cast<TranslationUnitDecl>(Parent)) {
      TU->setAnonymousNamespace(Namespc);
    } else if (NamespaceDecl *ND = dyn_cast<NamespaceDecl>(Parent)) {
      ND->setAnonymousNamespace(Namespc);
    } else {
      assert(isa<CXXFragmentDecl>(Parent));
    }

    CxxSema.CurContext->addDecl(Namespc);

    // C++ [namespace.unnamed]p1.  An unnamed-namespace-definition
    //   behaves as if it were replaced by
    //     namespace unique { /* empty body */ }
    //     using namespace unique;
    //     namespace unique { namespace-body }
    //   where all occurrences of 'unique' in a translation unit are
    //   replaced by the same identifier and this identifier differs
    //   from all other identifiers in the entire program.

    // We just create the namespace with an empty name and then add an
    // implicit using declaration, just like the standard suggests.
    //
    // CodeGen enforces the "universally unique" aspect by giving all
    // declarations semantically contained within an anonymous
    // namespace internal linkage.
    if (!PrevNS) {
      UD = UsingDirectiveDecl::Create(Context.CxxAST, Parent,
                                      /* 'using' */ LBrace,
                                      /* 'namespace' */ SourceLocation(),
                                      /* qualifier */ NestedNameSpecifierLoc(),
                                      /* identifier */ SourceLocation(),
                                      Namespc, /* Ancestor */ Parent);
      UD->setImplicit();
      Parent->addDecl(UD);
      getCurrentScope()->UsingDirectives.insert(UD);
    }
  }

  CxxSema.ActOnDocumentableDecl(Namespc);

  // Although we could have an invalid decl (i.e. the namespace name is a
  // redefinition), push it as current DeclContext and try to continue parsing.
  // FIXME: We should be able to push Namespc here, so that the each DeclContext
  // for the namespace has the declarations that showed up in that particular
  // namespace definition.
  CxxSema.PushDeclContext(NamespcScope, Namespc);
  return Namespc;
}

clang::Expr *Sema::actOnCxxFoldExpr(clang::SourceLocation LParenLoc,
                                    clang::Expr *LHS, const Token &FoldTok,
                                    clang::SourceLocation EllipsisLoc,
                                    clang::Expr *RHS,
                                    clang::SourceLocation RParenLoc) {
  clang::IdentifierInfo *II = &Context.CxxAST.Idents.get(FoldTok.getSpelling());
  auto Iter = FoldOpToClangKind.find(II);

  // TODO: Should this be an error message or an assertion?
  assert (Iter != FoldOpToClangKind.end() && "Invalid operator name");
  clang::tok::TokenKind ClangTK = Iter->second;
  // Convert given fold toke into the correct C++ fold token operator kind.
  return CxxSema.ActOnCXXFoldExpr(getCurClangScope(), LParenLoc, LHS, ClangTK,
                                  EllipsisLoc, RHS, RParenLoc).get();
}
clang::CppxPartialEvalExpr *
Sema::buildPartialInPlaceNewExpr(const Syntax *ConstructKW,
                                 clang::Expr *PtrExpr,
                                 clang::SourceLocation Loc) {
  return clang::CppxPartialEvalExpr::Create(Context.CxxAST,
    // FIXME: I need to keep track of the allocated memory for this.
                                            new PartialInPlaceNewExpr(
                                              *this, ConstructKW, PtrExpr),
                                            Loc);

}

void Sema::diagnoseElabCycleError(Declaration *CycleTerminalDecl) {
  assert(CycleTerminalDecl && "Invalid terminal cycle");
  assert(!DeclsBeingElaborated.empty() && "We cannot have an empty stack and a "
         "declaration cycle.");
  assert(CycleTerminalDecl->IdDcl);
  Diags.Report(CycleTerminalDecl->IdDcl->getLoc(),
               clang::diag::err_decl_use_cycle);
  for (auto *CycleNote : DeclsBeingElaborated){
    if (CycleNote == CycleTerminalDecl)
      continue;

    Diags.Report(CycleNote->IdDcl->getLoc(),
                 clang::diag::note_cycle_entry);
  }
}

void Sema::createInPlaceNew() {
  // No scope needed here because we don't do lookup!
  // Getting Translation unit scope
  clang::Scope *TUScope = clang::Sema::getScopeForDeclContext(
      getCurClangScope(), Context.CxxAST.getTranslationUnitDecl());
  SaveAndRestoreClangDCAndScopeRAII ScopeAndDC(*this);
  setClangDeclContext(Context.CxxAST.getTranslationUnitDecl());
  reEnterClangScope(TUScope);

  // Sort of resuming the translation unit scope so I can correctly create
  // my do nothing new/delete functions.
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  clang::TranslationUnitDecl *TU = Context.CxxAST.getTranslationUnitDecl();
  clang::DeclContext *Owner = TU;
  {
    // Create type before this?
    clang::QualType NewReturnTy =
               Context.CxxAST.getPointerType(BuiltinTypes.find("void")->second);
    Types.push_back(BuiltinTypes.find("uint64")->second);
    Types.push_back(NewReturnTy);

    clang::FunctionProtoType::ExtProtoInfo EPI;
    EPI.ExceptionSpec.Type = clang::EST_BasicNoexcept;
    EPI.ExtInfo = Context.CxxAST.getDefaultCallingConvention(false, false);
    EPI.Variadic = false;
    clang::SourceLocation Loc = TU->getBeginLoc();
    clang::QualType NewFnTy =
                        Context.CxxAST.getFunctionType(NewReturnTy, Types, EPI);

    ClangScopeRAII newFuncParams(*this, clang::Scope::DeclScope |
                               clang::Scope::FunctionPrototypeScope |
                               clang::Scope::FunctionDeclarationScope,
                               clang::SourceLocation());

    clang::TypeSourceInfo *FnTInfo = BuildFunctionTypeLoc(Context.CxxAST,
                                                          NewFnTy,
                                                          Loc, Loc, Loc,
                                                          clang::SourceRange(),
                                                          Loc, Params);
    clang::IdentifierInfo *II = &Context.CxxAST.Idents.get("__GoldInplaceNew");
    clang::DeclarationName FnName(II);
    InPlaceNew = clang::FunctionDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                            FnName, FnTInfo->getType(), FnTInfo,
                                            clang::SC_None);
    // Setting the declaration to be implicit.
    {
      clang::TypeSourceInfo *P0SrcInfo = BuildAnyTypeLoc(Context.CxxAST, Types[0], Loc);
      clang::IdentifierInfo *II = &Context.CxxAST.Idents.get("sz");
      clang::DeclarationName Name(II);
      clang::ParmVarDecl *P = clang::ParmVarDecl::Create(Context.CxxAST, InPlaceNew,
                                                         Loc, Loc, Name,
                  Context.CxxAST.getAdjustedParameterType(P0SrcInfo->getType()),
                                            P0SrcInfo, clang::SC_None, nullptr);
      Params.push_back(P);
    }
    // Creating parameter 1.
    {
      clang::TypeSourceInfo *P1SrcInfo = BuildAnyTypeLoc(Context.CxxAST, Types[1], Loc);
      clang::IdentifierInfo *II = &Context.CxxAST.Idents.get("ptr");
      clang::DeclarationName Name(II);
      clang::ParmVarDecl *P = clang::ParmVarDecl::Create(Context.CxxAST, InPlaceNew,
                                                         Loc, Loc, Name,
                  Context.CxxAST.getAdjustedParameterType(P1SrcInfo->getType()),
                                         P1SrcInfo, clang::SC_None, nullptr);
      Params.push_back(P);
    }
    InPlaceNew->setParams(Params);
    InPlaceNew->setInlineSpecified(true);
    // Rebuilding function type so we have parameters
    FnTInfo = BuildFunctionTypeLoc(Context.CxxAST,
                                   NewFnTy,
                                   Loc, Loc, Loc,
                                   clang::SourceRange(),
                                   Loc, Params);
    InPlaceNew->setType(FnTInfo->getType());
    InPlaceNew->setTypeSourceInfo(FnTInfo);
    {
      ClangScopeRAII FuncBody(*this, clang::Scope::FnScope |
                              clang::Scope::DeclScope |
                              clang::Scope::CompoundStmtScope,
                              clang::SourceLocation());
      clang::QualType ResultType = NewReturnTy;
      clang::ExprValueKind ValueKind = getCxxSema()
              .getValueKindForDeclReference(ResultType, Params[1], Loc);
      clang::DeclarationNameInfo DNI({&Context.CxxAST.Idents.get("ptr")}, Loc);

      auto RefExpr =
        clang::DeclRefExpr::Create(Context.CxxAST,
                                   clang::NestedNameSpecifierLoc(),
                                   clang::SourceLocation(), Params[1],
                                   /*RefersToEnclosingVariableOrCapture*/false,
                                   /*NameLoc*/Loc, ResultType, ValueKind);
      auto Cast =
        clang::ImplicitCastExpr::Create(Context.CxxAST, ResultType,
                                        clang::CK_LValueToRValue, RefExpr,
                                        nullptr, ValueKind,
                                        clang::FPOptionsOverride());
      clang::ReturnStmt *RetStmt = clang::ReturnStmt::Create(Context.CxxAST,
                                                        Loc, Cast, nullptr);
      llvm::SmallVector<clang::Stmt* , 1> BlockStmts({RetStmt});
      clang::CompoundStmt *Block = clang::CompoundStmt::Create(Context.CxxAST,
                                                                BlockStmts,
                                                                Loc, Loc);
      InPlaceNew->setBody(Block);
    }
  }
  InPlaceNew->setImplicit();
  Owner->addDecl(InPlaceNew);
}

clang::FunctionDecl *Sema::getInPlaceNew() {
  return InPlaceNew;
}


/// This creates the operator new and delete then uses the rebuilder in order
/// to create declarations so they can be looked up by gold's lookup.
/// Without have to change the names of the functions in C++ land.
void Sema::createBuiltinOperatorNewDeleteDecls() {
  if (CxxSema.GlobalNewDeleteDeclared)
    return;

  CxxSema.DeclareGlobalNewDelete();
  clang::DeclarationName OpNewName
            = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_New);
  clang::DeclarationName OpArrayNew
      = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Array_New);
  clang::DeclarationName OpDeleteName
         = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Delete);
  clang::DeclarationName OpArrayDelete
      = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Array_Delete);
  Declaration *TUDeclaration = getTranslationUnit();
  auto *TU = cast<clang::TranslationUnitDecl>(TUDeclaration->Cxx);
  auto NewOperators = TU->lookup(OpNewName);

  // Rebuilding operator new.
  for (clang::Decl *D : NewOperators) {
    ClangToGoldDeclRebuilder Rebuilder(Context, *this);
    Rebuilder.rebuildDeclWithNewName(TUDeclaration,
                                     OpInfo.GoldDecl_OpNew->getName(), D,
                                     getTranslationUnitScope());
  }

  auto DeleteOperators = TU->lookup(OpDeleteName);
  for (clang::Decl *D : DeleteOperators) {
    ClangToGoldDeclRebuilder Rebuilder(Context, *this);
    Rebuilder.rebuildDeclWithNewName(TUDeclaration,
                                     OpInfo.GoldDecl_OpDelete->getName(), D,
                                     getTranslationUnitScope());
  }

  auto NewArrayOps = TU->lookup(OpArrayNew);
  for (clang::Decl *D : NewArrayOps) {
    ClangToGoldDeclRebuilder Rebuilder(Context, *this);
    Rebuilder.rebuildDeclWithNewName(TUDeclaration,
                                     OpInfo.GoldDecl_OpArray_New->getName(), D,
                                     getTranslationUnitScope());
  }

  auto ArrayDeleteOps = TU->lookup(OpArrayDelete);
  for (clang::Decl *D : ArrayDeleteOps) {
    ClangToGoldDeclRebuilder Rebuilder(Context, *this);
    Rebuilder.rebuildDeclWithNewName(TUDeclaration,
                                     OpInfo.GoldDecl_OpArray_Delete->getName(),
                                     D, getTranslationUnitScope());
  }
}

clang::DeclarationNameInfo
Sema::rebuildDeclarationNameInfo(const clang::DeclarationNameInfo &DNI) {
  if (DNI.getName().getNameKind() == clang::DeclarationName::Identifier) {
    clang::DeclarationName Name = DNI.getName();
    if (Name.getAsIdentifierInfo() == OpInfo.GoldDecl_OpNew) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_New);
    } else if (Name.getAsIdentifierInfo() == OpInfo.GoldDecl_OpArray_New) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Array_New);
    } else if (Name.getAsIdentifierInfo() == OpInfo.GoldDecl_OpDelete) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Delete);
    } else if (Name.getAsIdentifierInfo() == OpInfo.GoldDecl_OpArray_Delete) {
      Name = Context.CxxAST.DeclarationNames.getCXXOperatorName(clang::OO_Array_Delete);
    }
    return clang::DeclarationNameInfo(Name, DNI.getLoc());
  } else {
    // There are no conversion made for non-identifier names (yet).
    return DNI;
  }
}


clang::QualType Sema::TransformCppxTypeExprType(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::TypeLocBuilder &TLB, clang::CppxTypeExprTypeLoc TL) {
  auto Ty = TL.getType()->getAs<clang::CppxTypeExprType>();

  assert(Ty && "invalid type pointer");
  assert(Ty->getTyExpr() && "Invalid type expression");
  DependentExprTransformer rebuilder(*this, Context, TemplateArgs, Loc, Entity);
  clang::Expr *Ret = rebuilder.transformDependentExpr(Ty->getTyExpr());
  if (!Ret)
    // Returning an empty type because this was an error.
    return clang::QualType();
  clang::QualType OutTy;
  clang::TypeSourceInfo *TInfo = nullptr;
  if (Ty->isForConstruct()) {
    OutTy = Ret->getType();
    if (OutTy->isDependentType()) {
      // This should never happen unless we have an impossible/invalid
      // AST structure, that we failed to account for.
      llvm_unreachable("Invalid type transformation");
    }
    if (OutTy->isPointerType()) {
      OutTy = OutTy->getPointeeType();
    } else {
      Diags.Report(Ret->getExprLoc(),
                   clang::diag::err_construct_on_non_pointer_ty);
      return clang::QualType();
    }
  } else {
    TInfo = getTypeSourceInfoFromExpr(Ret, Ret->getExprLoc());
    if (!TInfo)
      return clang::QualType();

    OutTy = TInfo->getType();
  }
  TInfo = BuildAnyTypeLoc(Context.CxxAST, TLB, OutTy,
                          Ty->getTyExpr()->getExprLoc());
  return TInfo->getType();
}

clang::Expr *Sema::TransformCppxDependentMemberAccessExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxDependentMemberAccessExpr *E) {
  DependentExprTransformer rebuilder(*this, Context, TemplateArgs, Loc, Entity);
  return rebuilder.transformDependentExpr(E);
}

clang::Expr *Sema::TransformCppxTemplateOrArrayExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxTemplateOrArrayExpr *E) {
  DependentExprTransformer rebuilder(*this, Context, TemplateArgs, Loc, Entity);
  return rebuilder.transformDependentExpr(E);
}

clang::Expr *Sema::TransformCppxCallOrConstructorExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxCallOrConstructorExpr *E) {
  DependentExprTransformer rebuilder(*this, Context, TemplateArgs, Loc, Entity);
  return rebuilder.transformDependentExpr(E);
}
clang::Expr *Sema::TransformCppxDerefOrPtrExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxDerefOrPtrExpr *E) {
  DependentExprTransformer rebuilder(*this, Context, TemplateArgs, Loc, Entity);
  return rebuilder.transformDependentExpr(E);
}

clang::ParsedTemplateArgument Sema::convertExprToTemplateArg(clang::Expr *E) {
  // Type parameters start here.
  if (E->getType()->isTypeOfTypes()) {
    clang::TypeSourceInfo *TInfo = getTypeSourceInfoFromExpr(
                                          E, E->getExprLoc());
    if (!TInfo)
      return clang::ParsedTemplateArgument();

    return getCxxSema().ActOnTemplateTypeArgument(
               getCxxSema().CreateParsedType(TInfo->getType(), TInfo));
  }

  if (E->getType()->isTemplateType()) {
    clang::TemplateDecl *TD =
      E->getType()->getAs<clang::CppxTemplateType>()->getTemplateDecl();

    return clang::ParsedTemplateArgument(clang::ParsedTemplateArgument::Template,
                                         (void *)TD, E->getExprLoc());
  }

  // Anything else is a constant expression?
  clang::ExprResult ConstExpr(E);
  ConstExpr = getCxxSema().ActOnConstantExpression(ConstExpr);
  return clang::ParsedTemplateArgument(clang::ParsedTemplateArgument::NonType,
      ConstExpr.get(), E->getExprLoc());
}

// BuildCXXNew - added to help with inplace __GoldInplaceNew/.construct
// when used with a dependent expression.
namespace {
  struct UsualDeallocFnInfo {
    UsualDeallocFnInfo() : Found(), FD(nullptr) {}
    UsualDeallocFnInfo(clang::Sema &S, clang::DeclAccessPair Found)
        : Found(Found), FD(dyn_cast<clang::FunctionDecl>(Found->getUnderlyingDecl())),
          Destroying(false), HasSizeT(false), HasAlignValT(false),
          CUDAPref(clang::Sema::CFP_Native) {
      using namespace clang;
      // A function template declaration is never a usual deallocation function.
      if (!FD)
        return;
      unsigned NumBaseParams = 1;
      if (FD->isDestroyingOperatorDelete()) {
        Destroying = true;
        ++NumBaseParams;
      }

      if (NumBaseParams < FD->getNumParams() &&
          S.Context.hasSameUnqualifiedType(
              FD->getParamDecl(NumBaseParams)->getType(),
              S.Context.getSizeType())) {
        ++NumBaseParams;
        HasSizeT = true;
      }

      if (NumBaseParams < FD->getNumParams() &&
          FD->getParamDecl(NumBaseParams)->getType()->isAlignValT()) {
        ++NumBaseParams;
        HasAlignValT = true;
      }

      // In CUDA, determine how much we'd like / dislike to call this.
      if (S.getLangOpts().CUDA)
        if (auto *Caller = dyn_cast<FunctionDecl>(S.CurContext))
          CUDAPref = S.IdentifyCUDAPreference(Caller, FD);
    }

    explicit operator bool() const { return FD; }

    bool isBetterThan(const UsualDeallocFnInfo &Other, bool WantSize,
                      bool WantAlign) const {
      // C++ P0722:
      //   A destroying operator delete is preferred over a non-destroying
      //   operator delete.
      if (Destroying != Other.Destroying)
        return Destroying;

      // C++17 [expr.delete]p10:
      //   If the type has new-extended alignment, a function with a parameter
      //   of type std::align_val_t is preferred; otherwise a function without
      //   such a parameter is preferred
      if (HasAlignValT != Other.HasAlignValT)
        return HasAlignValT == WantAlign;

      if (HasSizeT != Other.HasSizeT)
        return HasSizeT == WantSize;

      // Use CUDA call preference as a tiebreaker.
      return CUDAPref > Other.CUDAPref;
    }

    clang::DeclAccessPair Found;
    clang::FunctionDecl *FD;
    bool Destroying, HasSizeT, HasAlignValT;
    clang::Sema::CUDAFunctionPreference CUDAPref;
  };
}


/// Determine whether the given function is a non-placement
/// deallocation function.
static bool isNonPlacementDeallocationFunction(clang::Sema &S, clang::FunctionDecl *FD) {
  using namespace clang;
  if (CXXMethodDecl *Method = dyn_cast<CXXMethodDecl>(FD))
    return S.isUsualDeallocationFunction(Method);

  if (FD->getOverloadedOperator() != OO_Delete &&
      FD->getOverloadedOperator() != OO_Array_Delete)
    return false;

  unsigned UsualParams = 1;

  if (S.getLangOpts().SizedDeallocation && UsualParams < FD->getNumParams() &&
      S.Context.hasSameUnqualifiedType(
          FD->getParamDecl(UsualParams)->getType(),
          S.Context.getSizeType()))
    ++UsualParams;

  if (S.getLangOpts().AlignedAllocation && UsualParams < FD->getNumParams() &&
      S.Context.hasSameUnqualifiedType(
          FD->getParamDecl(UsualParams)->getType(),
          S.Context.getTypeDeclType(S.getStdAlignValT())))
    ++UsualParams;

  return UsualParams == FD->getNumParams();
}

/// Select the correct "usual" deallocation function to use from a selection of
/// deallocation functions (either global or class-scope).
static UsualDeallocFnInfo resolveDeallocationOverload(
    clang::Sema &S, clang::LookupResult &R, bool WantSize, bool WantAlign,
    llvm::SmallVectorImpl<UsualDeallocFnInfo> *BestFns = nullptr) {
  using namespace clang;
  UsualDeallocFnInfo Best;

  for (auto I = R.begin(), E = R.end(); I != E; ++I) {
    UsualDeallocFnInfo Info(S, I.getPair());
    if (!Info || !isNonPlacementDeallocationFunction(S, Info.FD) ||
        Info.CUDAPref == clang::Sema::CFP_Never)
      continue;

    if (!Best) {
      Best = Info;
      if (BestFns)
        BestFns->push_back(Info);
      continue;
    }

    if (Best.isBetterThan(Info, WantSize, WantAlign))
      continue;

    //   If more than one preferred function is found, all non-preferred
    //   functions are eliminated from further consideration.
    if (BestFns && Info.isBetterThan(Best, WantSize, WantAlign))
      BestFns->clear();

    Best = Info;
    if (BestFns)
      BestFns->push_back(Info);
  }

  return Best;
}

static bool hasNewExtendedAlignment(clang::Sema &S, clang::QualType AllocType) {
  return S.getLangOpts().AlignedAllocation &&
         S.getASTContext().getTypeAlignIfKnown(AllocType) >
             S.getASTContext().getTargetInfo().getNewAlign();
}

/// Determine whether a given type is a class for which 'delete[]' would call
/// a member 'operator delete[]' with a 'size_t' parameter. This implies that
/// we need to store the array size (even if the type is
/// trivially-destructible).
static bool doesUsualArrayDeleteWantSize(clang::Sema &S, clang::SourceLocation loc,
                                         clang::QualType allocType) {
  using namespace clang;
  const RecordType *record =
    allocType->getBaseElementTypeUnsafe()->getAs<RecordType>();
  if (!record) return false;

  // Try to find an operator delete[] in class scope.

  DeclarationName deleteName =
    S.Context.DeclarationNames.getCXXOperatorName(OO_Array_Delete);
  LookupResult ops(S, deleteName, loc, clang::Sema::LookupOrdinaryName);
  S.LookupQualifiedName(ops, record->getDecl());

  // We're just doing this for information.
  ops.suppressDiagnostics();

  // Very likely: there's no operator delete[].
  if (ops.empty()) return false;

  // If it's ambiguous, it should be illegal to call operator delete[]
  // on this thing, so it doesn't matter if we allocate extra space or not.
  if (ops.isAmbiguous()) return false;

  // C++17 [expr.delete]p10:
  //   If the deallocation functions have class scope, the one without a
  //   parameter of type std::size_t is selected.
  auto Best = resolveDeallocationOverload(
      S, ops, /*WantSize*/false,
      /*WantAlign*/hasNewExtendedAlignment(S, allocType));
  return Best && Best.HasSizeT;
}

static bool isLegalArrayNewInitializer(clang::CXXNewExpr::InitializationStyle Style,
                                       clang::Expr *Init) {
  using namespace clang;
  if (!Init)
    return true;
  if (ParenListExpr *PLE = dyn_cast<ParenListExpr>(Init))
    return PLE->getNumExprs() == 0;
  if (isa<ImplicitValueInitExpr>(Init))
    return true;
  else if (CXXConstructExpr *CCE = dyn_cast<CXXConstructExpr>(Init))
    return !CCE->isListInitialization() &&
           CCE->getConstructor()->isDefaultConstructor();
  else if (Style == CXXNewExpr::ListInit) {
    assert(isa<InitListExpr>(Init) &&
           "Shouldn't create list CXXConstructExprs for arrays.");
    return true;
  }
  return false;
}

clang::ExprResult
Sema::BuildCXXNew(clang::SourceRange Range, bool UseGlobal,
                  clang::SourceLocation PlacementLParen,
                  clang::MultiExprArg PlacementArgs,
                  clang::SourceLocation PlacementRParen,
                  clang::SourceRange TypeIdParens,
                  clang::QualType AllocType,
                  clang::TypeSourceInfo *AllocTypeInfo,
                  llvm::Optional<clang::Expr *> ArraySize,
                  clang::SourceRange DirectInitRange,
                  clang::Expr *Initializer,
                  bool UseGoldInplaceNew) {
  using namespace clang;
  SourceRange TypeRange = AllocTypeInfo->getTypeLoc().getSourceRange();
  SourceLocation StartLoc = Range.getBegin();

  CXXNewExpr::InitializationStyle initStyle;
  if (DirectInitRange.isValid()) {
    assert(Initializer && "Have parens but no initializer.");
    initStyle = CXXNewExpr::CallInit;
  } else if (Initializer && isa<InitListExpr>(Initializer))
    initStyle = CXXNewExpr::ListInit;
  else {
    assert((!Initializer || isa<ImplicitValueInitExpr>(Initializer) ||
            isa<CXXConstructExpr>(Initializer)) &&
           "Initializer expression that cannot have been implicitly created.");
    initStyle = CXXNewExpr::NoInit;
  }

  Expr **Inits = &Initializer;
  unsigned NumInits = Initializer ? 1 : 0;
  if (ParenListExpr *List = dyn_cast_or_null<ParenListExpr>(Initializer)) {
    assert(initStyle == CXXNewExpr::CallInit && "paren init for non-call init");
    Inits = List->getExprs();
    NumInits = List->getNumExprs();
  }

  // C++11 [expr.new]p15:
  //   A new-expression that creates an object of type T initializes that
  //   object as follows:
  InitializationKind Kind
      //     - If the new-initializer is omitted, the object is default-
      //       initialized (8.5); if no initialization is performed,
      //       the object has indeterminate value
      = initStyle == CXXNewExpr::NoInit
            ? InitializationKind::CreateDefault(TypeRange.getBegin())
            //     - Otherwise, the new-initializer is interpreted according to
            //     the
            //       initialization rules of 8.5 for direct-initialization.
            : initStyle == CXXNewExpr::ListInit
                  ? InitializationKind::CreateDirectList(
                        TypeRange.getBegin(), Initializer->getBeginLoc(),
                        Initializer->getEndLoc())
                  : InitializationKind::CreateDirect(TypeRange.getBegin(),
                                                     DirectInitRange.getBegin(),
                                                     DirectInitRange.getEnd());

  // C++11 [dcl.spec.auto]p6. Deduce the type which 'auto' stands in for.
  auto *Deduced = AllocType->getContainedDeducedType();
  if (Deduced && isa<DeducedTemplateSpecializationType>(Deduced)) {
    if (ArraySize)
      return ExprError(
          getCxxSema().Diag(ArraySize ? (*ArraySize)->getExprLoc() : TypeRange.getBegin(),
               diag::err_deduced_class_template_compound_type)
          << /*array*/ 2
          << (ArraySize ? (*ArraySize)->getSourceRange() : TypeRange));

    InitializedEntity Entity
      = InitializedEntity::InitializeNew(StartLoc, AllocType);
    AllocType = getCxxSema().DeduceTemplateSpecializationFromInitializer(
        AllocTypeInfo, Entity, Kind, MultiExprArg(Inits, NumInits));
    if (AllocType.isNull())
      return ExprError();
  } else if (Deduced) {
    bool Braced = (initStyle == CXXNewExpr::ListInit);
    if (NumInits == 1) {
      if (auto p = dyn_cast_or_null<InitListExpr>(Inits[0])) {
        Inits = p->getInits();
        NumInits = p->getNumInits();
        Braced = true;
      }
    }

    if (initStyle == CXXNewExpr::NoInit || NumInits == 0)
      return ExprError(getCxxSema().Diag(StartLoc, diag::err_auto_new_requires_ctor_arg)
                       << AllocType << TypeRange);
    if (NumInits > 1) {
      Expr *FirstBad = Inits[1];
      return ExprError(getCxxSema().Diag(FirstBad->getBeginLoc(),
                            diag::err_auto_new_ctor_multiple_expressions)
                       << AllocType << TypeRange);
    }
    if (Braced && !getCxxSema().getLangOpts().CPlusPlus17)
      getCxxSema().Diag(Initializer->getBeginLoc(), diag::ext_auto_new_list_init)
          << AllocType << TypeRange;
    Expr *Deduce = Inits[0];
    QualType DeducedType;
    if (getCxxSema().DeduceAutoType(AllocTypeInfo, Deduce, DeducedType) == clang::Sema::DAR_Failed)
      return ExprError(getCxxSema().Diag(StartLoc, diag::err_auto_new_deduction_failure)
                       << AllocType << Deduce->getType()
                       << TypeRange << Deduce->getSourceRange());
    if (DeducedType.isNull())
      return ExprError();
    AllocType = DeducedType;
  }

  // Per C++0x [expr.new]p5, the type being constructed may be a
  // typedef of an array type.
  if (!ArraySize) {
    if (const ConstantArrayType *Array
                              = Context.CxxAST.getAsConstantArrayType(AllocType)) {
      ArraySize = IntegerLiteral::Create(Context.CxxAST, Array->getSize(),
                                         Context.CxxAST.getSizeType(),
                                         TypeRange.getEnd());
      AllocType = Array->getElementType();
    }
  }

  if (getCxxSema().CheckAllocatedType(AllocType, TypeRange.getBegin(), TypeRange))
    return ExprError();

  // In ARC, infer 'retaining' for the allocated
  if (getCxxSema().getLangOpts().ObjCAutoRefCount &&
      AllocType.getObjCLifetime() == Qualifiers::OCL_None &&
      AllocType->isObjCLifetimeType()) {
    AllocType = Context.CxxAST.getLifetimeQualifiedType(AllocType,
                                    AllocType->getObjCARCImplicitLifetime());
  }

  QualType ResultType = Context.CxxAST.getPointerType(AllocType);

  if (ArraySize && *ArraySize &&
      (*ArraySize)->getType()->isNonOverloadPlaceholderType()) {
    ExprResult result = getCxxSema().CheckPlaceholderExpr(*ArraySize);
    if (result.isInvalid()) return ExprError();
    ArraySize = result.get();
  }
  // C++98 5.3.4p6: "The expression in a direct-new-declarator shall have
  //   integral or enumeration type with a non-negative value."
  // C++11 [expr.new]p6: The expression [...] shall be of integral or unscoped
  //   enumeration type, or a class type for which a single non-explicit
  //   conversion function to integral or unscoped enumeration type exists.
  // C++1y [expr.new]p6: The expression [...] is implicitly converted to
  //   std::size_t.
  llvm::Optional<uint64_t> KnownArraySize;
  if (ArraySize && *ArraySize && !(*ArraySize)->isTypeDependent()) {
    ExprResult ConvertedSize;
    if (getCxxSema().getLangOpts().CPlusPlus14) {
      assert(Context.CxxAST.getTargetInfo().getIntWidth()
             && "Builtin type of size 0?");

      ConvertedSize = getCxxSema().PerformImplicitConversion(*ArraySize,
                                                   Context.CxxAST.getSizeType(),
                                                    clang::Sema::AA_Converting);

      if (!ConvertedSize.isInvalid() &&
          (*ArraySize)->getType()->getAs<RecordType>())
        // Diagnose the compatibility of this conversion.
        getCxxSema().Diag(StartLoc, diag::warn_cxx98_compat_array_size_conversion)
          << (*ArraySize)->getType() << 0 << "'size_t'";
    } else {
      class SizeConvertDiagnoser : public clang::Sema::ICEConvertDiagnoser {
      protected:
        Expr *ArraySize;

      public:
        SizeConvertDiagnoser(Expr *ArraySize)
            : ICEConvertDiagnoser(/*AllowScopedEnumerations*/false, false, false),
              ArraySize(ArraySize) {}

        clang::Sema::SemaDiagnosticBuilder diagnoseNotInt(clang::Sema &S, SourceLocation Loc,
                                             QualType T) override {
          return S.Diag(Loc, diag::err_array_size_not_integral)
                   << S.getLangOpts().CPlusPlus11 << T;
        }

        clang::Sema::SemaDiagnosticBuilder diagnoseIncomplete(
            clang::Sema &S, SourceLocation Loc, QualType T) override {
          return S.Diag(Loc, diag::err_array_size_incomplete_type)
                   << T << ArraySize->getSourceRange();
        }

        clang::Sema::SemaDiagnosticBuilder diagnoseExplicitConv(
            clang::Sema &S, SourceLocation Loc, QualType T, QualType ConvTy) override {
          return S.Diag(Loc, diag::err_array_size_explicit_conversion) << T << ConvTy;
        }

        clang::Sema::SemaDiagnosticBuilder noteExplicitConv(
            clang::Sema &S, CXXConversionDecl *Conv, QualType ConvTy) override {
          return S.Diag(Conv->getLocation(), diag::note_array_size_conversion)
                   << ConvTy->isEnumeralType() << ConvTy;
        }

        clang::Sema::SemaDiagnosticBuilder diagnoseAmbiguous(
            clang::Sema &S, SourceLocation Loc, QualType T) override {
          return S.Diag(Loc, diag::err_array_size_ambiguous_conversion) << T;
        }

        clang::Sema::SemaDiagnosticBuilder noteAmbiguous(
            clang::Sema &S, CXXConversionDecl *Conv, QualType ConvTy) override {
          return S.Diag(Conv->getLocation(), diag::note_array_size_conversion)
                   << ConvTy->isEnumeralType() << ConvTy;
        }

        clang::Sema::SemaDiagnosticBuilder diagnoseConversion(clang::Sema &S, SourceLocation Loc,
                                                 QualType T,
                                                 QualType ConvTy) override {
          return S.Diag(Loc,
                        S.getLangOpts().CPlusPlus11
                          ? diag::warn_cxx98_compat_array_size_conversion
                          : diag::ext_array_size_conversion)
                   << T << ConvTy->isEnumeralType() << ConvTy;
        }
      } SizeDiagnoser(*ArraySize);

      ConvertedSize = getCxxSema().PerformContextualImplicitConversion(StartLoc,
                                                                     *ArraySize,
                                                                 SizeDiagnoser);
    }
    if (ConvertedSize.isInvalid())
      return ExprError();

    ArraySize = ConvertedSize.get();
    QualType SizeType = (*ArraySize)->getType();

    if (!SizeType->isIntegralOrUnscopedEnumerationType())
      return ExprError();

    // C++98 [expr.new]p7:
    //   The expression in a direct-new-declarator shall have integral type
    //   with a non-negative value.
    //
    // Let's see if this is a constant < 0. If so, we reject it out of hand,
    // per CWG1464. Otherwise, if it's not a constant, we must have an
    // unparenthesized array type.
    if (!(*ArraySize)->isValueDependent()) {
      // We've already performed any required implicit conversion to integer or
      // unscoped enumeration type.
      // FIXME: Per CWG1464, we are required to check the value prior to
      // converting to size_t. This will never find a negative array size in
      // C++14 onwards, because Value is always unsigned here!
      Expr::EvalContext EvalCtx(Context.CxxAST, getCxxSema().GetReflectionCallbackObj());
      if (Optional<llvm::APSInt> Value =
              (*ArraySize)->getIntegerConstantExpr(EvalCtx)) {
        if (Value->isSigned() && Value->isNegative()) {
          return ExprError(getCxxSema().Diag((*ArraySize)->getBeginLoc(),
                                diag::err_typecheck_negative_array_size)
                           << (*ArraySize)->getSourceRange());
        }

        if (!AllocType->isDependentType()) {
          unsigned ActiveSizeBits = ConstantArrayType::getNumAddressingBits(
              Context.CxxAST, AllocType, *Value);
          if (ActiveSizeBits > ConstantArrayType::getMaxSizeBits(Context.CxxAST))
            return ExprError(
                getCxxSema().Diag((*ArraySize)->getBeginLoc(), diag::err_array_too_large)
                << Value->toString(10) << (*ArraySize)->getSourceRange());
        }

        KnownArraySize = Value->getZExtValue();
      } else if (TypeIdParens.isValid()) {
        // Can't have dynamic array size when the type-id is in parentheses.
        getCxxSema().Diag((*ArraySize)->getBeginLoc(), diag::ext_new_paren_array_nonconst)
            << (*ArraySize)->getSourceRange()
            << FixItHint::CreateRemoval(TypeIdParens.getBegin())
            << FixItHint::CreateRemoval(TypeIdParens.getEnd());

        TypeIdParens = SourceRange();
      }
    }

    // Note that we do *not* convert the argument in any way.  It can
    // be signed, larger than size_t, whatever.
  }

  FunctionDecl *OperatorNew = nullptr;
  FunctionDecl *OperatorDelete = nullptr;
  unsigned Alignment =
      AllocType->isDependentType() ? 0 : Context.CxxAST.getTypeAlign(AllocType);
  unsigned NewAlignment = Context.CxxAST.getTargetInfo().getNewAlign();
  bool PassAlignment = getCxxSema().getLangOpts().AlignedAllocation &&
                       Alignment > NewAlignment;
  if (UseGoldInplaceNew) {
    OperatorNew = getInPlaceNew();
  } else {
    clang::Sema::AllocationFunctionScope Scope = UseGlobal ?
                                                clang::Sema::AFS_Global :
                                                clang::Sema::AFS_Both;
    if (!AllocType->isDependentType() &&
        !Expr::hasAnyTypeDependentArguments(PlacementArgs) &&
        getCxxSema().FindAllocationFunctions(
            StartLoc, SourceRange(PlacementLParen, PlacementRParen), Scope, Scope,
            AllocType, ArraySize.hasValue(), PassAlignment, PlacementArgs,
            OperatorNew, OperatorDelete))
      return ExprError();

  }
  // If this is an array allocation, compute whether the usual array
  // deallocation function for the type has a size_t parameter.
  bool UsualArrayDeleteWantsSize = false;
  if (ArraySize && !AllocType->isDependentType())
    UsualArrayDeleteWantsSize =
        doesUsualArrayDeleteWantSize(getCxxSema(), StartLoc, AllocType);

  SmallVector<Expr *, 8> AllPlaceArgs;
  if (OperatorNew) {
    auto *Proto = OperatorNew->getType()->castAs<FunctionProtoType>();
    clang::Sema::VariadicCallType CallType = Proto->isVariadic() ? clang::Sema::VariadicFunction
                                                    : clang::Sema::VariadicDoesNotApply;

    // We've already converted the placement args, just fill in any default
    // arguments. Skip the first parameter because we don't have a corresponding
    // argument. Skip the second parameter too if we're passing in the
    // alignment; we've already filled it in.
    unsigned NumImplicitArgs = PassAlignment ? 2 : 1;
    if (getCxxSema().GatherArgumentsForCall(PlacementLParen, OperatorNew, Proto,
                               NumImplicitArgs, PlacementArgs, AllPlaceArgs,
                               CallType))
      return ExprError();

    if (!AllPlaceArgs.empty())
      PlacementArgs = AllPlaceArgs;

    // We would like to perform some checking on the given `operator new` call,
    // but the PlacementArgs does not contain the implicit arguments,
    // namely allocation size and maybe allocation alignment,
    // so we need to conjure them.

    QualType SizeTy = Context.CxxAST.getSizeType();
    unsigned SizeTyWidth = Context.CxxAST.getTypeSize(SizeTy);

    llvm::APInt SingleEltSize(
        SizeTyWidth, Context.CxxAST.getTypeSizeInChars(AllocType).getQuantity());

    // How many bytes do we want to allocate here?
    llvm::Optional<llvm::APInt> AllocationSize;
    if (!ArraySize.hasValue() && !AllocType->isDependentType()) {
      // For non-array operator new, we only want to allocate one element.
      AllocationSize = SingleEltSize;
    } else if (KnownArraySize.hasValue() && !AllocType->isDependentType()) {
      // For array operator new, only deal with static array size case.
      bool Overflow;
      AllocationSize = llvm::APInt(SizeTyWidth, *KnownArraySize)
                           .umul_ov(SingleEltSize, Overflow);
      (void)Overflow;
      assert(
          !Overflow &&
          "Expected that all the overflows would have been handled already.");
    }

    IntegerLiteral AllocationSizeLiteral(
        Context.CxxAST,
        AllocationSize.getValueOr(llvm::APInt::getNullValue(SizeTyWidth)),
        SizeTy, SourceLocation());
    // Otherwise, if we failed to constant-fold the allocation size, we'll
    // just give up and pass-in something opaque, that isn't a null pointer.
    OpaqueValueExpr OpaqueAllocationSize(SourceLocation(), SizeTy, VK_RValue,
                                         OK_Ordinary, /*SourceExpr=*/nullptr);

    // Let's synthesize the alignment argument in case we will need it.
    // Since we *really* want to allocate these on stack, this is slightly ugly
    // because there might not be a `std::align_val_t` type.
    EnumDecl *StdAlignValT = getCxxSema().getStdAlignValT();
    QualType AlignValT =
        StdAlignValT ? Context.CxxAST.getTypeDeclType(StdAlignValT) : SizeTy;
    IntegerLiteral AlignmentLiteral(
        Context.CxxAST,
        llvm::APInt(Context.CxxAST.getTypeSize(SizeTy),
                    Alignment / Context.CxxAST.getCharWidth()),
        SizeTy, SourceLocation());
    ImplicitCastExpr DesiredAlignment(ImplicitCastExpr::OnStack, AlignValT,
                                      CK_IntegralCast, &AlignmentLiteral,
                                      VK_RValue, FPOptionsOverride());

    // Adjust placement args by prepending conjured size and alignment exprs.
    llvm::SmallVector<Expr *, 8> CallArgs;
    CallArgs.reserve(NumImplicitArgs + PlacementArgs.size());
    CallArgs.emplace_back(AllocationSize.hasValue()
                              ? static_cast<Expr *>(&AllocationSizeLiteral)
                              : &OpaqueAllocationSize);
    if (PassAlignment)
      CallArgs.emplace_back(&DesiredAlignment);
    CallArgs.insert(CallArgs.end(), PlacementArgs.begin(), PlacementArgs.end());

    getCxxSema().DiagnoseSentinelCalls(OperatorNew, PlacementLParen, CallArgs);

    getCxxSema().checkCall(OperatorNew, Proto, /*ThisArg=*/nullptr, CallArgs,
              /*IsMemberFunction=*/false, StartLoc, Range, CallType);

    // Warn if the type is over-aligned and is being allocated by (unaligned)
    // global operator new.
    if (PlacementArgs.empty() && !PassAlignment &&
        (OperatorNew->isImplicit() ||
         (OperatorNew->getBeginLoc().isValid() &&
          getCxxSema().getSourceManager().isInSystemHeader(OperatorNew->getBeginLoc())))) {
      if (Alignment > NewAlignment)
        getCxxSema().Diag(StartLoc, diag::warn_overaligned_type)
            << AllocType
            << unsigned(Alignment / Context.CxxAST.getCharWidth())
            << unsigned(NewAlignment / Context.CxxAST.getCharWidth());
    }
  }

  // Array 'new' can't have any initializers except empty parentheses.
  // Initializer lists are also allowed, in C++11. Rely on the parser for the
  // dialect distinction.
  if (ArraySize && !isLegalArrayNewInitializer(initStyle, Initializer)) {
    SourceRange InitRange(Inits[0]->getBeginLoc(),
                          Inits[NumInits - 1]->getEndLoc());
    getCxxSema().Diag(StartLoc, diag::err_new_array_init_args) << InitRange;
    return ExprError();
  }

  // If we can perform the initialization, and we've not already done so,
  // do it now.
  if (!AllocType->isDependentType() &&
      !Expr::hasAnyTypeDependentArguments(
          llvm::makeArrayRef(Inits, NumInits))) {
    // The type we initialize is the complete type, including the array bound.
    QualType InitType;
    if (KnownArraySize)
      InitType = Context.CxxAST.getConstantArrayType(
          AllocType,
          llvm::APInt(Context.CxxAST.getTypeSize(Context.CxxAST.getSizeType()),
                      *KnownArraySize),
          *ArraySize, clang::ArrayType::Normal, 0);
    else if (ArraySize)
      InitType =
          Context.CxxAST.getIncompleteArrayType(AllocType,
                                                clang::ArrayType::Normal,
                                                0);
    else
      InitType = AllocType;

    InitializedEntity Entity
      = InitializedEntity::InitializeNew(StartLoc, InitType);
    InitializationSequence InitSeq(getCxxSema(), Entity, Kind,
                                   MultiExprArg(Inits, NumInits));
    ExprResult FullInit = InitSeq.Perform(getCxxSema(), Entity, Kind,
                                          MultiExprArg(Inits, NumInits));
    if (FullInit.isInvalid())
      return ExprError();

    // FullInit is our initializer; strip off CXXBindTemporaryExprs, because
    // we don't want the initialized object to be destructed.
    // FIXME: We should not create these in the first place.
    if (CXXBindTemporaryExpr *Binder =
            dyn_cast_or_null<CXXBindTemporaryExpr>(FullInit.get()))
      FullInit = Binder->getSubExpr();

    Initializer = FullInit.get();

    // FIXME: If we have a KnownArraySize, check that the array bound of the
    // initializer is no greater than that constant value.

    if (ArraySize && !*ArraySize) {
      auto *CAT = Context.CxxAST.getAsConstantArrayType(Initializer->getType());
      if (CAT) {
        // FIXME: Track that the array size was inferred rather than explicitly
        // specified.
        ArraySize = IntegerLiteral::Create(
            Context.CxxAST, CAT->getSize(), Context.CxxAST.getSizeType(), TypeRange.getEnd());
      } else {
        getCxxSema().Diag(TypeRange.getEnd(), diag::err_new_array_size_unknown_from_init)
            << Initializer->getSourceRange();
      }
    }
  }

  // Mark the new and delete operators as referenced.
  if (OperatorNew) {
    if (getCxxSema().DiagnoseUseOfDecl(OperatorNew, StartLoc))
      return ExprError();
    getCxxSema().MarkFunctionReferenced(StartLoc, OperatorNew);
  }
  if (OperatorDelete) {
    if (getCxxSema().DiagnoseUseOfDecl(OperatorDelete, StartLoc))
      return ExprError();
    getCxxSema().MarkFunctionReferenced(StartLoc, OperatorDelete);
  }

  return CXXNewExpr::Create(Context.CxxAST, UseGlobal, OperatorNew, OperatorDelete,
                            PassAlignment, UsualArrayDeleteWantsSize,
                            PlacementArgs, TypeIdParens, ArraySize, initStyle,
                            Initializer, ResultType, AllocTypeInfo, Range,
                            DirectInitRange);
}


bool Sema::elaborateConstexpr(clang::Stmt *E) {
  GoldConstexprASTElaborator ConstExprElaborator(*this);
  return ConstExprElaborator.TraverseStmt(E);
}
} // namespace gold
