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
#include "clang/Sema/Lookup.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

namespace gold {

using namespace llvm;

static const llvm::StringMap<clang::QualType> createBuiltinTypeList(
    SyntaxContext &Context) {
  return {
    {"void", Context.CxxAST.VoidTy},
    {"bool", Context.CxxAST.BoolTy},
    {"null_t", Context.CxxAST.NullPtrTy},
    
    // character 
    {"char", Context.CxxAST.CharTy},
    {"char8", Context.CxxAST.getIntTypeForBitwidth(8, true)},
    {"char16", Context.CxxAST.getIntTypeForBitwidth(16, true)},
    {"char32", Context.CxxAST.getIntTypeForBitwidth(32, true)},

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
    {"float16", Context.CxxAST.HalfTy},
    {"float32", Context.CxxAST.getRealTypeForBitwidth(32)},
    {"float64", Context.CxxAST.getRealTypeForBitwidth(64)},
    {"float128", Context.CxxAST.getRealTypeForBitwidth(128)},
    {"double", Context.CxxAST.DoubleTy},

    // type of a type.
    {"type", Context.CxxAST.CppxKindTy}
  };
}

static llvm::StringMap<clang::UnaryOperatorKind> createUnaryOpMap() {
  return {
    // {"operator'&'", clang::AddrOf}, // FIXME: this needs to be designed
    {"operator'^'", clang::UO_Deref},
    {"operator'+'", clang::UO_Plus},
    {"operator'-'", clang::UO_Minus},
    // {"operator'~'", clang::UO_Not}, // FIXME: this needs to be designed
    {"operator'!'", clang::UO_LNot},
    {"operator'not'", clang::UO_LNot},
  };
}

static llvm::StringMap<clang::BinaryOperatorKind> createBinaryOpMap() {
  return {
    {"operator'+'" , clang::BO_Add},
    {"operator'-'" , clang::BO_Sub},
    {"operator'*'" , clang::BO_Mul},
    {"operator'/'" , clang::BO_Div},
    {"operator'%'" , clang::BO_Rem},
    {"operator'&'" , clang::BO_And},
    {"operator'|'" , clang::BO_Or},
    {"operator'^'" , clang::BO_Xor},
    {"operator'&&'" , clang::BO_LAnd},
    {"operator'and'", clang::BO_LAnd},
    {"operator'||'" , clang::BO_LOr},
    {"operator'or'" , clang::BO_LOr},
    {"operator'=='" , clang::BO_EQ},
    {"operator'<>'", clang::BO_NE},
    {"operator'<'", clang::BO_LT},
    {"operator'>'", clang::BO_GT},
    {"operator'<='", clang::BO_LE},
    {"operator'>='", clang::BO_GE},
    {"operator'+='" , clang::BO_AddAssign},
    {"operator'-='" , clang::BO_SubAssign},
    {"operator'*='" , clang::BO_MulAssign},
    {"operator'/='" , clang::BO_DivAssign},
    {"operator'%='" , clang::BO_RemAssign},
    {"operator'&='" , clang::BO_AndAssign},
    {"operator'|='" , clang::BO_OrAssign},
    {"operator'^='" , clang::BO_XorAssign}
  };
}

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : CxxSema(CxxSema), CurrentDecl(), Context(Context),
    Diags(Context.CxxAST.getSourceManager().getDiagnostics()),
    BuiltinTypes(createBuiltinTypeList(Context)),
    UnaryOpNames(createUnaryOpMap()),
    BinaryOpNames(createBinaryOpMap())
{
  NullTTy = Context.CxxAST.NullPtrTy;
  CharTy = Context.CxxAST.CharTy;
  Char8Ty = Context.CxxAST.getIntTypeForBitwidth(8, true);
  Char16Ty = Context.CxxAST.getIntTypeForBitwidth(16, true);
  Char32Ty = Context.CxxAST.getIntTypeForBitwidth(32, true);

  IntTy = Context.CxxAST.IntTy;
  Int8Ty = Context.CxxAST.getIntTypeForBitwidth(8, true);
  Int16Ty = Context.CxxAST.getIntTypeForBitwidth(16, true);
  Int32Ty = Context.CxxAST.getIntTypeForBitwidth(32, true);
  Int64Ty = Context.CxxAST.getIntTypeForBitwidth(64, true);
  Int128Ty = Context.CxxAST.getIntTypeForBitwidth(128, true);

  UIntTy = Context.CxxAST.UnsignedIntTy;
  UInt8Ty = Context.CxxAST.getIntTypeForBitwidth(8, false);
  UInt16Ty = Context.CxxAST.getIntTypeForBitwidth(16, false);
  UInt32Ty = Context.CxxAST.getIntTypeForBitwidth(32, false);
  UInt64Ty = Context.CxxAST.getIntTypeForBitwidth(64, false);
  UInt128Ty = Context.CxxAST.getIntTypeForBitwidth(128, false);

  Float16Ty = Context.CxxAST.HalfTy;
  Float32Ty = Context.CxxAST.getRealTypeForBitwidth(32);
  Float64Ty = Context.CxxAST.getRealTypeForBitwidth(64);
  Float128Ty = Context.CxxAST.getRealTypeForBitwidth(128);
  
  CxxSema.CurScope = nullptr;
  OperatorColonII = &Context.CxxAST.Idents.get("operator':'");
  OperatorArrowII = &Context.CxxAST.Idents.get("operator'->'");
  OperatorExclaimII = &Context.CxxAST.Idents.get("operator'!'");
  OperatorEqualsII = &Context.CxxAST.Idents.get("operator'='");
  OperatorIfII = &Context.CxxAST.Idents.get("operator'if'");
  OperatorElseII = &Context.CxxAST.Idents.get("operator'else'");
  OperatorReturnII = &Context.CxxAST.Idents.get("operator'return'");
  OperatorReturnsII = &Context.CxxAST.Idents.get("operator'returns'");
  OperatorDotII = &Context.CxxAST.Idents.get("operator'.'");
  OperatorForII = &Context.CxxAST.Idents.get("operator'for'");
  OperatorWhileII = &Context.CxxAST.Idents.get("operator'while'");
  OperatorInII = &Context.CxxAST.Idents.get("operator'in'");
  OperatorDotDotII = &Context.CxxAST.Idents.get("operator'..'");
  OperatorConstII =  &Context.CxxAST.Idents.get("operator'const'");
  OperatorRefII = &Context.CxxAST.Idents.get("operator'ref'");
  OperatorRRefII = &Context.CxxAST.Idents.get("operator'rref'");
  OperatorArrayBracketsII = &Context.CxxAST.Idents.get("operator'[]'");

  // All of the names of operators that we use.
  CPPOp_Plus = &Context.CxxAST.Idents.get("operator+");
  CPPOp_Minus = &Context.CxxAST.Idents.get("operator-");
  CPPOp_Mul = &Context.CxxAST.Idents.get("operator*");
  CPPOp_Div = &Context.CxxAST.Idents.get("operator/");
  CPPOp_Mod = &Context.CxxAST.Idents.get("operator%");
  CPPOp_BitWiseXOr = &Context.CxxAST.Idents.get("operator^");
  CPPOp_BitWiseOr = &Context.CxxAST.Idents.get("operator|");
  CPPOp_BitWiseAnd = &Context.CxxAST.Idents.get("operator&");
  CPPOp_BitWiseNot = &Context.CxxAST.Idents.get("operator~");
  CPPOp_BitWiseLeftShift = &Context.CxxAST.Idents.get("operator<<");
  CPPOp_BitWiseRightShift = &Context.CxxAST.Idents.get("operator>>");
  CPPOp_LOr = &Context.CxxAST.Idents.get("operator||");
  CPPOp_LAnd = &Context.CxxAST.Idents.get("operator&&");
  CPPOp_LNot = &Context.CxxAST.Idents.get("operator!");
  CPPOp_Less = &Context.CxxAST.Idents.get("operator<");
  CPPOp_Greater = &Context.CxxAST.Idents.get("operator>");
  CPPOp_LessEqual = &Context.CxxAST.Idents.get("operator<=");
  CPPOp_GreaterEqual = &Context.CxxAST.Idents.get("operator>=");
  CPPOp_Equal = &Context.CxxAST.Idents.get("operator==");
  CPPOp_NotEqual = &Context.CxxAST.Idents.get("operator!=");
  CPPOp_Assign = &Context.CxxAST.Idents.get("operator=");
  CPPOp_PlusAssign = &Context.CxxAST.Idents.get("operator+=");
  CPPOp_MinusAssign = &Context.CxxAST.Idents.get("operator-=");
  CPPOp_MulAssign = &Context.CxxAST.Idents.get("operator*=");
  CPPOp_DivAssign = &Context.CxxAST.Idents.get("operator/=");
  CPPOp_ModAssign = &Context.CxxAST.Idents.get("operator%=");
  CPPOp_BitWiseXOrAssign = &Context.CxxAST.Idents.get("operator^=");
  CPPOp_BitWiseOrAssign = &Context.CxxAST.Idents.get("operator|=");
  CPPOp_BitWiseAndAssign = &Context.CxxAST.Idents.get("operator&=");
  CPPOp_BitWiseLeftShiftAssign = &Context.CxxAST.Idents.get("operator<<=");
  CPPOp_BitWiseRightShiftAssign = &Context.CxxAST.Idents.get("operator>>=");
  CPPOp_ArrayAccess = &Context.CxxAST.Idents.get("operator[]");
  CPPOp_FunctionCall = &Context.CxxAST.Idents.get("operator()");
  CPPOp_Arrow = &Context.CxxAST.Idents.get("operator->");
}

Sema::~Sema() {

  assert(ScopeStack.empty() && "Scope stack is not empty.");
  delete getCurClangScope();
  CxxSema.CurScope = nullptr;
}

bool Sema::accessSpecifierIsValidInScope() const {
  return ScopeStack.back() && ScopeStack.back()->getKind() == SK_Class;
}

Scope *Sema::getCurrentScope() {
  if (ScopeStack.empty())
    return nullptr;
  return ScopeStack.back();
}

void Sema::pushScope(Scope *S) {
  // FIXME: The scope should self-describe itself. We can't rely on
  // the existence of Clang structures at the time we push a scope.
  // if (S->isDeclarationScope())
  //   CxxSema.PushFunctionScope();

  ScopeStack.push_back(S);
}

// llvm::SmallVector<Scope *, 4> ScopeStack;

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

void Sema::restoreDeclContext(Declaration *D) {
  CurrentDecl = D;
  getCxxSema().CurContext = clang::Decl::castToDeclContext(D->Cxx);
}

void Sema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);
  
  // FIXME: this might be an incorrect assertion.
  assert(D->Cxx && isa<clang::DeclContext>(D->Cxx)
         && "No Cxx declaration to push.");

  CurrentDecl = D;
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

bool Sema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}

bool Sema::lookupQualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, NNS->getScopeRep());
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

bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S) {
  assert(S);

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "Invalid id");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  // See if this is a builtin type, if we care about those.
  if (LookupKind == clang::Sema::LookupTagName ||
      LookupKind == clang::Sema::LookupAnyName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end())
      return true;
  }

  // This is done based on how CppLookUpName is handled, with a few exceptions,
  // this will return uninstantiated template declarations, namespaces,
  // and other kinds of declarations. This also handles some early elaboration
  // of some types.
  bool FoundFirstClassScope = false;
  for(;S; S = S->getParent()) {
    std::set<Declaration *> Found = S->findDecl(Id);
    if (!Found.empty()) {
      for (auto *FoundDecl : Found) {
        // If we find a name that hasn't been elaborated,
        // then we actually need to elaborate it.
        if (!FoundDecl->Cxx)
          Elaborator(Context, *this).elaborateDeclEarly(FoundDecl);

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
            ND = FD->getDescribedFunctionTemplate();
          else if (auto *VD = dyn_cast<clang::VarDecl>(ND))
            ND = VD->getDescribedVarTemplate();
          else
            llvm_unreachable("Unknown template function type");
        } else if (FoundDecl->declaresTemplateType()) {
          // This is used to get the correct template name.
          if (auto *RD = dyn_cast<clang::CXXRecordDecl>(FoundDecl->Cxx)) {
            ND = RD->getDescribedClassTemplate();
          }
        }
        R.addDecl(ND);
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
        if (DeclEntity->declaresRecord()) {
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

  // This is necessary because we are 
  return !R.empty();
}

bool Sema::scopeIsWithinClass() {
  return getCurrentScope()->getKind() & SK_Class;
}

bool Sema::scopeIsWithinClass(Scope *S) {
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

  if (D->declaresTemplateType() || D->declaresRecord())
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

clang::CppxTypeLiteral *Sema::buildTypeExpr(clang::QualType Ty, clang::SourceLocation Loc) {
  return buildAnyTypeExpr(Context.CxxAST.CppxKindTy, Ty, Loc);
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
  return buildAnyDeclRef(Context.CxxAST.getCppxNamespaceType(D->getNamespace()),
                         D, Loc);
}

clang::CppxDeclRefExpr *
Sema::buildAnyDeclRef(clang::QualType KindTy, clang::Decl *D,
                      clang::SourceLocation Loc) {
  assert(D && "Invalid declaration to reference.");
  return clang::CppxDeclRefExpr::create(Context.CxxAST, KindTy, D, Loc);
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
  // Diags.Report(DeclExpr->getExprLoc(),
  //                         clang::diag::err_expression_result_type_not_namespace)
  //     << DeclExpr;
  // return nullptr;
}



bool Sema::IsUnaryOperator(llvm::StringRef OpName) const {
  auto It = UnaryOpNames.find(OpName);
  return It != UnaryOpNames.end();
}

bool Sema::GetUnaryOperatorKind(llvm::StringRef OpName,
                                clang::UnaryOperatorKind &Kind) const{
  auto It = UnaryOpNames.find(OpName);
  if (It == UnaryOpNames.end())
    return true;

  Kind = It->second;
  return false;
}

bool Sema::IsBinaryOperator(llvm::StringRef OpName) const {
  auto It = BinaryOpNames.find(OpName);
  return It != BinaryOpNames.end();
}

bool Sema::GetBinaryOperatorKind(llvm::StringRef OpName,
    clang::BinaryOperatorKind &Kind) const {
  auto It = BinaryOpNames.find(OpName);
  if (It == BinaryOpNames.end())
    return true;
  Kind = It->second;
  return false;
}

} // namespace gold

