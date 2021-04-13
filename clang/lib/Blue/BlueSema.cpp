//===- BlueSema.cpp - Semantic Analysis of Blue ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Blue::Sema class, which performs semantic analysis
//  for the Blue language.
//
//===----------------------------------------------------------------------===//


#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/TypeLocUtil.h"

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueScope.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Blue/BlueElaborator.h"

namespace blue {

const llvm::StringMap<clang::QualType> Sema::createBuiltinTypeList() {
  return {
    {"void", CxxAST.VoidTy},
    {"bool", CxxAST.BoolTy},
    {"null_t", CxxAST.NullPtrTy},

    // character
    {"char", CxxAST.CharTy},
    {"char8", CxxAST.Char8Ty},
    {"char16", CxxAST.Char16Ty},
    {"char32", CxxAST.Char32Ty},

    // signed integers.
    {"int", CxxAST.IntTy},
    {"int8", CxxAST.SignedCharTy},
    {"int16", CxxAST.ShortTy},
    {"int32", CxxAST.IntTy},
    {"int64", CxxAST.LongTy},
    {"int128", CxxAST.Int128Ty},

    // unsigned integers.
    {"uint", CxxAST.UnsignedIntTy},
    {"uint8", CxxAST.UnsignedCharTy},
    {"uint16", CxxAST.UnsignedShortTy},
    {"uint32", CxxAST.UnsignedIntTy},
    {"uint64", CxxAST.UnsignedLongTy},
    {"uint128", CxxAST.UnsignedInt128Ty},

    // floating point types.
    {"float", CxxAST.DoubleTy},
    {"float32", CxxAST.FloatTy},
    {"float64", CxxAST.DoubleTy},
    {"float128", CxxAST.Float128Ty}
  };
}

static llvm::StringMap<clang::BinaryOperatorKind> getBinOpMapping() {
  return llvm::StringMap<clang::BinaryOperatorKind>{
    {"+", clang::BO_Add},
    {"-", clang::BO_Sub},
    {"^", clang::BO_Xor},
    {"/", clang::BO_Div},
    {"%", clang::BO_Rem},
    {"*", clang::BO_Mul},
    {"|", clang::BO_Or},
    {"<<", clang::BO_Shl},
    {">>", clang::BO_Shr},
    {"or", clang::BO_LOr},
    {"and", clang::BO_LAnd},
    {"<", clang::BO_LT},
    {">", clang::BO_GT},
    {"<=", clang::BO_LE},
    {">=", clang::BO_GE},
    {"==", clang::BO_EQ},
    {"!=", clang::BO_NE},
    {"=", clang::BO_Assign},
    {"+=", clang::BO_AddAssign},
    {"-=", clang::BO_SubAssign},
    {"*=", clang::BO_MulAssign},
    {"/=", clang::BO_DivAssign},
    {"%=", clang::BO_RemAssign},
    {"^=", clang::BO_XorAssign},
    {"|=", clang::BO_OrAssign},
    {"&=", clang::BO_AndAssign},
    {"<<=", clang::BO_ShlAssign},
    {">>=", clang::BO_ShrAssign}
  };
}

static llvm::StringMap<clang::UnaryOperatorKind> getUnaryOperatorPrefixMapping() {
  return llvm::StringMap<clang::UnaryOperatorKind>{
    {"++", clang::UO_PreInc},
    {"--", clang::UO_PreDec},
    {"not", clang::UO_LNot},
    {"^", clang::UO_AddrOf},
    {"+", clang::UO_Plus},
    {"-", clang::UO_Minus}
  };
}

static llvm::StringMap<clang::UnaryOperatorKind> getUnaryOperatorPostfixMapping() {
  return llvm::StringMap<clang::UnaryOperatorKind>{
    {"^", clang::UO_Deref},
    {"++", clang::UO_PostInc},
    {"--", clang::UO_PostDec}
  };
}

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : Context(Context), CxxSema(CxxSema),
    CxxAST(Context.CxxAST),
    BinOpMap(getBinOpMapping()),
    UnaryPrefixOpMap(getUnaryOperatorPrefixMapping()),
    UnaryPostfixOpMap(getUnaryOperatorPostfixMapping()),
    DefaultCharTy(CxxAST.CharTy),
    BuiltinTypes(createBuiltinTypeList())
{ }

Sema::~Sema() {
}

clang::Sema &Sema::getCxxSema() {
  return CxxSema;
}

clang::ASTContext &Sema::getCxxAST() {
  return CxxAST;
}

Scope *Sema::getCurrentScope() const {
  return ScopeStack.empty() ? nullptr : ScopeStack.back();
}

void Sema::enterScope(Scope::Kind K, const Syntax *S) {
  // FIXME: We're leaking scopes. We probably want to keep them bound to the
  // syntax for which they're created, especially for syntaxes that correspond
  // to declarations, so that we can easily find their associated lookup
  // tables. See the comments in leaveScope and saveScope.
  //
  // NOTE: Do not allocate this through the Context. It might be deleted.
  pushScope(new Scope(K, S, getCurrentScope()));
}

void Sema::leaveScope(const Syntax *S) {
  assert(getCurrentScope()->getTerm() == S);
  // FIXME: Delete the scope. Note that we don't delete the scope in saveScope.
  popScope();
}

Scope *Sema::saveScope(const Syntax *S) {
  assert(getCurrentScope()->getTerm() == S);
  // FIXME: Queue the scope for subsequent deletion?
  Scope *Scope = getCurrentScope();
  popScope();
  return Scope;
}

void Sema::pushScope(Scope *S) {
  assert(S && "Invalid scope");
  ScopeStack.push_back(S);
}

Scope *Sema::popScope() {
  Scope *R = ScopeStack.back();
  ScopeStack.pop_back();
  return R;
}

bool Sema::scopeIsClass() const {
  return getCurrentScope()->isClassScope();
}

clang::DeclContext *Sema::getCurClangDeclContext() const {
  return CxxSema.CurContext;
}

void Sema::pushDecl(Declaration *D) {
  assert(D->getOwner() == CurrentDecl);
  CurrentDecl = D;
  if (D->getCxx())
    getCxxSema().CurContext = clang::Decl::castToDeclContext(D->getCxx());
}

void Sema::setCurrentDecl(Declaration *D) {
  CurrentDecl = D;
}

void Sema::setClangDeclContext(clang::DeclContext *DC) {
  CxxSema.CurContext = DC;
}

void Sema::popDecl() {
  CurrentDecl = CurrentDecl->getOwner();
  getCxxSema().CurContext = CurrentDecl ?
    clang::Decl::castToDeclContext(CurrentDecl->getCxx()) : nullptr;
}


bool Sema::lookupUnqualifiedName(clang::LookupResult &R) {
  return lookupUnqualifiedName(R, getCurrentScope());
}


static void addIfNotDuplicate(clang::LookupResult &R, clang::NamedDecl *ND) {
  for (clang::Decl *D : R) {
    if (D == ND) {
      return;
    }
  }
  R.addDecl(ND);
}



bool Sema::lookupUnqualifiedName(clang::LookupResult &R, Scope *S) {
  assert(S && "lookup in non-existent scope");
  clang::DiagnosticsEngine &Diags = getCxxSema().Diags;

  clang::DeclarationName Name = R.getLookupName();
  clang::IdentifierInfo *Id = Name.getAsIdentifierInfo();
  assert(Id && "looking up non-existent name");

  clang::Sema::LookupNameKind LookupKind = R.getLookupKind();

  // First check if this is a builtin type name.
  if (LookupKind == clang::Sema::LookupAnyName) {
    auto BuiltinMapIter = BuiltinTypes.find(Id->getName());
    if (BuiltinMapIter != BuiltinTypes.end()) {
      if (BuiltinMapIter->second.isNull()) {
        Diags.Report(clang::SourceLocation(),
                     clang::diag::err_invalid_builtin_type) << Id;
        return true;
      }
      // This is a special case where the token is a built in type and
      // therefore can't return anything because that it doesn't
      // have a declaration. But it's not an error.
      return false;
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
  // bool FoundFirstClassScope = false;
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
        return false;

      bool FoundInNamespace = false;
      for (clang::UsingDirectiveDecl *UD : S->UsingDirectives) {
        assert(isa<clang::CppxNamespaceDecl>(UD->getNominatedNamespace()));

        clang::CppxNamespaceDecl *NS =
          cast<clang::CppxNamespaceDecl>(UD->getNominatedNamespace());
        std::set<Declaration *> NSFound = NS->getBlueScopeRep()->findDecl(Id);

        // We found the name in more than one namespace.
        if (FoundInNamespace && !NSFound.empty()) {
          Diags.Report(R.getNameLoc(), clang::diag::err_ambiguous_reference)
            << Name;
          return true;
        }

        FoundInNamespace = !NSFound.empty();
        Found = NSFound;
      }
    }

    if (!Found.empty()) {
      for (auto *FoundDecl : Found) {
        // Skipping this particular declaration to avoid triggering
        // double early elaboration.
        // if (FoundDecl == NotThisOne)
        //   continue;
        // If we find a name that hasn't been elaborated,
        // then we actually need to elaborate it.
        if (phaseOf(FoundDecl) < Phase::Typing) {
          if (FoundDecl->IsElaborating) {
            continue;
          }
          // TODO: In order to implement out of order elaboration we will need
          // to slightly change how we are doing elaboration. We will need an
          // identification phase. Without it, matching the declarations to their
          // identifiers becomes much more difficult.
          Elaborator(*this).elaborateDeclEarly(FoundDecl);
        }

        // Attempting to add special processing of declarations being elaborated
        // during a constant expression, and require full elaboration before
        // use.
        // if (CxxSema.isConstantEvaluated() || isInDeepElaborationMode()) {
        //   // If we aren't 100% completed then do complete elaboration.
        //   if ((phaseOf(FoundDecl) < Phase::Initialization)) {
        //     EnterDeepElabRAII DeepElab(*this);
        //     // change the elaboration context back to PotentiallyEvaluated.
        //     clang::EnterExpressionEvaluationContext ConstantEvaluated(CxxSema,
        //         clang::Sema::ExpressionEvaluationContext::PotentiallyEvaluated);
        //     AttrElabRAII Attr(*this, false);
        //     Elaborator(Context, *this).elaborateDeclEarly(FoundDecl);
        //   }
        // }

        // TODO: Add this back when we have namespaces elaborating && !FoundDecl->declaresNamespace()
        if (FoundDecl->IsElaborating ) {
          // This might be allowed in some scenarios Specifically when we
          // reference a class type within
          diagnoseElabCycleError(FoundDecl);
          return false;
        }

        // Skip early elaboration of declarations with nested name specifiers.
        // if (FoundDecl->hasNestedNameSpecifier())
        //   continue;

        // if (!FoundDecl->Cxx) {
        //   AttrElabRAII Attr(*this, false);
        //   Elaborator(Context, *this).elaborateDeclTypeEarly(FoundDecl);
        // }

        if (!FoundDecl->getCxx()) {
          // llvm_unreachable("referenced a declaration that contains an error.");
          return true;
        }

        clang::NamedDecl *ND = cast<clang::NamedDecl>(FoundDecl->getCxx());

        // FIXME: check if this is a tag decl, not a type decl!
        if (LookupKind == clang::Sema::LookupTagName &&
            !isa<clang::TypeDecl>(ND)) {
          // FIXME: Give a proper diagnostic once we implement hiding.
          // unsigned DiagID = Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
          //                                         "Tag is hidden.");
          // Diags.Report(clang::SourceLocation(), DiagID);
          return true;
        }

        // If there is a described template, add that to the result instead
        // of the bare declaration.
        if (FoundDecl->isFunctionTemplate()) {
          if (auto *FD = dyn_cast<clang::FunctionDecl>(ND))
            ND = FD->isFunctionTemplateSpecialization() ?
              FD->getPrimaryTemplate() : FD->getDescribedFunctionTemplate();
          else if (auto *VD = dyn_cast<clang::VarDecl>(ND))
            ND = VD->getDescribedVarTemplate();
          else
            llvm_unreachable("Unknown template function type");
        } else if (FoundDecl->isTypeTemplate()) {
          // We want the canonical declaration of a template unless it is
          // a specialization.
          using Specialization = clang::ClassTemplateSpecializationDecl;
          using Record = clang::CXXRecordDecl;
          if (auto *CD = dyn_cast<Specialization>(FoundDecl->getCxx())) {
            ND = CD->getSpecializedTemplate();
          } else if (auto *RD = dyn_cast<Record>(FoundDecl->getCxx())) {
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
          if (auto *RD = dyn_cast<clang::CXXRecordDecl>(FoundDecl->getCxx())) {
            ND = cast<clang::NamedDecl>(RD->getCanonicalDecl());
          }
        }
        if (auto *VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(
                                                               FoundDecl->getCxx())) {
          ND = VTSD->getSpecializedTemplate();
        }
        addIfNotDuplicate(R, ND);
      }
      break;
    }

  //   // This only triggers one time because it's difficult to figure out what kind
  //   // of scope we are actually processing when we run into these issues.
  //   // There will be more problems like this. That's because scopes are confusing.
  //   if (S->getKind() == SK_Class && !FoundFirstClassScope) {
  //     FoundFirstClassScope = true;
  //     // Checking that if we are in side of a record and within that record has base classes.
  //     Declaration *DeclEntity = S->Entity;
  //     if (DeclEntity) {
  //       if (DeclEntity->declaresTagDef()) {
  //         if (DeclEntity->Cxx) {
  //           clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(DeclEntity->Cxx);
  //           // We do this because if for whatever reason if this hasn't been initially
  //           // elaborated yet but if we are some how in side of it then there is a
  //           // really big problem
  //           if (!RD)
  //             llvm_unreachable("Cyclic depdency detected unable to continue.");
  //           // Basically, if this is true we found something then exit the loop.
  //           if (lookupInSideOfRecordBases(*this, getCxxSema().getASTContext(),
  //               R, RD, Name)) {
  //             break;
  //           }
  //         }
  //       }
  //     }
  //   }
  }
  return R.empty();
  // // Iterate through all scopes and stop if we find something.
  // for(; S; S = S->getParent()) {
  //   // FIXME: implement
  // }

  // return true;
}

bool Sema::lookupQualifiedName(clang::LookupResult &R) {
  Scope *LookupScope = nullptr;
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
    LookupScope = CurNNSLookupDecl.NNS->BlueScope;
    break;
  case NNSK_NamespaceAlias: {
    if (auto *Ns = dyn_cast<clang::CppxNamespaceDecl>(
                                      CurNNSLookupDecl.Alias->getNamespace())) {
      LookupScope = Ns->BlueScope;
    } else {
      getCxxSema().Diags.Report(Ns->getLocation(),
                                clang::diag::err_expected_namespace);
      return false;
    }
    break;
  }
  case NNSK_Record:{
    LookupScope = CurNNSLookupDecl.RebuiltClassScope;
  }
  break;
  }
  return lookupUnqualifiedName(R, LookupScope);
}

void Sema::createBitwiseBuiltinFunctions() {
  buildBitAnd();
  buildBitOr();
  buildBitXOr();
  buildBitShr();
  buildBitShl();
  buildBitNot();
}


static clang::QualType getBuiltinTypeOrFail(Sema& SemaRef,
                                            llvm::StringRef TyName) {
  auto It = SemaRef.BuiltinTypes.find(TyName);
  assert(It != SemaRef.BuiltinTypes.end() && "Invalid type name");
  return It->second;
}


static clang::FunctionDecl *createBinaryBW(Sema& SemaRef,
                                           clang::BinaryOperatorKind Op,
                                           llvm::StringRef FnName,
                                           llvm::StringRef Parm1,
                                           llvm::StringRef Parm2) {
  // Attempting to re-set up the translation unit's global scope.
  // The declare and define a function.
  Declaration *TUDecl = SemaRef.getTUDecl();
  auto TU = cast<clang::TranslationUnitDecl>(TUDecl->getCxx());
  // auto TUDC = cast<clang::DeclContext>(TUDecl->getCxx());
  clang::Sema::ContextRAII TUContext(SemaRef.getCxxSema(), TU);
  ResumeScopeRAII BlueScope(SemaRef, TUDecl->SavedScope, nullptr);
  llvm::SmallVector<clang::QualType, 3> Types;

  clang::ASTContext &Ctx = SemaRef.getCxxAST();
  clang::QualType ReturnType = Ctx.getAutoDeductType();
  clang::SourceLocation Loc = TU->getBeginLoc();
  // clang::CppxTypeLiteral *CTL = SemaRef.buildTypeExpr(Ctx.getAutoDeductType(), Loc);
  Types.emplace_back(getBuiltinTypeOrFail(SemaRef, Parm1));
  Types.emplace_back(getBuiltinTypeOrFail(SemaRef, Parm2));

  clang::FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpec.Type = clang::EST_BasicNoexcept;
  EPI.ExtInfo = Ctx.getDefaultCallingConvention(false, false);
  EPI.Variadic = false;
  clang::IdentifierInfo *II = &Ctx.Idents.get(FnName);
  clang::DeclarationName Name(II);
  clang::DeclarationNameInfo NameInfo(Name, Loc);
  clang::FunctionDecl *FnDecl = clang::FunctionDecl::Create(
    Ctx, TU, Loc, Loc, Name, clang::QualType(), /*TInfo*/nullptr, clang::SC_None,
    /*isInlineSpecified*/true, /*hasWrittenPrototype*/false,
    clang::ConstexprSpecKind::Constexpr, /*TrailingRequiresClause*/nullptr);
  clang::QualType FnTy = Ctx.getFunctionType(ReturnType, Types, EPI);

  clang::IdentifierInfo *xParmName = &Ctx.Idents.get("x");
  clang::IdentifierInfo *yParmName = &Ctx.Idents.get("y");
  // Creating parameter
  // Add the parameter to the constructor.
  llvm::SmallVector<clang::ParmVarDecl *, 3> Params;
  clang::ParmVarDecl *LHS = clang::ParmVarDecl::Create(
      Ctx, FnDecl, Loc, Loc,xParmName, Types[0], /*TInfo=*/nullptr,
      clang::SC_None, nullptr);
  Params.emplace_back(LHS);

  clang::ParmVarDecl *RHS = clang::ParmVarDecl::Create(
      Ctx, FnDecl, Loc, Loc, yParmName, Types[1], /*TInfo=*/nullptr,
      clang::SC_None, nullptr);
  Params.emplace_back(RHS);

  FnDecl->setType(FnTy);
  FnDecl->setParams(Params);
  auto FnTyInfo = gold::BuildFunctionTypeLoc(Ctx, FnTy,
                                             Loc, Loc, Loc,
                                             clang::SourceRange(Loc, Loc), Loc,
                                             Params);
  FnDecl->setTypeSourceInfo(FnTyInfo);
  {
    // SemaRef.getCxxSema().ActOnStartOfFunctionDef(nullptr, FnDecl);
    clang::Sema::SynthesizedFunctionScope Scope(SemaRef.getCxxSema(), FnDecl);
    clang::Sema::ContextRAII FnCtx(SemaRef.getCxxSema(), FnDecl);
    clang::Sema::CompoundScopeRAII CompoundScope(SemaRef.getCxxSema());
    clang::NestedNameSpecifierLoc NNSLoc;
    clang::SourceLocation BadLoc;
    clang::Expr *LHSRef = clang::DeclRefExpr::Create(Ctx, NNSLoc,
      /*TemplateKWLoc*/BadLoc, LHS, /*RefersToEnclosingVariableOrCapture*/false,
      Loc, LHS->getType(), clang::ExprValueKind::VK_LValue, LHS);

    clang::Expr *RHSRef = clang::DeclRefExpr::Create(Ctx, NNSLoc,
      /*TemplateKWLoc*/BadLoc, RHS, /*RefersToEnclosingVariableOrCapture*/false,
      Loc, RHS->getType(), clang::ExprValueKind::VK_LValue, RHS);
    clang::ExprResult BinOp = SemaRef.getCxxSema().BuildBinOp(
      /*Scope=*/nullptr, Loc, Op, LHSRef, RHSRef);

    auto RetStmt = SemaRef.getCxxSema().BuildReturnStmt(Loc, BinOp.get());
    llvm::SmallVector<clang::Stmt *, 1> RetArray;
    RetArray.emplace_back(RetStmt.get());
    auto NewBody = SemaRef.getCxxSema().ActOnCompoundStmt(Loc, Loc,
                                                          RetArray,
                                                          /*isStmtExpr=*/false);
    // clang::Stmt *NewBody = clang::CompoundStmt::Create(Ctx, RetArray, Loc, Loc);
    // SemaRef.getCxxSema().ActOnFinishFunctionBody(FnDecl, NewBody,
    //                                             /*IsInstantiation=*/true);
    FnDecl->setBody(NewBody.get());

  }
  SemaRef.getCxxSema().PushOnScopeChains(FnDecl, SemaRef.getCurClangScope(),
                                        /*AddToContext*/false);
  TU->addDecl(FnDecl);
  Declaration *BD = new Declaration(TUDecl, nullptr, nullptr, nullptr);
  BD->CurrentPhase = Phase::Initialization;
  BD->setCxx(SemaRef, FnDecl);
  BD->Id = II;
  TUDecl->SavedScope->addDeclLookup(BD);
  return FnDecl;
}

void Sema::buildBitAnd() {
  if (DidLoadBWAnd)
    return;
  // Creating the basic overloads.
  createBinaryBW(*this, clang::BO_And, "bit_and", "int", "int");
  createBinaryBW(*this, clang::BO_And, "bit_and", "int8", "int8");
  createBinaryBW(*this, clang::BO_And, "bit_and", "int16", "int16");
  createBinaryBW(*this, clang::BO_And, "bit_and", "int64", "int64");
  createBinaryBW(*this, clang::BO_And, "bit_and", "int128", "int128");
  createBinaryBW(*this, clang::BO_And, "bit_and", "uint", "uint");
  createBinaryBW(*this, clang::BO_And, "bit_and", "uint8", "uint8");
  createBinaryBW(*this, clang::BO_And, "bit_and", "uint16", "uint16");
  createBinaryBW(*this, clang::BO_And, "bit_and", "uint64", "uint64");
  createBinaryBW(*this, clang::BO_And, "bit_and", "uint128", "uint128");
  DidLoadBWAnd = true;
}

void Sema::buildBitOr() {
  if (DidLoadBWOr)
    return;
  // Creating the basic overloads.
  createBinaryBW(*this, clang::BO_Or, "bit_or", "int", "int");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "int8", "int8");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "int16", "int16");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "int64", "int64");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "int128", "int128");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "uint", "uint");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "uint8", "uint8");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "uint16", "uint16");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "uint64", "uint64");
  createBinaryBW(*this, clang::BO_Or, "bit_or", "uint128", "uint128");
  DidLoadBWOr = true;
}

void Sema::buildBitXOr() {
  if (DidLoadBWXOr)
    return;
  // Creating the basic overloads.
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "int", "int");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "int8", "int8");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "int16", "int16");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "int64", "int64");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "int128", "int128");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "uint", "uint");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "uint8", "uint8");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "uint16", "uint16");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "uint64", "uint64");
  createBinaryBW(*this, clang::BO_Xor, "bit_xor", "uint128", "uint128");
  DidLoadBWXOr = true;
}

void Sema::buildBitShr() {
  if (DidLoadBWShr)
    return;
  // Creating the basic overloads.
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "int", "int");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "int8", "int8");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "int16", "int16");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "int64", "int64");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "int128", "int128");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "uint", "uint");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "uint8", "uint8");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "uint16", "uint16");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "uint64", "uint64");
  createBinaryBW(*this, clang::BO_Shr, "bit_shr", "uint128", "uint128");
  DidLoadBWShr = true;
}

void Sema::buildBitShl() {
  if (DidLoadBWShl)
    return;
  // Creating the basic overloads.
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "int", "int");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "int8", "int8");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "int16", "int16");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "int64", "int64");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "int128", "int128");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "uint", "uint");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "uint8", "uint8");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "uint16", "uint16");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "uint64", "uint64");
  createBinaryBW(*this, clang::BO_Shl, "bit_shl", "uint128", "uint128");
  DidLoadBWShl = true;
}


static clang::FunctionDecl *createUnaryBW(Sema& SemaRef,
                                           clang::UnaryOperatorKind Op,
                                           llvm::StringRef FnName,
                                           llvm::StringRef Parm1) {
  // Attempting to re-set up the translation unit's global scope.
  // The declare and define a function.
  Declaration *TUDecl = SemaRef.getTUDecl();
  auto TU = cast<clang::TranslationUnitDecl>(TUDecl->getCxx());
  // auto TUDC = cast<clang::DeclContext>(TUDecl->getCxx());
  clang::Sema::ContextRAII TUContext(SemaRef.getCxxSema(), TU);
  ResumeScopeRAII BlueScope(SemaRef, TUDecl->SavedScope, nullptr);
  llvm::SmallVector<clang::QualType, 3> Types;

  clang::ASTContext &Ctx = SemaRef.getCxxAST();
  clang::QualType ReturnType = Ctx.getAutoDeductType();
  clang::SourceLocation Loc = TU->getBeginLoc();
  Types.emplace_back(getBuiltinTypeOrFail(SemaRef, Parm1));

  clang::FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpec.Type = clang::EST_BasicNoexcept;
  EPI.ExtInfo = Ctx.getDefaultCallingConvention(false, false);
  EPI.Variadic = false;
  clang::IdentifierInfo *II = &Ctx.Idents.get(FnName);
  clang::DeclarationName Name(II);
  clang::DeclarationNameInfo NameInfo(Name, Loc);
  clang::FunctionDecl *FnDecl = clang::FunctionDecl::Create(
    Ctx, TU, Loc, Loc, Name, clang::QualType(), /*TInfo*/nullptr, clang::SC_None,
    /*isInlineSpecified*/true, /*hasWrittenPrototype*/false,
    clang::ConstexprSpecKind::Constexpr, /*TrailingRequiresClause*/nullptr);
  clang::QualType FnTy = Ctx.getFunctionType(ReturnType, Types, EPI);

  clang::IdentifierInfo *xParmName = &Ctx.Idents.get("x");
  // Creating parameter
  // Add the parameter to the constructor.
  llvm::SmallVector<clang::ParmVarDecl *, 3> Params;
  clang::ParmVarDecl *LHS = clang::ParmVarDecl::Create(
      Ctx, FnDecl, Loc, Loc,xParmName, Types[0], /*TInfo=*/nullptr,
      clang::SC_None, nullptr);
  Params.emplace_back(LHS);

  FnDecl->setType(FnTy);
  FnDecl->setParams(Params);
  auto FnTyInfo = gold::BuildFunctionTypeLoc(Ctx, FnTy,
                                             Loc, Loc, Loc,
                                             clang::SourceRange(Loc, Loc), Loc,
                                             Params);
  FnDecl->setTypeSourceInfo(FnTyInfo);
  {
    // SemaRef.getCxxSema().ActOnStartOfFunctionDef(nullptr, FnDecl);
    clang::Sema::SynthesizedFunctionScope Scope(SemaRef.getCxxSema(), FnDecl);
    clang::Sema::ContextRAII FnCtx(SemaRef.getCxxSema(), FnDecl);
    clang::Sema::CompoundScopeRAII CompoundScope(SemaRef.getCxxSema());
    clang::NestedNameSpecifierLoc NNSLoc;
    clang::SourceLocation BadLoc;
    clang::Expr *LHSRef = clang::DeclRefExpr::Create(Ctx, NNSLoc,
      /*TemplateKWLoc*/BadLoc, LHS, /*RefersToEnclosingVariableOrCapture*/false,
      Loc, LHS->getType(), clang::ExprValueKind::VK_LValue, LHS);

    clang::ExprResult BinOp = SemaRef.getCxxSema().BuildUnaryOp(
      /*Scope=*/nullptr, Loc, Op, LHSRef);

    auto RetStmt = SemaRef.getCxxSema().BuildReturnStmt(Loc, BinOp.get());
    llvm::SmallVector<clang::Stmt *, 1> RetArray;
    RetArray.emplace_back(RetStmt.get());
    auto NewBody = SemaRef.getCxxSema().ActOnCompoundStmt(Loc, Loc,
                                                          RetArray,
                                                          /*isStmtExpr=*/false);
    FnDecl->setBody(NewBody.get());

  }
  SemaRef.getCxxSema().PushOnScopeChains(FnDecl, SemaRef.getCurClangScope(),
                                        /*AddToContext*/false);
  TU->addDecl(FnDecl);
  Declaration *BD = new Declaration(TUDecl, nullptr, nullptr, nullptr);
  BD->CurrentPhase = Phase::Initialization;
  BD->setCxx(SemaRef, FnDecl);
  BD->Id = II;
  TUDecl->SavedScope->addDeclLookup(BD);
  return FnDecl;
}

void Sema::buildBitNot() {
  if (DidLoadBWNot)
    return;
  // Creating the basic overloads.
  createUnaryBW(*this, clang::UO_Not, "bit_not", "int");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "int8");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "int16");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "int64");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "int128");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "uint");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "uint8");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "uint16");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "uint64");
  createUnaryBW(*this, clang::UO_Not, "bit_not", "uint128");
  DidLoadBWNot = true;
}


clang::CppxTypeLiteral *Sema::buildTypeExpr(clang::QualType Ty,
                                            clang::SourceLocation Loc) {
  return buildAnyTypeExpr(CxxAST.CppxKindTy, Ty, Loc);
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
  return buildAnyTypeExpr(KindTy, gold::BuildAnyTypeLoc(Context.CxxAST, Ty, Loc));
}

clang::TypeSourceInfo *
Sema::getTypeSourceInfoFromExpr(const clang::Expr *TyExpr,
                                clang::SourceLocation Loc) {
  if (!TyExpr) {
    return nullptr;
  }

  if (!TyExpr->getType()->isTypeOfTypes()) {
    getCxxSema().Diags.Report(Loc, clang::diag::err_not_a_type);
    return nullptr;
  }

  if (const clang::CppxTypeLiteral *Ty
                                   = dyn_cast<clang::CppxTypeLiteral>(TyExpr)) {
    return Ty->getValue();
  }

  llvm_unreachable("Invaild type expression evaluates to type of types.");
}

clang::CppxTypeLiteral *Sema::buildTypeExprTypeFromExpr(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                    bool IsConstruct) {
  return buildAnyTypeExpr(Context.CxxAST.CppxKindTy,
                          gold::BuildAnyTypeLoc(Context.CxxAST,
                                          Context.CxxAST.getCppxTypeExprTy(
                                            E, IsConstruct),
                                          Loc));
}
clang::CppxTypeLiteral *Sema::buildTypeExprTypeFromExprLiteral(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                         bool IsConstructExpr){
  clang::TypeSourceInfo *TInfo = gold::BuildAnyTypeLoc(Context.CxxAST,
                  Context.CxxAST.getCppxTypeExprTy(E, IsConstructExpr), Loc);
  return buildTypeExpr(TInfo);
}

clang::QualType Sema::buildQualTypeExprTypeFromExpr(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                    bool IsConstruct) {
  clang::TypeSourceInfo *TInfo = gold::BuildAnyTypeLoc(Context.CxxAST,
                  Context.CxxAST.getCppxTypeExprTy(E, IsConstruct), Loc);
  return TInfo->getType();
}

clang::CppxTypeLiteral *
Sema::buildFunctionTypeExpr(clang::QualType FnTy, clang::SourceLocation BeginLoc,
                            clang::SourceLocation LParenLoc,
                            clang::SourceLocation RParenLoc,
                            clang::SourceRange ExceptionSpecRange,
                            clang::SourceLocation EndLoc,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  return buildTypeExpr(gold::BuildFunctionTypeLoc(Context.CxxAST, FnTy,
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

clang::QualType Sema::getQualTypeFromTypeExpr(const clang::Expr *TyExpr) {
  if (!TyExpr) {
    return clang::QualType();
  }
  if (!TyExpr->getType()->isTypeOfTypes()) {
    getCxxSema().Diags.Report(TyExpr->getExprLoc(), clang::diag::err_not_a_type);
    return clang::QualType();
  }
  if (const clang::CppxTypeLiteral *Ty
                                   = dyn_cast<clang::CppxTypeLiteral>(TyExpr)) {

    return Ty->getValue()->getType();
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
  setLookupScope(D);
  return buildAnyDeclRef(Context.CxxAST.CppxNamespaceTy, D, Loc);
}

clang::CppxDeclRefExpr *Sema::buildNSDeclRef(clang::NamespaceAliasDecl *D,
                                             clang::SourceLocation Loc) {
  setLookupScope(D);
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
    getCxxSema().Diags.Report(Loc, clang::diag::err_expected_namespace);
    return nullptr;
  }
  getCxxSema().Diags.Report(Loc, clang::diag::err_expected_namespace);
  return nullptr;
}

static bool checkSimplNameMatchRedecl(Sema &SemaRef, Declaration *D) {
  auto otherPossibleDecls = SemaRef.getCurrentScope()->findDecl(D->Id);
  otherPossibleDecls.erase(D);
  if (!otherPossibleDecls.empty()) {
    clang::SourceLocation Loc = D->Def->getLocation();
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_redefinition)
                                      << D->Id->getName();
    for (Declaration *OtherDecl : otherPossibleDecls) {
      if (OtherDecl->getCxx()) {
        SemaRef.getCxxSema().notePreviousDefinition(
                              cast<clang::NamedDecl>(OtherDecl->getCxx()), Loc);
      } else {
        SemaRef.getCxxSema().Diags.Report(OtherDecl->Def->getLocation(),
                                          clang::diag::note_use_ifdef_guards);
      }
    }
    return true;
  }
  return false;
}

bool Sema::checkForRedeclaration(Declaration *D) {
  assert(D->getCxx() && "Invalid declaration");
  switch(D->getCxx()->getKind()) {
  case clang::Decl::Typedef:
  case clang::Decl::TypeAlias:
  case clang::Decl::Var:
  case clang::Decl::Field:
    return checkSimplNameMatchRedecl(*this, D);
  case clang::Decl::Function:
    // This requeires more infor.
    llvm_unreachable("FunctionDecl redeclaration not implemented.");
  // case clang::Decl::TranslationUnit:
  // case clang::Decl::ExternCContext:
  case clang::Decl::CppxNamespace:
  case clang::Decl::Namespace:
  case clang::Decl::NamespaceAlias:
  case clang::Decl::Enum:
  case clang::Decl::Record:
  case clang::Decl::CXXRecord:
  case clang::Decl::ClassTemplateSpecialization:
  case clang::Decl::ClassTemplatePartialSpecialization:
  case clang::Decl::VarTemplateSpecialization:
  case clang::Decl::VarTemplatePartialSpecialization:
  case clang::Decl::CXXDeductionGuide:
  case clang::Decl::CXXMethod:
  case clang::Decl::CXXConstructor:
  case clang::Decl::CXXDestructor:
  case clang::Decl::CXXConversion:
  case clang::Decl::UsingShadow:
  case clang::Decl::ConstructorUsingShadow:
  case clang::Decl::FunctionTemplate:
  case clang::Decl::ClassTemplate:
  case clang::Decl::VarTemplate:
  case clang::Decl::TypeAliasTemplate:
  // case clang::Decl::ObjCProtocol:
  // case clang::Decl::ObjCInterface:
  // case clang::Decl::Empty:
  case clang::Decl::UsingDirective:
  // case clang::Decl::Label:
  case clang::Decl::UnresolvedUsingTypename:
  case clang::Decl::TemplateTypeParm:
  case clang::Decl::EnumConstant:
  case clang::Decl::UnresolvedUsingValue:
  case clang::Decl::IndirectField:
  case clang::Decl::MSProperty:
  case clang::Decl::MSGuid:
  case clang::Decl::TemplateParamObject:
  case clang::Decl::ObjCIvar:
  case clang::Decl::ObjCAtDefsField:
  case clang::Decl::NonTypeTemplateParm:
  case clang::Decl::TemplateTemplateParm:
  case clang::Decl::Using:
  case clang::Decl::UsingPack:
  case clang::Decl::ObjCMethod:
  case clang::Decl::ObjCCategory:
  case clang::Decl::ObjCCategoryImpl:
  case clang::Decl::ObjCImplementation:
  case clang::Decl::ObjCProperty:
  case clang::Decl::ObjCCompatibleAlias:
  case clang::Decl::LinkageSpec:
  case clang::Decl::Export:
  case clang::Decl::ObjCPropertyImpl:
  case clang::Decl::PragmaComment:
  case clang::Decl::PragmaDetectMismatch:
  case clang::Decl::FileScopeAsm:
  case clang::Decl::AccessSpec:
  case clang::Decl::Friend:
  case clang::Decl::FriendTemplate:
  case clang::Decl::StaticAssert:
  case clang::Decl::Block:
  case clang::Decl::Captured:
  case clang::Decl::ClassScopeFunctionSpecialization:
  case clang::Decl::Import:
  case clang::Decl::OMPThreadPrivate:
  case clang::Decl::OMPAllocate:
  case clang::Decl::OMPRequires:
  case clang::Decl::OMPCapturedExpr:
  case clang::Decl::OMPDeclareReduction:
  case clang::Decl::OMPDeclareMapper:
  case clang::Decl::BuiltinTemplate:
  case clang::Decl::Decomposition:
  case clang::Decl::Binding:
  case clang::Decl::Concept:
  case clang::Decl::LifetimeExtendedTemporary:
  case clang::Decl::RequiresExprBody:
  case clang::Decl::CXXFragment:
  case clang::Decl::CXXMetaprogram:
  case clang::Decl::CXXInjection:
  case clang::Decl::CXXStmtFragment:
  case clang::Decl::CppxPartial:
  case clang::Decl::ImplicitParam:
  case clang::Decl::ParmVar:
  case clang::Decl::ObjCTypeParam:
  default:
    llvm::errs() << "Invalid type of declaration, unable to continue.\n";
    D->getCxx()->dump();
    llvm_unreachable("Invalid declararation to process.");
  }
}

unsigned Sema::computeTemplateDepth() const {
  unsigned Count = 0;
  Scope *Cur = ScopeStack.back();
  while(Cur != TUDecl->SavedScope) {
    if (Cur->isTemplateScope()) {
      ++Count;
    }
    Cur = Cur->getParent();
  }
  return Count;
}

clang::Scope *Sema::getCurClangScope() {
  return CxxSema.CurScope;
}

clang::Scope *Sema::enterClangScope(unsigned int ScopeFlags) {
  CxxSema.CurScope = new clang::Scope(getCurClangScope(), ScopeFlags,
                                      getCxxSema().Diags);
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

void Sema::addDeclToDecl(clang::Decl *Cxx, Declaration *Blue) {
  assert(Cxx && "Invalid clang declaration");
  assert(Blue && "Invalid blue declaration");

  DeclToDecl.insert({Cxx, Blue});
}

Declaration *Sema::getDeclaration(clang::Decl *Cxx) {
  assert(Cxx && "Invalid declaration.");
  auto Iter = DeclToDecl.find(Cxx);
  if (Iter == DeclToDecl.end())
    return nullptr;

  return Iter->second;
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
      new LateElaboratedClass(*this, Victim));
  Victim->TemplateScope
                   = CxxSema.getCurScope()->getParent()->isTemplateParamScope();
}

void Sema::diagnoseElabCycleError(Declaration *CycleTerminalDecl) {
  assert(CycleTerminalDecl && "Invalid terminal cycle");
  assert(!DeclsBeingElaborated.empty() && "We cannot have an empty stack and a "
         "declaration cycle.");
  // assert(CycleTerminalDecl->IdDcl);
  getCxxSema().Diags.Report(CycleTerminalDecl->Def->getLocation(),
                            clang::diag::err_decl_use_cycle);
  for (auto *CycleNote : DeclsBeingElaborated){
    if (CycleNote == CycleTerminalDecl)
      continue;
    getCxxSema().Diags.Report(CycleNote->Def->getLocation(),
                              clang::diag::note_cycle_entry);
  }
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
    CxxSema.Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  const clang::FunctionProtoType *NewFPT
                                 = NewFuncTy->getAs<clang::FunctionProtoType>();
  if (!NewFPT) {
    CxxSema.Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  clang::QualType ExtInfoAdjustedTy(
               Context.CxxAST.adjustFunctionType(NewFPT, EI), /*Qualifiers=*/0);
  if (ExtInfoAdjustedTy.isNull()) {
    CxxSema.Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  clang::QualType ExceptionAdjustedTy
      = Context.CxxAST.getFunctionTypeWithExceptionSpec(ExtInfoAdjustedTy, ESI);
  if (ExceptionAdjustedTy.isNull()) {
    CxxSema.Diags.Report(Loc, clang::diag::err_invalid_function_type);
    return true;
  }
  auto ParmVarDecls = FD->parameters();
  llvm::SmallVector<clang::ParmVarDecl *, 32> Parameters(ParmVarDecls.begin(),
                                                         ParmVarDecls.end());
  clang::TypeSourceInfo *TInfo = gold::BuildFunctionTypeLoc(Context.CxxAST,
                                                      ExceptionAdjustedTy,
                                                      FD->getBeginLoc(),
                                                      clang::SourceLocation(),
                                                      clang::SourceLocation(),
                                                      clang::SourceRange(),
                                                      FD->getEndLoc(),
                                                      Parameters);
  if (!TInfo) {
    CxxSema.Diags.Report(Loc, clang::diag::err_invalid_function_type);
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
  blue::Scope *PrevScope = nullptr;
  if (CppxNamespaceDecl *Prev = dyn_cast_or_null<CppxNamespaceDecl>(PrevNS))
    PrevScope = Prev->BlueScope;


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
      // UD = UsingDirectiveDecl::Create(Context.CxxAST, Parent,
      //                                 /* 'using' */ LBrace,
      //                                 /* 'namespace' */ SourceLocation(),
      //                                 /* qualifier */ NestedNameSpecifierLoc(),
      //                                 /* identifier */ SourceLocation(),
      //                                 Namespc, /* Ancestor */ Parent);
      // UD->setImplicit();
      // Parent->addDecl(UD);
      // getCurrentScope()->UsingDirectives.insert(UD);
      // llvm_unreachable("Using directive not implemented yet.");
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

Scope *Sema::duplicateScopeForNestedNameContext(Declaration *D) {
  assert(D && "invalid declaration");
  assert(D->getCxx() && "Declaration hasn't been elaborated yet");
  assert(D->SavedScope && "Declaration has no scope to duplicate");
  // Entering a new scope that we can use for lookup.
  enterScope(D->SavedScope->getKind(), D->SavedScope->getTerm());
  Scope *NextScope = getCurrentScope();
  for (const auto &DeclPair : D->SavedScope->getDeclMap()) {
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
    return CurNNSLookupDecl.NNS->getBlueScopeRep();
  case NNSK_NamespaceAlias: {
    clang::Decl *AliasedNS = CurNNSLookupDecl.Alias->getAliasedNamespace();
    if (auto *NNS = dyn_cast<clang::CppxNamespaceDecl>(AliasedNS))
      return NNS->BlueScope;
    // FIXME: The namespace alias doesn't contain a CppxNamespaceDecl
    llvm_unreachable("Invalid namespace alias");
  }

  case NNSK_Record:{
    return CurNNSLookupDecl.RebuiltClassScope;
  }
  } // switch (CurNNSKind)

  llvm_unreachable("Invalid or unknown nested name specifier type");
}

// Get the identifier of the current lookup scope, or "true" for a global NNS
std::pair<bool, clang::IdentifierInfo *> Sema::getLookupScopeName() const {
  return getLookupScopeName(CurNNSLookupDecl, CurNNSKind);
}

std::pair<bool, clang::IdentifierInfo *>
Sema::getLookupScopeName(Sema::NNSLookupDecl const &D, Sema::NNSKind K) const {
  switch (K) {
  case NNSK_Empty:
    return {false, nullptr};
  case NNSK_Global:
    return {true, nullptr};
  case NNSK_Namespace:
    return {false, D.NNS->getIdentifier()};
  case NNSK_NamespaceAlias: {
    clang::Decl *AliasedNS = D.Alias->getAliasedNamespace();
    if (auto *NNS = dyn_cast<clang::CppxNamespaceDecl>(AliasedNS))
      return {false, NNS->getIdentifier()};
    // FIXME: The namespace alias doesn't contain a CppxNamespaceDecl
    llvm_unreachable("Invalid namespace alias");
  }

  // FIXME: supply enough information in the NNSLookupDecl to create this.
  case NNSK_Record:
    return {false, nullptr};
  default:
    llvm_unreachable("Invalid nested name lookup!");
  } // switch (K);
}

} // end namespace Blue
