//===--- SemaLambda.cpp - Semantic Analysis for C++11 Lambdas -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++ lambda expressions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTLambda.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/SemaLambda.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/STLExtras.h"

#include "clang/Gold/GoldDeclarator.h"
#include "clang/Gold/GoldSema.h"

#include <algorithm>

using namespace clang;
using namespace sema;

void Sema::ActOnStartOfGoldLambdaDefinition(
  gold::Sema &GoldSema, LambdaIntroducer &Intro,
  llvm::SmallVectorImpl<clang::ParmVarDecl *> &EParams,
  Scope *CurScope, bool IsMutable) {
  LambdaScopeInfo *const LSI = getCurLambda();
  assert(LSI && "LambdaScopeInfo should be on stack!");

  // Determine if we're within a context where we know that the lambda will
  // be dependent, because there are template parameters in scope.
  bool KnownDependent;
  if (LSI->NumExplicitTemplateParams > 0) {
    auto *TemplateParamScope = CurScope->getTemplateParamParent();
    assert(TemplateParamScope &&
           "Lambda with explicit template param list should establish a "
           "template param scope");
    assert(TemplateParamScope->getParent());
    KnownDependent = TemplateParamScope->getParent()
                                       ->getTemplateParamParent() != nullptr;
  } else {
    KnownDependent = CurScope->getTemplateParamParent() != nullptr;
  }

  // Determine the signature of the call operator.
  TypeSourceInfo *MethodTyInfo;
  bool ExplicitParams = true;
  bool ExplicitResultType = true;
  bool ContainsUnexpandedParameterPack = false;
  SourceLocation EndLoc;
  SmallVector<ParmVarDecl *, 8> Params;
  QualType DefaultTypeForNoTrailingReturn = Context.getAutoDeductType();

  FunctionProtoType::ExtProtoInfo EPI(Context.getDefaultCallingConvention(
      /*IsVariadic=*/false, /*IsCXXMethod=*/true));
  EPI.HasTrailingReturn = true;
  EPI.TypeQuals.addConst();
  LangAS AS = getDefaultCXXMethodAddrSpace();
  if (AS != LangAS::Default)
    EPI.TypeQuals.addAddressSpace(AS);

  // FIXME: determine when this is true.
  bool NoDeclarator = false;
  if (NoDeclarator) {
    // C++11 [expr.prim.lambda]p4:
    //   If a lambda-expression does not include a lambda-declarator, it is as
    //   if the lambda-declarator were ().

    // C++1y [expr.prim.lambda]:
    //   The lambda return type is 'auto', which is replaced by the
    //   trailing-return type if provided and/or deduced from 'return'
    //   statements
    // We don't do this before C++1y, because we don't support deduced return
    // types there.
    QualType DefaultTypeForNoTrailingReturn = Context.getAutoDeductType();
    QualType MethodTy =
        Context.getFunctionType(DefaultTypeForNoTrailingReturn, None, EPI);
    MethodTyInfo = Context.getTrivialTypeSourceInfo(MethodTy);
    ExplicitParams = false;
    ExplicitResultType = false;
    EndLoc = Intro.Range.getEnd();
  } else {
    // TODO: set up a qualified type based on any macro attributes
    clang::SourceLocation Loc;
    if (!EParams.empty()) {
      Loc = EParams.front()->getBeginLoc();
      EndLoc = EParams.back()->getBeginLoc();
    }

    llvm::SmallVector<QualType, 4> ParamTypes;
    std::transform(EParams.begin(), EParams.end(),
                   std::back_inserter(ParamTypes),
                   [](clang::ParmVarDecl *D) { return D->getType(); });
    QualType MethodTy = Context.getFunctionType(
      Context.getAutoDeductType(), ParamTypes, EPI);
    MethodTyInfo =
      gold::BuildFunctionTypeLoc(Context, MethodTy, Loc, Loc,
                           EndLoc, clang::SourceRange(Loc, Loc),
                           EndLoc, EParams);
    assert(MethodTyInfo && "no type from lambda-declarator");

    // C++11 [expr.prim.lambda]p5:
    //   This function call operator is declared const (9.3.1) if and only if
    //   the lambda-expression's parameter-declaration-clause is not followed
    //   by mutable. It is neither virtual nor declared volatile. [...]
    if (!IsMutable)
      MethodTyInfo->getType().addConst();

    // TODO: include the return type
    // ExplicitResultType = FTI.hasTrailingReturnType();
    ExplicitResultType = false;

    // Check for unexpanded parameter packs in the method type.
    if (MethodTyInfo->getType()->containsUnexpandedParameterPack())
      DiagnoseUnexpandedParameterPack(Intro.Range.getBegin(), MethodTyInfo,
                                      UPPC_DeclarationType);
  }

  CXXRecordDecl *Class = createLambdaClosureType(Intro.Range, MethodTyInfo,
                                                 KnownDependent, Intro.Default);
  // FIXME: get the constexpr spec kind
  CXXMethodDecl *Method =
      startLambdaDefinition(Class, Intro.Range, MethodTyInfo, EndLoc, Params,
                            CSK_unspecified,
                            /*RequiresClause=*/nullptr);
  Method->setParams(EParams);
  for (auto *D : EParams)
    D->setDeclContext(Method);

  if (ExplicitParams)
    CheckCXXDefaultArguments(Method);

  // This represents the function body for the lambda function, check if we
  // have to apply optnone due to a pragma.
  AddRangeBasedOptnone(Method);

  // code_seg attribute on lambda apply to the method.
  if (Attr *A = getImplicitCodeSegOrSectionAttrForFunction(Method, /*IsDefinition=*/true))
    Method->addAttr(A);

  // Attributes on the lambda apply to the method.
  // FIXME: process attributes
  // ProcessDeclAttributes(CurScope, Method, ParamInfo);

  // Number the lambda for linkage purposes if necessary.
  handleLambdaNumbering(Class, Method);

  // Introduce the function call operator as the current declaration context.
  PushDeclContext(CurScope, Method);

  // Build the lambda scope.
  buildLambdaScope(LSI, Method, Intro.Range, Intro.Default, Intro.DefaultLoc,
                   ExplicitParams, ExplicitResultType, !Method->isConst());

  // C++11 [expr.prim.lambda]p9:
  //   A lambda-expression whose smallest enclosing scope is a block scope is a
  //   local lambda expression; any other lambda expression shall not have a
  //   capture-default or simple-capture in its lambda-introducer.
  //
  // For simple-captures, this is covered by the check below that any named
  // entity is a variable that can be captured.
  //
  // For DR1632, we also allow a capture-default in any context where we can
  // odr-use 'this' (in particular, in a default initializer for a non-static
  // data member).
  if (Intro.Default != LCD_None && !Class->getParent()->isFunctionOrMethod() &&
      (getCurrentThisType().isNull() ||
       CheckCXXThisCapture(SourceLocation(), /*Explicit*/true,
                           /*BuildAndDiagnose*/false)))
    Diag(Intro.DefaultLoc, diag::err_capture_default_non_local);

  // Distinct capture names, for diagnostics.
  llvm::SmallSet<IdentifierInfo*, 8> CaptureNames;

  // Handle explicit captures.
  SourceLocation PrevCaptureLoc
    = Intro.Default == LCD_None? Intro.Range.getBegin() : Intro.DefaultLoc;
  for (auto C = Intro.Captures.begin(), E = Intro.Captures.end(); C != E;
       PrevCaptureLoc = C->Loc, ++C) {
    if (C->Kind == LCK_This || C->Kind == LCK_StarThis) {
      if (C->Kind == LCK_StarThis)
        Diag(C->Loc, !getLangOpts().CPlusPlus17
                             ? diag::ext_star_this_lambda_capture_cxx17
                             : diag::warn_cxx14_compat_star_this_lambda_capture);

      // C++2a [expr.prim.lambda]p8:
      //  If a lambda-capture includes a capture-default that is =,
      //  each simple-capture of that lambda-capture shall be of the form
      //  "&identifier", "this", or "* this". [ Note: The form [&,this] is
      //  redundant but accepted for compatibility with ISO C++14. --end note ]
      if (Intro.Default == LCD_ByCopy && C->Kind != LCK_StarThis)
        Diag(C->Loc, !getLangOpts().CPlusPlus20
                         ? diag::ext_equals_this_lambda_capture_cxx20
                         : diag::warn_cxx17_compat_equals_this_lambda_capture);

      // C++11 [expr.prim.lambda]p12:
      //   If this is captured by a local lambda expression, its nearest
      //   enclosing function shall be a non-static member function.
      QualType ThisCaptureType = getCurrentThisType();
      if (ThisCaptureType.isNull()) {
        Diag(C->Loc, diag::err_this_capture) << true;
        continue;
      }

      CheckCXXThisCapture(C->Loc, /*Explicit=*/true, /*BuildAndDiagnose*/ true,
                          /*FunctionScopeIndexToStopAtPtr*/ nullptr,
                          C->Kind == LCK_StarThis);
      if (!LSI->Captures.empty())
        LSI->ExplicitCaptureRanges[LSI->Captures.size() - 1] = C->ExplicitRange;
      continue;
    }

    assert(C->Id && "missing identifier for capture");

    if (C->Init.isInvalid())
      continue;

    VarDecl *Var = nullptr;
    if (C->Init.isUsable()) {
      Diag(C->Loc, getLangOpts().CPlusPlus14
                       ? diag::warn_cxx11_compat_init_capture
                       : diag::ext_init_capture);

      // If the initializer expression is usable, but the InitCaptureType
      // is not, then an error has occurred - so ignore the capture for now.
      // for e.g., [n{0}] { }; <-- if no <initializer_list> is included.
      // FIXME: we should create the init capture variable and mark it invalid
      // in this case.
      if (C->InitCaptureType.get().isNull())
        continue;

      if (C->Init.get()->containsUnexpandedParameterPack() &&
          !C->InitCaptureType.get()->getAs<PackExpansionType>())
        DiagnoseUnexpandedParameterPack(C->Init.get(), UPPC_Initializer);

      unsigned InitStyle;
      switch (C->InitKind) {
      case LambdaCaptureInitKind::NoInit:
        llvm_unreachable("not an init-capture?");
      case LambdaCaptureInitKind::CopyInit:
        InitStyle = VarDecl::CInit;
        break;
      case LambdaCaptureInitKind::DirectInit:
        InitStyle = VarDecl::CallInit;
        break;
      case LambdaCaptureInitKind::ListInit:
        InitStyle = VarDecl::ListInit;
        break;
      }
      Var = createLambdaInitCaptureVarDecl(C->Loc, C->InitCaptureType.get(),
                                           C->EllipsisLoc, C->Id, InitStyle,
                                           C->Init.get());
      // C++1y [expr.prim.lambda]p11:
      //   An init-capture behaves as if it declares and explicitly
      //   captures a variable [...] whose declarative region is the
      //   lambda-expression's compound-statement
      if (Var) {
        PushOnScopeChains(Var, CurScope, false);

        DeclarationNameInfo Name(C->Id, C->Loc);
        LookupResult R(*this, Name, LookupOrdinaryName);
        GoldSema.lookupUnqualifiedName(R);
        if (R.isAmbiguous())
          continue;
        if (R.empty()) {
          // FIXME: Disable corrections that would add qualification?
          CXXScopeSpec ScopeSpec;
          DeclFilterCCC<VarDecl> Validator{};
          if (DiagnoseEmptyLookup(CurScope, ScopeSpec, R, Validator))
            continue;
        }

        SourceLocation Loc;
        UsingDecl *Using =
          UsingDecl::Create(Context, CurContext, Loc, NestedNameSpecifierLoc(),
                            Name, false);
        UsingShadowDecl *Shadow =
          BuildUsingShadowDecl(CurScope, Using, Var, nullptr);
        GoldSema.getCurrentScope()->Shadows.insert(Shadow);
      }
    } else {
      assert(C->InitKind == LambdaCaptureInitKind::NoInit &&
             "init capture has valid but null init?");

      // C++11 [expr.prim.lambda]p8:
      //   If a lambda-capture includes a capture-default that is &, the
      //   identifiers in the lambda-capture shall not be preceded by &.
      //   If a lambda-capture includes a capture-default that is =, [...]
      //   each identifier it contains shall be preceded by &.
      if (C->Kind == LCK_ByRef && Intro.Default == LCD_ByRef) {
        Diag(C->Loc, diag::err_reference_capture_with_reference_default)
            << FixItHint::CreateRemoval(
                SourceRange(getLocForEndOfToken(PrevCaptureLoc), C->Loc));
        continue;
      } else if (C->Kind == LCK_ByCopy && Intro.Default == LCD_ByCopy) {
        Diag(C->Loc, diag::err_copy_capture_with_copy_default)
            << FixItHint::CreateRemoval(
                SourceRange(getLocForEndOfToken(PrevCaptureLoc), C->Loc));
        continue;
      }

      // C++11 [expr.prim.lambda]p10:
      //   The identifiers in a capture-list are looked up using the usual
      //   rules for unqualified name lookup (3.4.1)
      DeclarationNameInfo Name(C->Id, C->Loc);
      LookupResult R(*this, Name, LookupOrdinaryName);
      GoldSema.lookupUnqualifiedName(R);
      if (R.isAmbiguous())
        continue;
      if (R.empty()) {
        // FIXME: Disable corrections that would add qualification?
        CXXScopeSpec ScopeSpec;
        DeclFilterCCC<VarDecl> Validator{};
        if (DiagnoseEmptyLookup(CurScope, ScopeSpec, R, Validator))
          continue;
      }

      Var = R.getAsSingle<VarDecl>();
      if (Var && DiagnoseUseOfDecl(Var, C->Loc))
        continue;
    }

    // C++11 [expr.prim.lambda]p8:
    //   An identifier or this shall not appear more than once in a
    //   lambda-capture.
    if (!CaptureNames.insert(C->Id).second) {
      if (Var && LSI->isCaptured(Var)) {
        Diag(C->Loc, diag::err_capture_more_than_once)
            << C->Id << SourceRange(LSI->getCapture(Var).getLocation())
            << FixItHint::CreateRemoval(
                   SourceRange(getLocForEndOfToken(PrevCaptureLoc), C->Loc));
      } else
        // Previous capture captured something different (one or both was
        // an init-cpature): no fixit.
        Diag(C->Loc, diag::err_capture_more_than_once) << C->Id;
      continue;
    }

    // C++11 [expr.prim.lambda]p10:
    //   [...] each such lookup shall find a variable with automatic storage
    //   duration declared in the reaching scope of the local lambda expression.
    // Note that the 'reaching scope' check happens in tryCaptureVariable().
    if (!Var) {
      Diag(C->Loc, diag::err_capture_does_not_name_variable) << C->Id;
      continue;
    }

    // Ignore invalid decls; they'll just confuse the code later.
    if (Var->isInvalidDecl())
      continue;

    if (!Var->hasLocalStorage()) {
      Diag(C->Loc, diag::err_capture_non_automatic_variable) << C->Id;
      Diag(Var->getLocation(), diag::note_previous_decl) << C->Id;
      continue;
    }

    // C++11 [expr.prim.lambda]p23:
    //   A capture followed by an ellipsis is a pack expansion (14.5.3).
    SourceLocation EllipsisLoc;
    if (C->EllipsisLoc.isValid()) {
      if (Var->isParameterPack()) {
        EllipsisLoc = C->EllipsisLoc;
      } else {
        Diag(C->EllipsisLoc, diag::err_pack_expansion_without_parameter_packs)
            << (C->Init.isUsable() ? C->Init.get()->getSourceRange()
                                   : SourceRange(C->Loc));

        // Just ignore the ellipsis.
      }
    } else if (Var->isParameterPack()) {
      ContainsUnexpandedParameterPack = true;
    }

    if (C->Init.isUsable()) {
      addInitCapture(LSI, Var);
    } else {
      TryCaptureKind Kind = C->Kind == LCK_ByRef ? TryCapture_ExplicitByRef :
                                                   TryCapture_ExplicitByVal;
      tryCaptureVariable(Var, C->Loc, Kind, EllipsisLoc);
    }
    if (!LSI->Captures.empty())
      LSI->ExplicitCaptureRanges[LSI->Captures.size() - 1] = C->ExplicitRange;
  }
  finishLambdaExplicitCaptures(LSI);

  LSI->ContainsUnexpandedParameterPack |= ContainsUnexpandedParameterPack;

  // Add lambda parameters into scope.
  addLambdaParameters(Intro.Captures, Method, CurScope);

  // Enter a new evaluation context to insulate the lambda from any
  // cleanups from the enclosing full-expression.
  PushExpressionEvaluationContext(
      LSI->CallOperator->isConsteval()
          ? ExpressionEvaluationContext::ConstantEvaluated
          : ExpressionEvaluationContext::PotentiallyEvaluated);
}
