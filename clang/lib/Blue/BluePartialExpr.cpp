//===- BluePartialExpr.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  The implementation for partial expressions.
//
//===----------------------------------------------------------------------===//
#include "clang/Blue/BluePartialExpr.h"
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

#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueElaborator.h"
#include "clang/Sema/TypeLocUtil.h"
#include "clang/Basic/DiagnosticSema.h"

#include "clang/Blue/BlueExprMarker.h"

namespace blue {
static NameExprState determineNextStateFromExpr(clang::Expr *E) {
  assert(E && "Invalid expression cannot determine next state");
  // if (!E) {
  //   return BuildingNormalNameAccessExpr;
  // }
  clang::QualType Ty = E->getType();
  if (Ty->isCppxNamespaceType()) {
    return BuildingNamespaceExpr;
  } else if (Ty->isKindType()) {
    return BuildingBaseQualifiedExpr;
  } else if(Ty->isTemplateType()) {
    return BuildingTemplateQualifiedExpr;
  } else {
    return BuildingNormalNameAccessExpr;
  }
}

  clang::Expr *PartialNameAccessExprImpl::buildKnownMemberExpr(
                 clang::SourceLocation IdLoc, clang::IdentifierInfo *MemberId) {
  // We need to check if the current record decl is a base of the current class.
  // before we make this into an implicit this expression, because it could fail.
  if (!NonNameLHS) {
    if (AllowImplicitThisExpr) {
      clang::QualType ThisPtrTy = SemaRef.getCxxSema().getCurrentThisType();
      if (ThisPtrTy.isNull()) {
        error(IdLoc) << "unable to get this expression from current context";
        return nullptr;
      }
      NonNameLHS = SemaRef.getCxxSema().BuildCXXThisExpr(IdLoc, ThisPtrTy, true);
    } else if(!NonNameLHS) {
      // We need to build a member reference expression in so we can make a
      // member pointer.
      SemaRef.getCxxSema().Diags.Report(IdLoc,
                                   clang::diag::err_member_call_without_object);
      return nullptr;
    }
  }
  clang::UnqualifiedId UnqualId;
  UnqualId.setIdentifier(MemberId, IdLoc);
  clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
  if (NonNameLHS->getType()->isPointerType())
    AccessTokenKind = clang::tok::TokenKind::arrow;
  clang::ExprResult Access =
    SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                               NonNameLHS, IdLoc,
                                               AccessTokenKind, SS,
                                               clang::SourceLocation(),
                                               UnqualId, nullptr);
  if (Access.isInvalid())
    return nullptr;

  return Access.get();
}

clang::Expr *
PartialNameAccessExprImpl::normalAccess_appendName_updateTransition(
                            clang::SourceLocation Loc, clang::LookupResult &R) {
  switch(R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
    if (NonNameLHS) {
      clang::UnqualifiedId UnqualId;
      UnqualId.setIdentifier(R.getLookupName().getAsIdentifierInfo(), Loc);
      clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
      if (NonNameLHS->getType()->isPointerType())
        AccessTokenKind = clang::tok::TokenKind::arrow;
      clang::ExprResult Access =
        SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                                  NonNameLHS, Loc,
                                                  AccessTokenKind, SS,
                                                  clang::SourceLocation(),
                                                  UnqualId, nullptr);
      if (Access.isInvalid())
        return nullptr;

      return Access.get();
    } else {
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), nullptr,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                                 R.getLookupNameInfo(),
                                                 // Not sure if needs ADL or
                                                 // not it's going to depend on
                                                 // the kind of function it is.
                                                 /*ADL=*/false,
                                                 /*Overloaded*/true,
                                                 R.begin(),
                                                 R.end());
    }
  }
  break;
  case clang::LookupResult::Found: {
    clang::Decl *RHSDecl = R.getAsSingle<clang::Decl>();
    if (auto Ns = dyn_cast<clang::CppxNamespaceDecl>(RHSDecl)) {
      State = BuildingNamespaceExpr;
      CurExpr = SemaRef.buildNSDeclRef(Ns, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto NsAD = dyn_cast<clang::NamespaceAliasDecl>(RHSDecl)) {
      State = BuildingNamespaceExpr;
      CurExpr = SemaRef.buildNSDeclRef(NsAD, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto Record = dyn_cast<clang::CXXRecordDecl>(RHSDecl)) {
      if (Record->isEnum()) {
        error(Loc) << "Invalid member specification, given type name is an enum";
        return nullptr;
      }
      if (auto CTD = Record->getDescribedClassTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(CTD, Loc);
        extenedSSFromExpr(CurExpr);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(Record, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto TD = dyn_cast<clang::TemplateDecl>(RHSDecl)) {
      State = BuildingTemplateQualifiedExpr;
      CurExpr = SemaRef.buildTemplateType(TD, Loc);
      return getIncompleteExpr();

    } else if (auto TyAlias = dyn_cast<clang::TypedefNameDecl>(RHSDecl)) {
      if (auto TemplateAlias = TyAlias->getDescribedTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(TemplateAlias, Loc);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(TyAlias, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto Parm = dyn_cast<clang::ParmVarDecl>(RHSDecl)) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << NonNameLHS << Parm;
      return nullptr;
    } else if (auto FD = dyn_cast<clang::FieldDecl>(RHSDecl)) {
      clang::RecordDecl *RD = FD->getParent();
      clang::QualType MemberTy(RD->getTypeForDecl(), 0);
      // If we have a NonNameLHS than we don't do anything here and just construct
      // the member expression
      if (NonNameLHS) {
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());
      }
      clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
      if (CurThisTy.isNull()) {
        // This can't logically happen because if we are not inside of a class
        // and a LHS expression is a type then it isn't processed as a nested
        // name specifier.
        llvm_unreachable("Referencing a member without being inside a class "
                        "and without an object.");
      }
      clang::QualType CurrentClass = CurThisTy->getPointeeType();
      assert (!CurrentClass.isNull() && "invalid pointee type.");
      // if the type we are referencing is a base of the type we are inside
      // the we can build a member expression. If it isn't then we should
      // build a DeclRefExpr.
      if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, MemberTy)) {
        // Reference the field by name and construct the correct member
        // access expression.
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());

      }
      // In this case we are referencing a class we are not in side of,
      // and won't have an implicit this expression.
      return clang::DeclRefExpr::Create(SemaRef.getCxxAST(),
                                  SS.getWithLocInContext(SemaRef.getCxxAST()),
                                        clang::SourceLocation(), FD,
                                        /*Capture=*/false, Loc,
                                        FD->getType(), clang::VK_LValue);
    } else if (auto MD = dyn_cast<clang::CXXMethodDecl>(RHSDecl)) {
      clang::CXXRecordDecl *RD = MD->getParent();
      clang::QualType MemberTy(RD->getTypeForDecl(), 0);

      // If we have a NonNameLHS than we don't do anything here and just construct
      // the member expression
      if (NonNameLHS) {
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());
      }
      clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
      if (CurThisTy.isNull()) {
        // This can't logically happen because if we are not inside of a class
        // and a LHS expression is a type then it isn't processed as a nested
        // name specifier.
        llvm_unreachable("Referencing a member without being inside a class "
                        "and without an object.");
      }
      clang::QualType CurrentClass = CurThisTy->getPointeeType();
      assert (!CurrentClass.isNull() && "invalid pointee type.");
      // if the type we are referencing is a base of the type we are inside
      // the we can build a member expression. If it isn't then we should
      // build a DeclRefExpr.
      if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, MemberTy)) {
        // Reference the field by name and construct the correct member
        // access expression.
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());

      }
      // In this case we are referencing a class we are not in side of,
      // and won't have an implicit this expression.
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), RD,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                                 R.getLookupNameInfo(),
                                                 // Not sure if needs ADL or
                                                 // not it's going to depend on
                                                 // the kind of function it is.
                                                 /*ADL=*/false,
                                                 /*Overloaded*/false,
                                                 R.begin(),
                                                 R.end());

    } else {
      // This is an error and we are not sure how to handle this!
      error(Loc) << "invalid name expression";
      return nullptr;
    }
  }
  break;
  case clang::LookupResult::NotFoundInCurrentInstantiation:
  case clang::LookupResult::NotFound:
    return buildNoMemberError(Loc, R.getLookupName().getAsIdentifierInfo());

  case clang::LookupResult::FoundUnresolvedValue:
    // FIXME: I need to figure out when this can occur, then create
    // that situation within a test and build appropriate error message.
    // I suspect that this may have something to do with variable template
    // declarations.
    llvm_unreachable("Not sure how handle unresolved values.");
  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }
  llvm_unreachable("SHould never reach here!");
}



clang::Expr *
PartialNameAccessExprImpl::baseQualified_appendName_updateTransition(
                            clang::SourceLocation Loc, clang::LookupResult &R) {
  switch(R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
    if (NonNameLHS) {
      clang::UnqualifiedId UnqualId;
      UnqualId.setIdentifier(R.getLookupName().getAsIdentifierInfo(), Loc);
      clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
      if (NonNameLHS->getType()->isPointerType())
        AccessTokenKind = clang::tok::TokenKind::arrow;
      clang::ExprResult Access =
        SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                                  NonNameLHS, Loc,
                                                  AccessTokenKind, SS,
                                                  clang::SourceLocation(),
                                                  UnqualId, nullptr);
      if (Access.isInvalid())
        return nullptr;

      return Access.get();
    } else {
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), nullptr,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                                 R.getLookupNameInfo(),
                                                 // Not sure if needs ADL or
                                                 // not it's going to depend on
                                                 // the kind of function it is.
                                                 /*ADL=*/false,
                                                 /*Overloaded*/true,
                                                 R.begin(),
                                                 R.end());
    }
  }
  break;
  case clang::LookupResult::Found: {
    clang::Decl *RHSDecl = R.getAsSingle<clang::Decl>();
    if (isa<clang::CppxNamespaceDecl>(RHSDecl)) {
      // error(Loc) << "cannot access a namespace through a type";
      return nullptr;

    } else if (isa<clang::NamespaceAliasDecl>(RHSDecl)) {
      error(Loc) << "cannot access a namespace through a type";
      return nullptr;

    } else if (auto Record = dyn_cast<clang::CXXRecordDecl>(RHSDecl)) {
      if (Record->isEnum()) {
        error(Loc) << "Invalid member specification, given type name is an enum";
        return nullptr;
      }
      if (auto CTD = Record->getDescribedClassTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(CTD, Loc);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(Record, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto TD = dyn_cast<clang::TemplateDecl>(RHSDecl)) {
      State = BuildingTemplateQualifiedExpr;
      CurExpr = SemaRef.buildTemplateType(TD, Loc);
      return getIncompleteExpr();

    } else if (auto TyAlias = dyn_cast<clang::TypedefNameDecl>(RHSDecl)) {
      if (auto TemplateAlias = TyAlias->getDescribedTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(TemplateAlias, Loc);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(TyAlias, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto Parm = dyn_cast<clang::ParmVarDecl>(RHSDecl)) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << NonNameLHS << Parm ;
      return nullptr;

    } else if (auto FD = dyn_cast<clang::FieldDecl>(RHSDecl)) {
      clang::RecordDecl *RD = FD->getParent();
      clang::QualType MemberTy(RD->getTypeForDecl(), 0);
      // If we have a NonNameLHS than we don't do anything here and just construct
      // the member expression
      if (NonNameLHS) {
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());
      }
      clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
      if (CurThisTy.isNull()) {
        // This can't logically happen because if we are not inside of a class
        // and a LHS expression is a type then it isn't processed as a nested
        // name specifier.
        llvm_unreachable("Referencing a member without being inside a class "
                        "and without an object.");
      }
      clang::QualType CurrentClass = CurThisTy->getPointeeType();
      assert (!CurrentClass.isNull() && "invalid pointee type.");
      // if the type we are referencing is a base of the type we are inside
      // the we can build a member expression. If it isn't then we should
      // build a DeclRefExpr.
      if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, MemberTy)) {
        // Reference the field by name and construct the correct member
        // access expression.
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());

      }
      // In this case we are referencing a class we are not in side of,
      // and won't have an implicit this expression.
      return clang::DeclRefExpr::Create(SemaRef.getCxxAST(),
                                  SS.getWithLocInContext(SemaRef.getCxxAST()),
                                        clang::SourceLocation(), FD,
                                        /*Capture=*/false, Loc,
                                        FD->getType(), clang::VK_LValue);
    } else if (auto MD = dyn_cast<clang::CXXMethodDecl>(RHSDecl)) {
      clang::CXXRecordDecl *RD = MD->getParent();
      clang::QualType MemberTy(RD->getTypeForDecl(), 0);

      // If we have a NonNameLHS than we don't do anything here and just construct
      // the member expression
      if (NonNameLHS) {
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());
      }
      clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
      if (CurThisTy.isNull()) {
        // This can't logically happen because if we are not inside of a class
        // and a LHS expression is a type then it isn't processed as a nested
        // name specifier.
        llvm_unreachable("Referencing a member without being inside a class "
                        "and without an object.");
      }
      clang::QualType CurrentClass = CurThisTy->getPointeeType();
      assert (!CurrentClass.isNull() && "invalid pointee type.");
      // if the type we are referencing is a base of the type we are inside
      // the we can build a member expression. If it isn't then we should
      // build a DeclRefExpr.
      if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, MemberTy)) {
        // Reference the field by name and construct the correct member
        // access expression.
        return buildKnownMemberExpr(Loc, R.getLookupName().getAsIdentifierInfo());

      }
      // In this case we are referencing a class we are not in side of,
      // and won't have an implicit this expression.
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), RD,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                                 R.getLookupNameInfo(),
                                                 // Not sure if needs ADL or
                                                 // not it's going to depend on
                                                 // the kind of function it is.
                                                 /*ADL=*/false,
                                                 /*Overloaded*/false,
                                                 R.begin(),
                                                 R.end());

    } else {
      error(Loc) << "invalid name expression";
      return nullptr;
    }
  }
  break;
  case clang::LookupResult::NotFoundInCurrentInstantiation:
  case clang::LookupResult::NotFound:
    return buildNoMemberError(Loc, R.getLookupName().getAsIdentifierInfo());
  case clang::LookupResult::FoundUnresolvedValue:
    // FIXME: I need to figure out when this can occur, then create
    // that situation within a test and build appropriate error message.
    // I suspect that this may have something to do with variable template
    // declarations.
    llvm_unreachable("Not sure how handle unresolved values.");
  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }
  llvm_unreachable("should never happen!");
}


clang::Expr *
PartialNameAccessExprImpl::NamespaceQualified_appendName_updateTransition(
                             clang::SourceLocation Loc,clang::LookupResult &R) {
  switch(R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
      // FIXME: This not an error when we are inside of a class, and don't have a
      // NonNameLHS. If any of that isn not true we have an error.


      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), nullptr,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                                 R.getLookupNameInfo(),
                                                 // Not sure if needs ADL or
                                                 // not it's going to depend on
                                                 // the kind of function it is.
                                                 /*ADL=*/false, true,
                                                 R.begin(),
                                                 R.end());

      // we will need to figure out if this is a member or static method.
      // llvm_unreachable("Method overload resolution not implemented yet.");
  }
  break;
  case clang::LookupResult::Found: {
    clang::Decl *RHSDecl = R.getAsSingle<clang::Decl>();
    if (auto Ns = dyn_cast<clang::CppxNamespaceDecl>(RHSDecl)) {
      State = BuildingNamespaceExpr;
      CurExpr = SemaRef.buildNSDeclRef(Ns, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto NsAD = dyn_cast<clang::NamespaceAliasDecl>(RHSDecl)) {
      State = BuildingNamespaceExpr;
      CurExpr = SemaRef.buildNSDeclRef(NsAD, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto Record = dyn_cast<clang::CXXRecordDecl>(RHSDecl)) {
      if (Record->isEnum()) {
        error(Loc) << "Invalid member specification, given type name is an enum";
        return nullptr;
      }
      if (auto CTD = Record->getDescribedClassTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(CTD, Loc);
        extenedSSFromExpr(CurExpr);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(Record, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto TD = dyn_cast<clang::TemplateDecl>(RHSDecl)) {
      State = BuildingTemplateQualifiedExpr;
      CurExpr = SemaRef.buildTemplateType(TD, Loc);
      return getIncompleteExpr();

    } else if (auto TyAlias = dyn_cast<clang::TypedefNameDecl>(RHSDecl)) {
      if (auto TemplateAlias = TyAlias->getDescribedTemplate()) {
        State = BuildingTemplateQualifiedExpr;
        CurExpr = SemaRef.buildTemplateType(TemplateAlias, Loc);
        return getIncompleteExpr();
      }
      State = BuildingBaseQualifiedExpr;
      CurExpr = SemaRef.buildTypeExprFromTypeDecl(TyAlias, Loc);
      extenedSSFromExpr(CurExpr);
      return getIncompleteExpr();

    } else if (auto Parm = dyn_cast<clang::ParmVarDecl>(RHSDecl)) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << NonNameLHS << Parm ;
      return nullptr;
    } else if (auto VD = dyn_cast<clang::VarDecl>(RHSDecl)) {
      return clang::DeclRefExpr::Create(SemaRef.getCxxAST(),
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                        clang::SourceLocation(), VD,
                                        /*Capture=*/false, Loc,
                                        VD->getType(), clang::VK_LValue);

    } else if (auto FD = dyn_cast<clang::FunctionDecl>(RHSDecl)) {
      if (NonNameLHS) {
        SemaRef.getCxxSema().Diags.Report(Loc,
                            clang::diag::err_cannot_access_through_nested_name);
        return nullptr;
      }
      clang::UnresolvedSet<1> USet;
      USet.addDecl(FD);
      return clang::UnresolvedLookupExpr::Create(SemaRef.getCxxAST(), nullptr,
                                    SS.getWithLocInContext(SemaRef.getCxxAST()),
                                            R.getLookupNameInfo(),
                                            /*ADL=*/false,
                                            /*Overloaded*/false,
                                            USet.begin(),
                                            USet.end());

    } else {
      // This is an error and we are not sure how to handle this!
      error(Loc) << "invalid name expression";
      return nullptr;
    }
  }
  break;
  case clang::LookupResult::NotFoundInCurrentInstantiation:
  case clang::LookupResult::NotFound:
    return buildNoMemberError(Loc, R.getLookupName().getAsIdentifierInfo());

  case clang::LookupResult::FoundUnresolvedValue:
    // FIXME: I need to figure out when this can occur, then create
    // that situation within a test and build appropriate error message.
    // I suspect that this may have something to do with variable template
    // declarations.
    llvm_unreachable("Not sure how handle unresolved values.");
  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }
  llvm_unreachable("SHould never reach here!");
}

clang::CXXRecordDecl *PartialNameAccessExprImpl::getCXXRecordFromLHS() {
  if (!NonNameLHS) {
    if (InsideClass) {
      // Retrieveing the current this pointer type by removing the pointer.
      clang::QualType Ty = SemaRef.getCxxSema().getCurrentThisType()->getPointeeType();
      return Ty->getAsCXXRecordDecl();
    } else {
      llvm_unreachable("Logically this can never happen.");
      return nullptr;
    }
  } else {
    clang::QualType Ty = NonNameLHS->getType();
    if (Ty->isPointerType()) {
      Ty = Ty->getPointeeType();
    }
    if (!Ty->isClassType()) {
      error(NonNameLHS->getExprLoc())
          << "invalid member access, expression is not an object";
      return nullptr;
    }
    return Ty->getAsCXXRecordDecl();
  }
}

void PartialNameAccessExprImpl::extenedSSFromExpr(clang::Expr *E) {
  assert(E && "Invalid expression");
  using namespace clang;
  QualType Ty = E->getType();
  SourceLocation Loc = E->getExprLoc();
  ASTContext &CxxAST = SemaRef.getCxxAST();
  SourceLocation DefaultLoc;
  if (Ty->isKindType()) {
    TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(E, Loc);
    if (!TInfo) {
      error(Loc) << "invalid type for nested name specifier.";
      return;
    }
    SS.Extend(CxxAST, DefaultLoc, TInfo->getTypeLoc(), Loc);
  } else if (Ty->isCppxNamespaceType()) {
    Decl *PossibleNsDcl = SemaRef.getDeclFromExpr(E, Loc);

    if (auto NsDcl = dyn_cast<clang::CppxNamespaceDecl>(PossibleNsDcl)) {
      SS.Extend(CxxAST, NsDcl, Loc, Loc);
      return;
    }
    if (auto NsAlias = dyn_cast<clang::NamespaceAliasDecl>(PossibleNsDcl)){
      SS.Extend(CxxAST, NsAlias, Loc, Loc);
      return;
    }
    llvm_unreachable("Unknown namespace type");

  } else if (Ty->isTemplateType()) {
    llvm_unreachable("Invalid/incomplete template not implemented yet.");
  } else {
    E->dump();
    llvm_unreachable("Can't update the current scope spec.");
  }
}
clang::Expr *PartialNameAccessExprImpl::buildNoMemberError(
                                                      clang::SourceLocation Loc,
                                                  clang::IdentifierInfo *Name) {
  switch(State) {
    case AwaitingBaseExpr:
      llvm_unreachable("Cannot call no member error without a current expression");
    case BuildingNormalNameAccessExpr:{
      assert(NonNameLHS && "Invalid state.");
      auto Ty = NonNameLHS->getType();
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << Ty << Name;
    }
    break;
    case BuildingNamespaceExpr:{
      assert(CurExpr && "Invalid state.");
      auto NsDcl = cast<clang::NamedDecl>(SemaRef.getDeclFromExpr(CurExpr,
                                                        CurExpr->getExprLoc()));
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << NsDcl->getIdentifier() << Name;
    }
    break;
    case BuildingBaseQualifiedExpr:{
      assert(CurExpr && "Invalid state.");
      clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(CurExpr,
                                                         CurExpr->getExprLoc());
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << TInfo->getType() << Name;
    }
    break;
    case BuildingTemplateQualifiedExpr:{
      assert(CurExpr && "Invalid state.");
      auto TemplateDecl = cast<clang::NamedDecl>(SemaRef.getDeclFromExpr(CurExpr,
                                                  CurExpr->getExprLoc()));
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_no_member)
                                        << Name << TemplateDecl;
    }
    break;
    default:
      llvm_unreachable("invalid state");
  }
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::setIsWithinClass(bool IsInClassScope) {
  switch(State) {
    case AwaitingBaseExpr:
      InsideClass = IsInClassScope;
      break;
    default:
      llvm::outs() << "Current state = " << State << "\n";
      llvm_unreachable("Invalid state for function call.");
  }
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::allowUseOfImplicitThis(bool Value) {
  switch(State) {
    case AwaitingBaseExpr:
      AllowImplicitThisExpr = Value;
      break;
    default:
      llvm::outs() << "Current state = " << State << "\n";
      llvm_unreachable("Invalid state for function call.");
  }
  return nullptr;
}

clang::Expr *PartialNameAccessExprImpl::setBaseExpr(clang::Expr *B) {
  switch(State) {
    case AwaitingBaseExpr: {

      // I think we should always record this?
      CurExpr = B;
      clang::QualType Ty = B->getType();
      if (!Ty->isCppxNamespaceType()
          && !Ty->isTemplateType()
          && !Ty->isKindType()) {
        NonNameLHS = B;
      } else {

        // RD->dump();
        if (Ty->isKindType()) {
          clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                      CurExpr, B->getExprLoc());
          clang::SourceLocation Loc = B->getExprLoc();
          clang::CXXRecordDecl *RD = TInfo->getType()->getAsCXXRecordDecl();
          clang::QualType Type(RD->getTypeForDecl(), 0);
          SemaRef.getCxxSema().RequireCompleteType(Loc, Type,
                                   clang::diag::err_incomplete_nested_name_spec,
                                                    clang::SourceRange(Loc, Loc));
        }
      }
      State = determineNextStateFromExpr(CurExpr);
      // TODO: Update the current CXXScopeSpecifier for a given context.
      switch(State) {
        // We can't do anything with a template until we know if it has arguments
        // or not (meaning we are refering to the current type or another instantiated type).
        case BuildingTemplateQualifiedExpr:
          llvm::outs() << "Setting base as template qualified expression\n";
        case BuildingNormalNameAccessExpr:
          // We don't need to extend the current name specifier in these cases.
          break;
        case BuildingNamespaceExpr:
        case BuildingBaseQualifiedExpr:
          extenedSSFromExpr(CurExpr);
          break;
        default:
          llvm_unreachable("Invalid state.");
      }
    }
    break;
    default:
      llvm::outs() << "Current state = " << State << "\n";
      llvm_unreachable("Invalid state transition.");
  }
  return nullptr;
}

static bool lookForNameInType(Sema &SemaRef,
                              clang::QualType Ty,
                              clang::SourceLocation PrevExprLoc,
                              clang::SourceLocation IdLoc,
                              clang::IdentifierInfo *Id,
                              clang::LookupResult &R) {
  if (auto ElabTy = Ty->getAs<clang::ElaboratedType>()) {
    Ty = ElabTy->desugar();
  }
  if (Ty->isPointerType()) {
    Ty = Ty->getPointeeType();
  }
  const auto *TST = Ty->getAs<clang::TemplateSpecializationType>();
  if (!(Ty->isStructureOrClassType() || Ty->isUnionType()
        || Ty->isEnumeralType()) && !TST) {
    SemaRef.getCxxSema().Diags.Report(IdLoc,
                                      clang::diag::err_invalid_type_for_name_spec)
                                      << Ty;
    return true;
  }


  clang::TagDecl *TD = Ty->getAsTagDecl();
  // TODO: Implement using statements.
  // if (SemaRef.elaboratingUsingInClassScope() && TST) {
  if (!TD) {
    if (TST) {
      TD = cast<clang::TagDecl>(TST->getTemplateName().getAsTemplateDecl()
                                ->getTemplatedDecl());
    }
  }
  // Fetching declaration to ensure that we actually have the current scope
  // for lookup.
  // Attempthing to fetch the declaration now.
  Declaration *DeclForTy = SemaRef.getDeclaration(TD);
  if (!DeclForTy && isa<clang::ClassTemplateSpecializationDecl>(TD)) {
    auto *CTSD = cast<clang::ClassTemplateSpecializationDecl>(TD);
    auto *Primary = CTSD->getSpecializedTemplate();
    DeclForTy = SemaRef.getDeclaration(Primary);
  }
  assert(DeclForTy);

  // TODO: Migrate the ClangToGoldDeclRebuilder into blue. This is only used to
  //  handle template specializations.

  // ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
  // clang::SourceRange Range = clang::SourceRange(Op->getArgument(0)->getLoc(),
  //                                               RHS->getLoc());
  // if (Rebuilder.finishDecl(DeclForTy, Range))
  //   return nullptr;
  // Processing if we have a single name.
  clang::DeclarationNameInfo DNI({Id}, IdLoc);

  auto Ret = TD->lookup(DNI.getName());
  for (clang::NamedDecl *ND : Ret)
    R.addDecl(ND);
  R.resolveKind();
  return !R.empty();
}

static bool doOuterContextLookupContextLookup(clang::LookupResult &R,
                                  clang::DeclContext *DC) {
  assert(DC && "Missing DeclContext.");
  while(DC) {
    if (!DC->isLookupContext()) {
      DC = DC->getLookupParent();
      continue;
    }
    auto Dcls = DC->lookup(R.getLookupName());
    if (!Dcls.empty()) {
      for(auto D : Dcls) {
        R.addDecl(D);
      }
      return false;
    }
    DC = DC->getLookupParent();
  }
  return true;
}

static bool doSimpleDCLookup(clang::LookupResult &R, clang::DeclContext *DC) {
  assert(DC && "Invalid DeclContext");
  auto Dcls = DC->lookup(R.getLookupName());
  if (!Dcls.empty()) {
    for(auto D : Dcls) {
      R.addDecl(D);
    }
    return false;
  }
  return true;
}

clang::Expr *PartialNameAccessExprImpl::appendName(clang::SourceLocation L,
                                                   clang::IdentifierInfo *Id) {
  clang::DeclarationNameInfo DNI({Id}, L);
  clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                        clang::Sema::LookupOrdinaryName);
  switch(State) {
    case BuildingNormalNameAccessExpr:{
      assert(CurExpr == NonNameLHS && "Precondition failure");
      if (getIsInTemplateInstantiation()) {
        clang::QualType Ty = CurExpr->getType().getDesugaredType(
                                                           SemaRef.getCxxAST());
        if (Ty->isPointerType())
          Ty = Ty->getPointeeType();
        const auto *TST = Ty->getAs<clang::TemplateSpecializationType>();
        if ( !(Ty->isStructureOrClassType() || Ty->isUnionType()
              || Ty->isEnumeralType()) && !TST) {
          SemaRef.getCxxSema().Diags.Report(L,
                                    clang::diag::err_invalid_type_for_name_spec)
                                        << Ty;
          return nullptr;
        }
        clang::TagDecl *TD = Ty->getAsTagDecl();
        if (!TD) {
          error(L) << "cannot access members from this type";
          return nullptr;
        }
        if (doSimpleDCLookup(R, TD)) {
          if (doOuterContextLookupContextLookup(R, SemaRef.getCurClangDeclContext())) {
            std::string Msg = "unable to find '" + Id->getName().str() + "'";
            error(L) << Msg;
            return nullptr;
          }
        }
      } else {

        clang::QualType TyToLookInside = CurExpr->getType().getDesugaredType(
                                                            SemaRef.getCxxAST());

        // now we have to do normal lookup and to find the next declaration?

        if (!lookForNameInType(SemaRef, TyToLookInside,
                              CurExpr->getExprLoc(), L, Id, R)) {
          if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
    
            std::string Msg = "unable to find '" + Id->getName().str() + "'";
            error(L) << Msg;
            return nullptr;
          }
          R.resolveKind();
        }
      }
      return normalAccess_appendName_updateTransition(L, R);
    }
    break;
    case BuildingNamespaceExpr:{
        auto NsDcl = cast<clang::CppxNamespaceDecl>(SemaRef.getDeclFromExpr(
                                               CurExpr, CurExpr->getExprLoc()));
        if (getIsInTemplateInstantiation()) {
          if (doSimpleDCLookup(R, NsDcl)) {
            std::string Msg = "unable to find '" + Id->getName().str() + "'";
            error(L) << Msg;
            return nullptr;
          }
       } else if (SemaRef.lookupUnqualifiedName(R, NsDcl->getBlueScopeRep())) {
          std::string Msg = "unable to find '" + Id->getName().str() + "'";
          error(L) << Msg;
          return nullptr;
        }
        R.resolveKind();
        return NamespaceQualified_appendName_updateTransition(L, R);
      }
      break;
    case BuildingBaseQualifiedExpr: {
      clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(CurExpr, L);
      if (!TInfo) {
        llvm_unreachable("Invalid type for state unable to continue");
      }

      if (getIsInTemplateInstantiation()) {
        clang::QualType Ty = TInfo->getType().getDesugaredType(
                                                            SemaRef.getCxxAST());
        if (Ty->isPointerType())
          Ty = Ty->getPointeeType();
        const auto *TST = Ty->getAs<clang::TemplateSpecializationType>();
        if (!(Ty->isStructureOrClassType() || Ty->isUnionType()
            || Ty->isEnumeralType()) && !TST) {
          SemaRef.getCxxSema().Diags.Report(L,
                                    clang::diag::err_invalid_type_for_name_spec)
                                        << Ty;
          return nullptr;
        }
        clang::TagDecl *TD = Ty->getAsTagDecl();
        if (!TD){
          error(L) << "cannot access members from this type";
          return nullptr;
        }
        if (doSimpleDCLookup(R, TD)) {
          if (doOuterContextLookupContextLookup(R, SemaRef.getCurClangDeclContext())) {
            std::string Msg = "unable to find '" + Id->getName().str() + "'";
            error(L) << Msg;
            return nullptr;
          }
        }
      } else {
        clang::QualType TyToLookInside = TInfo->getType();
        // now we have to do normal lookup and to find the next declaration?
        if (TyToLookInside->isPointerType())
          TyToLookInside = TyToLookInside->getPointeeType();

        if (!lookForNameInType(SemaRef, TyToLookInside,
                              CurExpr->getExprLoc(), L, Id, R)) {
          SemaRef.getCxxSema().Diags.Report(L, clang::diag::err_no_member)
              << Id->getName() << TyToLookInside;
          return nullptr;
        }
      }
      R.resolveKind();
      return baseQualified_appendName_updateTransition(L, R);
    }
    break;
    case BuildingTemplateQualifiedExpr:
      // If we append a name to a template qualified expression
      // (ie an incomplete template expression), it constitutes an error.
      // llvm_unreachable("BuildingTemplateQualifiedExpr expr lookup not implemented yet.");
      // clang::QualType TyToLookInside = CurExpr->getType().getDesugaredType(SemaRef.getCxxAST());
      // clang::LookupResult R(SemaRef.getCxxSema(), {{Id}, L},
      //                       clang::Sema::LookupOrdinaryName);
      // // now we have to do normal lookup and to find the next declaration?
      // if (TyToLookInside->isPointerType())
      //   TyToLookInside = TyToLookInside->getPointeeType();
      // if (!lookForNameInType(SemaRef, TyToLookInside,
      //                       CurExpr->getExprLoc(), L, Id, R)) {
      //   if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
      //     error(L) << "unable to find " << Id->getName();
      //     // This means we didn't find anything in scope.
      //     return nullptr;
      //   }
      // }
      // llvm_unreachable("This may need to be a regular error");
      error(L) << "unable to access members through template name";
      break;
    default:
      llvm::errs() << "Invalid state " << State << "\n";
      llvm_unreachable("State not implemented yet!");
  }
  return getIncompleteExpr();
}

clang::Expr *
PartialNameAccessExprImpl::appendElementExpr(clang::SourceLocation B,
                                             clang::SourceLocation E,
                                  clang::TemplateArgumentListInfo &TemplateArgs,
          llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArguments,
                             llvm::SmallVectorImpl<clang::Expr *> &OnlyExprArgs)
{
  switch(State) {
    case BuildingBaseQualifiedExpr:
    case BuildingNamespaceExpr:
    case BuildingNormalNameAccessExpr:
      error(CurExpr->getExprLoc()) << "arguments given to a non-template name.";
      break;
    case BuildingTemplateQualifiedExpr:{
        // This is the only case that's allowed in this scenario.
        auto TemplateDcl = cast<clang::TemplateDecl>(
                       SemaRef.getDeclFromExpr(CurExpr, CurExpr->getExprLoc()));
        clang::TemplateName TName(TemplateDcl);
        clang::Sema::TemplateTy TemplateTyName
                                         = clang::Sema::TemplateTy::make(TName);
        clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
        clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
            SemaRef.getCurClangScope(), SS, /*TemplateKWLoc*/ B,
            TemplateTyName, TemplateDcl->getIdentifier(), CurExpr->getExprLoc(),
            /*LAngleLoc*/B, InArgs, /*RAngleLoc*/ E, false, true);

        if (Result.isInvalid()) {
          SemaRef.getCxxSema().Diags.Report(CurExpr->getExprLoc(),
                                     clang::diag::err_failed_to_translate_expr);
          return nullptr;
        }

        clang::SourceLocation IdLoc = CurExpr->getExprLoc();
        clang::QualType InstantiatedTemplatTy = Result.get().get();
        const clang::LocInfoType *LocInfoTy =  cast<clang::LocInfoType>(
                                            InstantiatedTemplatTy.getTypePtr());
        clang::TypeSourceInfo *NewTInfo = LocInfoTy->getTypeSourceInfo();
        CurExpr = SemaRef.buildTypeExpr(NewTInfo);
        if (NewTInfo->getType()->isDependentType() && !getIsInTemplateInstantiation()) {
          // If we have an LHS then we may need to do something slightly
          // different here?
          if (NonNameLHS) {
            llvm_unreachable("Not sure if this is possible or not yet.");
          }
          clang::Expr *Ret =
           rebuildNestedNameSpecifier(SS.getWithLocInContext(SemaRef.getCxxAST()));

          // Modifying things.
          clang::DeclarationNameInfo DNI({TemplateDcl->getIdentifier()}, IdLoc);
          Ret = clang::CppxDependentMemberAccessExpr::Create(
            SemaRef.getCxxAST(), Ret, SemaRef.getCxxAST().DependentTy,
            IdLoc, DNI);
          Ret = clang::CppxTemplateOrArrayExpr::Create(SemaRef.getCxxAST(),
                                                       Ret, OnlyExprArgs);
          return Ret;
        }
        State = BuildingBaseQualifiedExpr;
        SS.Extend(SemaRef.getCxxAST(), IdLoc, NewTInfo->getTypeLoc(), IdLoc);
        return getIncompleteExpr();
      }
      break;
    default:
      llvm_unreachable("State not implemented yet!");
  }
  return nullptr;
}

// static clang::Expr *rebuildTypeForNNS(Sema &SemaRef, const clang::Type *Ty) {

//   // clang::DeclarationNameInfo DNI(
//   //   CurNNS->getAsNamespace()->getDeclName(), NNS.getBeginLoc());
//   // return clang::CppxDependentMemberAccessExpr::Create(
//   //   SemaRef.getCxxAST(), NonNameLHS, SemaRef.getCxxAST().DependentTy,
//   //   NNS.getBeginLoc(), DNI);
// }
clang::Expr *PartialNameAccessExprImpl::doSingleRebuild(clang::NestedNameSpecifierLoc NNS,
                              clang::NestedNameSpecifier *CurNNS,
                              clang::Expr *LHS) {
  switch(CurNNS->getKind()) {
    case clang::NestedNameSpecifier::Identifier:{
      clang::DeclarationNameInfo DNI({CurNNS->getAsIdentifier()},
                                    NNS.getLocalBeginLoc());
      return clang::CppxDependentMemberAccessExpr::Create(
        SemaRef.getCxxAST(), LHS, SemaRef.getCxxAST().DependentTy,
        NNS.getLocalBeginLoc(), DNI);
    }
    case clang::NestedNameSpecifier::NamespaceAlias:{
      clang::DeclarationNameInfo DNI(
        CurNNS->getAsNamespaceAlias()->getDeclName(), NNS.getLocalBeginLoc());
      return clang::CppxDependentMemberAccessExpr::Create(
        SemaRef.getCxxAST(), LHS, SemaRef.getCxxAST().DependentTy,
        NNS.getLocalBeginLoc(), DNI);
    }
    case clang::NestedNameSpecifier::Namespace:{
      clang::DeclarationNameInfo DNI(
        CurNNS->getAsNamespace()->getDeclName(), NNS.getLocalBeginLoc());
      return clang::CppxDependentMemberAccessExpr::Create(
        SemaRef.getCxxAST(), LHS, SemaRef.getCxxAST().DependentTy,
        NNS.getLocalBeginLoc(), DNI);
    }
    case clang::NestedNameSpecifier::TypeSpec:{
      // Templates should never occur here!
      clang::DeclarationNameInfo DNI(
        CurNNS->getAsRecordDecl()->getDeclName(), NNS.getLocalBeginLoc());
      return clang::CppxDependentMemberAccessExpr::Create(
        SemaRef.getCxxAST(), LHS, SemaRef.getCxxAST().DependentTy,
        NNS.getLocalBeginLoc(), DNI);
    }
    case clang::NestedNameSpecifier::TypeSpecWithTemplate:{

      const clang::Type *Ty = CurNNS->getAsType();
      const auto *TST = Ty->getAs<clang::TemplateSpecializationType>();
      if (!TST) {
        // We may have to handle elaborated types also.
        Ty->dump();
        llvm_unreachable("Not sure what the type is!");
      }
      clang::IdentifierInfo *Id = nullptr;
      clang::TemplateName TN = TST->getTemplateName();
      switch(TN.getKind()) {
        case clang::TemplateName::Template:
          Id = TN.getAsTemplateDecl()->getIdentifier();
          break;
        case clang::TemplateName::OverloadedTemplate:{
            // I need to grab the first decl and get an identifier from it.
            // llvm_unreachable("OverloadedTemplate name resolution Not implemented");
            auto OT = TN.getAsOverloadedTemplate();
            if (OT->size() == 0) {
              llvm_unreachable("Incorrectly generated template overload name");
            }
            for (auto D : *OT) {
              if (auto NamedDcl = dyn_cast<clang::NamedDecl>(D)) {
                Id = NamedDcl->getIdentifier();
                break;
              }
            }
            if (!Id)
              // TODO: I need to figure out if this needs to be an error message
              // instead of a hard failure.
              llvm_unreachable("Incorrectly generated template overload name");
          }
          break;
        case clang::TemplateName::AssumedTemplate:
          Id = TN.getAsAssumedTemplateName()->getDeclName().getAsIdentifierInfo();
          break;
        case clang::TemplateName::QualifiedTemplate:
          // FIXME: If we have this then we have an error
          llvm_unreachable("this shouldn't be possible?");
          break;
        case clang::TemplateName::DependentTemplate:
          Id = const_cast<clang::IdentifierInfo *>(
                          TN.getAsDependentTemplateName()->getIdentifier());
          break;
        case clang::TemplateName::SubstTemplateTemplateParm:
          Id = TN.getAsSubstTemplateTemplateParm()->getParameter()->getIdentifier();
          break;
        case clang::TemplateName::SubstTemplateTemplateParmPack:
          // I'm not sure this is correct but AFAIK this can't occur as part
          // the name part of the nested name specifier, because that wouldn't
          // make any sense.
          llvm_unreachable("We can't have a parameter pack inside of a "
                            "nested name specifier.");
          break;
        default:
          llvm_unreachable("invalid template name kind");
      }
      assert(Id && "failed to correctly set identifier");

      // Reconstructing the previous dependent expression using the given
      // template name.
      clang::DeclarationNameInfo DNI({Id}, NNS.getLocalBeginLoc());
      clang::Expr *AccessExpr = clang::CppxDependentMemberAccessExpr::Create(
        SemaRef.getCxxAST(), LHS, SemaRef.getCxxAST().DependentTy,
        NNS.getLocalBeginLoc(), DNI);
      llvm::SmallVector<clang::Expr *, 16> AdjustedArguments;
      auto TL = NNS.getTypeLoc().getAs<clang::TemplateSpecializationTypeLoc>();
      unsigned Index = 0;
      for(clang::TemplateArgument TA : TST->template_arguments()) {
        clang::TemplateArgumentLoc TAL = TL.getArgLoc(Index);
        clang::Expr *ToAppend = nullptr;
        switch(TA.getKind()) {
          case clang::TemplateArgument::Type:
            ToAppend = SemaRef.buildTypeExpr(TAL.getTypeSourceInfo());
            break;
          case clang::TemplateArgument::Declaration:
            ToAppend = TAL.getSourceDeclExpression();
            break;
          case clang::TemplateArgument::NullPtr:
            ToAppend = TAL.getSourceNullPtrExpression();
            break;
          case clang::TemplateArgument::Integral:
            ToAppend = TAL.getSourceIntegralExpression();
            break;
          case clang::TemplateArgument::Template:
            ToAppend = SemaRef.buildTemplateType(
                  TA.getAsTemplate().getAsTemplateDecl(),TAL.getLocation());
            break;
          case clang::TemplateArgument::TemplateExpansion:
            llvm_unreachable("Template expansion handling not implemented yet.");
            break;
          case clang::TemplateArgument::Expression:
            AdjustedArguments.emplace_back(TAL.getSourceExpression());
            break;
          case clang::TemplateArgument::Pack:
            llvm_unreachable("I don't know what this is?!");
          default:
            llvm_unreachable("Invalid template argument kind.");
        }
        AdjustedArguments.emplace_back(ToAppend);
        ++Index;
      }
      return clang::CppxTemplateOrArrayExpr::Create(SemaRef.getCxxAST(),
                                                    AccessExpr,
                                                    AdjustedArguments);
    }
    case clang::NestedNameSpecifier::Global:
      llvm_unreachable("Global nested name specifier not implemented yet");

    case clang::NestedNameSpecifier::Super:
      llvm_unreachable("__super not supported");

    default:
      llvm_unreachable("Invalid nested name specifier kind");
  }
}

clang::Expr *PartialNameAccessExprImpl::rebuildNestedNameSpecifier(clang::NestedNameSpecifierLoc NNS) {
  clang::NestedNameSpecifierLoc NNSPrev = NNS.getPrefix();
  clang::Expr *LHS = nullptr;
  clang::NestedNameSpecifier *CurNNS = NNS.getNestedNameSpecifier();
  if (NNSPrev) {
    LHS = rebuildNestedNameSpecifier(NNSPrev);
    if (!LHS)
      llvm_unreachable("Rebuilding an nested name specifier should never fail");
  } else {
    if (NonNameLHS) {
      LHS = NonNameLHS;
    } else {
      // The last thing within the current declaration is the current nested
      // name specifier.
      switch(CurNNS->getKind()) {
        case clang::NestedNameSpecifier::Identifier:
          llvm_unreachable("This may be the only error case, where we have some "
                           "unknown dependent identifier");
        case clang::NestedNameSpecifier::NamespaceAlias:
          return SemaRef.buildNSDeclRef(CurNNS->getAsNamespaceAlias(), NNS.getLocalBeginLoc());
        case clang::NestedNameSpecifier::Namespace:
          return SemaRef.buildNSDeclRef(
            cast<clang::CppxNamespaceDecl>(CurNNS->getAsNamespace()),
            NNS.getLocalBeginLoc());
        case clang::NestedNameSpecifier::TypeSpec:
          return SemaRef.buildTypeExpr(clang::QualType(CurNNS->getAsType(), 0),
                                       NNS.getLocalBeginLoc());
        case clang::NestedNameSpecifier::TypeSpecWithTemplate:
          return SemaRef.buildTypeExpr(clang::QualType(CurNNS->getAsType(), 0),
                                       NNS.getLocalBeginLoc());
        case clang::NestedNameSpecifier::Global:
          llvm_unreachable("Global nested name specifier not implemented yet");

        case clang::NestedNameSpecifier::Super:
          llvm_unreachable("__super not supported");

        default:
          llvm_unreachable("Invalid nested name specifier kind");
      }
    }
  }
  return doSingleRebuild(NNS, CurNNS, LHS);

}

clang::Expr *PartialNameAccessExprImpl::completeExpr() {
    switch(State) {
    case BuildingNormalNameAccessExpr:
      if (NonNameLHS == CurExpr) {
        return NonNameLHS;
      }
      error(NonNameLHS->getExprLoc())
            << "cannot not complete expression as written";
      return nullptr;
    case BuildingNamespaceExpr:
      if (NonNameLHS) {
        error(NonNameLHS->getExprLoc())
              << "cannot not complete expression as written, expression "
                 "evaluates to an incomplete expression";
        return nullptr;
      }
      return CurExpr;
    case BuildingBaseQualifiedExpr:
      if (NonNameLHS) {
        error(NonNameLHS->getExprLoc())
              << "cannot not complete expression as written, expression "
                 "evaluates to an incomplete expression";
        return nullptr;
      }
      return CurExpr;
    case BuildingTemplateQualifiedExpr: {
      // Verifying template name.
      // bool 	isCurrentClassName (const IdentifierInfo &II, Scope *S, const CXXScopeSpec *SS=nullptr)
      // clang::QualType ThisPtrTy = SemaRef.getCxxSema().getCurrentThisType();
      // if (ThisPtrTy )
      auto TempLoc = CurExpr->getExprLoc();
      clang::Decl *CurDecl = SemaRef.getDeclFromExpr(CurExpr, TempLoc);
      if (!CurDecl) {
        error(TempLoc) << "invalid reference to a template";
        return nullptr;
      }
      if (auto TD = dyn_cast<clang::TemplateDecl>(CurDecl)) {
        // Sema
        // SS.Extend(SemaRef.getCxxAST(), TD->getIdentifier(), TempLoc, TempLoc);
        clang::IdentifierInfo *ID = TD->getIdentifier();
        if (SemaRef.getCxxSema().isCurrentClassName(*ID,
                                                    SemaRef.getCurClangScope(),
                                                    &SS))
        {
          llvm_unreachable("Self referencing a class template not implemented yet.");
        } else {
          return CurExpr;
        }
      } else {
        // This may be impossible.
        error(TempLoc) << "invalid reference to a template name";
        return nullptr;
      }
      
      // if ()
      // If we append a name to a template qualified expression
      // (ie an incomplete template expression), it constitutes an error.
      llvm_unreachable("I need to implement self name referencing for in class "
                       "name lookup for templates without arguments.");
      }
      break;
    default:
      llvm_unreachable("State not implemented yet!");
  }
  return nullptr;
}

clang::DiagnosticBuilder PartialNameAccessExprImpl::error(clang::SourceLocation Loc) {
  return SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_blue_elaboration);
}

}