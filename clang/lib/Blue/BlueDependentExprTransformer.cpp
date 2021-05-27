//===- BlueDependentExprTransformer.cpp -----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file is specifically used to generate code from a given dependent
//  expression, that could possibly change meaning or type.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueDependentExprTransformer.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/AST/ExprCppx.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Blue/BlueExprMarker.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"



namespace blue {

DependentExprTransformer::DependentExprTransformer(Sema &S, SyntaxContext &Ctx,
                           const clang::MultiLevelTemplateArgumentList &TemplateArgs,
                           clang::SourceLocation Loc,
                           clang::DeclarationName Entity)
  :SemaRef(S), Context(Ctx), TemplateArgs(TemplateArgs), InstantiationLoc(Loc),
  EntityName(Entity)
{ }

clang::Expr *DependentExprTransformer::transformDependentExpr(clang::Expr *E) {
  assert(E && "Invalid expression.");
  if (auto DMAE = dyn_cast<clang::CppxDependentMemberAccessExpr>(E))
    return transformCppxDependentMemberAccessExpr(DMAE);

  if (auto TOAE = dyn_cast<clang::CppxTemplateOrArrayExpr>(E))
    return transformCppxTemplateOrArrayExpr(TOAE);

  if (auto LT = dyn_cast<clang::CppxTypeLiteral>(E))
    return transformCppxLiteralType(LT);

  if (auto PartialExpr = dyn_cast<clang::CppxPartialEvalExpr>(E))
    return transformDependentExpr(PartialExpr->completeExpr());

  // if (auto DerefOrPtr = dyn_cast<clang::CppxDerefOrPtrExpr>(E))
  //   return transformCppxDerefOrPtrExpr(DerefOrPtr);
  return SemaRef.getCxxSema().SubstExpr(E, TemplateArgs).get();
}

// static clang::TagDecl *handleNestedNameQualifier(DependentExprTransformer &T,
//     Sema &SemaRef, clang::CppxDependentMemberAccessExpr *E,
//     clang::QualType Ty, clang::TagDecl *TD) {
//   if (E->getNameQualifierExpr()) {
//     clang::Expr *QualifierResult
//                           = T.transformDependentExpr(E->getNameQualifierExpr());
//     if (QualifierResult) {
//       if (auto TyLit = dyn_cast<clang::CppxTypeLiteral>(QualifierResult)) {
//         auto *NextTD = TyLit->getValue()->getType()->getAsCXXRecordDecl();
//         if (NextTD) {
//           if (auto RootRD = Ty->getAsCXXRecordDecl()) {
//             if (RootRD->isDerivedFrom(NextTD)) {
//               return NextTD;
//             } else {
//               SemaRef.getCxxSema().Diags.Report(TyLit->getExprLoc(),
//                                 clang::diag::err_nested_namespecifier_not_base)
//                                     << Ty << TyLit->getValue()->getType();
//               return nullptr;
//             }
//           } else {
//             SemaRef.getCxxSema().Diags.Report(TyLit->getExprLoc(),
//                             clang::diag::err_nested_namespecifier_not_a_class)
//                                 << TyLit->getValue()->getType();
//             return nullptr;
//           }
//         } else {
//           SemaRef.getCxxSema().Diags.Report(TyLit->getExprLoc(),
//                                 clang::diag::err_invalid_type_for_name_spec)
//                                 << TyLit->getValue()->getType();
//           return nullptr;
//         }
//       } else {
//         SemaRef.getCxxSema().Diags.Report(TyLit->getExprLoc(),
//                               clang::diag::err_not_a_type);
//         return nullptr;
//       }
//     } else {
//         SemaRef.getCxxSema().Diags.Report(E->getNameQualifierExpr()->getExprLoc(),
//                              clang::diag::err_qualified_name_no_a_type);
//         return nullptr;
//     }
//   }
//   return TD;
// }

static void addIfNotDuplicate(clang::LookupResult &R, clang::NamedDecl *ND) {
  for (clang::Decl *D : R) {
    if (D == ND) {
      return;
    }
  }
  R.addDecl(ND, ND->getAccess());
}

static bool isInsideOfClassContext(clang::DeclContext *DC) {
  while(DC) {
    if (DC->isRecord())
      return true;
    DC = DC->getParent();
  }
  return false;
}
static bool isCXXThisValidInContext(clang::DeclContext *DC) {
  while(DC) {
    if (auto MD = dyn_cast<clang::CXXMethodDecl>(DC)) {
      return !MD->isStatic();
    }
    DC = DC->getParent();
  }
  return false;
}

clang::Expr *DependentExprTransformer::transformCppxDependentMemberAccessExpr(
                                      clang::CppxDependentMemberAccessExpr *E) {
  assert(!E->isImplicitAccess() && "Implicit access not implemented.");
  assert(!E->getNameQualifierExpr() && "Name qualifiers aren't deducable before this point in blue.");
  // E->dump();
  // llvm_unreachable("transformCppxDependentMemberAccessExpr not implemented yet.");
  // if (E->isImplicitAccess()) {
  //   // Implicit dependent member access is supported by other means.
  //   // The CppxDependentMemberAccessExpr can only occur between two members
  //   // where the type of the expression is unknown until instantiation.
  //   E->dump();
  //   llvm_unreachable("Dependent implicit access not implemented/supported.");
  // }
  clang::Expr *Ret = transformDependentExpr(E->getBase());
  if (!Ret) {
    SemaRef.getCxxSema().Diags.Report(E->getBase()->getExprLoc(),
                         clang::diag::err_invalid_dependent_expr);
    return nullptr;
  }
  // clang::CppxPartialEvalExpr *PartialE = nullptr;
  // PartialE = dyn_cast<clang::CppxPartialEvalExpr>(Ret);
  // if (!PartialE) {
  //   PartialE = SemaRef.createPartialExpr(E->getExprLoc(),
  //                      isInsideOfClassContext(SemaRef.getCurClangDeclContext()),
  //                      isCXXThisValidInContext(SemaRef.getCurClangDeclContext()),
  //                             Ret, true);
  // }
  // llvm_unreachable("Working on it.");
  // Retrieving member information.
  // auto DNI = E->getMemberNameInfo();
  // return PartialE->appendName(DNI.getLoc(), DNI.getName().getAsIdentifierInfo());
  // Attempting to do lookup for the next name?

  if (Ret->getType()->isNamespaceType() || Ret->getType()->isTemplateType()) {
    // Create a partial expression here and attempt to append the new name to it.
    llvm_unreachable("Start of partial expression lookup is incomplete.");
  }
  clang::QualType Ty;
  bool NeedsArrow = false;
  if (Ret->getType()->isTypeOfTypes()) {
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(Ret,
                                                             Ret->getExprLoc());
    if (!TInfo) {
      return nullptr;
    }

    Ty = TInfo->getType();
    Ty = transformType(Ty);
  } else {
    Ty = Ret->getType();
    Ty = transformType(Ty);
    if (Ty.isNull())
      return nullptr;

    if (Ty->isPointerType()) {
      Ty = Ty->getPointeeType();
      NeedsArrow = true;
    }
    // Doing a 2ndary transformation of the inner type of a pointer because this
    // may be the actual dependent type.
    Ty = transformType(Ty);
    if (Ty.isNull())
      return nullptr;
  }
  // Getting innner most unqualified type for look up.
  Ty = Ty.getCanonicalType();

  // Handling errors from pointer type.
  if (Ty.isNull()) {
    SemaRef.getCxxSema().Diags.Report(Ret->getExprLoc(),
                         clang::diag::err_not_a_type);
    return nullptr;
  }
  if (auto TSTTy = dyn_cast<clang::TemplateSpecializationType>(Ty)) {
    Ty = TSTTy->desugar();
  }
  if (!isa<clang::TagType>(Ty)) {
    SemaRef.getCxxSema().Diags.Report(Ret->getExprLoc(),
                       clang::diag::err_typecheck_member_reference_struct_union)
                         << Ty;
    return nullptr;
  }

  clang::TagDecl *TD = Ty->getAsTagDecl();
  clang::SourceLocation Loc(E->getExprLoc());
  clang::SourceRange Range(Loc, Loc);
  if (isa<clang::ClassTemplateSpecializationDecl>(TD)) {
    if (SemaRef.getCxxSema().RequireCompleteType(Loc, Ty,
                                   clang::diag::err_incomplete_nested_name_spec,
                                                Range)){
      return nullptr;
    }
  }
  // clang::TagDecl *NextTD = handleNestedNameQualifier(*this, SemaRef, E, Ty,
  //                                                    TD);
  // if (!NextTD)
  //   return nullptr;

  if (!Ret->getType()->isTypeOfTypes()) {
    clang::CXXScopeSpec SS;
    clang::SourceLocation Loc;
    clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
    if (NeedsArrow)
      AccessTokenKind = clang::tok::TokenKind::arrow;

    bool MayBePseudoDestructor = false;
    clang::ParsedType ObjectTy;
    Ret = SemaRef.getCxxSema().ActOnStartCXXMemberReference(nullptr, Ret, Loc,
                                                            AccessTokenKind,
                                                            ObjectTy,
                                                   MayBePseudoDestructor).get();
    if (!Ret)
      return nullptr;
  }

  // TD = NextTD;

  // auto ClsDcl = dyn_cast<clang::CXXRecordDecl>(TD);
  clang::DeclarationName MemberName = E->getMember();
  clang::DeclarationNameInfo DNI(MemberName, Ret->getExprLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                        clang::Sema::LookupMemberName,
                        clang::Sema::NotForRedeclaration);
  // if (ClsDcl) {
    // SemaRef.getCxxSema().LookupQualifiedName(R, ClsDcl, false);
    // for(auto D : R.asUnresolvedSet()) {
    //   R.addDecl(D, D->getAccess());
    // }
  // } else {
  auto Members = TD->lookup(MemberName);
  for(auto D : Members) {
    R.addDecl(D, D->getAccess());
  }
  // }

  R.resolveKind();
  switch (R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
    clang::SourceLocation Loc = Ret->getExprLoc();
    return buildUnresolvedCall(Ret, dyn_cast<clang::CXXRecordDecl>(TD), R, Loc);
  }

  case clang::LookupResult::Found:
    return buildMemberAccessExpr(Ret, Ty, R.getFoundDecl(),
                                 dyn_cast<clang::CXXRecordDecl>(TD),
                                 R, Ret->getExprLoc(), NeedsArrow);
  case clang::LookupResult::NotFoundInCurrentInstantiation: {
  case clang::LookupResult::NotFound:
    SemaRef.getCxxSema().Diags.Report(Ret->getExprLoc(),
                          clang::diag::err_no_member)
                          << DNI.getName().getAsString() << Ty;
    return nullptr;
  }
  case clang::LookupResult::FoundUnresolvedValue:
    // This logically can never happen.
    llvm_unreachable("FoundUnresolvedValue can never happen because everything "
                     "that is dependent should have aleady been evaluated.");

  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }

  llvm_unreachable("Incomplete elaboration of lookup results.");
}

clang::Expr *DependentExprTransformer::transformCppxTemplateOrArrayExpr(
      clang::CppxTemplateOrArrayExpr *E) {
  assert(E->getBase() && "Missing base id expression");
  clang::Expr *IdExpr = E->getBase();
  if (auto PartialE = dyn_cast<clang::CppxPartialEvalExpr>(IdExpr)) {
    clang::TemplateArgumentListInfo TemplateArgs(E->getBeginLoc(), E->getEndLoc());
    llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

    if (transformTemplateArguments(E, TemplateArgs, ParsedArguments))
      return nullptr;
    clang::SourceLocation Loc = E->getBase()->getExprLoc();
    llvm::SmallVector<clang::Expr *, 16> OnlyExpr;
    return PartialE->appendElementExpr(Loc, Loc, TemplateArgs, ParsedArguments,
                                       OnlyExpr);
  } else {
    IdExpr = transformDependentExpr(IdExpr);
    if (!IdExpr)
      return nullptr;
  }
  if (auto PartialE = dyn_cast<clang::CppxPartialEvalExpr>(IdExpr)) {
    clang::TemplateArgumentListInfo TemplateArgs(E->getBeginLoc(), E->getEndLoc());
    llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

    if (transformTemplateArguments(E, TemplateArgs, ParsedArguments))
      return nullptr;
    clang::SourceLocation Loc = E->getBase()->getExprLoc();
    llvm::SmallVector<clang::Expr *, 16> OnlyExpr;
    return PartialE->appendElementExpr(Loc, Loc, TemplateArgs, ParsedArguments,
                                       OnlyExpr);
  }
  clang::QualType IdExprTy = IdExpr->getType();
  if (IdExprTy->isTypeOfTypes() || IdExprTy->isNamespaceType()) {
    SemaRef.getCxxSema().Diags.Report(IdExpr->getExprLoc(),
                      clang::diag::err_non_array_or_template_with_sqr_brackets);
    return nullptr;
  }

  if (IdExprTy->isTemplateType()) {
    // Handling class (and possible variable?) template transformation
    clang::Decl *Decl = SemaRef.getDeclFromExpr(IdExpr, IdExpr->getExprLoc());
    if (auto TmpltDcl = dyn_cast<clang::TemplateDecl>(Decl))
      return transformTemplateInstantiation(E, TmpltDcl);
    IdExpr->dump();
    llvm_unreachable("Invalid template declaration node");
  }

  // clang::CppxPartialEvalExpr *PartialE = nullptr;
  clang::OverloadExpr *OverloadExpr = dyn_cast<clang::OverloadExpr>(IdExpr);

  // Create a normal array access.
  if (!OverloadExpr) {
    llvm::SmallVector<clang::Expr *, 4> ArgExprs;

    for (auto A : E->arguments()) {
      clang::Expr *TArg = transformDependentExpr(A);
      if (!TArg) {
        SemaRef.getCxxSema().Diags.Report(A->getExprLoc(),
                             clang::diag::err_failed_to_translate_expr);
        continue;
      }
      clang::QualType ArgTy = TArg->getType();
      if (ArgTy->isNamespaceType()
          || ArgTy->isTemplateType()
          || ArgTy->isTypeOfTypes()) {
        SemaRef.getCxxSema().Diags.Report(A->getExprLoc(),
                             clang::diag::err_invalid_result_type);
        continue;
      }
      ArgExprs.emplace_back(TArg);
    }

    if (ArgExprs.size() == 0) {
      SemaRef.getCxxSema().Diags.Report(E->getExprLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }

    // Create the first subscript out of the base expression.
    auto SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
      /*Scope=*/nullptr, IdExpr, ArgExprs[0]->getExprLoc(), ArgExprs[0],
      ArgExprs[0]->getExprLoc());

    if (SubscriptExpr.isInvalid())
      return nullptr;

    // Then use the previous subscripts as bases, recursively.
    for (unsigned I = 1; I < ArgExprs.size(); ++I) {
      SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
        /*Scope=*/nullptr, SubscriptExpr.get(), ArgExprs[I]->getExprLoc(),
        ArgExprs[I], ArgExprs[I]->getExprLoc());

      // We don't know what will happen if we try to recover, so just quit.
      if (SubscriptExpr.isInvalid())
        return nullptr;
    }

    return SubscriptExpr.get();
  }

  // We have an overload set, meaning this must be some kind of
  // overloaded function or function template.
  clang::SourceLocation Loc = IdExpr->getExprLoc();
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  clang::TemplateArgumentListInfo ArgInfo(Loc, Loc);
  llvm::SmallVector<clang::ParsedTemplateArgument, 32> ParsedArgs;
  if (transformTemplateArguments(E, ArgInfo, ParsedArgs))
    return nullptr;

  // Converting into a new kind of collection.
  for (clang::TemplateArgumentLoc A : ArgInfo.arguments())
    ActualArgs.push_back(A.getArgument());

  clang::TemplateArgumentList TAL(clang::TemplateArgumentList::OnStack,
                                  ActualArgs);
  if (auto UME = dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
    return clang::UnresolvedMemberExpr::Create(Context.CxxAST,
                                               UME->hasUnresolvedUsing(),
                                               UME->getBase(),
                                               UME->getBaseType(),
                                               UME->isArrow(),
                                               UME->getOperatorLoc(),
                                               UME->getQualifierLoc(),
                                               UME->getTemplateKeywordLoc(),
                                               UME->getNameInfo(),
                                               &ArgInfo,
                                               UME->decls_begin(),
                                               UME->decls_end());
  }
  if (auto ULE = dyn_cast<clang::UnresolvedLookupExpr>(OverloadExpr)) {
    return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
                                               ULE->getNamingClass(),
                                               ULE->getQualifierLoc(),
                                               clang::SourceLocation(),
                                               ULE->getNameInfo(),
                                               /* ADL=*/ false,
                                               &ArgInfo,
                                               ULE->decls_begin(),
                                               ULE->decls_end());
  }
  llvm_unreachable("Unknown overload expression.");
}

// clang::Expr *
// DependentExprTransformer::transformCppxDerefOrPtrExpr(
//     clang::CppxDerefOrPtrExpr* E) {
//   clang::Expr *InnerExpr = transformDependentExpr(E->getValue());
//   if (!InnerExpr)
//     return nullptr;
//   clang::QualType Ty = InnerExpr->getType();
//   if (Ty->isNamespaceType() || Ty->isTemplateType()) {
//     SemaRef.getCxxSema().Diags.Report(E->getExprLoc(),
//                          clang::diag::err_deref_or_ptr_for_invalid_type);
//     return nullptr;
//   }
//   if (Ty->isTypeOfTypes()) {
//     clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InnerExpr,
//                                                                E->getExprLoc());
//     if (!TInfo)
//       return nullptr;
//     clang::QualType RetType = Context.CxxAST.getPointerType(TInfo->getType());
//     return SemaRef.buildTypeExpr(RetType, E->getExprLoc());
//   }

//   // Creating a dereference expression.
//   clang::ExprResult UnaryOpRes = SemaRef.getCxxSema().BuildUnaryOp(
//     /*scope*/nullptr, E->getExprLoc(), clang::UO_Deref, InnerExpr);

//   ExprMarker(Context.CxxAST, SemaRef).Visit(InnerExpr);
//   return UnaryOpRes.get();
// }

clang::Expr *
DependentExprTransformer::transformTemplateInstantiation(
               clang::CppxTemplateOrArrayExpr *E, clang::TemplateDecl *Decl) {

  clang::TemplateArgumentListInfo TemplateArgs(E->getBeginLoc(), E->getEndLoc());
  llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

  if (transformTemplateArguments(E, TemplateArgs, ParsedArguments))
    return nullptr;

  clang::TemplateDecl *CTD = dyn_cast<clang::TemplateDecl>(Decl);
  assert(CTD && "Invalid CppxDeclRefExpr");

  clang::CXXScopeSpec SS;
  clang::TemplateName TName(CTD);
  clang::Sema::TemplateTy TemplateTyName = clang::Sema::TemplateTy::make(TName);
  clang::IdentifierInfo *II = CTD->getIdentifier();
  clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
  clang::SourceLocation Loc = E->getBase()->getExprLoc();
  if (clang::VarTemplateDecl *VTD = dyn_cast<clang::VarTemplateDecl>(CTD)) {
    clang::DeclarationNameInfo DNI(VTD->getDeclName(), Loc);
    clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                          clang::Sema::LookupAnyName);
    R.addDecl(VTD);
    clang::ExprResult ER = SemaRef.getCxxSema().BuildTemplateIdExpr(
          SS, Loc, R, false, &TemplateArgs);
    if (ER.isInvalid())
      return nullptr;
    return ER.get();
  } else {
    clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
      SemaRef.getCurClangScope(), SS,
      // TemplateKWLoc
      Loc,
      TemplateTyName, II, Loc,
      //LAngleLoc
      Loc, InArgs,
      //RAngleLoc
      Loc, false, false);

    if (Result.isInvalid()) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_failed_to_translate_expr);
      return nullptr;
    }

    clang::QualType Ty(Result.get().get());
    clang::SourceRange Range(Loc, Loc);
    auto LocInfoTy = Ty->getAs<clang::LocInfoType>();
    if (SemaRef.getCxxSema().RequireCompleteType(Loc, LocInfoTy->getType(),
                                   clang::diag::err_incomplete_nested_name_spec,
                                                Range))
      return nullptr;
    return SemaRef.buildTypeExpr(LocInfoTy->getTypeSourceInfo());
  }
}


/// This takes the given arguments and attempts to transform them
/// into something that we could use for our template instantiation.
bool DependentExprTransformer::transformTemplateArguments(
    clang::CppxTemplateOrArrayExpr *E, clang::TemplateArgumentListInfo &ArgInfo,
    llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArgs) {

  for (auto A : E->arguments()) {
    clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
      SemaRef.getCxxSema(),
                  clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
    /*LambdaContextDecl=*/nullptr,
          clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);
    clang::Expr *TArg = transformDependentExpr(A);
    if (!TArg) {
      SemaRef.getCxxSema().Diags.Report(A->getExprLoc(),
                           clang::diag::err_failed_to_translate_expr);
      continue;
    }

    auto TemplateArg = SemaRef.convertExprToTemplateArg(TArg);
    if (TemplateArg.isInvalid())
      return true;

    ParsedArgs.emplace_back(TemplateArg);
    // Also building template Argument Info.
    if (TArg->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ArgTInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                  TArg, A->getExprLoc());
      if (!ArgTInfo)
        return true;
      clang::TemplateArgument Arg(ArgTInfo->getType());
      ArgInfo.addArgument({Arg, ArgTInfo});
    } else if (TArg->getType()->isTemplateType()) {
      clang::Decl *D = SemaRef.getDeclFromExpr(TArg, TArg->getExprLoc());
      if (!D)
        return true;

      clang::TemplateDecl *TD = cast<clang::TemplateDecl>(D);
      clang::TemplateName Template(TD);
      if (Template.isNull())
        return true;
      clang::TemplateArgument Arg(Template);
      clang::TemplateArgumentLocInfo TALoc(Context.CxxAST,
                                           clang::NestedNameSpecifierLoc(),
                                           TArg->getExprLoc(),
                                           clang::SourceLocation());
      ArgInfo.addArgument({Arg, TALoc});
    } else {
      clang::TemplateArgument Arg(TArg);
      ArgInfo.addArgument({Arg, TArg});
    }
  }
  return false;
}

clang::Expr *DependentExprTransformer::transformCppxLiteralType(
                                                    clang::CppxTypeLiteral *E) {
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(E,
                                                               E->getExprLoc());
  if (!TInfo)
    return nullptr;

  TInfo = SemaRef.getCxxSema().SubstType(TInfo, TemplateArgs, InstantiationLoc,
                                         EntityName);
  if (!TInfo)
    return nullptr;
  return SemaRef.buildTypeExpr(TInfo);
}


clang::QualType DependentExprTransformer::transformType(clang::QualType Ty) {
  if (isa<clang::SubstTemplateTypeParmType>(Ty)) {
    return transformSubstTemplateTypeParmType(
                              Ty->getAs<clang::SubstTemplateTypeParmType>());
  } else if (isa<clang::TemplateSpecializationType>(Ty)) {
    return transformTemplateSpecializationType(
                              Ty->getAs<clang::TemplateSpecializationType>());
  } else if (isa<clang::DeducedTemplateSpecializationType>(Ty)) {
    llvm_unreachable("DeducedTemplateSpecializationType not supported by blue.");
  } else if (isa<clang::SubstTemplateTypeParmPackType>(Ty)){
    llvm_unreachable("SubstTemplateTypeParmPackType not implemented yet.");
  } else if (isa<clang::TemplateTypeParmType>(Ty)) {
    llvm_unreachable("TemplateTypeParmType not implemented yet.");
  } else if (isa<clang::CppxTypeExprType>(Ty)) {
    return transformTypeExprType(Ty->getAs<clang::CppxTypeExprType>());
  }
  return Ty;
}

clang::QualType DependentExprTransformer::transformSubstTemplateTypeParmType(
                                const clang::SubstTemplateTypeParmType *STTPT) {
  return STTPT->getReplacementType();
}

clang::QualType DependentExprTransformer::transformTemplateSpecializationType(
               const clang::TemplateSpecializationType* TemplateSpecialization) {
  return TemplateSpecialization->desugar();
}

clang::QualType
DependentExprTransformer::transformTypeExprType(const clang::CppxTypeExprType *Ty) {
  assert(Ty->getTyExpr() && "Invalid type expression");
  clang::Expr *Ret = transformDependentExpr(Ty->getTyExpr());
  if (!Ret)
    // Returning an empty type because this was an error.
    return clang::QualType();

  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(Ret,
                                                           Ret->getExprLoc());
  if (!TInfo)
    return clang::QualType();
  TInfo = gold::BuildAnyTypeLoc(Context.CxxAST, TInfo->getType(), Ret->getExprLoc());
  if (!TInfo)
    return clang::QualType();

  if (Ty->isForConstruct()) {
    llvm_unreachable("Impossible type expression for call to construct.");
  }
  return TInfo->getType();
}

clang::Expr *DependentExprTransformer::buildDeclRefExpr(clang::VarDecl *VD,
                                                    clang::SourceLocation Loc) {
  return clang::DeclRefExpr::Create(Context.CxxAST,
                                    clang::NestedNameSpecifierLoc(),
                                    Loc, VD, /*Capture=*/false, Loc,
                                    VD->getType(), clang::VK_LValue);
}

clang::Expr *DependentExprTransformer::buildUnresolvedCall(clang::Expr *LHS,
                                                       clang::CXXRecordDecl *RD,
                                                 clang::LookupResult &Overloads,
                                                    clang::SourceLocation Loc,
                                                    bool UnresolvedUsing) {
  clang::CXXScopeSpec SS;
  clang::TypeSourceInfo *TInfo
     = gold::BuildAnyTypeLoc(Context.CxxAST, Context.CxxAST.getTypeDeclType(RD),
                           LHS->getExprLoc());
  SS.Extend(Context.CxxAST, clang::SourceLocation(), TInfo->getTypeLoc(),
            LHS->getExprLoc());
  auto NNSWithLoc = SS.getWithLocInContext(Context.CxxAST);
  if (LHS->getType()->isTypeOfTypes()) {
    clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
    if (!CurThisTy.isNull()) {
      clang::QualType CurrentClass = CurThisTy->getPointeeType();
      clang::QualType ThisPtrTy = SemaRef.getCxxSema().getCurrentThisType();
      if (ThisPtrTy.isNull()) {
          error(Loc) << "invalid this pointer type";
          return nullptr;
      }
      assert (!CurrentClass.isNull() && "invalid pointee type.");
      // clang::CXXScopeSpec SS;
      if (auto SSExpr = dyn_cast<clang::CppxCXXScopeSpecExpr>(LHS)) {
        SS = SSExpr->getScopeSpec();
      }
      auto ImpliciThisExpr = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
                                                            ThisPtrTy, true);
      if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, TInfo->getType())
          || CurrentClass == TInfo->getType()) {
        // if (isa<clang::UnresolvedLookupExpr>(E) || isa<clang::DeclRefExpr>(E)) {
          clang::UnqualifiedId UnqualId;
          clang::IdentifierInfo *MemberId = Overloads.getLookupName().getAsIdentifierInfo();
          UnqualId.setIdentifier(MemberId, Loc);
          clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::arrow;
          clang::ExprResult Access =
            SemaRef.getCxxSema().ActOnMemberAccessExpr(nullptr, ImpliciThisExpr,
              Loc, AccessTokenKind, SS, clang::SourceLocation(), UnqualId, nullptr);
          if (Access.isInvalid())
            return nullptr;

          return Access.get();
        // }
      }
    }
    // llvm_unreachable("Working on it.");
    return clang::UnresolvedLookupExpr::Create(
          Context.CxxAST, RD, NNSWithLoc,
          Overloads.getLookupNameInfo(), /*ADL=*/false,
          /*Overloaded*/true, Overloads.asUnresolvedSet().begin(),
          Overloads.asUnresolvedSet().end());
  }

  clang::TemplateArgumentListInfo TemplateArgs;
  return clang::UnresolvedMemberExpr::Create(Context.CxxAST,
      UnresolvedUsing, LHS, LHS->getType(), LHS->getType()->isPointerType(),
      Loc, NNSWithLoc, clang::SourceLocation(), clang::DeclarationNameInfo(),
      &TemplateArgs, Overloads.begin(), Overloads.end());
}

static clang::Expr *buildMemberExpr(Sema &SemaRef, clang::Expr *Base,
                                     clang::SourceLocation OpLoc,
                                     bool isArrow, clang::LookupResult &R) {
  clang::ExprResult BaseResult
          = SemaRef.getCxxSema().PerformMemberExprBaseConversion(Base, isArrow);

  if (!BaseResult.get())
    return nullptr;
  clang::CXXScopeSpec SS;

  Base = BaseResult.get();
  clang::QualType BaseType = Base->getType();
  assert(Base);
  return SemaRef.getCxxSema().BuildMemberReferenceExpr(Base, BaseType, OpLoc,
      isArrow, SS, clang::SourceLocation(), nullptr, R, nullptr,
      /*S*/nullptr).get();
}

clang::Expr *DependentExprTransformer::buildMemberAccessExpr(
    clang::Expr *LHS, clang::QualType Ty, clang::NamedDecl *Dcl,
    clang::CXXRecordDecl *RD, clang::LookupResult &Overloads,
    clang::SourceLocation Loc, bool IsArrow, bool UnresolvedUsing) {
  // If we are a type literal.
  if (isa<clang::CppxTypeLiteral>(LHS) || isa<clang::CppxCXXScopeSpecExpr>(LHS)) {
    auto *TInfo = SemaRef.getTypeSourceInfoFromExpr(LHS, LHS->getExprLoc());
    if (!TInfo)
      return nullptr;
    clang::CXXScopeSpec SS;
    if (auto SSE = dyn_cast<clang::CppxCXXScopeSpecExpr>(LHS)) {
      SS = SSE->getScopeSpec();
    }
    clang::IdentifierInfo *MemberId = Dcl->getIdentifier();
    // This is to handle type lookup.
    if (auto InnerTy = dyn_cast<clang::TypeDecl>(Dcl)) {
      return SemaRef.buildTypeExprFromTypeDecl(InnerTy, Loc);
    } else if (auto TemplateDcl = dyn_cast<clang::TemplateDecl>(Dcl)) {
        return SemaRef.buildTemplateType(TemplateDcl, Loc);
    } else if (auto FieldDcl = dyn_cast<clang::FieldDecl>(Dcl)) {
      // Checking to see if we are derived from the current thing or not.
      clang::SourceLocation Loc = LHS->getExprLoc();
      clang::QualType CurThisTy = SemaRef.getCxxSema().getCurrentThisType();
      if (!CurThisTy.isNull()) {

        clang::QualType CurrentClass = CurThisTy->getPointeeType();
        clang::QualType ThisPtrTy = SemaRef.getCxxSema().getCurrentThisType();
        if (ThisPtrTy.isNull()) {
          error(Loc) << "unable to get this expression from current context";
          return nullptr;
        }
        assert (!CurrentClass.isNull() && "invalid pointee type.");
        auto ImpliciThisExpr = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
                                                              ThisPtrTy, true);
        if (SemaRef.getCxxSema().IsDerivedFrom(Loc, CurrentClass, TInfo->getType())
            || CurrentClass == TInfo->getType()) {
          if (isa<clang::CXXMethodDecl>(Dcl) || isa<clang::FieldDecl>(Dcl)) {
            clang::UnqualifiedId UnqualId;
            UnqualId.setIdentifier(MemberId, Loc);
            clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::arrow;
            clang::ExprResult Access =
              SemaRef.getCxxSema().ActOnMemberAccessExpr(nullptr, ImpliciThisExpr,
                                                         Loc, AccessTokenKind, SS,
                                                         clang::SourceLocation(),
                                                          UnqualId, nullptr);
            if (Access.isInvalid())
              return nullptr;

            return Access.get();
          }
        }
      }
      clang::QualType ResultType = FieldDcl->getType();
      clang::ExprValueKind ValueKind = SemaRef.getCxxSema()
              .getValueKindForDeclReference(ResultType, FieldDcl, Loc);
      clang::DeclRefExpr *DRE =
        SemaRef.getCxxSema().BuildDeclRefExpr(FieldDcl, ResultType, ValueKind,
                                              Overloads.getLookupNameInfo(),
                                              SS.getWithLocInContext(SemaRef.getCxxAST()),
                                              FieldDcl, clang::SourceLocation(),
                                              nullptr);
      return DRE;

    } else if (auto VarDcl = dyn_cast<clang::VarDecl>(Dcl)) {
      return buildDeclRefExpr(VarDcl, Loc);

    } else if (isa<clang::CXXMethodDecl>(Dcl)) {
      return buildUnresolvedCall(LHS, RD, Overloads, Loc);

    } else if (auto ECD = dyn_cast<clang::EnumConstantDecl>(Dcl)) {
      clang::QualType ResultType = ECD->getType();
      clang::ExprValueKind ValueKind = SemaRef.getCxxSema()
                            .getValueKindForDeclReference(ResultType, ECD, Loc);
      clang::DeclRefExpr *DRE =
        SemaRef.getCxxSema().BuildDeclRefExpr(ECD, ResultType, ValueKind,
                                              Overloads.getLookupNameInfo(),
                                              clang::NestedNameSpecifierLoc(),
                                              ECD, clang::SourceLocation(),
                                              nullptr);
      return DRE;
    } else {
      Dcl->dump();
      llvm_unreachable("Unknown type of decl expression.");
    }
  } else {
    if (isa<clang::ValueDecl>(Dcl)) {
      return buildMemberExpr(SemaRef, LHS, Loc, IsArrow, Overloads);
    } else {
      Dcl->dump();
      // TODO: Create a valid error message for this
      llvm_unreachable("Cannot access this through . operator.");
    }
  }
}

clang::DiagnosticBuilder DependentExprTransformer::error(clang::SourceLocation Loc) {
  return SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_blue_elaboration);
}

} // end namespace gold.
