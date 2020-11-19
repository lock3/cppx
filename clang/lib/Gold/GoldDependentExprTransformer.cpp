#include "clang/Gold/GoldDependentExprTransformer.h"
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

namespace gold {

DependentExprTransformer::DependentExprTransformer(Sema &S, SyntaxContext &Ctx,
                           const clang::MultiLevelTemplateArgumentList &TemplateArgs,
                           clang::SourceLocation Loc,
                           clang::DeclarationName Entity)
  :SemaRef(S), Context(Ctx), TemplateArgs(TemplateArgs), InstantiationLoc(Loc),
  EntityName(Entity)
{ }
clang::Expr *DependentExprTransformer::transformDependentExpr(clang::Expr *E) {
  if (auto DMAE = dyn_cast<clang::CppxDependentMemberAccessExpr>(E)) {
    return transformCppxDependentMemberAccessExpr(DMAE);
  }
  if (auto LT = dyn_cast<clang::CppxTypeLiteral>(E)) {
    return transformCppxLiteralType(LT);
  }

  // This serves as a place holder for all of the Cppx things that I might
  // encounter but have yet to write code for.
  unsigned DiagID = SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "Unknown type of cppx dependent expression.");
  SemaRef.Diags.Report(clang::SourceLocation(), DiagID);
  return nullptr;
}

clang::Expr *DependentExprTransformer::transformCppxDependentMemberAccessExpr(
                                      clang::CppxDependentMemberAccessExpr *E) {
  if (E->isImplicitAccess())
    llvm_unreachable("Implicit access not handled yet?");

  clang::Expr *Ret = transformDependentExpr(E->getBase());
  if (!Ret) {
    SemaRef.Diags.Report(E->getExprLoc(),
                         clang::diag::err_invalid_dependent_expr);
    return nullptr;
  }

  if (Ret->getType()->isNamespaceType() || Ret->getType()->isTemplateType()) {
    // TODO: It might be best to split the || and figure out what the expression
    // is resulting in and display a corresponding error message.
    SemaRef.Diags.Report(E->getExprLoc(),
                         clang::diag::err_invalid_dependent_expr);
    return nullptr;
  }

  if (Ret->getType()->isTypeOfTypes()) {
    // TODO: Handle type expressions here.
    // llvm_unreachable("We have a type expression.");
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(Ret,
                                                             Ret->getExprLoc());
    if (!TInfo)
      return nullptr;

    clang::QualType Ty = TInfo->getType();
    if (isa<clang::SubstTemplateTypeParmType>(Ty)) {
      Ty = transformSubstTemplateTypeParmType(
                                 Ty->getAs<clang::SubstTemplateTypeParmType>());
    } else {
      llvm_unreachable("Unhandled type expression.");
    }

    if (!isa<clang::TagType>(Ty)) {
      llvm_unreachable("Emit error message because type is not a tag that "
                       "could use the member access expression.");
    }
    clang::TagDecl *TD = Ty->getAsTagDecl();
    auto Members = TD->lookup(E->getMember());
    if (Members.size() != 1) {
      llvm_unreachable("possible lookup failure? We may have multiple things here?");
    }
    if (auto InnerTy = dyn_cast<clang::TypeDecl>(Members[0])) {
      return SemaRef.buildTypeExprFromTypeDecl(InnerTy, E->getExprLoc());
    } else if (auto TemplateDecl = dyn_cast<clang::TemplateDecl>(Members[0])) {
      // return SemaRef.buildTypeExprFromTypeDecl(InnerTy, E->getExprLoc());
      llvm_unreachable("Template members not implemented yet.");
    } else if (auto MemberDecl = dyn_cast<clang::FieldDecl>(Members[0])) {
      llvm_unreachable("Field declaration not handled yet.");
    } else if (auto VarDcl = dyn_cast<clang::VarDecl>(Members[0])) {
      return buildDeclRefExpr(VarDcl, Ret->getExprLoc());
    } else {
      llvm_unreachable("We don't have code for hanlding things that are not "
                       "Type declarations yet.");
    }
  }
  // Handle the DeclRefExpr loading expression from here?
  Ret->dump();
  llvm_unreachable("We DON'T have a type expression.");
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

clang::QualType DependentExprTransformer::transformSubstTemplateTypeParmType(
                                const clang::SubstTemplateTypeParmType *STTPT) {
  return STTPT->getReplacementType();
}

clang::Expr *DependentExprTransformer::buildDeclRefExpr(clang::VarDecl *VD,
                                                    clang::SourceLocation Loc) {
  return clang::DeclRefExpr::Create(Context.CxxAST,
                                    clang::NestedNameSpecifierLoc(),
                                    Loc, VD, /*Capture=*/false, Loc,
                                    VD->getType(), clang::VK_LValue);
}

} // end namespace gold.