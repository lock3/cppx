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
    Ty = transformType(Ty);
    // if (isa<clang::SubstTemplateTypeParmType>(Ty)) {
    //   Ty = transformSubstTemplateTypeParmType(
    //                             Ty->getAs<clang::SubstTemplateTypeParmType>());
    // } else if (isa<clang::TemplateSpecializationType>(Ty)) {
    //   Ty = transformTemplateSpecializationType(
    //                             Ty->getAs<clang::TemplateSpecializationType>());
    // } else {
    //   Ty->dump();
    //   llvm_unreachable("Unhandled kind of dependent type expression");
    // }

    if (!isa<clang::TagType>(Ty)) {
      Ty->dump();
      llvm_unreachable("Emit error message because type is not a tag that "
                       "could use the member access expression.");
    }

    clang::TagDecl *TD = Ty->getAsTagDecl();
    // Retrieving nested name specifier from within type.
    if (E->getNameQualifierExpr()) {
      clang::Expr *QualifierResult
                            = transformDependentExpr(E->getNameQualifierExpr());
      if (QualifierResult) {
        if (auto TyLit = dyn_cast<clang::CppxTypeLiteral>(QualifierResult)) {
          auto *NextTD = TyLit->getValue()->getType()->getAsCXXRecordDecl();
          if (NextTD) {
            if (auto RootRD = Ty->getAsCXXRecordDecl()) {
              if (RootRD->isDerivedFrom(NextTD)) {
                TD = NextTD;
              } else {
                SemaRef.Diags.Report(TyLit->getExprLoc(),
                                 clang::diag::err_nested_namespecifier_not_base)
                                     << Ty << TyLit->getValue()->getType();
                return nullptr;
              }
            } else {
              SemaRef.Diags.Report(TyLit->getExprLoc(),
                              clang::diag::err_nested_namespecifier_not_a_class)
                                  << TyLit->getValue()->getType();
              return nullptr;
            }
          } else {
            SemaRef.Diags.Report(TyLit->getExprLoc(),
                                 clang::diag::err_invalid_type_for_name_spec)
                                 << TyLit->getValue()->getType();
            return nullptr;
          }
        } else {
          SemaRef.Diags.Report(TyLit->getExprLoc(),
                               clang::diag::err_not_a_type);
          return nullptr;
        }
      } else {
        // Emit an error message and just use the original tag declaration
        // or something like that?
        llvm_unreachable("Invalid declaration");
      }
    }
    auto Members = TD->lookup(E->getMember());
    if (Members.size() != 1) {
      if (Members.size() < 1) {
        Context.CxxAST.getTranslationUnitDecl()->dump();
        llvm_unreachable("Didn't find anything?\n");
      }
      llvm_unreachable("possible lookup failure? We may have multiple things here?");
    }
    if (auto InnerTy = dyn_cast<clang::TypeDecl>(Members[0])) {
      return SemaRef.buildTypeExprFromTypeDecl(InnerTy, E->getExprLoc());
    } else if (auto TemplateDecl = dyn_cast<clang::TemplateDecl>(Members[0])) {
      // return SemaRef.buildTypeExprFromTypeDecl(InnerTy, E->getExprLoc());
      llvm_unreachable("Template members not implemented yet.");
    } else if (auto MemberDecl = dyn_cast<clang::FieldDecl>(Members[0])) {
      Members[0]->dump();
      llvm_unreachable("Field declaration not handled yet.");
    } else if (auto VarDcl = dyn_cast<clang::VarDecl>(Members[0])) {
      return buildDeclRefExpr(VarDcl, Ret->getExprLoc());
    } else if (auto FnDecl = dyn_cast<clang::CXXMethodDecl>(Members[0])) {
      clang::DeclarationNameInfo TempDNI(E->getMember(), Ret->getExprLoc());
      return buildUnresolvedCall(Ret, cast<clang::CXXRecordDecl>(TD), Members,
                                 TempDNI, Ret->getExprLoc());
    } else {
      Members[0]->dump();
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


clang::QualType DependentExprTransformer::transformType(clang::QualType Ty) {
  if (isa<clang::SubstTemplateTypeParmType>(Ty)) {
    return transformSubstTemplateTypeParmType(
                              Ty->getAs<clang::SubstTemplateTypeParmType>());
  } else if (isa<clang::TemplateSpecializationType>(Ty)) {
    return transformTemplateSpecializationType(
                              Ty->getAs<clang::TemplateSpecializationType>());
  } else if (isa<clang::DeducedTemplateSpecializationType>(Ty)) {
    llvm_unreachable("DeducedTemplateSpecializationType not supported by gold.");
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
  TInfo = BuildAnyTypeLoc(Context.CxxAST, TInfo->getType(), Ret->getExprLoc());
  if (!TInfo)
    return clang::QualType();

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
                                   clang::DeclContext::lookup_result &Overloads,
                                          const clang::DeclarationNameInfo &DNI,
                                                    clang::SourceLocation Loc) {
  clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                        clang::Sema::LookupOrdinaryName);
  for(auto D : Overloads) {
    R.addDecl(D, D->getAccess());
  }
  if (LHS->getType()->isTypeOfTypes()) {
    return clang::UnresolvedLookupExpr::Create(
          Context.CxxAST, RD, clang::NestedNameSpecifierLoc(), DNI, /*ADL=*/false,
          /*Overloaded*/true, R.asUnresolvedSet().begin(),
          R.asUnresolvedSet().end());
    // clang::CXXScopeSpec SS;
    // bool IsArrow = false;
    // clang::QualType BaseType = ElaboratedLHS->getType();
    // if (ElaboratedLHS->getType()->isPointerType())
    //   IsArrow = true;
    // auto BaseExpr
    //     = SemaRef.getCxxSema().PerformMemberExprBaseConversion(ElaboratedLHS,
    //                                                            IsArrow);
    // if (BaseExpr.isInvalid())
    //   return nullptr;
    // clang::TemplateArgumentListInfo TemplateArgs;
    // return clang::UnresolvedMemberExpr::Create(Context.CxxAST, false,
    //                                            BaseExpr.get(), BaseType,
    //                                            IsArrow, Op->getLoc(),
    //                                            SS.getWithLocInContext(Context.CxxAST),
    //                                            clang::SourceLocation(),
    //                                            ULE->getNameInfo(),
    //                                            &TemplateArgs,
    //                                            ULE->decls_begin(), ULE->decls_end());
  }
  llvm_unreachable("Member access version of call expression not implemented yet.");
}
} // end namespace gold.