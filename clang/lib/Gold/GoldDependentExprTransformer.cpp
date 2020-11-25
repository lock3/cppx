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
  return SemaRef.getCxxSema().SubstExpr(E, TemplateArgs).get();
}

static clang::TagDecl *handleNestedNameQualifier(DependentExprTransformer &T,
    Sema &SemaRef, clang::CppxDependentMemberAccessExpr *E,
    clang::QualType Ty, clang::TagDecl *TD) {
  if (E->getNameQualifierExpr()) {
    clang::Expr *QualifierResult
                          = T.transformDependentExpr(E->getNameQualifierExpr());
    if (QualifierResult) {
      if (auto TyLit = dyn_cast<clang::CppxTypeLiteral>(QualifierResult)) {
        auto *NextTD = TyLit->getValue()->getType()->getAsCXXRecordDecl();
        if (NextTD) {
          if (auto RootRD = Ty->getAsCXXRecordDecl()) {
            if (RootRD->isDerivedFrom(NextTD)) {
              return NextTD;
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
  return TD;
}

clang::Expr *DependentExprTransformer::transformCppxDependentMemberAccessExpr(
                                      clang::CppxDependentMemberAccessExpr *E) {
  // llvm::outs() << "Called DMAE for = " << E->getMember().getAsString() << "\n";
  if (E->isImplicitAccess()) {
    E->dump();
    // if (!E->getBase()) {
    //   llvm::outs() << "We are missing the base expression!?\n";
    // } else {
    //   llvm::outs() << "We have the base expression: = \n";
    //   E->getBase()->dump();
    // }
    // Context.CxxAST.getTranslationUnitDecl()->dump();
    llvm_unreachable("Implicit access not handled yet?");
  }

  clang::Expr *Ret = transformDependentExpr(E->getBase());
  if (!Ret) {
    SemaRef.Diags.Report(E->getBase()->getExprLoc(),
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

  clang::QualType Ty;
  bool NeedsArrow = false;
  if (Ret->getType()->isTypeOfTypes()) {
    // TODO: Handle type expressions here.
    // llvm_unreachable("We have a type expression.");
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(Ret,
                                                             Ret->getExprLoc());
    if (!TInfo)
      return nullptr;

    Ty = TInfo->getType();
    Ty = transformType(Ty);
  } else {
    Ty = Ret->getType();
    Ty = transformType(Ty);
    if (Ty.isNull())
      return nullptr;

    if (Ty->isPointerType()){
      Ty = Ty->getPointeeType();
      NeedsArrow = true;
    }
    // Doing a 2ndary transformation of the inner type of a pointer because this
    // may be the actual dependent type.
    Ty = transformType(Ty);
    if (Ty.isNull())
      return nullptr;
  }

  // Handling errors from pointer type.
  if (Ty.isNull()) {
    // FIXME: This needs an error message.
    llvm_unreachable("Invalid type");
  }

  if (!isa<clang::TagType>(Ty)) {
    // FIXME: This needs an error message.
    Ty->dump();
    llvm_unreachable("Emit error message because type is not a tag that "
                     "could use the member access expression.");
  }

  clang::TagDecl *TD = Ty->getAsTagDecl();
  if (!TD) {
    llvm_unreachable("failed to get type as tag?!\n");
  }
  clang::TagDecl *NextTD = handleNestedNameQualifier(*this, SemaRef, E, Ty,
                                                     TD);
  if (!NextTD) {
    return nullptr;
  }

  if (!Ret->getType()->isTypeOfTypes()) {
    clang::CXXScopeSpec SS;
    clang::SourceLocation Loc;
    clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
    if (NeedsArrow)
      AccessTokenKind = clang::tok::TokenKind::arrow;

    // clang::UnqualifiedId Id;
    // Id.setIdentifier(E->getMember().getAsIdentifierInfo(), E->getExprLoc());
    bool MayBePseudoDestructor = false;
    clang::ParsedType ObjectTy;
    Ret = SemaRef.getCxxSema().ActOnStartCXXMemberReference(nullptr, Ret, Loc,
                                                            AccessTokenKind,
                                                            ObjectTy,
                                                   MayBePseudoDestructor).get();
    if (!Ret)
      llvm_unreachable("Invalid member access expression?\n");
  }

  TD = NextTD;

  auto Members = TD->lookup(E->getMember());
  clang::DeclarationNameInfo DNI(E->getMember(), Ret->getExprLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                        clang::Sema::LookupOrdinaryName);
  for(auto D : Members) {
    R.addDecl(D, D->getAccess());
  }

  R.resolveKind();
  switch (R.getResultKind()) {
  case clang::LookupResult::FoundOverloaded: {
    llvm_unreachable("Overloaded function candidate not implemented.");
  }
  break;
  case clang::LookupResult::Found:{
    return buildMemberAccessExpr(Ret, Ty, R.getFoundDecl(),
                                 cast<clang::CXXRecordDecl>(TD),
                                 R, Ret->getExprLoc(), NeedsArrow);
  }
  break;

  case clang::LookupResult::NotFound:
    SemaRef.Diags.Report(Ret->getExprLoc(),
                          clang::diag::err_no_member)
                          << DNI.getName().getAsString() << Ty;
    return nullptr;
  case clang::LookupResult::NotFoundInCurrentInstantiation: {
    llvm_unreachable("I'm not sure what's going on here.");
    break;
  }
  case clang::LookupResult::FoundUnresolvedValue:
    llvm_unreachable("Working on FoundUnresolvedValue.");
    break;

  case clang::LookupResult::Ambiguous:
    llvm_unreachable("Ambigious?");
    // SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    return nullptr;
  }

  // TODO: Implement the non-inside type functionality.
  // Handle the DeclRefExpr loading expression from here?
  // Ret->dump();
  llvm_unreachable("shouldn't have reached here!.");

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
                                                 clang::LookupResult &Overloads,
                                                    clang::SourceLocation Loc) {
  if (LHS->getType()->isTypeOfTypes()) {
    return clang::UnresolvedLookupExpr::Create(
          Context.CxxAST, RD, clang::NestedNameSpecifierLoc(),
          Overloads.getLookupNameInfo(), /*ADL=*/false,
          /*Overloaded*/true, Overloads.asUnresolvedSet().begin(),
          Overloads.asUnresolvedSet().end());
  }
  llvm_unreachable("Member access version of call expression not implemented yet.");
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
    clang::SourceLocation Loc, bool IsArrow) {

  // If we are a type literal.
  if (isa<clang::CppxTypeLiteral>(LHS)) {
    // This is to handle type lookup.
    if (auto InnerTy = dyn_cast<clang::TypeDecl>(Dcl)) {
      return SemaRef.buildTypeExprFromTypeDecl(InnerTy, Loc);
    } else if (isa<clang::TemplateDecl>(Dcl)) {
      // return SemaRef.buildTypeExprFromTypeDecl(InnerTy, E->getExprLoc());
      llvm_unreachable("Template members not implemented yet.");
    } else if (isa<clang::FieldDecl>(Dcl)) {
      Dcl->dump();
      llvm_unreachable("Field declaration not handled yet.");
    } else if (auto VarDcl = dyn_cast<clang::VarDecl>(Dcl)) {
      return buildDeclRefExpr(VarDcl, Loc);
    } else if (isa<clang::CXXMethodDecl>(Dcl)) {
      return buildUnresolvedCall(LHS, RD, Overloads, Loc);
    } else {
      Dcl->dump();
      // TODO: Create a valid error message for this
      llvm_unreachable("We don't have code for hanlding things that are not "
                        "Type declarations yet.");
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

} // end namespace gold.