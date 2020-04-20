//=== GoldExprElaborator.cpp - Elaboration for Gold Expressions -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ExprElaborator interface, which creates
//  clang::Expr nodes out of gold expressions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/AST/ExprCppx.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringMap.h"
#include "clang/AST/OperationKinds.h"


#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldExprMarker.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldTokens.h"


#include <cstring>

namespace gold {

using TypeInfo = ExprElaborator::TypeInfo;
using Expression = ExprElaborator::Expression;

ExprElaborator::ExprElaborator(SyntaxContext &Context, Sema &SemaRef,
      clang::DeclContext *DC, gold::Scope *GoldScope)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
  CurrentLookUpContext(DC), OwningScope(GoldScope)
{ }

Expression ExprElaborator::elaborateExpr(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S), clang::QualType());
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if (isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));
  if (isa<ElemSyntax>(S))
    return elaborateElementExpr(cast<ElemSyntax>(S));
  assert(false && "Unsupported expression.");
}

static clang::IntegerLiteral *
createIntegerLiteral(clang::ASTContext &CxxAST, Token T, clang::QualType IntType,
                     clang::SourceLocation Loc) {
  llvm::APInt Value;
  unsigned Width = 0;

  // If we don't have a specified type, just create a default int.
  if (IntType.isNull() || IntType == CxxAST.AutoDeductTy)
    IntType = CxxAST.IntTy;

  // TODO: support all kinds of integer types.
  if (IntType == CxxAST.IntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    long long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.ShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.UnsignedShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    unsigned short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedIntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    unsigned int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else {
    assert(false && "Unsupported integer type.");
  }

  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);

  return clang::IntegerLiteral::Create(CxxAST, Value, IntType, Loc);
}

// TODO: Refactor into this location.
static clang::TypeSourceInfo*
HandleClassTemplateSelection(ExprElaborator& Elab, Sema &SemaRef,
    SyntaxContext& Context, clang::TypeSourceInfo* IdExpr, const ElemSyntax *Elem) {
  clang::CXXScopeSpec SS;
  clang::Sema::TemplateTy Template;
  clang::UnqualifiedId TemplateName;
  clang::ParsedType ObjectType;
  // FIXME: Handling non-nested elements.
  if (!isa<AtomSyntax>(Elem->getObject()))
    llvm_unreachable("Nested/qualified name access to template syntax "
        "not implemented yet.");
  const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Elem->getObject());
  clang::IdentifierInfo &II = Context.CxxAST.Idents.get(Atom->getSpelling());
  TemplateName.setIdentifier(&II, Atom->getLoc());
  bool MemberOfUnknownSpecialization = false;
  
  if (clang::TemplateNameKind TNK = SemaRef.getCxxSema().isTemplateName(
      SemaRef.getCurClangScope(), SS, /*hasTemplateKeyword=*/false,
      TemplateName, ObjectType, /*EnteringContext*/false, Template,
      MemberOfUnknownSpecialization)) {

    llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

    const ListSyntax *ElemArgs = cast<ListSyntax>(Elem->getArguments());
    for(const Syntax *SyntaxArg : ElemArgs->children()) {
      clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
        SemaRef.getCxxSema(),
        clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
        /*LambdaContextDecl=*/nullptr,
        /*ExprContext=*/
        clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);

      // TODO: Attempt to process this initially as a template template
      // parameter and see if it fails or not, if it fails then it's not a
      // template template parameter.
      Expression ArgExpr = Elab.elaborateExpr(SyntaxArg);
      if (ArgExpr.isNull()) 
        return nullptr;
      
      if (ArgExpr.is<clang::NamespaceDecl *>()) {
        // FIXME: Figure out the correct error message to display here.
        // Basically we need to say that a namespace is not a type.
        llvm::errs() << "Unable to a namespace name for a template argument\n";
        return nullptr;
      }

      if (ArgExpr.is<clang::TypeSourceInfo *>()) {
        // TODO: Figure out how to handle template template parameters here?
        // Because currently that's impossible.
        auto *SrcInfo = ArgExpr.get<clang::TypeSourceInfo *>();
        ParsedArguments.emplace_back(
          SemaRef.getCxxSema().ActOnTemplateTypeArgument(
            SemaRef.getCxxSema().CreateParsedType(SrcInfo->getType(), SrcInfo)));
      }
      
      if (ArgExpr.is<clang::Expr *>()) {
        // Leveraging constant expression evaluation from clang's sema class.
        clang::ExprResult ConstExpr(ArgExpr.get<clang::Expr*>());
        ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(ConstExpr);
        ParsedArguments.emplace_back(clang::ParsedTemplateArgument::NonType,
            ConstExpr.get(), SyntaxArg->getLoc());
      }
    }
    switch(TNK) {
    case clang::TemplateNameKind::TNK_Concept_template:{
      llvm_unreachable("TNK_Concept_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Dependent_template_name:{
      llvm_unreachable("TNK_Dependent_template_name has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Function_template:{
      llvm_unreachable("TNK_Function_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Non_template:{
      // TODO: It might be best to emit something here indicating that we
      // found something that wasn't a templat that is being used as a template
      // But within the function that I got this from clang doesn't emit an error
      // they simply return false instead.
      //
      // This was taken from the function Parser::ParseUnqualifiedIdTemplateId
      // in ParseExprCXX.cpp line:2281
      return nullptr;
    }
    case clang::TemplateNameKind::TNK_Type_template:{
      clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
      clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
        SemaRef.getCurClangScope(), SS,
        /*TemplateKWLoc*/ clang::SourceLocation(), Template, &II, Atom->getLoc(),
        /*LAngleLoc*/ clang::SourceLocation(), InArgs,
        /*RAngleLoc*/ clang::SourceLocation(), false, false);
      if (Result.isInvalid()) {
        // TODO: Figure out correct error message this.
        llvm::errs() << "We hae an invalid result ?!\n";
        llvm_unreachable("We have an invlaid result for ActOnTemplateIdType.");
      }
      clang::QualType Ty(Result.get().get());
      const clang::LocInfoType *TL = cast<clang::LocInfoType>(Ty.getTypePtr());
      clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST,
          TL->getType(), Atom->getLoc());
      return TInfo;
    }
    case clang::TemplateNameKind::TNK_Undeclared_template:{
      llvm_unreachable("TNK_Undeclared_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Var_template:{
      llvm_unreachable("TNK_Var_template has not been implemented yet.");
      break;
    }
    }
  }
  return nullptr;
}

Expression ExprElaborator::elaborateElementExpr(const ElemSyntax *Elem) {
  Expression IdExpr = elaborateExpr(Elem->getObject());
  if (IdExpr.is<clang::NamespaceDecl *>()) {
    llvm_unreachable("Nested namespace access to template syntax not implemented yet.");
    return nullptr;
  }
  if (IdExpr.is<clang::TypeSourceInfo *>()) {
    return HandleClassTemplateSelection(*this, SemaRef, Context,
      IdExpr.get<clang::TypeSourceInfo*>(), Elem);
  }

  if (IdExpr.is<clang::Expr *>()) {
    llvm::errs() << "Elaboration of array indexing is not implemented yet\n";
    return nullptr;
  }

  llvm_unreachable("Unable to handle indexing into given expression within the AST.");
}

static ExprElaborator::Expression
createIdentAccess(SyntaxContext &Context, Sema &SemaRef, const AtomSyntax *S,
                  clang::QualType Ty, clang::SourceLocation Loc) {
  // Attempting to classify identifiers?
  // classifyName()
  // TODO: We need to refactor this to do multiple types of look up.
  // For example, we need to refactor the way functions, and type names
  // are handled. This is so that we can appropriately handle lookup of names
  // in multiple contexts. This will also need to be refactored in order to
  // correctly handle things like getting names for candidate sets.

  // Step one. Look up the name within the current context, to see if it exists.
  clang::ASTContext &CxxAST = Context.CxxAST;
  
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(S->getSpelling())}, Loc);
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());
  if (!R.empty()) {
    if (!R.isSingleResult()) {
      // This needs to be changed because we are literally looking up a
      // multitude of things, and this is only an error in some of the cases,
      // for example if we a set of function overloads then this isn't going to
      // work correctly and we may need to simply return access to a function
      // address rather then something else?
      SemaRef.Diags.Report(S->getLoc(), clang::diag::err_multiple_declarations);
      return nullptr;
    }


    if(clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>()) {
      clang::QualType FoundTy = VD->getType();
      VD->setIsUsed();

      // If the user annotated the DeclRefExpr with an incorrect type.
      if (!Ty.isNull() && Ty != FoundTy) {
        SemaRef.Diags.Report(Loc, clang::diag::err_type_annotation_mismatch)
          << FoundTy << Ty;
        return nullptr;
      }

      if (isa<clang::FieldDecl>(VD)) {
        // Building this access.
        clang::FieldDecl* Field = cast<clang::FieldDecl>(VD);
        clang::RecordDecl* RD = Field->getParent();
        // FIXME: Add CV qualifiers here if needed
        clang::QualType ThisTy(RD->getTypeForDecl(), 0);
        clang::QualType ThisPtrTy = SemaRef.getContext().CxxAST.getPointerType(ThisTy);
        clang::Expr* This = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
            ThisPtrTy, true);
        clang::DeclAccessPair FoundDecl = clang::DeclAccessPair::make(Field,
              clang::AccessSpecifier::AS_public);
        clang::CXXScopeSpec SS;
        clang::ExprResult MemberExpr
            = SemaRef.getCxxSema().BuildFieldReferenceExpr(
              This, true, clang::SourceLocation(), SS, Field, FoundDecl, DNI
            );
        clang::Expr *Ret = MemberExpr.get();
        if (!Ret) {
          SemaRef.Diags.Report(Loc, clang::diag::err_no_member)
              << Field << ThisTy;
        }
        return Ret;
      }
      // Need to check if the result is a CXXMethodDecl because that's a
      // ValueDecl.
      if(isa<clang::CXXMethodDecl>(VD)) {
        llvm_unreachable("We haven't implemented processing of CXXMethodDecls "
            "yet.");
      }

      if(isa<clang::FunctionDecl>(VD)) {
        llvm_unreachable("We haven't implemented processing of function names yet "
            "yet.");
      }



      // Checking if the current declaration is a variable.
      // FIXME: discern whether this is an lvalue or rvalue properly
      clang::DeclRefExpr *DRE =
        clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                  clang::SourceLocation(), VD, /*Capture=*/false,
                                  Loc, FoundTy, clang::VK_LValue);
      return DRE;
    }
    

    // Processing the case when the returned result is a type.
    if (const clang::TagDecl *TD = R.getAsSingle<clang::TagDecl>()) {
      return BuildAnyTypeLoc(CxxAST, CxxAST.getTypeDeclType(TD), Loc);
    }
    // llvm::outs() << "This isn't implemented yet!?\n";
    // llvm_unreachable("Unhandled expression type.");
  }
  return nullptr;
}

Expression ExprElaborator::elaborateAtom(const AtomSyntax *S,
                                         clang::QualType ExplicitType) {
  Token T = S->Tok;

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(CxxAST, T, ExplicitType, S->getTokenLoc());
  case tok::DecimalFloat:
    break;
  case tok::BinaryInteger:
    break;
  case tok::HexadecimalInteger:
    break;
  case tok::HexadecimalFloat:
    break;
  case tok::Identifier:
    return createIdentAccess(Context, SemaRef, S, ExplicitType, S->getLoc());
  case tok::Character:
    break;
  case tok::String:
    break;

  /// Keyword Literals

  case tok::IntKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.IntTy, S->getLoc());
  case tok::VoidKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.VoidTy, S->getLoc());
  case tok::BoolKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.BoolTy, S->getLoc());
  case tok::CharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CharTy, S->getLoc());
  case tok::Wchar_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.WCharTy, S->getLoc());
  case tok::Wint_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.WIntTy, S->getLoc());
  case tok::Char8_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char8Ty, S->getLoc());
  case tok::Char16_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char16Ty, S->getLoc());
  case tok::Char32_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Char32Ty, S->getLoc());
  case tok::SignedCharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.SignedCharTy, S->getLoc());
  case tok::ShortKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.ShortTy, S->getLoc());
  case tok::LongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongTy, S->getLoc());
  case tok::LongLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongLongTy, S->getLoc());
  case tok::Int128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Int128Ty, S->getLoc());
  case tok::UnsignedCharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedCharTy, S->getLoc());
  case tok::UnsignedShortKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedShortTy, S->getLoc());
  case tok::UnsignedKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedIntTy, S->getLoc());
  case tok::UnsignedLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedLongTy, S->getLoc());
  case tok::UnsignedLongLongKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedLongLongTy, S->getLoc());
  case tok::Uint128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.UnsignedInt128Ty, S->getLoc());
  case tok::FloatKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.FloatTy, S->getLoc());
  case tok::DoubleKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.DoubleTy, S->getLoc());
  case tok::LongDoubleKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.LongDoubleTy, S->getLoc());
  case tok::Float128_tKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.Float128Ty, S->getLoc());
  case tok::TypeKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CppxKindTy, S->getLoc());

  default: break;
  }

  return nullptr;
}

// Mapping of Gold's fused operator strings to clang Opcodes.
static const llvm::StringMap<clang::BinaryOperatorKind> BinaryOperators = {
  {"operator'+'" , clang::BO_Add},
  {"operator'-'" , clang::BO_Sub},
  {"operator'*'" , clang::BO_Mul},
  {"operator'/'" , clang::BO_Div},
  {"operator'%'" , clang::BO_Rem},
  {"operator'&'" , clang::BO_And},
  {"operator'|'" , clang::BO_Or},
  {"operator'^'" , clang::BO_Xor},
  {"operator'&&'" , clang::BO_LAnd},
  {"operator'||'" , clang::BO_LOr},
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

Expression ExprElaborator::elaborateCall(const CallSyntax *S) {
  if (isa<ElemSyntax>(S->getCallee()))
    return elaborateElemCall(S);
  // I may need to work on this a bit in order to make sure that everything
  // still works as expected when it comes to constructing functions.
  if(isa<CallSyntax>(S->getCallee())) {
    // This will need to be done recursively, because we will create the member
    // access or scope lookup and on the way out we need to actually create the
    // call here with the returned decl, this could be a function decl, or a
    // member function call, or a qualifying name expression.

    const CallSyntax *InnerCall = cast<CallSyntax>(S->getCallee());
    Expression Expr = elaborateCall(InnerCall);

    if (Expr.isNull()) {
      // FIXME: Need to add diagnostics here.
      return nullptr;
    }
    clang::Expr *E = Expr.get<clang::Expr*>();
    // Getting arguments for current function call.
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
    for (const Syntax *A : ArgList->children()) {
      ExprElaborator Elab(Context, SemaRef);
      Expression Argument = Elab.elaborateExpr(A);

      // FIXME: What kind of expression is the unary ':typename' expression?
      if (Argument.is<clang::TypeSourceInfo *>()) {
        SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
        return nullptr;
      }
      Args.push_back(Argument.get<clang::Expr *>());
    }
    clang::ExprResult Call =
      SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                        E, S->getCalleeLoc(),
                                        Args, S->getCalleeLoc());
    return Call.get();
  }
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  FusedOpKind Op = getFusedOpKind(SemaRef, Callee->getSpelling());

  // a fused operator':' call
  if (Op == FOK_Colon) {
    Elaborator Elab(SemaRef.getContext(), SemaRef);

    // If the LHS of the operator':' call is just a name, we can try to
    // reference or create it.
    if (isa<AtomSyntax>(S->getArgument(0))) {
      // FIXME: replace this with a normal type elaboration
      clang::QualType T = Elab.getOperatorColonType(S);
      return elaborateAtom(cast<AtomSyntax>(S->getArgument(0)), T);
    }

    // Otherwise, we need to continue elaborating the LHS until it is an atom.
    elaborateExpr(S->getArgument(0));
    return nullptr;
  }

  if (Op == FOK_MemberAccess) {
    // Need to locate do variable/type lookup.
    const ListSyntax *Args =  cast<ListSyntax>(S->getArguments());
    return elaborateMemberAccess(Args->getChild(0), S, Args->getChild(1));
  }

  llvm::StringRef Spelling = Callee->getSpelling();

  // Check if this is a binary operator.
  auto BinOpMapIter = BinaryOperators.find(Spelling);
  if (BinOpMapIter != BinaryOperators.end()) {
    return elaborateBinOp(S, BinOpMapIter->second);
  }

  // Try to construct a normal function-call expression.
  // First do unqualified lookup.
  clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Spelling)}, S->getLoc());
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
    // FIXME: Figure out how to correctly output the diagnostic here.
    llvm::errs() << "Failed to locate given name: \n";
    return nullptr;
  }

  // Parsing all arguments because in most cases this needs to be done first.
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  for (const Syntax *A : ArgList->children()) {
    ExprElaborator Elab(Context, SemaRef);
    Expression Argument = Elab.elaborateExpr(A);

    // FIXME: What kind of expression is the unary ':typename' expression?
    if (Argument.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
      return nullptr;
    }
    Args.push_back(Argument.get<clang::Expr *>());
  }

  // If we found something, see if it is viable.
  if (!R.empty()) {
    clang::Expr *Fn = nullptr;

    R.resolveKind();
    if (R.isOverloadedResult()) {
      Fn =
        clang::UnresolvedLookupExpr::Create(CxxAST, R.getNamingClass(),
                                            clang::NestedNameSpecifierLoc(),
                                            R.getLookupNameInfo(), /*ADL=*/true,
                                            /*Overloaded=*/true, R.begin(),
                                            R.end());
    } else if (R.isSingleResult()) {
      
      clang::Decl *Decl = R.getAsSingle<clang::Decl>();

      if (isa<clang::ValueDecl>(Decl)) { 
        clang::ValueDecl *VD = dyn_cast<clang::ValueDecl>(Decl);
        // This had better be a reference to a function.
        clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(VD);
        if (!FD)
          return nullptr;

        Fn =
          clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                    clang::SourceLocation(), VD, /*Capture=*/false,
                                    S->getLoc(), VD->getType(), clang::VK_RValue);
      } else if (isa<clang::CXXRecordDecl>(Decl)) {
        clang::CXXRecordDecl *Record = cast<clang::CXXRecordDecl>(Decl);
        clang::QualType Ty = Context.CxxAST.getTypeDeclType(Record);
        clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST, Ty,
            Callee->getLoc());
        clang::ExprResult ConstructorExpr =
          SemaRef.getCxxSema().BuildCXXTypeConstructExpr(TInfo, S->getLoc(),
                                                      Args, S->getLoc(), false);
        if (!ConstructorExpr.get()) {
          SemaRef.Diags.Report(S->getLoc(),
                               clang::diag::err_coroutine_invalid_func_context)
                               << Ty << "a constructor";
          return nullptr;
        }
        return ConstructorExpr.get();
      }

      if (!Fn)
        return nullptr;

      // Create the call.
      clang::ExprResult Call =
        SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                          Fn, S->getCalleeLoc(),
                                          Args, S->getCalleeLoc());
      if (Call.isInvalid()) {
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_failed_to_translate_expr);
        return nullptr;
      }
      return Call.get();
    }

  } else {
    // This handles the special case of a built in type constructor
    // call/implicit cast.
    auto BuiltInIter = SemaRef.BuiltinTypes.find(Spelling);
    if (BuiltInIter == SemaRef.BuiltinTypes.end()) {
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_unknown_typename) << Spelling;
      return nullptr;
    }
    clang::ParsedType PT = clang::ParsedType::make(BuiltInIter->second);
    clang::ExprResult ConstructorExpr =
      SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(PT, S->getLoc(), Args,
                                                     S->getLoc(), false);
    return ConstructorExpr.get();
  }
  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

Expression ExprElaborator::elaborateMemberAccess(const Syntax *LHS,
    const CallSyntax *Op, const Syntax *RHS) {
Expression ElaboratedLHS = elaborateExpr(LHS);
  if(ElaboratedLHS.is<clang::Expr*>()) {
    if (isa<AtomSyntax>(RHS)) {
      const AtomSyntax *RHSAtom = cast<AtomSyntax>(RHS);
      // TODO: figure out how to make the pointer work correctly?

      clang::UnqualifiedId Id;
      clang::IdentifierInfo *IdInfo = &Context.CxxAST.Idents.get(
        RHSAtom->getSpelling());

      // TODO: Figure out how to get the desired scope.
      Id.setIdentifier(IdInfo, RHSAtom->getLoc());
      clang::CXXScopeSpec SS;
      clang::SourceLocation Loc;
      clang::ExprResult HandledLHS = SemaRef.getCxxSema().ActOnMemberAccessExpr(
        SemaRef.getCurClangScope(), ElaboratedLHS.get<clang::Expr*>(), Op->getLoc(),
        clang::tok::TokenKind::period, SS, Loc, Id, nullptr);
      if (HandledLHS.get()) {
        
        if (isa<clang::MemberExpr>(HandledLHS.get())) {
          clang::MemberExpr *MemberExpression
            = cast<clang::MemberExpr>(HandledLHS.get());
          MemberExpression->getMemberDecl()->setIsUsed();
        }
      } else {
        llvm::outs() << "We were not able to elaborate the member access expression.\n";
      }
      return HandledLHS.get();
    }
    llvm_unreachable("Currently unable to handle member access from non-variables.");
  } 
  if (ElaboratedLHS.is<clang::TypeSourceInfo*>()) {
    return elaborateNestedLookUpAccess(ElaboratedLHS, Op, RHS);
  }

  llvm_unreachable("Member access to anything other then a member variable "
      "not implemented yet.");
}



static ExprElaborator::Expression handleLookUpInsideType(Sema &SemaRef,
    clang::ASTContext &CxxAST, Expression Previous,
    const CallSyntax *Op, const Syntax *RHS) {
  clang::TypeSourceInfo *TInfo = Previous.get<clang::TypeSourceInfo*>();
  clang::QualType QT = TInfo->getType();
  const clang::Type *T = QT.getTypePtrOrNull();
  clang::TagDecl *TD = T->getAsTagDecl();
  if (!TD) {
    // TODO: Figure out the appropriate diagnostic message to output here.
    // SemaRef.Diags.Report(LHS->getLoc(), clang::diag::err_no_member)
    //     << TD << Op.Loc;
    llvm::errs() << "Type " << TD->getNameAsString()
                 << " doesn't have any members.";
    return nullptr;
  }

  // Processing if is a single name.
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(RHS)) {
    // clang::DeclarationName 
    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())},
      Atom->getLoc());
    auto R = TD->lookup(DNI.getName());
    if (R.size() != 1u) {
      SemaRef.Diags.Report(RHS->getLoc(), clang::diag::err_no_member)
        << Atom->getSpelling() << TD;
      return nullptr;
    }
    // R.front();
    clang::NamedDecl *ND = R.front();
    if (clang::TypeDecl *TD = dyn_cast<clang::TypeDecl>(ND)) {
      clang::QualType Ty = CxxAST.getTypeDeclType(TD);
      return BuildAnyTypeLoc(CxxAST, Ty, RHS->getLoc());
    }
    if (clang::NamespaceDecl *NsDecl = dyn_cast<clang::NamespaceDecl>(ND)) {
      return NsDecl;
    }

    // FIXME: This needs to support referencing base members by qualified name.
    llvm_unreachable("Direct referencing of member variables it not permitted yet.");
  }

  llvm_unreachable("Unknown syntax encountered during nested member lookup.");
}

Expression ExprElaborator::elaborateNestedLookUpAccess(Expression Previous,
                                                       const CallSyntax *Op,
                                                       const Syntax *RHS) {
  assert(!Previous.isNull() && "Expression scoping.");
  if (Previous.is<clang::TypeSourceInfo*>()) {
    return handleLookUpInsideType(SemaRef, Context.CxxAST, Previous, Op, RHS);
  }

  if (Previous.is<clang::NamespaceDecl*>()) {
    llvm_unreachable("Nested namespace declarations not implemented");
  }

  if (Previous.is<clang::Expr *>()) {
    // assert(!"Nested access to static variables it no implemented yet.");
    llvm_unreachable("Nested access to static variables not implemented");
  }

  llvm_unreachable("Expression type not an expression, type, or namespace");
}



Expression ExprElaborator::elaborateElemCall(const CallSyntax *S) {
  const ElemSyntax *Callee = cast<ElemSyntax>(S->getCallee());

  Expression IdResult = elaborateExpr(Callee->getObject());


  // Build the template argument list.
  clang::TemplateArgumentListInfo TemplateArgs(Callee->getLoc(), Callee->getLoc());
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  for (const Syntax *SS : Callee->getArguments()->children()) {
    ExprElaborator ParamElaborator(Context, SemaRef);
    Expression ParamExpression = ParamElaborator.elaborateExpr(SS);
    if (ParamExpression.isNull())
      return nullptr;

    if (ParamExpression.is<clang::TypeSourceInfo *>()) {
      auto *TypeParam = ParamExpression.get<clang::TypeSourceInfo *>();
      clang::TemplateArgument Arg(TypeParam->getType());
      TemplateArgs.addArgument({Arg, TypeParam});
      ActualArgs.emplace_back(Arg);
    } else {
      clang::TemplateArgument Arg(ParamExpression.get<clang::Expr *>(),
                                  clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ParamExpression.get<clang::Expr *>()});
      ActualArgs.emplace_back(Arg);
    }
  }
  clang::TemplateArgumentList TemplateArgList(
      clang::TemplateArgumentList::OnStack, ActualArgs);
  clang::Expr *Fn = nullptr;
  // Attempting to correctly handle the result of an Id expression.
  if (clang::Expr *IdExpr = IdResult.dyn_cast<clang::Expr *>()) {
    if (clang::OverloadExpr *OverloadExpr
                               = dyn_cast<clang::OverloadExpr>(IdExpr)) {
      if (OverloadExpr->getNumDecls() == 1) {
        clang::NamedDecl *ND = *OverloadExpr->decls_begin();
        
        // We will need to in the future figure out how to correctly handle this?
        if (isa<clang::TemplateDecl>(ND)) {
          
          // We need to instantiate the template with parameters.
          if (clang::UnresolvedMemberExpr *MemAccess
                  = dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
            clang::FunctionTemplateDecl *FTD
                                    = dyn_cast<clang::FunctionTemplateDecl>(ND);
            clang::FunctionDecl *FD
                = SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
                  &TemplateArgList, S->getLoc());
            if (!FD) {
              llvm_unreachable("Function template instantiation failure.");
            }
            Fn = clang::MemberExpr::Create(Context.CxxAST,
                MemAccess->getBase(), MemAccess->isArrow(),
                MemAccess->getOperatorLoc(), MemAccess->getQualifierLoc(),
                clang::SourceLocation(), FD,
                clang::DeclAccessPair::make(FD, ND->getAccess()),
                MemAccess->getMemberNameInfo(), &TemplateArgs, IdExpr->getType(),
                MemAccess->getValueKind(), MemAccess->getObjectKind(),
                clang::NonOdrUseReason::NOUR_None);
            if (Fn) {
              // TODO: Create an error message to put here so I can indicate lookup
              // failure maybe?
              return nullptr;
            }
          } else {
            llvm_unreachable("We don't have code for processing of non-member "
                "lookup expressions.");
          }
        }
      } else {
        llvm_unreachable("Resolution of multiple declarations isn't "
            "implemented yet.");
      }
    }
  }
  // FIXME: this can be anything
  // const AtomSyntax *Id = cast<AtomSyntax>(Callee->getObject());

  // // Try to construct a normal function-call expression.
  // // First do unqualified lookup.
  // clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Id->getSpelling())},
  //   S->getLoc());
  // clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  // R.setTemplateNameLookup(true);
  // SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  // if (R.empty())
  //   return nullptr;


  // Build the ULE if we found something.
  
  // R.resolveKind();
  // if (R.isOverloadedResult()) {
  //   Fn =
  //     clang::UnresolvedLookupExpr::Create(CxxAST, R.getNamingClass(),
  //                                         clang::NestedNameSpecifierLoc(),
  //                                       Callee->getLoc(), R.getLookupNameInfo(),
  //                              /*ADL=*/true, &TemplateArgs, R.begin(), R.end());
  // } else {
  //   llvm_unreachable("Non-overloaded template call?");
  // }
  // llvm_unreachable("What did we do here?!");
  // Get the passed arguments.
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  assert(ArgList && "Unexpected argument format.");
  for (const Syntax *A : ArgList->children()) {
    ExprElaborator Elab(Context, SemaRef);
    Expression Argument = Elab.elaborateExpr(A);

    // FIXME: What kind of expression is the unary ':typename' expression?
    if (Argument.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
      return nullptr;
    }

    Args.push_back(Argument.get<clang::Expr *>());
  }

  // Create the call.
  clang::MultiExprArg MultiArgs(Args);
  clang::ExprResult Call =
    SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                       Fn, S->getCalleeLoc(),
                                       MultiArgs, S->getCalleeLoc());
  if (Call.isInvalid()) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  return Call.get();
}

Expression ExprElaborator::elaborateBinOp(const CallSyntax *S,
                                          clang::BinaryOperatorKind Op) {
  const Syntax *LHSSyntax = S->getArgument(0);
  const Syntax *RHSSyntax = S->getArgument(1);

  Expression LHS = elaborateExpr(LHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>() || LHS.isNull()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression RHS = elaborateExpr(RHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>() || RHS.isNull()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  // FIXME: Replace with ActOnBinOp so precedence issues get warnings.
  clang::ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr,
                                               S->getLoc(), Op,
                                               LHS.get<clang::Expr *>(),
                                               RHS.get<clang::Expr *>());
  if (Res.isInvalid()) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  ExprMarker(Context.CxxAST, SemaRef).Visit(LHS.get<clang::Expr *>());
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHS.get<clang::Expr *>());

  return Res.get();
}

/// Create an expression for a block condition. Ex:
///
/// \code
/// if:
///   expr_1
///   expr_2
///   ...
///   expr_n
/// \endcode
/// We just create a logical and expression with n terms: one for each
/// sub expression.
Expression
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  Expression LHS, RHS;

  {
    ExprElaborator ExEl(Context, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));

    if (LHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(1));

    if (RHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(1)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }

  clang::ExprResult BinOp =
    SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                    clang::tok::ampamp,
                                    LHS.get<clang::Expr *>(),
                                    RHS.get<clang::Expr *>());
  if (BinOp.isInvalid()) {
    SemaRef.Diags.Report(Conditions->getLoc(),
                         clang::diag::err_invalid_block_condition);
    return nullptr;
  }

  // For all remaining terms, append them to the back of the && expression.
  // Ex., if we had `1 && 2`, we would append `3` to get `1 && 2 && 3`.
  for (unsigned I = 2; I < Conditions->getNumChildren(); ++I) {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(I));

    BinOp =
      SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                      clang::tok::ampamp, BinOp.get(),
                                      RHS.get<clang::Expr *>());
    if (BinOp.isInvalid()) {
      SemaRef.Diags.Report(Conditions->getLoc(),
                           clang::diag::err_invalid_block_condition);
      return nullptr;
    }
  }

  return BinOp.get();
}

static clang::Expr *handleArrayMacro(SyntaxContext &Context, Sema &SemaRef,
                                     const MacroSyntax *S) {
  const ArraySyntax *ArrayInit = cast<ArraySyntax>(S->getBlock());
  const ListSyntax *Init = cast<ListSyntax>(ArrayInit->getChild(0));

  llvm::SmallVector<clang::Expr *, 8> Elements;
  for (const Syntax *SI :  Init->children()) {
    Expression Element = ExprElaborator(Context, SemaRef).elaborateExpr(SI);

    if (Element.is<clang::TypeSourceInfo *>() || Element.isNull())
      return nullptr;

    Elements.push_back(Element.get<clang::Expr *>());
  }

  clang::ExprResult InitList =
    SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Elements,
                                       S->getLoc());
  if (InitList.isInvalid())
    return nullptr;

  return InitList.get();
}

Expression ExprElaborator::elaborateMacro(const MacroSyntax *S) {
  assert (isa<AtomSyntax>(S->getCall()) && "Unexpected macro call");

  const AtomSyntax *Call = cast<AtomSyntax>(S->getCall());

  if (Call->getSpelling() == "if")
    assert(false && "If expression processing not implemented yet.");
  else if (Call->getSpelling() == "while")
    assert(false && "while loop processing not implemented yet.");
  else if(Call->getSpelling() == "for")
    assert(false && "For loop processing not implemented yet.");
  else if (Call->getSpelling() == "array")
    return handleArrayMacro(Context, SemaRef, S);
  else
    // FIXME: Need to handle any other conditions here.
    assert(false && "Unsupported macro");
}




//===----------------------------------------------------------------------===//
//                        Type Expression Elaboration                         //
//===----------------------------------------------------------------------===//

// Get a vector of declarators.
static void getDeclarators(Declarator *D,
                           llvm::SmallVectorImpl<Declarator *> &Decls) {
  while (D) {
    Decls.push_back(D);
    D = D->Next;
  }
}

Expression ExprElaborator::elaborateTypeExpr(Declarator *D) {
  // The type of a declarator is constructed back-to-front.
  llvm::SmallVector<Declarator *, 4> Decls;
  getDeclarators(D, Decls);

  // The type is computed from back to front. Start by assuming the type
  // is auto. This will be replaced if an explicit type specifier is given.
  clang::QualType AutoType = CxxAST.getAutoDeductType();
  TypeInfo *TInfo = BuildAnyTypeLoc(CxxAST, AutoType, D->getLoc());
  for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
    D = *Iter;
    switch (D->Kind) {
    case DK_Identifier:
      // The identifier is not part of the type.
      break;

    case DK_Pointer: {
      Expression TypeExpr = elaboratePointerType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Array: {
      Expression TypeExpr = elaborateArrayType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Function: {
      Expression TypeExpr = elaborateFunctionType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Type: {
      Expression TypeExpr = elaborateExplicitType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }
    case DK_TemplateType:{
      llvm_unreachable("I'm not sure exactly how this works but I'll figure it "
                       "out!");
      break;
    }
    default:
      llvm_unreachable("Invalid declarator");
    }
  }
  return TInfo;
}

Expression ExprElaborator::elaboratePointerType(Declarator *D, TypeInfo *Ty) {
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (BaseTypeExpr.is<clang::Expr *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType = BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::QualType PtrType = CxxAST.getPointerType(BaseType);

  return BuildAnyTypeLoc(CxxAST, PtrType, D->getType()->getLoc());
}

Expression ExprElaborator::elaborateArrayType(Declarator *D, TypeInfo *Ty) {
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (BaseTypeExpr.is<clang::Expr *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  Expression IndexExpr =
    ExprElaborator(Context, SemaRef).elaborateExpr(D->Data.Index);

  // FIXME: what do we do for an empty array index, such as []int = {...}
  if (IndexExpr.is<clang::TypeSourceInfo *>() || IndexExpr.isNull()) {
    SemaRef.Diags.Report(D->Data.Index->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType =
    BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::Expr *Index = IndexExpr.get<clang::Expr *>();

  clang::Expr::EvalResult IdxResult;
  clang::Expr::EvalContext
    EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

  if (!Index->EvaluateAsConstantExpr(IdxResult, clang::Expr::EvaluateForCodeGen,
                                     EvalCtx))
    return nullptr;

  clang::QualType ArrayType =
    Context.CxxAST.getConstantArrayType(BaseType, IdxResult.Val.getInt(), Index,
                                        clang::ArrayType::Normal, 0);
  return BuildAnyTypeLoc(CxxAST, ArrayType, D->getType()->getLoc());
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
Expression ExprElaborator::elaborateFunctionType(Declarator *D, TypeInfo *Ty) {
  const auto *Call = cast<CallSyntax>(D->Call);

  // FIXME: Handle array-based arguments.
  assert(isa<ListSyntax>(D->Data.ParamInfo.Params)
         && "Array parameters not supported");
  const Syntax *Args = D->Data.ParamInfo.Params;

  // Elaborate the parameter declarations in order to get their types, and save
  // the resulting scope with the declarator.
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  SemaRef.enterScope(SK_Parameter, Call);
  for (const Syntax *P : Args->children()) {
    Elaborator Elab(Context, SemaRef);
    clang::ValueDecl *VD =
      cast_or_null<clang::ValueDecl>(Elab.elaborateDeclSyntax(P));
    if (!VD)
      return nullptr;

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");

    assert(isa<clang::ParmVarDecl>(VD) && "Parameter is not a ParmVarDecl");

    Types.push_back(VD->getType());
    Params.push_back(cast<clang::ParmVarDecl>(VD));
  }
  D->Data.ParamInfo.ConstructedScope = SemaRef.saveScope(Call);

  // FIXME: We probably need to configure parts of the prototype (e.g.,
  // make this noexcept by default).
  clang::FunctionProtoType::ExtProtoInfo EPI;

  using clang::SourceLocation;
  using clang::SourceRange;

  clang::QualType FnTy = CxxAST.getFunctionType(Ty->getType(), Types, EPI);
  return BuildFunctionTypeLoc(CxxAST, FnTy,
    SourceLocation(), SourceLocation(), SourceLocation(),
    SourceRange(), SourceLocation(), Params);
}



Expression ExprElaborator::elaborateExplicitType(Declarator *D, TypeInfo *Ty) {
  assert(isa<clang::AutoType>(Ty->getType()));
  assert(D->Kind == DK_Type);
  
  // FIXME: We should really elaborate the entire type expression. We're
  // just cheating for now.
  if (const auto *Atom = dyn_cast<AtomSyntax>(D->Data.Type)) {
    clang::SourceLocation Loc = Atom->getLoc();

    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())}, Loc);
    clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupTagName);
    if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())){
      return nullptr;
    }

    if (R.empty()) {
      auto BuiltinMapIter = SemaRef.BuiltinTypes.find(Atom->getSpelling());
      if (BuiltinMapIter == SemaRef.BuiltinTypes.end())
        return nullptr;
      
      return BuildAnyTypeLoc(CxxAST, BuiltinMapIter->second, Loc);
    }

    clang::TypeDecl *TD = R.getAsSingle<clang::TypeDecl>();
    clang::QualType TDType(TD->getTypeForDecl(), 0);
    return BuildAnyTypeLoc(CxxAST, TDType, Loc);
  }
  
  return elaborateExpr(D->Data.Type);
  // Elaborating the member access syntax from a call.
  // FIXME: In the future this may need to be expanded to include meta functions.
  // if (const CallSyntax *Call = dyn_cast<CallSyntax>(D->Data.Type)) {
  // }
}

void dumpExpression(ExprElaborator::Expression Expr, llvm::raw_ostream& Out) {
  if (Expr.isNull()) {
    Out << "[Null Expr]\n";
    return;
  }

  if (Expr.is<clang::Expr *>()) {
    llvm::outs() << "Type = clang::Expr *\n";
    Expr.get<clang::Expr *>()->dump(Out);
    llvm::outs() << "\n";
    return;
  }

  if (Expr.is<clang::TypeSourceInfo *>()) {
    llvm::outs() << "Type = clang::TypeSourceInfo *\n";
    Expr.get<clang::TypeSourceInfo *>()->getType().dump(Out);
    llvm::outs() << "\n";
    return;
  }

  if (Expr.is<clang::NamespaceDecl *>()) {
    llvm::outs() << "Type = clang::NamespaceDecl *\n";
    Expr.get<clang::NamespaceDecl *>()->dump(Out);
    llvm::outs() << "\n";
    return;
  }

  // if (Expr.is<clang::TemplateName *>()) {
  //   llvm::outs() << "Type = clang::TemplateName *\n";
  //   Expr.get<clang::TemplateName *>()->dump(Out);
  //   llvm::outs() << "\n";
  //   return;
  // }
  llvm::outs() << "[Unknown Expression type]\n";
}
} // namespace gold
