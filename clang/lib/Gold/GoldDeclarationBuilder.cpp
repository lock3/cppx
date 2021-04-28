//===- GoldDeclarationBuilder.cpp -----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
// Implementation for the declaration builder.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldDeclarationBuilder.h"
#include "clang/Gold/GoldDeclaratorBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"


#include "clang/Sema/Lookup.h"

namespace gold {

static bool determineTagKind(const AtomSyntax *Name, Declaration *D);
static bool isTagLikeDeclOrForwardDecl(Sema &SemaRef, Declaration *TheDecl,
                                       bool &EncounteredError);
DeclarationBuilder::DeclarationBuilder(Sema &S)
  :DeclarationBuilder(S.getContext(), S)
{ }

Declaration *DeclarationBuilder::build(const Syntax *S) {
  gold::Scope *CurrentScope = SemaRef.getCurrentScope();
  gold::Declarator *Dcl = nullptr;
  DeclaratorBuilder BuildDeclarator(Context, SemaRef, *this);

  Dcl = BuildDeclarator(S);
  if (!Dcl)
    return nullptr;

  // Recording additional attributes associated with the name.
  // This is to handle the special case of the . member access operator
  // picking up the attributes from a nested name declaration with attributes.
  for(const Syntax *Attr : Attrs)
    BuildDeclarator.Name->recordAttribute(Attr);

  Declaration *ParentDecl = SemaRef.getCurrentDecl();
  // FIXME: manage memory
  Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, InitExpr);
  TheDecl->Id = BuildDeclarator.Id;
  TheDecl->OpInfo = BuildDeclarator.OpInfo;
  TheDecl->InitOpUsed = InitOperatorUsed;

  // Getting information that's necessary in order to correctly restore
  // a declaration's context during early elaboration.
  TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  TheDecl->ScopeForDecl = SemaRef.getCurrentScope();
  if (checkDeclaration(S, TheDecl))
    return nullptr;

  assert(!(OpInfo && !TheDecl->declaresFunction()) && "unimplemented operator");
  Scope *CurScope = SemaRef.getCurrentScope();

  // If we're in namespace or parameter scope and this identifier already
  // exists, consider it a redeclaration.
  // TODO: distinguish between redefinition, redeclaration, and redeclaration
  // with different type.
  if ((CurScope->isNamespaceScope() || CurScope->isParameterScope()) &&
      !TheDecl->declaresFunction() && !TheDecl->SpecializationArgs && Id) {
    auto DeclSet = CurScope->findDecl(Id);

    if (!DeclSet.empty() && DeclSet.size() != 1u) {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "redefinition of identifier %0");
      SemaRef.Diags.Report(TheDecl->getEndOfDecl(), DiagID) << Id;
      TheDecl->setPreviousDecl(*DeclSet.begin());
    }
  }

  SemaRef.getCurrentScope()->addDecl(TheDecl);
  TheDecl->CurrentPhase = Phase::Identification;
  return TheDecl;
}


/// start := complexNameSpecifier TemplateTemplateSpecializationNameOrCall declaredType? '=' initExpr ;
///
/// complexNameSpecifier := globalNameSpecifier? (NameSpecifier '.')* ;
/// NameSpecifier := TemplateTemplateSpecializationOrName
///
/// globalNameSpecifier := '.'
///     DK_GlobalNamespecifier - new
///
/// declaredType := ':' (UndeducedTypeExpr | 'namespace' | 'type') ;
///   DK_Type
///
/// // This is only to tell what type this actually is
/// initExpr := (('class' | 'enum') '(' expr (',' expr)* ')' ) | 'namespace'| 'union' | UnresolvedExpr ;
///   Still not really part of declarator (but maybe it should be, at least in some cases).
///   DK_Init - new, this is going to be used to denote the thing on the RHS of the = if one exists.
///
/// TemplateTemplateSpecializationOrName := TemplateDecl | TemplateSpecialization | Name ;
///
/// TemplateDecl := Name '[' TemplateParamDecls ']' ;
///   DK_TemplateParams
///
/// TemplateSpecialization := PartialSpecialization | ExplicitSpecialization ;
///
/// PartialSpecialization := TemplateDecl '[' TemplateArgs ']' ;
///   DK_PartialSpecialization - new
///
/// ExplicitSpecialization := Name '[' TemplateArgs ']'
///   DK_ExplicitSpecialization - new
///
/// TemplateTemplateSpecializationNameOrCall := TemplateTemplateSpecializationOrName | call ;
///
/// call := TemplateTemplateSpecializationOrName '(' ParameterDeclarations ')'
///   DK_Function
///
/// name := identifier ; Identified based on context.
///   DK_Name
///   DK_NameSpecifier - new
///
/// The sequence of possible declarator combinations will be
///   DK_GlobalNamespecifier?
///   (DK_NameSpecifier ( DK_ExplicitSpecialization | (DK_TemplateParams (DK_PartialSpecialization)?)) )*
///   DK_Name (DK_ExplicitSpecialization | (DK_TemplateParams (DK_PartialSpecialization)?) )?
///   DK_Function?
///   DK_Type?
bool DeclarationBuilder::verifyDeclaratorChain(const Syntax *DeclExpr,
                                               Declaration *TheDecl) {
  Declarator *Dcl = TheDecl->Decl;
  assert(Dcl && "Invalid declarator");
  Declarator *Cur = Dcl;

  auto ReportMissing = [&](int ErrKind) -> bool {
    if (Cur == nullptr) {
      // Some how the name for this was a dot.
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(DeclExpr->getLoc(),
                            clang::diag::err_expected_declarator_chain_sequence)
                            << ErrKind;
      return true;
    }
    return false;
  };

  if (Cur->isUsingDirective())
    return false;

  if (Cur->isGlobalNameSpecifier()) {
    TheDecl->GlobalNsSpecifier = Cur->getAsGlobalNameSpecifier();
    Cur = Cur->Next;
    if (ReportMissing(2))
      return true;
  }

  // Checking for Nested Name Specifier
  while(Cur->isNestedNameSpecifier()) {
    TheDecl->NNSInfo.emplace_back();
    TheDecl->NNSInfo.back().Name = Cur->getAsNestedNameSpecifier();
    Cur = Cur->Next;
    if (ReportMissing(2))
      return true;
    if (Cur->isTemplateParameters()) {
      TheDecl->NNSInfo.back().Template = Cur->getAsTemplateParams();
      Cur = Cur->Next;
      if (ReportMissing(2))
        return true;
    }
    if (Cur->isSpecialization()) {
      TheDecl->NNSInfo.back().SpecializationArgs = Cur->getAsSpecialization();
      Cur = Cur->Next;
      if (ReportMissing(2))
        return true;
    }
  }

  // We expect the regular identifier at this point.
  if (Cur->isIdentifier()) {
    TheDecl->IdDcl = Cur->getAsIdentifier();
    Cur = Cur->Next;
  } else {
    if (RequiresDeclOrError)
      SemaRef.Diags.Report(DeclExpr->getLoc(),
                          clang::diag::err_expected_declarator_chain_sequence)
                          << 2;
    return true;
  }

  // Jump to the very end and make sure that we can properly do deduction.
  if (Cur == nullptr)
    return false;

  // Checking for template arguments, then specializations
  if (Cur->isTemplateParameters()) {
    TheDecl->Template = Cur->getAsTemplateParams();
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  if (Cur->isSpecialization()) {
    TheDecl->SpecializationArgs = Cur->getAsSpecialization();
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  while (Cur->isArray() || Cur->isPointer()) {
    clang::SourceLocation Loc = Cur->getLoc();
    Cur = Cur->Next;
    if (!Cur) {
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(Loc,
                             clang::diag::err_invalid_declaration);
      return true;
    }
  }

  if (Cur->isFunction()) {
    TheDecl->FunctionDcl = Cur->getAsFunction();
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  while (Cur->isArray() || Cur->isPointer()) {
    clang::SourceLocation Loc = Cur->getLoc();
    Cur = Cur->Next;
    if (!Cur) {
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(Loc,
                             clang::diag::err_invalid_declaration);
      return true;
    }
  }

  // Every declaration will end with an optional type. If there is another
  // declarator chunk after this, the declaration is ill-formed.
  if (Cur->isType()) {
    TheDecl->TypeDcl = Cur->getAsType();
    Cur = Cur->Next;
  }
  if (Cur != nullptr) {
    if (RequiresDeclOrError) {
      if (ConversionTypeSyntax)
        SemaRef.Diags.Report(Cur->getLoc(),
                         clang::diag::err_conversion_operator_with_return_type);
      else
        SemaRef.Diags.Report(Cur->getLoc(),
                             clang::diag::err_invalid_declaration);
    }
    return true;
  }

  return false;
}
static bool checkNamespaceDecl(Sema &SemaRef, const Syntax *DeclExpr,
                               Declaration *TheDecl) {
  if (!TheDecl->declaresNamespace())
    return false;

  if (TheDecl->GlobalNsSpecifier) {
    SemaRef.Diags.Report(TheDecl->GlobalNsSpecifier->getLoc(),
                         clang::diag::err_invalid_declaration)
                         << /*global name specifier*/2;
    return true;
  }
  if (TheDecl->SpecializationArgs) {
    SemaRef.Diags.Report(TheDecl->SpecializationArgs->getLoc(),
                         clang::diag::err_invalid_declaration)
                         << /*specialization arguments*/1;
    return true;
  }
  if (TheDecl->Template) {
    SemaRef.Diags.Report(TheDecl->Template->getLoc(),
                         clang::diag::err_invalid_declaration)
                         << /*template parameters*/0;
    return true;
  }

  if (!TheDecl->NNSInfo.empty())
    for (const auto &DInfo : TheDecl->NNSInfo) {
      if (DInfo.SpecializationArgs) {
        SemaRef.Diags.Report(DInfo.SpecializationArgs->getLoc(),
                             clang::diag::err_invalid_declaration)
                             << /*specialization arguments*/1;
        return true;
      }
      if (DInfo.Template) {
        SemaRef.Diags.Report(DInfo.Template->getLoc(),
                             clang::diag::err_invalid_declaration)
                             << /*template parameters*/0;
        return true;
      }
    }

  return false;
}

bool DeclarationBuilder::checkDeclaration(const Syntax *DeclExpr,
                                          Declaration *TheDecl) {
  // This attempts to verify that we have a valid chain of declarators.
  // and the conform to a specific order.
  if (verifyDeclaratorChain(DeclExpr, TheDecl))
    return true;

  // One thing to check is that we need to make sure that we aren't an
  // expression from a previous Scope.
  // Attempting to verify lookup.

  // Doing simple block lookup.
  clang::IdentifierInfo* Id = TheDecl->getId();
  Scope *CurScope = SemaRef.getCurrentScope();
  if (CurScope->isBlockScope()) {
    // If we're assigning to a name that already exist in the current block,
    // then we're not declaring anything. For example:
    // \code
    //    x = 3
    //    x = 4
    // \endcode
    // The first statement is a declaration. The second is an assignment.
    // FIXME: is this the right way to handle the lookup set?
    if (InitOperatorUsed == IK_Equals && !CurScope->findDecl(Id).empty())
      return true;
  }

  if (IsInsideEnum)
    if (checkEnumDeclaration(DeclExpr, TheDecl))
      return true;

  // Additional checks taking place here.
/*
  err_invalid_declarator_sequence
  0 - function declaration
  1 - namespace
  2 - template
  3 - nested name specifier
  4 - global name specifier
  5 - class
  6 - enum
  7 - union
  8 - template specialization
  9 - type
*/
  if (!EnableFunctions && TheDecl->FunctionDcl) {
    // This is not a declatation.
    if (RequiresDeclOrError)
      SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                           clang::diag::err_invalid_declarator_sequence)
                           << 0;

    return true;
  }

  if (checkNestedNameSpecifiers(DeclExpr, TheDecl))
    return true;

  if (classifyDecl(DeclExpr, TheDecl))
    return true;

  if (checkNamespaceDecl(SemaRef, DeclExpr, TheDecl))
    return true;

  if (checkRequiresType(DeclExpr, TheDecl))
    return true;

  return false;
}


bool DeclarationBuilder::checkEnumDeclaration(const Syntax *DeclExpr,
                                              Declaration *TheDecl) {
  TheDecl->SuspectedKind = UDK_EnumConstant;
  bool EncounteredError = false;
  if (isTagLikeDeclOrForwardDecl(SemaRef, TheDecl, EncounteredError)) {
    SemaRef.Diags.Report(DeclExpr->getLoc(),
                         clang::diag::err_invalid_declaration);
    return true;
    // return false;
  }
  // Because we are inside of an enum we are 100% sure that this is an error.
  if (TheDecl->GlobalNsSpecifier) {
    SemaRef.Diags.Report(TheDecl->GlobalNsSpecifier->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 4;
    return true;
  }

  if (!TheDecl->NNSInfo.empty()) {
    SemaRef.Diags.Report(TheDecl->NNSInfo.front().Name->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 3;
    return true;
  }

  if (TheDecl->FunctionDcl) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 0;
    return true;
  }

  if (TheDecl->TypeDcl) {
    SemaRef.Diags.Report(TheDecl->TypeDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 9;
    return true;
  }

  if (TheDecl->Template) {
    SemaRef.Diags.Report(TheDecl->Template->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 2;
    return true;
  }

  if (TheDecl->SpecializationArgs) {
    SemaRef.Diags.Report(TheDecl->SpecializationArgs->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 8;
    return true;
  }

  // TODO: It may be necessary to specifically include a test for TheDecl->Init
  if (TheDecl->Op)
    if (const auto *Call = dyn_cast<CallSyntax>(TheDecl->Op))
      FusedOpKind Op = getFusedOpKind(SemaRef, Call);
      bool AttributeOp = Op == FOK_Postattr || Op == FOK_Preattr;

      if ((InitOperatorUsed != IK_Equals) && !AttributeOp) {
        // This means we are not using the assignment operator, but we are using
        // something else like operator'!'. Indicating we cannot be an
        // enumeration declaration, and we are for some reason using function
        // decl syntax in this context.
        SemaRef.Diags.Report(Call->getCallee()->getLoc(),
                             clang::diag::err_invalid_declarator_sequence)
                             << 0;
        return true;
      }
  return false;
}

bool DeclarationBuilder::checkNestedNameSpecifiers(const Syntax *DeclExpr,
                                                   Declaration *TheDecl) {
  if (!EnableNestedNameSpecifiers) {
    if (!TheDecl->NNSInfo.empty()) {
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                            clang::diag::err_invalid_declarator_sequence)
                            << 3;
      return true;
    }
    if (TheDecl->GlobalNsSpecifier) {
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                            clang::diag::err_invalid_declarator_sequence)
                            << 4;
      return true;
    }
  }
  return false;
}

bool DeclarationBuilder::checkRequiresType(const Syntax *DeclExpr,
                                           Declaration *TheDecl) {
  switch (TheDecl->SuspectedKind) {
  default:
    // Default behavior is that we don't require anything.
    break;

  // In order to be correctly identified as an alias before type deduction
  // we have to have an explicit type.
  case UDK_NamespaceAlias:
  case UDK_TemplateAlias:
  case UDK_TypeAlias:
    // In order to identify any of these as aliases we must have already found
    // their type.
    break;
  // These are the ones that are subject to possible chang based on context.
  case UDK_VarTemplateOrTemplateAlias:
  case UDK_VarTemplateDecl:{
    if (RequireAliasTypes) {
      if (!TheDecl->TypeDcl) {
        if (RequiresDeclOrError)
          SemaRef.Diags.Report(DeclExpr->getLoc(),
                            clang::diag::err_expected_declarator_chain_sequence)
                              << 4;
        return true;
      }
    }
    break;
  }

  case UDK_Parameter:
  case UDK_TemplateParam:
  case UDK_TemplateTemplateParam: {
    if (RequireTypeForVariable) {
      if (!TheDecl->TypeDcl) {
        if (RequiresDeclOrError)
          SemaRef.Diags.Report(DeclExpr->getLoc(),
                               clang::diag::err_parameter_with_no_type);
        return true;
      }
    }
    break;
  }
  // We need to check each of these for a t
  case UDK_Function:
  case UDK_MemberFunction:
  case UDK_Constructor:
  case UDK_Destructor:
  case UDK_ConversionOperator:
  case UDK_MemberOperator:
  case UDK_LiteralOperator:
  case UDK_OperatorOverload:
  case UDK_PossibleConstructor:
  case UDK_PossibleDestructor:
  case UDK_PossibleMemberOperator:
  case UDK_PossibleConversionOperator: {
    if (RequireTypeForFunctions) {
      if (!TheDecl->TypeDcl) {
        if (RequiresDeclOrError)
          SemaRef.Diags.Report(DeclExpr->getLoc(),
                               clang::diag::err_function_with_no_type);
        return true;
      }
    }
    break;
  }
  // Not sure if this is possible or not based on context.
  case UDK_DeductionOnlyVariable:
    if (RequireTypeForVariable) {
      if (!TheDecl->TypeDcl) {
        if (RequiresDeclOrError)
          SemaRef.Diags.Report(DeclExpr->getLoc(),
                               clang::diag::err_variable_must_have_type);
        return true;
      }
    }
  }

  return false;
}


bool determineTagKind(const AtomSyntax *Name, Declaration *D) {
  if (Name->hasToken(tok::ClassKeyword)) {
    D->SuspectedKind = UDK_Class;
  } else if (Name->hasToken(tok::UnionKeyword)) {
    D->SuspectedKind = UDK_Union;
  } else if (Name->hasToken(tok::EnumKeyword)) {
    D->SuspectedKind = UDK_Enum;
  } else if (Name->hasToken(tok::NamespaceKeyword)) {
    D->SuspectedKind = UDK_Namespace;
  } else {
    // TODO: May need to put an error message here?
    return true;
  }
  return false;
}

// Returns false if this wasn't a tag like decl
bool isTagLikeDeclOrForwardDecl(Sema &SemaRef, Declaration *TheDecl,
                                bool &EncounteredError) {
  EncounteredError = false;
  if (!TheDecl->Init)
    return false;

  if (const auto *Name = dyn_cast<AtomSyntax>(TheDecl->Init)) {
    if (!determineTagKind(Name, TheDecl)) {
      if (TheDecl->SuspectedKind == UDK_Namespace) {
        EncounteredError = true;
        SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                             clang::diag::err_namespace_missing_body);
        return false;
      }
      TheDecl->IsDeclOnly = true;
      return true;
    }
  }

  if (const auto *Call = dyn_cast<CallSyntax>(TheDecl->Init)) {
    if (const auto *Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
      if (!determineTagKind(Name, TheDecl)) {
        if (TheDecl->SuspectedKind != UDK_Enum) {
          // This may need an error message?
          // that's because the only forward declaration that's valid with a call
          // is enum.
          // This has to be a declaration.
          unsigned ErrorIndicator = 0;
          switch(TheDecl->SuspectedKind) {
            case UDK_Namespace:
              ErrorIndicator = 2;
              break;
            case UDK_Class:
              ErrorIndicator = 1;
              break;
            case UDK_Union:
              ErrorIndicator = 0;
              break;
            default:
            llvm_unreachable("Invalid suspected kind.");
          }
          EncounteredError = true;
          SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                               clang::diag::err_invalid_macro_decl)
                               << ErrorIndicator;
          return true;
        }
        TheDecl->IsDeclOnly = true;
        return true;
      }
    }
  }

  if (const auto *Ms = dyn_cast<MacroSyntax>(TheDecl->Init)) {
    if (const auto *Name = dyn_cast<AtomSyntax>(Ms->getCall())) {
      if (!determineTagKind(Name, TheDecl)) {
        TheDecl->IsDeclOnly = false;
        return true;
      }
    } else if(const auto *Call = dyn_cast<CallSyntax>(Ms->getCall())) {
      if (const auto *Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
        if (!determineTagKind(Name, TheDecl)) {
          if (TheDecl->SuspectedKind == UDK_Namespace) {
            EncounteredError = true;
            SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                                clang::diag::err_invalid_macro_decl)
                                << 2;
            return false;
          }
          TheDecl->IsDeclOnly = false;
          return true;
        }
      }
    }
  }
  return false;
}

/// This handles any function decl/def combinartion.
/// Returns true if the declaration was recognized as a function and false if not.
static bool deduceFunctionSyntax(Sema &SemaRef, Declaration *TheDecl,
                                 bool &HadError) {
  HadError = false;
  // Making sure that if we have a body we mark it correctly.
  if (TheDecl->InitOpUsed == IK_None)
    TheDecl->IsDeclOnly = true;

  // Handling possible member function deduction,
  // STATIC functions within a class body will be labeled as member functions
  // that's because they don't have a different internal type.
  if (SemaRef.getCurrentScope()->getKind() == SK_Class) {
    TheDecl->SuspectedKind = UDK_MemberFunction;
    if (TheDecl->getId() == SemaRef.ConstructorII) {
      TheDecl->SuspectedKind = UDK_Constructor;
      return true;
    }

    if (TheDecl->getId() == SemaRef.DestructorII) {
      TheDecl->SuspectedKind = UDK_Destructor;
      return true;
    }

    // We are a recognized operator.
    if (TheDecl->OpInfo) {
      TheDecl->SuspectedKind = UDK_MemberOperator;
      return true;
    }

    // FIXME: Literal operator will need an error here because they are not
    // allowed within the body of a class.

    // FIXME: Conversion Operator will need to be figured out before moving
    // forward. UDK_ConversionOperator
    return true;
  }

  // Default label for this is simply a function.
  TheDecl->SuspectedKind = UDK_Function;

  // We have to identify these so we can emit an error when they are used
  // in the wrong context.
  if (TheDecl->getId() == SemaRef.ConstructorII) {
    if (!TheDecl->NNSInfo.empty()) {
      TheDecl->SuspectedKind = UDK_PossibleConstructor;
      // FIXME: Declarations with nested name specifiers that are not
      // namespace assignments must have an initialziation.
      return true;
    }
    SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                         clang::diag::err_special_member_function_non_member)
                         << 0;
    HadError = true;
    return true;
  }

  if (TheDecl->getId() == SemaRef.DestructorII) {
    if (!TheDecl->NNSInfo.empty()) {
      TheDecl->SuspectedKind = UDK_PossibleDestructor;
      return true;
    }
    // This cannot be decl only.
    SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                         clang::diag::err_special_member_function_non_member)
                         << 1;
    HadError = true;
    return true;
  }

  // Checking to see if we are an operator declaration.
  if (TheDecl->OpInfo) {
    TheDecl->SuspectedKind = UDK_OperatorOverload;
    if (!TheDecl->NNSInfo.empty())
      TheDecl->SuspectedKind = UDK_PossibleMemberOperator;

    return true;
  }
  // FIXME: Figure out how to determine if we are a literal operator
  // FIXME: Emit an error fo a Conversion operator without a NNS
  // UDK_PossibleConversionOperator

  // We just assume we are a UDK_Function.
  return true;
}

/// This meets the form x = y or x : t = y or x : t.
/// We attempt to deduce what kind of variable declaration we are dealing with
/// before punting.
/// Returns true if this is a valid variable declaration and false if not.
static bool deduceVariableSyntax(Sema &SemaRef, Declaration *TheDecl,
                                 bool &HadError) {

  HadError = false;
  if (TheDecl->InitOpUsed == IK_Exclaim) {
    HadError = true;
    SemaRef.Diags.Report(TheDecl->Init->getLoc(),
                         clang::diag::err_invalid_function_defintion_syntax);
    return false;
  }

  HadError = false;
  TheDecl->SuspectedKind = UDK_DeductionOnlyVariable;
  // FIXME: We may need to verify that the catch variable doesn't
  // have an assignment or something else attached to it.
  if (TheDecl->ScopeForDecl->getKind() == SK_Catch) {
    TheDecl->SuspectedKind = UDK_CatchVariable;
    return false;
  }
  // These are the remaining variable like declarations.
  if (TheDecl->Template) {
    TheDecl->SuspectedKind = UDK_VarTemplateOrTemplateAlias;
    // If we have template parameters and an assignment operator
    // we know we could only be a template aliase, variable template, or not a
    // declaration.
    if (TheDecl->TypeDcl) {
      TypeDeclarator *TD = TheDecl->TypeDcl->getAsType();

      // FIXME: this might not be an atom; what about `typeof(some_kind_type)`
      if (const auto *Atom = dyn_cast<AtomSyntax>(TD->getTyExpr())) {
        if (Atom->hasToken(tok::TypeKeyword)) {
          // If we are declaring a template in template scope, we have a
          // template template parameter.
          if (SemaRef.getCurrentScope()->isTemplateScope()) {
            TheDecl->SuspectedKind = UDK_TemplateTemplateParam;
            return true;
          }

          TheDecl->SuspectedKind = UDK_TemplateAlias;
          // We think this is an alias, but it isn't assigned to anything.
          if (TheDecl->InitOpUsed != IK_Equals) {
            HadError = true;
            SemaRef.Diags.Report(TD->getLoc(),
                            clang::diag::err_template_alias_missing_assignment);
          }

          // We know this has to be a declaration.
          // even if it's not valid.
          return true;
        }
      }
    }

    // If we have a specialization then we kind of have to be a variable
    // template if it's a declaration.
    if (TheDecl->SpecializationArgs) {
      TheDecl->SuspectedKind = UDK_VarTemplateDecl;
      return true;
    }

    // In this case we don't know it's a template or an array expression
    // The assumption is that we can figure that based on context later on.
    return true;
  }

  // The assumption here is that we don't know the type of the variable
  // so we have to process phase2/3 before we know the type of the variable.
  TheDecl->SuspectedKind = UDK_DeductionOnlyVariable;

  // parameter scope = this must be a parameter.
  if (SemaRef.getCurrentScope()->getKind() == SK_Parameter) {
    TheDecl->SuspectedKind = UDK_Parameter;
    return true;
  }

  // template scope = this must be a template parameter.
  if (SemaRef.getCurrentScope()->getKind() == SK_Template) {
    TheDecl->SuspectedKind = UDK_TemplateParam;
    return true;
  }

  if (TheDecl->TypeDcl) {
    TypeDeclarator *TD = TheDecl->TypeDcl->getAsType();
    if (const auto *Atom = dyn_cast<AtomSyntax>(TD->getTyExpr())) {

      // Any time we see a namespace keyword as a type we can assume we are
      // a namespace alias
      if (Atom->hasToken(tok::NamespaceKeyword)) {
        TheDecl->SuspectedKind = UDK_NamespaceAlias;
        return true;
      }
      if (Atom->hasToken(tok::TypeKeyword)) {
        // Outside of a template scope we are a type alias, inside we are a
        // parameter
        TheDecl->SuspectedKind = UDK_TypeAlias;
        return true;
      }
    }
  }

  // We are a variable, but phase 2/3 is required in order to tell what kind.
  return true;
}

bool DeclarationBuilder::checkClassifiedConversionOperator(
                                 const Syntax *DeclExpr, Declaration *TheDecl) {
  // Keep in mind that if we made it to this point the we MUST be a declaration
  // or at the very least an invalid declaration.

  if (!TheDecl->FunctionDcl) {
    SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                clang::diag::err_conversion_operator_decl_without_parameters);
    return true;
  }

  // If we reached here then we must be a conversion operator.
  TheDecl->SuspectedKind = UDK_ConversionOperator;
  bool DeclInTag = TheDecl->ScopeForDecl->getKind() == SK_Class;
  if (TheDecl->InitOpUsed == IK_None) {
    if (!DeclInTag) {
      SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                           clang::diag::err_conversion_operator_bad_location);
      return true;
    }
  } else {
    // In this case we have some kind of definition and we need are not
    // inside the body of a class/union, then we must have a NNS, if not
    // then we are an error.
    if (!DeclInTag && TheDecl->NNSInfo.empty()) {
        SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                             clang::diag::err_conversion_operator_bad_location);
      return true;
    }
  }

  // There are only two cases where this can be defined, is inside the body
  // of a class, the other is in a class but with a Nested name specifier
  return false;
}

bool DeclarationBuilder::checkClassifyUserDefinedLiteralOperator(
                                 const Syntax *DeclExpr, Declaration *TheDecl) {
  assert(TheDecl->IdDcl->isUserDefinedLiteral()
         && "Not a user defined literal.");

  if (!TheDecl->FunctionDcl) {
    SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
                        clang::diag::err_expected_declarator_chain_sequence)
                        << /*function*/3;
    return true;
  }
  if (TheDecl->IdDcl->getUserDefinedLiteralSuffix() == "") {
    SemaRef.Diags.Report(DeclExpr->getLoc(),
                       clang::diag::err_user_defined_literal_invalid_identifier)
                          << /*invalid suffix*/ 1 << 0;
    return true;
  }

  TheDecl->SuspectedKind = UDK_LiteralOperator;

  // Recording the identifier that's used to look up this declaration, when
  // used as an operator.
  TheDecl->UDLSuffixId = &Context.CxxAST.Idents.get(
                               {TheDecl->IdDcl->getUserDefinedLiteralSuffix()});

  return false;
}

bool DeclarationBuilder::classifyDecl(const Syntax *DeclExpr,
                                      Declaration *TheDecl) {
  if (ConversionTypeSyntax)
    return checkClassifiedConversionOperator(DeclExpr, TheDecl);

  if (TheDecl->Decl->isUsingDirective()) {
    TheDecl->SuspectedKind = UDK_UsingDirective;
    return false;
  }

  if (TheDecl->IdDcl && TheDecl->IdDcl->isUserDefinedLiteral())
    return checkClassifyUserDefinedLiteralOperator(DeclExpr, TheDecl);

  // this is handled within checkEnumDeclaration
  if (TheDecl->SuspectedKind == UDK_EnumConstant)
    return false;

  bool EncounteredError = false;
  if (isTagLikeDeclOrForwardDecl(SemaRef, TheDecl, EncounteredError)) {
    if (EncounteredError)
      return true;
    return false;
  }

  if (EncounteredError)
    return true;

  // Is it any kind of function?
  if (TheDecl->FunctionDcl) {
    // Doing a thing.
    // Verifying that we do infact have a valid type.
    if (deduceFunctionSyntax(SemaRef, TheDecl, EncounteredError)) {
      if (EncounteredError)
        return true;
      return false;
    }
  } else {
    // Then it's some kind of variable.
    if (deduceVariableSyntax(SemaRef, TheDecl, EncounteredError)) {
      if (EncounteredError)
        return true;
      return false;
    }
  }
  // if either of the deduced syntax returns an error and they are not a
  if (EncounteredError)
    return true;

  assert(TheDecl->SuspectedKind != UDK_None
         && "Declaration type never deduced and no error found");
  return false;
}

} // end namespace gold
