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

#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldElaborator.h"

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
  switch(CurrentScope->getKind()) {
    case SK_Namespace:
      Dcl = handleNamespaceScope(S);
      break;
    case SK_Parameter:
      Dcl = handleParameterScope(S);
      break;
    case SK_Template:
      Dcl = handleTemplateScope(S);
      break;
    case SK_Function:
      Dcl = handleFunctionScope(S);
      break;
    case SK_Block:
      Dcl = handleBlockScope(S);
      break;
    case SK_Class:
      Dcl = handleClassScope(S);
      break;
    case SK_Control:
      Dcl = handleControlScope(S);
      break;
    case SK_Catch:
      Dcl = handleCatchScope(S);
      break;
    case SK_Enum:
      Dcl = handleEnumScope(S);
      break;
  }

  if (!Dcl)
    return nullptr;

  Declaration *ParentDecl = SemaRef.getCurrentDecl();
  // FIXME: manage memory
  Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, InitExpr);
  TheDecl->Id = Id;
  TheDecl->OpInfo = OpInfo;
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

    if (!DeclSet.empty()) {
      assert((DeclSet.size() == 1) && "elaborated redefinition.");
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
  // Recording additional attributes associated with the name.
  // This is to handle the special case of the . member access operator
  // picking up the attributes from a nested name declaration with attributes.
  for(const Syntax *NodeWithAttr : AdditionalNodesWithAttrs) {
    TheDecl->IdDcl->recordAttributes(NodeWithAttr);
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

  if (Cur->isFunction()) {
    TheDecl->FunctionDcl = Cur->getAsFunction();
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  // This is the last thing so after cur == nullptr if not then we have an
  // invalid declarator
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

  // Doing simple blook lookup.
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
      if (InitOperatorUsed != IK_Equals) {
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
        SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                            clang::diag::err_invalid_declarator_sequence)
                            << 3;
      return true;
    }
    if (TheDecl->GlobalNsSpecifier) {
      if (RequiresDeclOrError)
        SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
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
  if (TheDecl->InitOpUsed == IK_Exlaim) {
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

Declarator *DeclarationBuilder::handleNamespaceScope(const Syntax *S) {
  EnableFunctions = true;
  EnableNamespaceDecl = true;
  EnableTags = true;
  EnableAliases = true;
  EnableTemplateParameters = false;
  RequireTypeForVariable = false;
  EnableNestedNameSpecifiers = true;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleParameterScope(const Syntax *S) {
  EnableFunctions = false;
  EnableNamespaceDecl = false;
  EnableTags = false;
  EnableAliases = false;  // TODO: This may need to be true in the future.
                          // But I'm not sure how we could pass a namespace as
                          // a parameter yet.
  EnableTemplateParameters = false;
  RequireTypeForVariable = true;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleTemplateScope(const Syntax *S) {
  // This is for template parameters.
  EnableFunctions = false;
  EnableNamespaceDecl = false;
  EnableTags = false;
  EnableAliases = false;
  // Template parameters cannot have template parameters unless they
  // are template template parameters, in which case they should be specified
  // differently.
  EnableTemplateParameters = false;
  RequireTypeForVariable = true;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleFunctionScope(const Syntax *S) {
  EnableFunctions = true;
  EnableNamespaceDecl = false;
  EnableTags = true;
  EnableAliases = true;
  EnableTemplateParameters = false;
  RequireTypeForVariable = false;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = true;
  RequireTypeForFunctions = true;
  RequiresDeclOrError = false;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleBlockScope(const Syntax *S) {
  EnableFunctions = true;
  EnableNamespaceDecl = false;
  EnableTags = true;
  EnableAliases = true;
  EnableTemplateParameters = false;
  RequireTypeForVariable = false;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = true;
  RequireTypeForFunctions = true;
  RequiresDeclOrError = false;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleClassScope(const Syntax *S) {
  EnableFunctions = true;
  EnableNamespaceDecl = false;
  EnableTags = true;
  EnableAliases = true;
  EnableTemplateParameters = true;
  RequireTypeForVariable = true;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  AllowShortCtorAndDtorSyntax = true;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleControlScope(const Syntax *S) {
  EnableFunctions = false;
  EnableNamespaceDecl = false;
  EnableTags = false;
  EnableAliases = false;
  RequireTypeForVariable = false;
  EnableTemplateParameters = false;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = false;
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleEnumScope(const Syntax *S) {
  EnableFunctions = false;
  EnableNamespaceDecl = false;
  EnableTags = false;
  EnableAliases = false;
  RequireTypeForVariable = false;
  EnableTemplateParameters = false;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  IsInsideEnum = true;
  // Special case where enum values are allowed to just be names.
  if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
    return handleIdentifier(Name, nullptr);
  }
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleCatchScope(const Syntax *S) {
  EnableFunctions = false;
  EnableNamespaceDecl = false;
  EnableTags = false;
  EnableAliases = false;
  RequireTypeForVariable = true;
  EnableTemplateParameters = false;
  EnableNestedNameSpecifiers = false;
  RequireAliasTypes = false;
  RequireTypeForFunctions = false;
  RequiresDeclOrError = true;
  return makeDeclarator(S);
}

static bool isParameterSyntax(Sema& SemaRef, const Syntax *S) {
  const auto *Call = dyn_cast<CallSyntax>(S);
  if (!Call)
    return false;
  FusedOpKind Op = getFusedOpKind(SemaRef, Call);
  if (Op == FOK_Colon) {
    return true;
  } else if (Op == FOK_Equals) {
    const Syntax *Arg = Call->getArgument(0);
    if (!Arg)
      return false;

    if (getFusedOpKind(SemaRef, dyn_cast<CallSyntax>(Arg)) == FOK_Colon)
      return true;
  }
  return false;
}

Declarator *
DeclarationBuilder::buildNestedTemplate(const ElemSyntax *Elem, Declarator *Next) {
  const auto *ElemArgs = cast<ListSyntax>(Elem->getArguments());
  Declarator *CurrentNext = Next;
  const Syntax *NextNameNode = nullptr;

  // Checking for nested template specifier.
  if (const auto *InnerTemplate = dyn_cast<ElemSyntax>(Elem->getObject())) {
    // We can be 100% sure we are some kind of specialization.
    Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
    CurrentNext = handleTemplateParams(InnerTemplate, ExplicitDcl);
    NextNameNode = InnerTemplate->getObject();
  } else {
    NextNameNode = Elem->getObject();
    // We assume that this is a specialization if we are @ a function declaration
    // and a template if not.
    if (ElemArgs->getNumChildren() == 0) {
      // The assumption here is that if the parameter list is empty then we
      // are some kind of specialization, or an error being elaborated.
      // It's a specializaton if this is a function, and a possible specialization
      // if it's something else, the error for this is determined later.
      Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
      CurrentNext = handleImplicitTemplateParams(Elem, ExplicitDcl);
    } else {
      // Attempting to figure out of this is a full specialization or a template.
      if (isParameterSyntax(SemaRef, ElemArgs->getChild(0))) {
        // We are template arguments.
        CurrentNext = handleTemplateParams(Elem, Next);
      } else {
        // We are an explicit specialization.
        Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
        CurrentNext = handleImplicitTemplateParams(Elem, ExplicitDcl);
      }
    }
  }
  Declarator *NameDcl = buildNestedOrRegularName(NextNameNode, CurrentNext);
  if (!NameDcl)
    return nullptr;
  NameDcl->recordAttributes(Elem);
  return NameDcl;
}

Declarator *
DeclarationBuilder::buildNestedNameSpec(const CallSyntax *Call, Declarator *Next) {
  // Current syntax is a . ?
  FusedOpKind OpKind = getFusedOpKind(SemaRef, Call);
  if (OpKind == FOK_MemberAccess) {
    Declarator *ConstructedName = nullptr;
    AdditionalNodesWithAttrs.insert(Call);
    ConstructedName = buildNestedOrRegularName(Call->getArgument(1), Next);
    // When this happens an error should have already been emitted.
    if (!ConstructedName)
      return nullptr;

    // Attempt to build up the LHS
    return buildNestedTemplateSpecializationOrName(Call->getArgument(0),
                                                    ConstructedName);
  } else {
    // if it's not a nested name then we have an error.
    if (RequiresDeclOrError)
      SemaRef.Diags.Report(Call->getLoc(),
                            clang::diag::err_invalid_declaration);
    return nullptr;
  }
}

Declarator *DeclarationBuilder::buildNestedOrRegularName(const Syntax *S, Declarator *Next) {
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    return buildNestedNameSpec(Call, Next);
  }
  return buildNestedName(S, Next);
}

Declarator *
DeclarationBuilder::buildNestedName(const Syntax *S, Declarator *Next) {
  if (const auto *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, Next);
  }

  if (const auto *SimpleName = dyn_cast<AtomSyntax>(S)) {
    return handleNestedNameSpecifier(SimpleName, Next);
  }
  if (RequiresDeclOrError)
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_declaration_kind)
                         << 2;
  return nullptr;
}

Declarator *
DeclarationBuilder::buildNestedTemplateSpecializationOrName(const Syntax *S,
                                                            Declarator *Next) {
  if (const auto *Es = dyn_cast<ErrorSyntax>(S))
    return handleErrorSyntax(Es, Next);

  if (const auto *Elem = dyn_cast<ElemSyntax>(S))
    return buildNestedTemplate(Elem, Next);

  return buildNestedOrRegularName(S, Next);
}

Declarator *
DeclarationBuilder::buildNameDeclarator(const Syntax *S, Declarator *Next) {
  if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S))
    return handleErrorSyntax(Es, Next);

  if (const auto *Name = dyn_cast<AtomSyntax>(S))
    return handleIdentifier(Name, Next);

  unsigned ErrorIndicator = 0;
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, dyn_cast<CallSyntax>(S));
    switch(OpKind) {
      case FOK_MemberAccess:{
        if (const auto *IdName = dyn_cast<AtomSyntax>(Call->getArgument(1))){
          AdditionalNodesWithAttrs.insert(Call);
          return buildNestedTemplateSpecializationOrName(Call->getArgument(0),
                                                handleIdentifier(IdName, Next));
        }

        if (const auto *Es = dyn_cast<ErrorSyntax>(Call->getArgument(1)))
          return handleErrorSyntax(Es, Next);
        // This might not be a declararation.
        ErrorIndicator = 2;
      }
      break;
      case FOK_Unknown:
        ErrorIndicator = 0;
        break;
      default:
        ErrorIndicator = 2;
    }
  } else {
    ErrorIndicator = 1;
  }
  if (RequiresDeclOrError)
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_declaration_kind)
                         << ErrorIndicator;
  return nullptr;
}

Declarator *
DeclarationBuilder::mainElementTemplateOrSpecialization(const ElemSyntax *Elem,
                                                        Declarator *Next) {
  const auto *ElemArgs = cast<ListSyntax>(Elem->getArguments());
  Declarator *CurrentNext = Next;
  const Syntax *NextNameNode = nullptr;
  if (const auto *InnerTemplate = dyn_cast<ElemSyntax>(Elem->getObject())) {
    // We can be 100% sure we are some kind of specialization, either explicit
    // or partial.
    Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
    CurrentNext = handleTemplateParams(InnerTemplate, ExplicitDcl);
    NextNameNode = InnerTemplate->getObject();
  } else {
    NextNameNode = Elem->getObject();
    // We assume that this is a specialization if we are @ a function declaration
    // and a template if not.
    if (ElemArgs->getNumChildren() == 0) {
      // The assumption here is that if the parameter list is empty then we
      // are some kind of specialization, or an error being elaborated.
      // It's a specializaton if this is a function, and a possible specialization
      // if it's something else, the error for this is determined later.
      Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
      CurrentNext = handleImplicitTemplateParams(Elem, ExplicitDcl);
    } else {
      // Attempting to figure out of this is a full specialization or a template.
      if (isParameterSyntax(SemaRef, ElemArgs->getChild(0))) {
        // We are template arguments.
        CurrentNext = handleTemplateParams(Elem, Next);
      } else {
        // We are an explicit specialization.
        Declarator *ExplicitDcl = handleSpecialization(Elem, Next);
        CurrentNext = handleImplicitTemplateParams(Elem, ExplicitDcl);
      }
    }
  }
  Declarator *NameDcl = buildNameDeclarator(NextNameNode, CurrentNext);
  if (!NameDcl)
    return nullptr;
  NameDcl->recordAttributes(Elem);
  return NameDcl;
}

Declarator *
DeclarationBuilder::buildTemplateOrNameDeclarator(const Syntax *S,
                                                  Declarator *Next) {
  if (const ElemSyntax *TemplateParams = dyn_cast<ElemSyntax>(S)) {
    return mainElementTemplateOrSpecialization(TemplateParams, Next);
  } else if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, Next);
  } else {
    return buildNameDeclarator(S, Next);
  }
}

Declarator *
DeclarationBuilder::buildTemplateFunctionOrNameDeclarator(const Syntax *S,
                                                          Declarator *Next) {
  if (const CallSyntax *Func = dyn_cast<CallSyntax>(S)) {
    if (isa<AtomSyntax>(Func->getCallee())) {

      FusedOpKind OpKind = getFusedOpKind(SemaRef, Func);
      if (OpKind == FOK_MemberAccess) {
        AdditionalNodesWithAttrs.insert(Func);
        return buildTemplateOrNameDeclarator(Func, Next);
      }
    }

    Declarator *Fn = handleFunction(Func, Next);
    Declarator *Temp = buildTemplateOrNameDeclarator(Func->getCallee(), Fn);
    Declarator *Cur = Temp;
    while(Cur) {
      if (isa<IdentifierDeclarator>(Cur))
        break;

      Cur = Cur->Next;
    }

    if (Cur)
      Cur->recordAttributes(Func);

    return Temp;
  } else if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, Next);
  }

  return buildTemplateOrNameDeclarator(S, Next);
}

Declarator *
DeclarationBuilder::buildUsingDirectiveDeclarator(const MacroSyntax *S) {
  InitExpr = S;
  return new UsingDirectiveDeclarator(S->getCall()->getLoc(), S);
}

Declarator *DeclarationBuilder::makeTopLevelDeclarator(const Syntax *S,
                                                       Declarator *Next) {
  // If we find an atom, then we're done.
  if(const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    if (const AtomSyntax *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {

      // Check for "builtin" operators in the declarator.
      if (Callee->getSpelling() == "operator':'") {
        RequiresDeclOrError = true;
        // The LHS is a template, name or function, and the RHS is
        // ALWAYS a type (or is always supposed to be a type.)
        return buildTemplateFunctionOrNameDeclarator(Call->getArgument(0),
                                        handleType(Call->getArgument(1), Next));

      } else if (Callee->getSpelling() == "operator'.'") {
        return buildNameDeclarator(Call, Next);

      } else if (Callee->getSpelling() == "operator'in'") {
        return makeTopLevelDeclarator(Call->getArgument(0), Next);
      }
    }
  } else if (const MacroSyntax *Macro = dyn_cast<MacroSyntax>(S)) {
    if (const AtomSyntax *Call = dyn_cast<AtomSyntax>(Macro->getCall()))
      if (Call->getToken().hasKind(tok::UsingKeyword))
        return buildUsingDirectiveDeclarator(Macro);
  } else if(const ErrorSyntax *Err = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Err, Next);
  }

  return buildTemplateFunctionOrNameDeclarator(S, Next);
}

Declarator *
DeclarationBuilder::appendConversionType(Declarator *CurDcl) {
  if (!CurDcl)
    return CurDcl;

  if (ConversionTypeSyntax) {
    Declarator *Temp = CurDcl;

    // Going until we reach the end of the declarator chain.
    while(Temp && Temp->Next) Temp = Temp->Next;

    // If we are the conversion operator the next thing to do is to update the
    // declarator chain by inserting the type after the function in the chain.
    Temp->Next = handleType(ConversionTypeSyntax, nullptr);
  }
  return CurDcl;
}

Declarator *DeclarationBuilder::makeDeclarator(const Syntax *S) {
  return appendConversionType(dispatchAndCreateDeclarator(S));
}

Declarator *DeclarationBuilder::dispatchAndCreateDeclarator(const Syntax *S) {

  // Handling a special case of an invalid enum identifier .name
  // without an assignment operator. This need to to output
  // the correct error message.
  if (IsInsideEnum)
    if (const auto *Name = dyn_cast<AtomSyntax>(S))
      return handleIdentifier(Name, nullptr);

  if (const auto *Macro = dyn_cast<MacroSyntax>(S))
    return makeTopLevelDeclarator(S, nullptr);

  const auto *Call = dyn_cast<CallSyntax>(S);

  if (!Call) {
    if (RequiresDeclOrError)
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_declaration_kind)
                           << 2;
    return nullptr;
  }

  const Syntax *Decl = nullptr;
  FusedOpKind OpKind = getFusedOpKind(SemaRef, Call);
  switch(OpKind) {

  case FOK_Equals:{
    const auto *Args = cast<ListSyntax>(Call->getArguments());
    Decl = Args->getChild(0);

    // This is to reject t.x as a declaration.
    // This checks if a declaration already exists in a parent scope.
    // For example, we are in a member function and are accessing a member.
    if (const AtomSyntax *LHS = dyn_cast<AtomSyntax>(Decl)) {
      clang::DeclarationNameInfo DNI({
          &Context.CxxAST.Idents.get(LHS->getSpelling())
        }, S->getLoc());
      if (!SemaRef.checkUnqualifiedNameIsDecl(DNI)) {
        // this may need an error message depending on context.
        return nullptr;
      }
    }

    // Explicilty ignoring declarations that use x.y or (x)y.
    // FIXME: THis will need to be removed eventually.
    if (const CallSyntax *Inner = dyn_cast<CallSyntax>(Decl))
      if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Inner->getCallee()))
        if (Atom->getSpelling() == "operator'()'") {
          // FIXME: This needs an diagnostic message here.
          return nullptr;
        }
    // TODO: Remove me.
    // // Attempting to verify if this is an ElemSyntax.
    // if (isa<ElemSyntax>(Decl))
    //   // This can't be a declaration, because would need to say":type" after
    //   // the name to be considered a template type.
    //   return nullptr;

    InitExpr = Args->getChild(1);
    InitOperatorUsed = IK_Equals;
    break;
  }
  case FOK_MemberAccess:{
    // Member access is a special case because it requires us to recurse and call
    // this a 2nd time iff it hase 1 argument instead of 2.
    if (Call->getNumArguments() != 1)
      // In the event that this is true then we are not a declaration, we are
      // the expression x.y
      return nullptr;

    return handleGlobalNameSpecifier(Call,
                              dispatchAndCreateDeclarator(Call->getArgument(0)));
    break;
  }
  case FOK_Colon:{
    RequiresDeclOrError = true;
    Decl = S;
    InitExpr = nullptr;
    break;
  }
  case FOK_In:{
    Decl = S;
    InitExpr = nullptr;
    break;
  }
  case FOK_Exclaim:{
    const auto *Args = cast<ListSyntax>(Call->getArguments());
    Decl = Args->getChild(0);
    InitExpr = Args->getChild(1);
    InitOperatorUsed = IK_Exlaim;
    break;
  }
  case FOK_Unknown:
  case FOK_Arrow:
  case FOK_If:
  case FOK_Else:
  case FOK_Return:
  case FOK_For:
  case FOK_While:
  case FOK_DotDot:
  case FOK_Const:
  case FOK_Ref:
  case FOK_RRef:
  case FOK_Brackets:
  case FOK_Throw:
  case FOK_Parens:
  case FOK_DotCaret:
  default: {
    // None of these operators can be the root of a declaration, with the exception
    // of very specific contexts.
    if (RequiresDeclOrError) {
      if (const AtomSyntax *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
        if (AllowShortCtorAndDtorSyntax &&
              (Callee->getSpelling() == "constructor"
              || Callee->getSpelling() == "destructor"
              || (Callee->isFused()
                  &&
                  Callee->getFusionBase() == tok::Conversion
                )
            )) {
          return buildTemplateFunctionOrNameDeclarator(Call, nullptr);
        }
      }
      SemaRef.Diags.Report(Call->getCallee()->getLoc(),
                           clang::diag::err_invalid_declaration_kind)
                           << 2;
    }
    return nullptr;
  }
  }

  // We are not a declaration and not an error here.
  if (!Decl)
    return nullptr;

  return makeTopLevelDeclarator(Decl, nullptr);
}

UnknownDeclarator *
DeclarationBuilder::handleUnknownADeclSyntax(const Syntax *S, Declarator *Next) {
  llvm_unreachable("DeclarationBuilder::handleUnknownADeclSyntax");
}

ErrorDeclarator *
DeclarationBuilder::handleErrorSyntax(const ErrorSyntax *S, Declarator *Next) {
  auto *D = new ErrorDeclarator(S, Next);
  return D;
}

GlobalNameSpecifierDeclarator *
DeclarationBuilder::handleGlobalNameSpecifier(const CallSyntax *S, Declarator *Next) {
  auto Ret = new GlobalNameSpecifierDeclarator(S, Next);
  Ret->recordAttributes(S);
  return Ret;
}

NestedNameSpecifierDeclarator *
DeclarationBuilder::handleNestedNameSpecifier(const AtomSyntax *S, Declarator *Next) {
  auto Ret = new NestedNameSpecifierDeclarator(S, Next);
  Ret->recordAttributes(S);
  return Ret;
}

IdentifierDeclarator *
DeclarationBuilder::handleIdentifier(const AtomSyntax *S, Declarator *Next) {
  // Don't bother with the unnamed name ("_")
  if (S->getToken().hasKind(tok::AnonymousKeyword)) {
    auto *D = new IdentifierDeclarator(S, Next);
    D->recordAttributes(S);
    return D;
  }

  // Translating the simple identifier.
  OriginalName = OriginalNameStorage = S->getSpelling();
  Id = &Context.CxxAST.Idents.get(OriginalName);
  std::string UDLSuffix;
  assert(OriginalName != "operator'.'");
  if (OriginalName.find('"') != llvm::StringRef::npos) {
    if (OriginalName.startswith("operator\"")) {
      OpInfo = SemaRef.OpInfo.getOpInfo(OriginalName);
      if (!OpInfo) {
        SemaRef.Diags.Report(S->getLoc(),
                             clang::diag::err_operator_cannot_be_overloaded)
                             << OriginalName;
        return nullptr;
      }
    } else if (OriginalName.startswith("literal\"")) {
      if (!S->getFusionArg()) {
        SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_user_defined_literal_invalid_identifier)
                             << /*invalid suffix*/ 0 << 0;
        return nullptr;
      }
      if (const AtomSyntax *Suffix = dyn_cast<AtomSyntax>(S->getFusionArg())){
        UDLSuffix = Suffix->getSpelling();
      }

    } else if (OriginalName.startswith("conversion\"")) {
      ConversionTypeSyntax = S->getFusionArg();
    }
  }
  auto *D = new IdentifierDeclarator(S, Next);
  D->recordAttributes(S);
  D->setUserDefinedLiteralSuffix(UDLSuffix);
  return D;
}

FunctionDeclarator *
DeclarationBuilder::handleFunction(const CallSyntax *S, Declarator *Next) {
  auto Ret = new FunctionDeclarator(S, Next);
  Ret->recordAttributes(S);
  return Ret;
}

TypeDeclarator *
DeclarationBuilder::handleType(const Syntax *S, Declarator *Next) {
  return new TypeDeclarator(S, Next);
}

TemplateParamsDeclarator *
DeclarationBuilder::handleTemplateParams(const ElemSyntax *S, Declarator *Next) {
  auto Ret = new TemplateParamsDeclarator(S, Next);
  Ret->recordAttributes(S);
  return Ret;
}

ImplicitEmptyTemplateParamsDeclarator *
DeclarationBuilder::handleImplicitTemplateParams(const ElemSyntax *Owner,
                                                 Declarator *Next) {
  return new ImplicitEmptyTemplateParamsDeclarator(Owner, Next);
}

SpecializationDeclarator *
DeclarationBuilder::handleSpecialization(const ElemSyntax *Specialization,
                                         Declarator *Next) {
  auto Ret = new SpecializationDeclarator(Specialization, Next);
  Ret->recordAttributes(Specialization);
  return Ret;
}


} // end namespace gold
