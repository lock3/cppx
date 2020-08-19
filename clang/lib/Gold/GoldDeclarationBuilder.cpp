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

#include "clang/Sema/Lookup.h"

namespace gold {

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
    case SK_Enum:
      Dcl = handleEnumScope(S);
      break;
    default:
      llvm_unreachable("unknown scope type");
  }
  Declaration *ParentDecl = SemaRef.getCurrentDecl();
  // FIXME: manage memory
  Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, InitExpr);
  TheDecl->Id = Id;
  TheDecl->OpInfo = OpInfo;

  // Getting information that's necessary in order to correctly restore
  // a declaration's context during early elaboration.
  TheDecl->ClangDeclaringScope = SemaRef.getCurClangScope();
  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  TheDecl->ScopeForDecl = SemaRef.getCurrentScope();

  // UnevaluatedDeclKind DK = UDK_None;
  if (checkDeclaration(S, TheDecl)) {
    return nullptr;
  }


  if (OpInfo && !TheDecl->declaresFunction())
    llvm_unreachable("unimplemented operator!");


  Scope *CurScope = SemaRef.getCurrentScope();

  // If we're in namespace or parameter scope and this identifier already
  // exists, consider it a redeclaration.
  // TODO: distinguish between redefinition, redeclaration, and redeclaration
  // with different type.
  if ((CurScope->isNamespaceScope() || CurScope->isParameterScope()) &&
      !TheDecl->declaresFunction()) {
    // FIXME: rewrite this!!
    auto DeclSet = CurScope->findDecl(Id);

    if (!DeclSet.empty()) {
      assert((DeclSet.size() == 1) && "elaborated redefinition.");
      TheDecl->setPreviousDecl(*DeclSet.begin());
    }
  }
  TheDecl->Decl->printSequence(llvm::outs() << "DeclaratorSeq = ");
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
  if (!Dcl) {
    if (RequiresDeclOrError) {
      SemaRef.Diags.Report(DeclExpr->getLoc(),
                           clang::diag::err_invalid_declaration);
    }
    return true;
  }

  Declarator *Cur = Dcl;
  auto ReportInvalidDeclarator = [&]() -> bool {
    if (Cur == nullptr) {
      // Some how the name for this was a dot.
      if (RequiresDeclOrError) {
        SemaRef.Diags.Report(DeclExpr->getLoc(),
                            clang::diag::err_invalid_declaration);
      }
      return true;
    }
    return false;
  };

  if (Cur->isGlobalNameSpecifier()) {
    TheDecl->GlobalNsSpecifier = Cur;
    Cur = Cur->Next;
    if (ReportInvalidDeclarator())
      return true;
  }

  // Checking for Nested Name Specifier
  while(Cur->isNestedNameSpecifier()) {
    Declarator *NNS = Cur;
    Declarator *NNSTemplateParams = nullptr;
    Declarator *NNSSpecialization = nullptr;
    Cur = Cur->Next;
    if (ReportInvalidDeclarator())
      return true;

    if (Cur->isTemplateParameters()
        || Cur->isImplicitTemplateParameters()) {
      NNSTemplateParams = Cur;
      Cur = Cur->Next;
      if (ReportInvalidDeclarator())
        return true;
    }
    if (Cur->isExplicitSpecialization()
        || Cur->isPartialSpecialization()) {
      NNSSpecialization = Cur;
      Cur = Cur->Next;
      if (ReportInvalidDeclarator())
        return true;
    }
    TheDecl->NNSInfo.emplace_back(NNSDeclaratorInfo{NNS, NNSTemplateParams,
                                                    NNSSpecialization});
  }

  // We expect the regular identifier at this point.
  if (Cur->isIdentifier()) {
    TheDecl->IdDcl = Cur;
    Cur = Cur->Next;
  } else {
    if (RequiresDeclOrError) {
      SemaRef.Diags.Report(DeclExpr->getLoc(),
                          clang::diag::err_invalid_declaration);
    }
    return true;
  }
  // Jump to the very end and make sure that we can properly do deduction.
  if (Cur == nullptr)
    return false;

  // Checking for template arguments, then specializations
  if (Cur->isTemplateParameters()
      || Cur->isImplicitTemplateParameters()) {
    TheDecl->TemplateParameters = Cur;
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  if (Cur->isPartialSpecialization()
      || Cur->isExplicitSpecialization()) {
    TheDecl->SpecializationArgs = Cur;
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  if (Cur->isFunction()) {
    TheDecl->FunctionDcl = Cur;
    Cur = Cur->Next;
  }
  if (Cur == nullptr)
    return false;

  // This is the last thing so after cur == nullptr if not then we have an
  // invalid declarator
  if (Cur->isType()){
    TheDecl->TypeDcl = Cur;
    Cur = Cur->Next;
  }
  if (Cur != nullptr) {
    if (RequiresDeclOrError) {
      SemaRef.Diags.Report(DeclExpr->getLoc(),
                          clang::diag::err_invalid_declaration);
    }
    return true;
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
  // if (!TheDecl->TypeDcl && OperatorEquals && !IsInsideEnum) {
  //   clang::DeclarationNameInfo DNI({TheDecl->getId()}, TheDecl->IdDcl->getLoc());
  //   clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  //   if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
  //     return true;
  //   }
  // }
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
    if (OperatorEquals && !CurScope->findDecl(Id).empty())
      return true;
  }

  if (IsInsideEnum) {
    return checkEnumDeclaration(DeclExpr, TheDecl);
  }
  return false;
//   bool HasError = false;
//   // If we have a function declaration and functions are not allowed in the current
//   // context.
//   if (!EnableFunctions && TheDecl->FunctionDcl) {
//     if (RequiresDeclOrError) {
//       SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
//                             clang::diag::err_invalid_declarator_sequence)
//                             << 0;
//     }
//     HasError = true;
//   }

//   // This implies that it's a variable, of some kind or at least uses the
//   // variable declaration syntax of x = y with the optional : type
//   if (RequireTypeForVariable && !TheDecl->FunctionDcl) {
//     if (RequiresDeclOrError) {
//       SemaRef.Diags.Report(TheDecl->IdDcl->getLoc(),
//                           clang::diag::err_type_form_declarator_sequence);
//     }
//     HasError = true;
//   }

//   // Verifying that if we do have template parameters emit an error.
//   if (!EnableTemplateParameters) {
//     if (TheDecl->TemplateParameters) {
//       if (RequiresDeclOrError) {
//         SemaRef.Diags.Report(TheDecl->TemplateParameters->getLoc(),
//                               clang::diag::err_invalid_declarator_sequence)
//                               << 2;
//       }
//       HasError = true;
//     }
//     if (TheDecl->SpecializationArgs) {
//       if (RequiresDeclOrError) {
//         SemaRef.Diags.Report(TheDecl->SpecializationArgs->getLoc(),
//                               clang::diag::err_invalid_declarator_sequence)
//                               << 8;
//       }
//       HasError = true;
//     }
//   }

//   // Verifying that if we do have template parameters emit an error.
//   if (!EnableNestedNameSpecifiers) {
//     if (TheDecl->GlobalNsSpecifier) {
//       if (RequiresDeclOrError) {
//         SemaRef.Diags.Report(TheDecl->GlobalNsSpecifier->getLoc(),
//                              clang::diag::err_invalid_declarator_sequence)
//                              << 4;
//       }
//       HasError = true;
//     }
//     // If we have nested name info and we shouldn't
//     if (!TheDecl->NNSInfo.empty()) {
//       if (RequiresDeclOrError) {
//         SemaRef.Diags.Report(TheDecl->NNSInfo.front().NNS->getLoc(),
//                              clang::diag::err_invalid_declarator_sequence)
//                              << 4;
//       }
//       HasError = true;
//     }
//   }

  // Checking the InitExpr for possible tags.

  // UDK_Class,                // The declaration is a class, either
  //                           // a declaration or definition.

  // UDK_Union,                // The declaration is a union, either
  //                           // a declaration or definition.

  // UDK_Enum,                 // The declaration is an Enum, either
  //                           // a declaration or definition.

  // UDK_Variable,             // This is a variable that occurs within global
  //                           // namespace, a namespace, or as the static
  //                           // member of a class (with the static attribute).

  // UDK_Namespace,            // Defines a nemaspace
  // UDK_NamespaceAlias,       // Declares a namespace alias

  // UDK_TemplateAlias,        // Suspected template alias. Currently this MUST
  //                           // have a type that evaluates to : type
  //                           // - cannot be a template specialization

  // // UDK_VarTemplateDecl,   // We can't handle this unless we have speculative
  //                           // evaluation.
  //                           // That's because we don't know all of the types
  //                           // at the time that this is identified, and I can't
  //                           // look them all up until phase 2.

  // UDK_TypeAlias,            // This must have a known, RHS that is a known type,
  //                           // or provide a : type.

  // UDK_Parameter,            // A function parameter

  // UDK_TemplateParam,        // Any template parameter as their type doesn't
  //                           // matter until they are elaborated, and used.

  // UDK_Field,                // Field associated with a class.

  // UDK_EnumConstant,         // A field declared within an enum.


  // UDK_PossibleVarTemplate,  // This is a variable template definiton outside of
  //                           // a class with a namespecifier.

  // UDK_MemberFunction,       // Only applies to member functions declared within
  //                           // The body of a class.
  //                           // In order to figure out if this is a member or just
  //                           // the definition of something defined within a namespace
  //                           // it would require additional lookup and evaluation.

  // UDK_Constructor,          // A function with the name constructor declared
  //                           // within the body of a class

  // UDK_Destructor,           // A function with the name Destructor declared within
  //                           // the body of a class.

  // UDK_ConversionOperator,   // A conversion operator within the body of a class.

  // UDK_MemberOperator,       // The declaration of a member operator overload.

  // UDK_Function,             // Declares a function

  // UDK_LiteralOperator,      // User defined literal operator declaration.

  // UDK_OperatorOverload,     // An operator overload that's not within a class.

  // UDK_PossibleConstructor,  // This is a function with a nested name specifier
  //                           // and the name Constructor
  // UDK_PossibleDestructor,   // This is a function with a nested name specifier
  //                           // and the name destructor
  // UDK_PossibleMemberOperator, // This is an operator overload with a
  //                             // nested name specifier.
  // UDK_PossibleConversionOperator, // This is conversion operator overload with a
  //                                 // nested name specifier.

  // UDK_VarTemplateOrTemplateAlais, // This requires that we must specifically
  //                                 // deduce the evaluated type before we could
  //                                 // evaluate this as either an alias or a variable.
  //                                 // but minimally we would know this is a template

  // UDK_DeductionOnlyVariable,  // In order to deduce what type of declartion this
  //                             // is it requires non-speculative evaluation of it's
  //                             // assigned epxression.
  //                             // This could be one of the following:
  //                             //    - TemplateAlias
  //                             //    - NamespaceAlias
  //                             //    - VarTemplate
  //                             //    - VarDecl
  //                             //    - TypeAlias
  //                             //    - FieldDecl
  //                             //    - Not a declaration, This fully depends
  //                             //      on where it's used.
  // UDK_CallOrFunctionDecl,     // Takes the form func() = expr
  // Things we don't have syntax for yet.
  /*
  UDK_UsingDecl,
  UDK_UsingDirective,
  UDK_UsingPackDecl,
  UDK_FriendClass,
  UDK_FriendUnion,
  UDK_FriendFunction,
  UDK_FriendName, // Since we don't have syntax for this yet I'm relying on how
                  // it's specified in C++.
  UDK_UsingShadowDecl, // non-constructor. using ::foo() also works in namespace.
  UDK_UsingShadowConstructor, // using Shadowed constrcutor, does not work in
                              // namespace
  UDK_Concept // Not sure if we plan on using concepts or if there's a chance
              // that their use could be at all similar to other declarations.
*/
  // bool IsDelete = true;
  // bool IsDefault = false;
  // bool IsTag = false;
  // bool IsDeclOnly = false;
  // bool IsEnum = false;
  // bool IsClass = false;
  // bool IsUnion = false;
  // bool HasEqualZero = false;
  // bool IsNamespace = false;
  // // Attempting to extract the initializer.
  // if (TheDecl->Init) {
  //   if (OperatorEquals) {
  //     // Checking to see if we have a class, union, enum, = 0, = default,
  //     // = delete, it could also be a namespace.
  //     if (const auto *Atom = dyn_cast<AtomSyntax>(TheDecl->Init)) {
  //       if (Atom->getSpelling() == "class") {
  //         IsDeclOnly = true;
  //         IsTag = true;
  //         IsClass = true;
  //       } else if (Atom->getSpelling() == "union") {
  //         IsDeclOnly = true;
  //         IsTag = true;
  //         IsUnion = true;
  //       } else if (Atom->getSpelling() == "enum") {
  //         IsDeclOnly = true;
  //         IsTag = true;
  //         IsEnum = true;
  //       } else if (Atom->getSpelling() == "default") {
  //         bool IsDefault = true;
  //       } else if (Atom->getSpelling() == "delete") {
  //         bool IsDelete = true;
  //       } else {
  //         Token T = Atom->getToken();
  //         if (T.getKind() == tok::DecimalInteger) {
  //           if (Atom->getSpelling() == "0") {
  //             // This means we could be a virtual function.
  //             // if we are inside of a class.
  //             HasEqualZero = true;
  //           }
  //         }
  //       }
  //     } else if (const auto *EnumCall = dyn_cast<CallSyntax>(TheDecl->Init)) {
  //       if (const auto *ExpectedEnumName = dyn_cast<AtomSyntax>(
  //                                                      EnumCall->getCallee())) {
  //         if (ExpectedEnumName->getSpelling() == "enum") {
  //           // This is an enum forward declaration with an underlying type.
  //           IsDeclOnly = true;
  //           IsTag = true;
  //           IsEnum = true;
  //         }
  //       }
  //     } else if (const auto *Macro = dyn_cast<MacroSyntax>(TheDecl->Init)) {
  //       // This means it it has the syntax something: that could be a class,
  //       // namespace, union, or enum. if it's not one of those then it's
  //       // 100% an error reguardless of if the error setting says otherwise.
  //       if (const auto *Atom = dyn_cast<AtomSyntax>(Macro->getCall())) {
  //         if (Atom->getSpelling() == "class") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsClass = true;
  //         } else if (Atom->getSpelling() == "union") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsUnion = true;
  //         } else if (Atom->getSpelling() == "enum") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsEnum = true;
  //         } else if (Atom->getSpelling() == "namespace") {
  //           IsDeclOnly = false;
  //           IsNamespace = true;
  //         }
  //       }
  //     } else if (const auto *MacroCall = dyn_cast<CallSyntax>(Macro->getCall())) {
  //       if (const auto *Atom = dyn_cast<AtomSyntax>(Macro->getCall())) {
  //         if (Atom->getSpelling() == "class") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsClass = true;
  //         } else if (Atom->getSpelling() == "union") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsUnion = true;
  //         } else if (Atom->getSpelling() == "enum") {
  //           IsDeclOnly = false;
  //           IsTag = true;
  //           IsEnum = true;
  //         } else {
  //           llvm_unreachable("This doesn't make sense?!");
  //         }
  //       }
  //     }
  //   } else {
  //     // this means it's a function body and we don't need to do anything about it?
  //     // I need to verify that I do infact have a valid function and this isn't
  //     // just x! or something like that.
  //     if (!FunctionDcl) {
  //       // This implies that we have a X! or something like that, either way it
  //       // doesn't make any sense, and is there for not a declaration.
  //       if (RequiresDeclOrError) {
  //         SemaRef.Diags.Report(DeclExpr->getLoc(),
  //                             clang::diag::err_invalid_declaration);
  //       }
  //       return true;
  //     }
  //   }
  // } else {
  //   // if we are a function then we are function declaration.
  //   if (FunctionDcl) {
  //     IsDeclOnly = true;
  //   }
  // }
  // // The we are kind of done here, we would need to
  // if (IsTag) {

  // }
  // if (!EnableNamespaceDecl) {
  // }
  // err_invalid_declarator_sequence : Error<
  // 0 = function declaration
  // 1 = namespace
  // 2 = template
  // 3 = nested name specifier
  // 4 = global name specifier
  // 5 = class
  // 6 = enum
  // 7 = union
  // 8 = template specialization

  // EnableFunctions
  // EnableTags
  // EnableAliases
  // EnableTemplateParameters
  // RequireTypeForVariable
  // EnableNestedNameSpecifiers
  // RequireAliasTypes
  // EnableNamespaceDecl
  // RequireTypeForFunctions
  // RequiresDeclOrError
  // return HasError;
}
bool DeclarationBuilder::checkEnumDeclaration(const Syntax *DeclExpr,
                                              Declaration *TheDecl) {
  // We know we are 100% have to be this or an error.
  TheDecl->SuspectedKind = UDK_EnumConstant;
  // Because we are inside of an enum we are 100% sure that this is an error.
  if (TheDecl->GlobalNsSpecifier) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 4;
    return true;
  }

  if (!TheDecl->NNSInfo.empty()) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 9;
    return true;
  }

  if (TheDecl->FunctionDcl) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 0;
    return true;
  }

  if (TheDecl->TypeDcl) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 9;
    return true;
  }

  if (TheDecl->TemplateParameters) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 2;
    return true;
  }

  if (TheDecl->SpecializationArgs) {
    SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                         clang::diag::err_invalid_declarator_sequence)
                         << 8;
    return true;
  }

  // TODO: It may be necessary to specifically include a test for
  // TheDecl->Init
  if (TheDecl->Op) {
    if (const auto *Call = dyn_cast<CallSyntax>(TheDecl->Op)) {
      if (const auto *Operator = dyn_cast<AtomSyntax>(Call->getCallee())) {
        if (Operator->getSpelling() != "operator'='") {
          // This means we are not using the assignment operator, but we are using
          // something else like operator'!'. Indicating we cannot be an
          // enumeration declaration, and we are for some reason using function
          // decl syntax in this context.
          SemaRef.Diags.Report(TheDecl->FunctionDcl->getLoc(),
                              clang::diag::err_invalid_declarator_sequence)
                              << 0;
          return true;
        }
      } else {
        llvm_unreachable("I don't know what happened here.");
      }
    }
  }
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
  return makeDeclarator(S);
}

Declarator *DeclarationBuilder::handleEnumScope(const Syntax *S) {
  // This is done 100% Seperate from all other declarations, because of how
  // limited enum declarations actually are.
  // llvm_unreachable("Working on it.");
  if (const auto *NameOnly = dyn_cast<AtomSyntax>(S)) {
    return handleIdentifier(NameOnly, nullptr);
  } else if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    EnableFunctions = false;
    EnableNamespaceDecl = false;
    EnableTags = false;
    EnableAliases = false;
    RequireTypeForVariable = false;
    EnableTemplateParameters = false;
    EnableNestedNameSpecifiers = false;
    RequireAliasTypes = false;
    RequireTypeForFunctions = false;
    IsInsideEnum = true;
    return makeDeclarator(Call);
  } else if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, nullptr);
  } else {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_declaration);
    return nullptr;
  }
}

static bool isParameterSyntax(const Syntax *S) {
  if (const auto * Call = dyn_cast<CallSyntax>(S)) {
    if (const auto *Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
      if (Name->getSpelling() == "operator':'") {
        return true;
      } else if (Name->getSpelling() == "operator'='") {
        if (const auto *InnerTypeOpCall
                                 = dyn_cast<CallSyntax>(Call->getArgument(0))) {
          if (const auto *InnerName
                         = dyn_cast<AtomSyntax>(InnerTypeOpCall->getCallee())) {
            if (InnerName->getSpelling() == "operator':'") {
              return true;
            }
          }
        }
      }
    }
  }
  return false;
}

Declarator *
DeclarationBuilder::buildNestedNameSpec(const Syntax *S, Declarator *Next) {
  llvm_unreachable("DeclarationBuilder::buildNestedNameSpec Working on it.");
}

Declarator *
DeclarationBuilder::buildNestedTemplate(const Syntax *S, Declarator *Next) {
  llvm_unreachable("DeclarationBuilder::buildNestedTemplate Working on it.");
}

Declarator *
DeclarationBuilder::buildNestedName(const Syntax *S, Declarator *Next) {
  llvm_unreachable("DeclarationBuilder::buildNestedTemplate Working on it.");
}

Declarator *
DeclarationBuilder::buildNameDeclarator(const Syntax *S, Declarator *Next) {
  if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, Next);
  }
  if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
    return handleIdentifier(Name, Next);
  }
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    if (const auto *Name = dyn_cast<AtomSyntax>(Call->getCallee())) {
      // This is where we handle complex names.
      if (Name->getSpelling() == "operator'.'") {
        // This is the first time we made it here then on the way out I can say
        // with 100% certinty that the name in argument place 1 is the
        // Main identifier for the declaration, Any arguments to the left
        // are nested name specifiers.
        llvm::outs() << "Complex name implementation hasn't been implemented yet.\n";
        return nullptr;
      }
    }
  }
  if (HasType)
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_declaration);
  return nullptr;
}

Declarator *
DeclarationBuilder::mainElementTemplateOrSpecialization(const ElemSyntax *Elem,
                                                        Declarator *Next) {
  const auto *ElemArgs = cast<ListSyntax>(Elem->getArguments());
  if (const auto *InnerTemplate = dyn_cast<ElemSyntax>(Elem->getObject())) {
    // We can be 100% sure we are some kind of specialization, either explicit
    // or partial.
    llvm_unreachable("Partial and explicit explicit([][])"
                     "specialization not implemented yet.");
  } else {
    // We assume that this is a specialization if we are @ a function declaration
    // and a template if not.
    if (ElemArgs->getNumChildren() == 0) {
      if (HasFunctionCallSyntax) {
      Declarator *ExplicitDcl = handleExplicitSpecialization(Elem, Next);
      Declarator *TDcl = handleImplicitTemplateParams(Elem, ExplicitDcl);
      Declarator *NameDcl = buildNameDeclarator(Elem->getObject(),TDcl);
      if (!NameDcl)
        return nullptr;
      NameDcl->recordAttributes(Elem);
      return NameDcl;
      } else {
        Declarator *TDcl = handleTemplateParams(Elem, Next);
        Declarator *NameDcl = buildNameDeclarator(Elem->getObject(),TDcl);
        NameDcl->recordAttributes(Elem);
        return NameDcl;
      }
    }
    // Attempting to figure out of this is a full specialization or template
    if (isParameterSyntax(ElemArgs->getChild(0))) {
      // We are template arguments.
      Declarator *TDcl = handleTemplateParams(Elem, Next);
      Declarator *NameDcl = buildNameDeclarator(Elem->getObject(), TDcl);
      if (!NameDcl)
        return nullptr;
      NameDcl->recordAttributes(Elem);
      return NameDcl;
    } else {
      // We are an explicit specialization.
      Declarator *ExplicitDcl = handleExplicitSpecialization(Elem, Next);
      Declarator *TDcl = handleImplicitTemplateParams(Elem, ExplicitDcl);
      Declarator *NameDcl = buildNameDeclarator(Elem->getObject(),TDcl);
      if (!NameDcl)
        return nullptr;
      NameDcl->recordAttributes(Elem);
      return NameDcl;
    }
  }

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
    // This can only occur at this level.
    HasFunctionCallSyntax = true;
    Declarator *Temp = buildTemplateOrNameDeclarator(Func->getCallee(),
                               handleFunction(Func, Next));
    Temp->recordAttributes(Func);
    return Temp;
  } else if (const ErrorSyntax *Es = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Es, Next);
  }

  return buildTemplateOrNameDeclarator(S, Next);
}

Declarator *DeclarationBuilder::makeTopLevelDeclarator(const Syntax *S,
                                   Declarator *Next) {
  // If we find an atom, then we're done.
  if(const CallSyntax *Call = dyn_cast<CallSyntax>(S)) {
    if (const AtomSyntax *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {

      // Check for "builtin" operators in the declarator.
      if (Callee->getSpelling() == "operator':'") {
        HasType = true;
        // The LHS is a template, name or function, and the RHS is
        // ALWAYS a type (or is always supposed to be a type.)
        return buildTemplateFunctionOrNameDeclarator(Call->getArgument(0),
                                        handleType(Call->getArgument(1), Next));

      } else if (Callee->getSpelling() == "operator'.'") {
        // TODO: It might be necessary in the future to decompose this name into
        // a meaningful outside of class definition, However, this would need be
        // processed slightly different.
        // return nullptr;
        llvm_unreachable("Nested name specifier not implemented yet.");
      } else if (Callee->getSpelling() == "operator'in'") {
        return makeTopLevelDeclarator(Call->getArgument(0), Next);
      }
    }
  } else if(const ErrorSyntax *Err = dyn_cast<ErrorSyntax>(S)) {
    return handleErrorSyntax(Err, Next);
  }

  return buildTemplateFunctionOrNameDeclarator(S, Next);
}

Declarator *DeclarationBuilder::makeDeclarator(const Syntax *S) {
  const Syntax *Decl = nullptr;
  // Declarations only appear in calls.
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    if (const auto *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
      llvm::StringRef Op = Callee->getToken().getSpelling();
      // Need to figure out if this is a declaration or expression?
      // Unpack the declarator.
      if (Op == "operator'='") {
        const auto *Args = cast<ListSyntax>(Call->getArguments());
        Decl = Args->getChild(0);

        // This is to reject t.x as a declaration.
        // This checks if a declaration already exists in a parent scope.
        // For example, we are in a member function and are accessing a member.
        // FIXME: This may also need to be removed.
        if(const AtomSyntax *LHS = dyn_cast<AtomSyntax>(Decl)) {
          clang::DeclarationNameInfo DNI({
              &Context.CxxAST.Idents.get(LHS->getSpelling())
            }, S->getLoc());
          clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
          if (SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
            return nullptr;
          }
        }

        // Explicilty ignoring declarations that use x.y or (x)y.
        // FIXME: THis will need to be removed eventually.
        if (const CallSyntax *Inner = dyn_cast<CallSyntax>(Decl))
          if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Inner->getCallee()))
            if (Atom->getSpelling() == "operator'.'" ||
                Atom->getSpelling() == "operator'()'")
              return nullptr;

        // Attempting to verify if this is an ElemSyntax.
        if (isa<ElemSyntax>(Decl))
          // This can't be a declaration, because would need to say":type" after
          // the name to be considered a template type.
          return nullptr;

        InitExpr = Args->getChild(1);
        OperatorEquals = true;

      } else if (Op == "operator'!'") {
        const auto *Args = cast<ListSyntax>(Call->getArguments());
        Decl = Args->getChild(0);
        InitExpr = Args->getChild(1);
      } else if (Op == "operator':'") {
        HasType = true;
        Decl = S;
        InitExpr = nullptr;
      } else if (Op == "operator'in'") {
        Decl = S;
        InitExpr = nullptr;
      } else if (Op == "operator'[]'") {
        // We always return false here because any type alias must indicate
        // have a ": type" after it or it's not a template alias.
        return nullptr;
      } else {
        // Syntactically, this is not a declaration.
        return nullptr;
      }
    }
  }
  if (!Decl) {
    return nullptr;
  }
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
  llvm_unreachable("DeclarationBuilder::handleNestedNameSpecifier");
}

IdentifierDeclarator *
DeclarationBuilder::handleIdentifier(const AtomSyntax *S, Declarator *Next) {
  // Translating the simple identifier.
  OriginalName = S->getSpelling();
  Id = &Context.CxxAST.Idents.get(OriginalName);
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
      llvm_unreachable("User defined literal declarations not "
                        "imeplemented yet.");
    } else if (OriginalName.startswith("conversion\"")) {
      llvm_unreachable("User defined conversion declarations not "
                        "imeplemented yet.");
    }
  }
  auto *D = new IdentifierDeclarator(S, Next);
  D->recordAttributes(S);
  return D;
}

FunctionDeclarator *
DeclarationBuilder::handleFunction(const CallSyntax *S, Declarator *Next,
                                   bool IsVariadic) {
  const auto *Args = cast<ListSyntax>(S->getArguments());
  auto Ret = new FunctionDeclarator(Args, nullptr, Next, IsVariadic);
  Ret->recordAttributes(S);
  return Ret;
}

TypeDeclarator *
DeclarationBuilder::handleType(const Syntax *S, Declarator *Next) {
  return new TypeDeclarator(S, Next);
}

TemplateParamsDeclarator *
DeclarationBuilder::handleTemplateParams(const ElemSyntax *S, Declarator *Next) {
  auto Ret = new TemplateParamsDeclarator(
                            cast<ListSyntax>(S->getArguments()), nullptr, Next);
  Ret->recordAttributes(S);
  return Ret;
}

ImplicitEmptyTemplateParamsDeclarator *
DeclarationBuilder::handleImplicitTemplateParams(const ElemSyntax *Owner,
                                                 Declarator *Next) {
  return new ImplicitEmptyTemplateParamsDeclarator(
                     cast<ListSyntax>(Owner->getArguments()), nullptr, Next);
}

ExplicitSpecializationDeclarator *
DeclarationBuilder::handleExplicitSpecialization(
                      const ElemSyntax *SpecializationOwner, Declarator *Next) {
  auto Ret = new ExplicitSpecializationDeclarator(
                cast<ListSyntax>(SpecializationOwner->getArguments()), Next);
  Ret->recordAttributes(SpecializationOwner);
  return Ret;
}

PartialSpecializationDeclarator *
DeclarationBuilder::handlePartialSpecialization(
                      const ElemSyntax *SpecializationOwner, Declarator *Next) {
  llvm_unreachable("DeclarationBuilder::handlePartialSpecialization");
}

} // end namespace gold
