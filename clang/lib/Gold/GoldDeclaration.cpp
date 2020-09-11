//===- GoldDeclaration.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implementation for the gold declaration.
//
//===----------------------------------------------------------------------===//

#include "clang/Gold/GoldDeclaration.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

#include "llvm/Support/raw_ostream.h"
namespace gold {

llvm::StringRef unevaluatedDeclKindToStr(UnevaluatedDeclKind UDK) {
  switch(UDK) {
  case UDK_None:
    return "UDK_None";
  case UDK_File:
    return "UDK_File";
  case UDK_Class:
    return "UDK_Class";
  case UDK_Union:
    return "UDK_Union";
  case UDK_Enum:
    return "UDK_Enum";
  case UDK_Namespace:
    return "UDK_Namespace";
  case UDK_NamespaceAlias:
    return "UDK_NamespaceAlias";
  case UDK_TemplateAlias:
    return "UDK_TemplateAlias";
  case UDK_VarTemplateDecl:
    return "UDK_VarTemplateDecl";
  case UDK_TypeAlias:
    return "UDK_TypeAlias";
  case UDK_Parameter:
    return "UDK_Parameter";
  case UDK_TemplateParam:
    return "UDK_TemplateParam";
  case UDK_TemplateTemplateParam:
    return "UDK_TemplateTemplateParam";
  case UDK_EnumConstant:
    return "UDK_EnumConstant";
  case UDK_MemberFunction:
    return "UDK_MemberFunction";
  case UDK_Constructor:
    return "UDK_Constructor";
  case UDK_Destructor:
    return "UDK_Destructor";
  case UDK_Function:
    return "UDK_Function";
  case UDK_ConversionOperator:
    return "UDK_ConversionOperator";
  case UDK_MemberOperator:
    return "UDK_MemberOperator";
  case UDK_LiteralOperator:
    return "UDK_LiteralOperator";
  case UDK_OperatorOverload:
    return "UDK_OperatorOverload";
  case UDK_PossibleConstructor:
    return "UDK_PossibleConstructor";
  case UDK_PossibleDestructor:
    return "UDK_PossibleDestructor";
  case UDK_PossibleMemberOperator:
    return "UDK_PossibleMemberOperator";
  case UDK_PossibleConversionOperator:
    return "UDK_PossibleConversionOperator";
  case UDK_VarTemplateOrTemplateAlias:
    return "UDK_VarTemplateOrTemplateAlias";
  case UDK_DeductionOnlyVariable  :
    return "UDK_DeductionOnlyVariable";
  }
  llvm_unreachable("Invalid UnevaluatedDeclKind");
}

llvm::StringRef phaseToStr(Phase p) {
  switch(p) {
  case Phase::Unprocessed:
    return "Unprocessed";
  case Phase::Identification:
    return "Identification";
  case Phase::Typing:
    return "Typing";
  case Phase::Initialization:
    return "Initialization";
  }
  llvm_unreachable("Invalid Phase");
}

Declaration::~Declaration() {
  delete SavedScope;
}

clang::SourceLocation Declaration::getEndOfDecl() const {
  const Declarator *D = Decl;
  if (!D)
    return clang::SourceLocation();

  if (Init)
    return Init->getLoc();

  while(D->Next) {
    D = D->Next;
  }
  return D->getLoc();
}


// A declarator declares a variable, if it does not declare a function.
bool Declaration::declaresVariable() const {
  switch(SuspectedKind) {
  default: return false;
  case UDK_NamespaceAlias:
  case UDK_TemplateAlias:
  case UDK_VarTemplateDecl:
  case UDK_TypeAlias:
  case UDK_Parameter:
  case UDK_TemplateParam:
  case UDK_VarTemplateOrTemplateAlias:
  case UDK_DeductionOnlyVariable:
    return true;
  }
}

bool Declaration::templateHasDefaultParameters() const {
  // TODO: This is necessary for figuring out if a template parameter has
  // delayed evaluation or not.
  llvm_unreachable("This isn't implemented yet, but it may need to be in the "
      "near future.");
}

bool Declaration::declaresInitializedVariable() const {
  return declaresVariable() && Init;
}

bool Declaration::declaresForwardRecordDecl() const {
  switch(SuspectedKind) {
  case UDK_Class:
  case UDK_Union:
  case UDK_Enum:
    return IsDeclOnly;
  default:
    return false;
  }
}

bool Declaration::declaresTagDef() const {
  switch(SuspectedKind) {
  default: return false;
  case UDK_Class:
  case UDK_Union:
  case UDK_Enum:
    return !IsDeclOnly;
  }
}

bool Declaration::declaresNamespace() const {
  if (Cxx)
    return isa<clang::NamespaceDecl>(Cxx);
  return SuspectedKind == UDK_Namespace;
}

bool Declaration::declaresTemplateType() const {
  return Template && !FunctionDcl;
}

bool Declaration::declaresFunction() const {
  return FunctionDcl;
}

bool Declaration::declaresFunctionWithImplicitReturn() const {
  return declaresFunction() && InitOpUsed == IK_Equals;
}

bool Declaration::declaresPossiblePureVirtualFunction() const {
  if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    if (const CallSyntax *Call = dyn_cast<CallSyntax>(Op))
      if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Call->getCallee()))
        if (Name->getSpelling() == "operator'='")
          if (const LiteralSyntax *Lit
                                = dyn_cast<LiteralSyntax>(Call->getArgument(1)))
            if (Lit->getToken().getKind() == tok::DecimalInteger)
              if (Lit->getSpelling() == "0")
                return true;
  }
  return false;
}

static bool isSpecialExpectedAssignedFuncValue(const Syntax *Op, TokenKind TK) {
  if (const CallSyntax *Call = dyn_cast<CallSyntax>(Op))
    if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(Call->getCallee()))
      if (Name->getSpelling() == "operator'='")
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Call->getArgument(1)))
          if (Atom->getToken().getKind() == TK)
            return true;
  return false;
}

bool Declaration::declaresDefaultedFunction() const {
 if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    return isSpecialExpectedAssignedFuncValue(Op, tok::DefaultKeyword);
  }
  return false;
}


bool Declaration::declaresDeletedFunction() const {
 if (declaresFunction() || declaresFunctionTemplate()) {
    if (!Op)
      return false;
    return isSpecialExpectedAssignedFuncValue(Op, tok::DeleteKeyword);
  }
  return false;
}

bool Declaration::declaresMemberVariable() const {
  // This can't be deduced without elaboration.
  return declaresVariable() && Cxx && clang::isa<clang::FieldDecl>(Cxx);
}

bool Declaration::declaresMemberFunction() const {
  return SuspectedKind == UDK_MemberFunction;
}

bool Declaration::declaresConstructor() const {
  return SuspectedKind == UDK_Constructor;
}

bool Declaration::declaresDestructor() const {
  return SuspectedKind == UDK_Destructor;
}

// A declarator declares a template if it's first non-id declarator is
// declares template parameters.
bool Declaration::declaresFunctionTemplate() const {
  // TODO: In the future we would need to extend this definition to make sure
  // that everything works as expected whe we do have an identifier that
  // is infact also a template name.
  return FunctionDcl && Template;
}


bool Declaration::declaresOperatorOverload() const {
  if (!OpInfo)
    return false;
  return declaresFunction();
}

bool Declaration::declaresTypeAlias() const {
  return Cxx && isa<clang::TypeAliasDecl>(Cxx);
}

bool Declaration::declIsStatic() const {
  if (!IdDcl->UnprocessedAttributes)
    return false;

  auto Iter = std::find_if(IdDcl->UnprocessedAttributes->begin(),
      IdDcl->UnprocessedAttributes->end(), [](const Syntax *S) -> bool {
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(S)) {
          if (Atom->getSpelling() == "static") {
            return true;
          }
        }
        return false;
      });
  return Iter != IdDcl->UnprocessedAttributes->end();
}

bool Declaration::declaresFunctionDecl() const {
  return declaresFunction() && !Init;
}

bool Declaration::declaresFunctionDef() const {
  return declaresFunction() && Init;
}

bool Declaration::hasNestedNameSpecifier() const {
  return !NNSInfo.empty();
}

llvm::StringRef Declaration::getSuspectedKindStr() const {
  switch(SuspectedKind) {
#define GOLD_CASE_TO_STR(NAME) case NAME: return #NAME;
  GOLD_CASE_TO_STR(UDK_None)
  GOLD_CASE_TO_STR(UDK_File)
  GOLD_CASE_TO_STR(UDK_Class)
  GOLD_CASE_TO_STR(UDK_Union)
  GOLD_CASE_TO_STR(UDK_Enum)
  GOLD_CASE_TO_STR(UDK_EnumConstant)
  GOLD_CASE_TO_STR(UDK_Namespace)
  GOLD_CASE_TO_STR(UDK_NamespaceAlias)
  GOLD_CASE_TO_STR(UDK_TemplateAlias)
  GOLD_CASE_TO_STR(UDK_VarTemplateDecl)
  GOLD_CASE_TO_STR(UDK_TypeAlias)
  GOLD_CASE_TO_STR(UDK_Parameter)
  GOLD_CASE_TO_STR(UDK_TemplateParam)
  GOLD_CASE_TO_STR(UDK_VarTemplateOrTemplateAlias)
  GOLD_CASE_TO_STR(UDK_DeductionOnlyVariable)
  GOLD_CASE_TO_STR(UDK_MemberFunction)
  GOLD_CASE_TO_STR(UDK_Constructor)
  GOLD_CASE_TO_STR(UDK_Destructor)
  GOLD_CASE_TO_STR(UDK_Function)
  GOLD_CASE_TO_STR(UDK_ConversionOperator)
  GOLD_CASE_TO_STR(UDK_MemberOperator)
  GOLD_CASE_TO_STR(UDK_LiteralOperator)
  GOLD_CASE_TO_STR(UDK_OperatorOverload)
  GOLD_CASE_TO_STR(UDK_PossibleConstructor)
  GOLD_CASE_TO_STR(UDK_PossibleDestructor)
  GOLD_CASE_TO_STR(UDK_PossibleMemberOperator)
  GOLD_CASE_TO_STR(UDK_PossibleConversionOperator)
#undef GOLD_CASE_TO_STR
  default:
    llvm::outs() << "Suspected kind number = " << SuspectedKind << "\n";
    llvm_unreachable("unknown suspected kind");
  }
}

bool Declaration::declaresInlineInitializedStaticVarDecl() const {
  if (!Cxx)
    return false;
  clang::VarDecl *VD = dyn_cast<clang::VarDecl>(Cxx);
  if (!VD)
    return false;
  return VD->isInline() && VD->getStorageClass() == clang::SC_Static;
}

const IdentifierDeclarator *Declaration::getIdDeclarator() const {
  return IdDcl;
}

IdentifierDeclarator *Declaration::getIdDeclarator() {
  return IdDcl;
}

clang::DeclContext *Declaration::getCxxContext() const {
  return clang::Decl::castToDeclContext(Cxx);
}

void Declaration::setPreviousDecl(Declaration *Prev) {
  Prev->Next = this;
  First = Prev->First;
  Next = First;
}

bool Declaration::isDeclaredWithinClass() const {
  return ScopeForDecl->getKind() == SK_Class;
}
}
