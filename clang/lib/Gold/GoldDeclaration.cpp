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
#include "clang/Gold/GoldOperatorInfo.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/AST/TemplateBase.h"
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
  case UDK_UsingDirective:
    return "UDK_UsingDirective";
  case UDK_CatchVariable:
    return "UDK_CatchVariable";
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
  case UDK_CatchVariable:
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

bool Declaration::declaresNamespaceWithNestedName() const {
  return declaresNamespace() && (!NNSInfo.empty() || GlobalNsSpecifier);
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

bool Declaration::declaresConversionOperator() const {
  return SuspectedKind == UDK_ConversionOperator;
}

bool Declaration::declaresUserDefinedLiteral() const {
  return SuspectedKind == UDK_LiteralOperator;
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
bool Declaration::declIsConstexpr() const {
  if (!IdDcl->UnprocessedAttributes)
    return false;

  auto Iter = std::find_if(IdDcl->UnprocessedAttributes->begin(),
      IdDcl->UnprocessedAttributes->end(), [](const Syntax *S) -> bool {
        if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(S)) {
          if (Atom->getSpelling() == "constexpr") {
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

bool Declaration::isVariadic() const {
  if (!declaresFunctionDecl())
    return false;
  return IsVariadic;
}

bool Declaration::declaresUsingDirective() const {
  return isa<UsingDirectiveDeclarator>(Decl);
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
  GOLD_CASE_TO_STR(UDK_CatchVariable)
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

void Declaration::dump(llvm::raw_ostream &os) const {
  os << "gold::Declaration" << this << "\n"
     << "  Phase = " << phaseToStr(CurrentPhase) <<"\n"
     << "  Declaration Context = " << Cxt << "\n"
     << "  Declarators: ";
  if (Decl)
    Decl->printSequence(os);
  else
    os << "nullptr\n";

  os << "Operator = ";
  if (Op) {
    os << "\n";
    Op->dump();
    os << "\n";
  } else
    os << "nullptr\n";

  os << "Init = ";
  if (Init) {
    os << "\n";
    Init->dump();
    os << "\n";
  } else
    os << "nullptr\n";
  os << "  Suspected kind = " << unevaluatedDeclKindToStr(SuspectedKind) << "\n";
  os << "  Gold Saved Scope = " << SavedScope << "\n";
  if (SavedScope) {
    Scope *S = SavedScope;
    do {
      os << "=============\n";
      S->dump();
      S = S->getParent();
    } while(S);
    os << "=============\n";
  }

  os << "  Id (" << Id << ") = " << (Id?Id->getName() : "") << "\n";

  if (OpInfo) {
    os << "  OpInfo (" << OpInfo << ") = "
       << OpInfo->getGoldDeclName()->getName() << "\n";
  }
  os << "  Cxx (" << Cxx << ")\n";
  if (Cxx)
    Cxx->dump(os);

  // Declaration *First = this;
  // Declaration *Next = this;

  os << "  ClangDeclaringScope = " << ClangDeclaringScope << "\n";
  os << "  Scope For decl = " << ScopeForDecl << "\n";
  if (ScopeForDecl) {
    Scope *S = ScopeForDecl;
    do {
      os << "=============\n";
      S->dump();
      S = S->getParent();
    } while(S);
    os << "=============\n";
  }

  os << "  Parent declaration = " << ParentDecl << "\n";
  if (ParentDecl) {
    os << "    Parent Decl Suspected kind = "
        << unevaluatedDeclKindToStr(ParentDecl->SuspectedKind) << "\n";
  }

  os << "  Declaring Context = " << DeclaringContext << "\n";
  if (DeclaringContext) {
    os << "  Dump of Decl context = \n";
    DeclaringContext->dumpDeclContext();
    if (clang::Decl *ClangDecl = dyn_cast<clang::Decl>(DeclaringContext)) {
      os << "  Decl context is a declaration.\n";
      ClangDecl->dump(os);
    }
  }
  // TODO: We may want to add these to the dump in the future.
  // Currently it's only extra information.
  // const CallSyntax *ES_Call = nullptr;
  // const AtomSyntax *ES_Name = nullptr;

  if (GlobalNsSpecifier) {
    os << "  Global namespace specifier given (" << GlobalNsSpecifier
       << ")= \n";
    GlobalNsSpecifier->getGlobalNameSpecifier()->dump();
  }

  if (!NNSInfo.empty())
    os << "  We have nested name specifiers\n";

  os << "  Id declareator (" << IdDcl <<  ") = ";
  if (IdDcl)
    os << IdDcl->getIdentifier()->getSpelling() << "\n";
  else
    os << "\n";

  if (Template) {
    os << "  Template Parameters (" << Template << ") = \n";
    Template->getParams()->dump();
    if (Template->getScope()) {
      os << "    Template Scope = " << Template->getScope() << "\n";
      if (Template->getScope()) {
        Scope *S = Template->getScope();
        do {
          os << "=============\n";
          S->dump();
          S = S->getParent();
        } while(S);
        os << "=============\n";
      }
    }
    if (Template->getTemplateParameterList()) {
      os << "    Template Parameter list = \n";
      for (clang::NamedDecl *ND : *Template->getTemplateParameterList()) {
        os << "      Template Paraemter = \n";
        ND->dump(os);
      }
    }
  }
  if (SpecializationArgs) {
    os << "  Specialization Args ("<< SpecializationArgs << ")= \n";
    if (SpecializationArgs->HasArguments()) {
      os << "    We have specialization arguments\n";
    }
    os << "    clang Specialization arguments\n";
    for (clang::TemplateArgumentLoc TAL : SpecializationArgs->getArgList().arguments()) {
      os << "      Template argument = \n";
      TAL.getArgument().dump(os);
    }
  }

  if (FunctionDcl) {
    os << "  Function Declarator = \n";
    if (FunctionDcl->getScope()) {
      os << "    We have a scope\n";
      Scope *S = FunctionDcl->getScope();
      do {
        os << "=============\n";
        S->dump();
        S = S->getParent();
      } while (S);
      os << "=============\n";
    } else
      os << "    Scope not assigned yet\n";

    if (FunctionDcl->getParams()) {
      os << "    FunctionDcl Parameters = \n";
      FunctionDcl->getParams()->dump();
    }
  }
  if (TypeDcl) {
    os << "  Type Declarator = \n";
    TypeDcl->getTyExpr()->dump();
  }

  if (!TemplateParamStorage.empty()) {
    os << "  Template param storage = \n";
    for(const auto *TemplateList : TemplateParamStorage) {
      os << "==============================\n";
      os << "   Template parameter List = \n";
      for (clang::NamedDecl *ND : *TemplateList) {
        os << "      Template Paraemter = \n";
        ND->dump(os);
      }
    }
    os << "==============================\n";
  }

  os << "  IsDeclOnly = " << IsDeclOnly << "\n";

  if (ScopeSpec.isSet()) {
    os << "  ScopeSpec = ";
    if (ScopeSpec.isValid()) {
      if (ScopeSpec.getScopeRep()) {
        ScopeSpec.getScopeRep()->dump(os);
        os << "\n";
      } else {
        os << "Failed to get scope rep\n";
      }
    } else {
      os << "INVALID!\n";
    }
  }

  os << "  IsRedeclaration = " << IsRedeclaration << "\n";
  os << "  NeedToBeElaboratedByClangBeforeUse = "
     << NeedToBeElaboratedByClangBeforeUse << "\n";
}
}
