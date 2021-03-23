#include "clang/Gold/GoldDeclaratorBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"

namespace gold {

Declarator *DeclaratorBuilder::operator()(const Syntax *S) {
  NodeLabeler LabelNodes(SemaRef, NodeLabels);
  LabelNodes(S);
  for (auto Each : NodeLabels) {
    llvm::outs() << "NODE: " << Each.second << '\n';
    Each.first->dump();
    llvm::outs() << "===------------------===\n";
  }

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

  for (const Declarator *D : Chain)
    llvm::outs() << D->getString() << " -> ";
  llvm::outs() << "\nEND CHAIN\n";

  return nullptr;
}

void DeclaratorBuilder::VisitSyntax(const Syntax *S) {
  ConstSyntaxVisitor<DeclaratorBuilder>::Visit(S);
}

void DeclaratorBuilder::VisitGoldCallSyntax(const CallSyntax *S) {
  const AtomSyntax *Callee = dyn_cast<AtomSyntax>(S->getCallee());

  // This is an array prefix.
  if (Callee && Callee->getSpelling() == "operator'[]'") {
    // os << "[] -> ";
    Chain.push_back(new ArrayDeclarator(S->getArgument(0), nullptr));
  }

  for (const Syntax *Arg : S->children()) {
    VisitSyntax(Arg);
  }

  if (Callee &&
      (Callee->getSpelling() == "postfix'^'" ||
       Callee->getSpelling() == "operator'^'")) {
    Chain.push_back(new PointerDeclarator(S->getArgument(0), nullptr));
  //   os << "^ -> ";
  }
}

using LabelMapTy = DeclaratorBuilder::LabelMapTy;
static inline bool isLeftOfRoot(LabelMapTy const &Labels,
                                const Syntax *S) {
  auto It = Labels.find(S);
  if (It == Labels.end())
    return false;
  return It->second < Labels.RootLabel;
}

void DeclaratorBuilder::VisitGoldElemSyntax(const ElemSyntax *S) {
  VisitSyntax(S->getObject());

  if (isLeftOfRoot(NodeLabels, S))
    llvm::outs() << "ELEM ON LEFT\n";

  Chain.push_back(new ArrayDeclarator(S->getObject(), nullptr));
  // os << "[] -> ";
}

void DeclaratorBuilder::VisitGoldAtomSyntax(const AtomSyntax *S) {
  // os << S->getSpelling() << " -> ";
  handleIdentifier(S, nullptr);
}

Declarator *DeclaratorBuilder::handleNamespaceScope(const Syntax *S) {
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
  ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleParameterScope(const Syntax *S) {
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
  ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleTemplateScope(const Syntax *S) {
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

Declarator *DeclaratorBuilder::handleFunctionScope(const Syntax *S) {
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
  ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleBlockScope(const Syntax *S) {
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

Declarator *DeclaratorBuilder::handleClassScope(const Syntax *S) {
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
  ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleControlScope(const Syntax *S) {
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

Declarator *DeclaratorBuilder::handleEnumScope(const Syntax *S) {
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
  ContextDeclaresNewName = true;
  // Special case where enum values are allowed to just be names.
  if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
    return handleIdentifier(Name, nullptr);
  }
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleCatchScope(const Syntax *S) {
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

Declarator *DeclaratorBuilder::makeDeclarator(const Syntax *S) {
  VisitSyntax(S);
  return nullptr;
  // Handle a special case of an invalid enum identifier `.[name]`
  // without an assignment operator.
  // if (IsInsideEnum)
  //   if (const auto *Name = dyn_cast<AtomSyntax>(S))
  //     return handleIdentifier(Name, nullptr);

  // if (const auto *Macro = dyn_cast<MacroSyntax>(S))
  //   return makeTopLevelDeclarator(Macro, nullptr);

  // const auto *Call = dyn_cast<CallSyntax>(S);
  // if (!Call) {
  //   if (RequiresDeclOrError)
  //     SemaRef.Diags.Report(S->getLoc(),
  //                          clang::diag::err_invalid_declaration_kind)
  //                          << 2;
  //   return nullptr;
  // }
}

IdentifierDeclarator *
DeclaratorBuilder::handleIdentifier(const AtomSyntax *S, Declarator *Next) {
  // Don't bother with the unnamed name ("_")
  if (S->getToken().hasKind(tok::AnonymousKeyword)) {
    auto *D = new IdentifierDeclarator(S, Next);
    D->recordAttributes(S);
    Chain.push_back(D);
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
  Chain.push_back(D);
  return D;
}

void DeclaratorBuilder::NodeLabeler::operator()(const Syntax *S) {
  if (!isa<CallSyntax>(S))
    return;

  const CallSyntax *Op = cast<CallSyntax>(S);
  if (getFusedOpKind(SemaRef, Op) != FOK_Colon)
    return;

  // Label the LHS subtree first, then the root, then the RHS subtree.
  // This way, any node on the LHS will be less than the root, and
  // any node on the RHS will be greater than the root.
  ConstSyntaxVisitor<NodeLabeler>::Visit(Op->getArgument(0));
  if (NodeLabels.insert({S, Label++}).second)
    NodeLabels.RootLabel = Label++;
  ConstSyntaxVisitor<NodeLabeler>::Visit(Op->getArgument(1));
}

void DeclaratorBuilder::NodeLabeler::VisitGoldCallSyntax(const CallSyntax *S) {
  if (S->getNumArguments() == 0) {
    NodeLabels.insert({S, Label++});
    return;
  }

  unsigned I = 0;
  ConstSyntaxVisitor<NodeLabeler>::Visit(S->getArgument(I));
  NodeLabels.insert({S, Label++});
  for (++I; I < S->getNumArguments(); ++I)
    ConstSyntaxVisitor<NodeLabeler>::Visit(S->getArgument(I));
}

void DeclaratorBuilder::NodeLabeler::VisitGoldElemSyntax(const ElemSyntax *S) {
  ConstSyntaxVisitor<NodeLabeler>::Visit(S->getObject());
  NodeLabels.insert({S, Label++});
  for (const Syntax *Arg : S->getArguments()->children())
    ConstSyntaxVisitor<NodeLabeler>::Visit(Arg);
}

void DeclaratorBuilder::NodeLabeler::VisitGoldAtomSyntax(const AtomSyntax *S) {
  NodeLabels.insert({S, Label++});
}


} // end namespace gold
