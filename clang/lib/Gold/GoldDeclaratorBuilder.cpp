#include "clang/Gold/GoldDeclaratorBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"

namespace gold {

void DeclaratorBuilder::VisitSyntax(const Syntax *S) {
  // ConstSyntaxVisitor<DeclarationBuilder2>::Visit(S);
}

void DeclaratorBuilder::VisitGoldCallSyntax(const CallSyntax *S) {
  // const AtomSyntax *Callee = dyn_cast<AtomSyntax>(S->getCallee());

  // if (Callee && Callee->getSpelling() == "operator'[]'") {
  //   os << "[] -> ";
  // }

  // for (auto Arg : S->getArguments()->children()) {
  //   VisitSyntax(Arg);
  // }

  // if (Callee &&
  //     (Callee->getSpelling() == "postfix'^'" ||
  //      Callee->getSpelling() == "operator'^'"))
  //   os << "^ -> ";
}

void DeclaratorBuilder::VisitGoldElemSyntax(const ElemSyntax *S) {
  // VisitSyntax(S->getObject());
  // os << "[] -> ";
}

void DeclaratorBuilder::VisitGoldAtomSyntax(const AtomSyntax *S) {
  // os << S->getSpelling() << " -> ";
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
  // Handle a special case of an invalid enum identifier `.[name]`
  // without an assignment operator.
  if (IsInsideEnum)
    if (const auto *Name = dyn_cast<AtomSyntax>(S))
      return handleIdentifier(Name, nullptr);

  // if (const auto *Macro = dyn_cast<MacroSyntax>(S))
  //   return makeTopLevelDeclarator(Macro, nullptr);

  const auto *Call = dyn_cast<CallSyntax>(S);
  if (!Call) {
    if (RequiresDeclOrError)
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_declaration_kind)
                           << 2;
    return nullptr;
  }
}

IdentifierDeclarator *
DeclaratorBuilder::handleIdentifier(const AtomSyntax *S, Declarator *Next) {
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

} // end namespace gold
