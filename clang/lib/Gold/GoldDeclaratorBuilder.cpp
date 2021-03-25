#include "clang/Gold/GoldDeclaratorBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"

namespace gold {

using ParentMapTy = DeclaratorBuilder::ParentMapTy;

Declarator *DeclaratorBuilder::operator()(const Syntax *S) {
  NodeLabeler LabelNodes(SemaRef, NodeLabels, NodeParents);
  LabelNodes(S);
  for (auto Each : NodeLabels) {
    const Syntax *P = nullptr;
    auto It = NodeParents.find(Each.first);
    if (It != NodeParents.end())
      P = It->second;

    llvm::outs() << "NODE: " << Each.second << " - " << Each.first
                 << ": PARENT: " << P << '\n';
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

  Declarator *D = Result;
  while (D) {
    llvm::outs() << D->getString() << " -> ";
    D = D->Next;
  }
  llvm::outs() << "\nEND CHAIN\n";

  return Dcl;
}

void DeclaratorBuilder::VisitSyntax(const Syntax *S) {
  ConstSyntaxVisitor<DeclaratorBuilder>::Visit(S);
}

using LabelMapTy = DeclaratorBuilder::LabelMapTy;
static inline bool isLeftOfRoot(LabelMapTy const &Labels,
                                const Syntax *S) {
  auto It = Labels.find(S);
  if (It == Labels.end())
    return false;
  return It->second < Labels.RootLabel;
}

void DeclaratorBuilder::VisitGoldCallSyntax(const CallSyntax *S) {
  const AtomSyntax *Callee = dyn_cast<AtomSyntax>(S->getCallee());
  FusedOpKind Op = getFusedOpKind(SemaRef, S);

  // A normal function declaration.
  if (Op == FOK_Unknown) {
    // TODO: what does a function template look like?
    VisitSyntax(S->getCallee());
    return buildFunction(S);
  }

  // TODO:
  // if (Op == FOK_MemberAccess)

  if (Op == FOK_Equals) {
    InitExpr = S->getArgument(1);
    InitOperatorUsed = IK_Equals;
    return VisitSyntax(S->getArgument(0));
  } else if (Op == FOK_Exclaim) {
    InitExpr = S->getArgument(1);
    InitOperatorUsed = IK_Exclaim;
    return VisitSyntax(S->getArgument(0));
  }

  // This is an array prefix.
  if (Op == FOK_Brackets)
    push(new ArrayDeclarator(S->getArgument(0), nullptr));

  bool LeftOfRoot = isLeftOfRoot(NodeLabels, S);
  if (!LeftOfRoot &&
      Callee && (Callee->getSpelling() == "postfix'^'" || Op == FOK_Caret))
    push(new PointerDeclarator(S->getArgument(0), nullptr));

  for (const Syntax *Arg : S->getArguments()->children())
    VisitSyntax(Arg);

  if (LeftOfRoot &&
      Callee && (Callee->getSpelling() == "postfix'^'" || Op == FOK_Caret))
    push(new PointerDeclarator(S->getArgument(0), nullptr));
}

void DeclaratorBuilder::VisitGoldElemSyntax(const ElemSyntax *S) {
  VisitSyntax(S->getObject());

  if (isLeftOfRoot(NodeLabels, S)) {
    const ListSyntax *Args = dyn_cast<ListSyntax>(S->getArguments());
    if (!Args || !Args->getNumChildren()) {
      push(new ArrayDeclarator(nullptr, nullptr));
      return;
    }

    SuppressDiagnosticsRAII Suppressor(SemaRef.getCxxSema());

    // Determine if this is a template or array.
    for (const Syntax *AA : Args->children()) {
      // We have a declaration in the index, this is definitely a
      // template declaration.
      Elaborator DeclElab(Context, SemaRef);
      DeclElab.setTemporaryElaboration();
      if (DeclElab.elaborateDeclSyntax(AA))
        return buildTemplate(S);

      ExprElaborator Elab(Context, SemaRef);
      Elab.setTemporaryElaboration();
      clang::Expr *E = Elab.elaborateExpr(AA);
      // Either this is ill-formed, so just build something and call it
      // a day, or it's a template specialization.
      if (!E || E->getType()->isTypeOfTypes())
        return buildTemplate(S);
    }

    ExprElaborator BaseElab(Context, SemaRef);
    clang::Expr *Base = BaseElab.elaborateExpr(S->getObject());
    if (Base)
      return;
  }

  buildArray(S->getArguments());
}

// True when Child is a node on the left hand side of Parent.
static inline bool isLeftOf(LabelMapTy const &Labels,
                            const Syntax *Child,
                            const Syntax *Parent) {
  if (!Child || !Parent)
    return false;

  auto ChildIt = Labels.find(Child);
  if (ChildIt == Labels.end())
    return false;
  auto ParentIt = Labels.find(Parent);
  if (ParentIt == Labels.end())
    return false;
  return ChildIt->second < ParentIt->second;
}

void DeclaratorBuilder::VisitGoldListSyntax(const ListSyntax *S) {
  // Lists only have signficant meaning as arguments to an  operator"->" call.
  if (const Syntax *Parent = getParent(S)) {
    if (const CallSyntax *ParentCall = dyn_cast<CallSyntax>(Parent)) {
      if (getFusedOpKind(SemaRef, ParentCall) == FOK_Arrow) {
        // A list on the LHS of an operator"->" is a function parameter list.
        if (isLeftOf(NodeLabels, S, Parent))
          return buildFunction(S);
      }
    }

  }

  // Elsewise, we can just see through the list.
  for (const Syntax *Param : S->children())
    VisitSyntax(Param);
}


void DeclaratorBuilder::VisitGoldAtomSyntax(const AtomSyntax *S) {
  if (isLeftOfRoot(NodeLabels, S))
    return buildIdentifier(S);
  buildType(S);
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

void DeclaratorBuilder::buildTemplate(const ElemSyntax *S) {
  const auto *Args = cast<ListSyntax>(S->getArguments());

  // FIXME: record the element attributes on to the base as well?
  if (isParameterSyntax(SemaRef, Args->getChild(0)))
    return buildTemplateParams(S);
  else
    return buildSpecialization(S);
}

void DeclaratorBuilder::buildName(const Syntax *S) {
  if (const ErrorSyntax *E = dyn_cast<ErrorSyntax>(S))
    return buildError(E);

  if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(S))
    return buildIdentifier(Name);

    unsigned ErrorIndicator = 0;
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, dyn_cast<CallSyntax>(S));
    switch(OpKind) {
      case FOK_MemberAccess:{
        if (const auto *IdName = dyn_cast<AtomSyntax>(Call->getArgument(1))){
          AdditionalNodesWithAttrs.insert(Call);
          return;
          // return buildNestedTemplateSpecializationOrName(Call->getArgument(0),
          //                                       buildIdentifier(IdName));
        }

        if (const auto *E = dyn_cast<ErrorSyntax>(Call->getArgument(1)))
          return buildError(E);
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
}

void DeclaratorBuilder::buildError(const ErrorSyntax *S) {
  push(new ErrorDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildGlobalNameSpecifier(const CallSyntax *S) {
  auto Result = new GlobalNameSpecifierDeclarator(S, nullptr);
  Result->recordAttributes(S);
  push(Result);
}

void DeclaratorBuilder::buildNestedNameSpecifier(const AtomSyntax *S) {
  auto Result = new NestedNameSpecifierDeclarator(S, nullptr);
  Result->recordAttributes(S);
  push(Result);
}

void DeclaratorBuilder::buildArray(const Syntax *S) {
  push(new ArrayDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildType(const Syntax *S) {
  push(new TypeDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildFunction(const CallSyntax *S) {
  push(new FunctionDeclarator(S->getArguments(), nullptr));
  Cur->recordAttributes(S);
}

void DeclaratorBuilder::buildFunction(const ListSyntax *S) {
  push(new FunctionDeclarator(S, nullptr));
  Cur->recordAttributes(S);
}

void DeclaratorBuilder::buildTemplateParams(const ElemSyntax *S) {
  push(new TemplateParamsDeclarator(S, nullptr));
  Cur->recordAttributes(S);
}

void DeclaratorBuilder::buildSpecialization(const ElemSyntax *S) {
  push(new ImplicitEmptyTemplateParamsDeclarator(S, nullptr));
  push(new SpecializationDeclarator(S, nullptr));
  Cur->recordAttributes(S);
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
    buildIdentifier(Name);
    return Result;
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
  if (IsInsideEnum) {
    if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
      buildIdentifier(Name);
      return Result;
    }
  }

  VisitSyntax(S);
  return Result;

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

void DeclaratorBuilder::buildIdentifier(const AtomSyntax *S) {
  // Don't bother with the unnamed name ("_")
  if (S->getToken().hasKind(tok::AnonymousKeyword)) {
    auto *D = new IdentifierDeclarator(S, nullptr);
    D->recordAttributes(S);
    push(D);
    return;
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
        return;
      }
    } else if (OriginalName.startswith("literal\"")) {
      if (!S->getFusionArg()) {
        SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_user_defined_literal_invalid_identifier)
                             << /*invalid suffix*/ 0 << 0;
        return;
      }
      if (const AtomSyntax *Suffix = dyn_cast<AtomSyntax>(S->getFusionArg())){
        UDLSuffix = Suffix->getSpelling();
      }

    } else if (OriginalName.startswith("conversion\"")) {
      ConversionTypeSyntax = S->getFusionArg();
    }
  }

  auto *D = new IdentifierDeclarator(S, nullptr);
  D->recordAttributes(S);
  D->setUserDefinedLiteralSuffix(UDLSuffix);
  push(D);
}

void DeclaratorBuilder::NodeLabeler::operator()(const Syntax *S) {
  if (!isa<CallSyntax>(S))
    return;

  const CallSyntax *Op = cast<CallSyntax>(S);
  FusedOpKind OpKind = getFusedOpKind(SemaRef, Op);
  switch(OpKind) {
  case FOK_Equals:
  case FOK_Exclaim:
    return this->operator()(Op->getArgument(0));
  case FOK_Colon:
    break;
  default:
    return;
  }

  ParentRAII AddParent(S, InteriorNodes, NodeParents);
  // Label the LHS subtree first, then the root, then the RHS subtree.
  // This way, any node on the LHS will be less than the root, and
  // any node on the RHS will be greater than the root.
  ConstSyntaxVisitor<NodeLabeler>::Visit(Op->getArgument(0));
  if (insertParent(S, Label++))
    NodeLabels.RootLabel = Label;
  ConstSyntaxVisitor<NodeLabeler>::Visit(Op->getArgument(1));
}

void DeclaratorBuilder::NodeLabeler::VisitGoldCallSyntax(const CallSyntax *S) {
  // If we don't know what the callee is, we need to label it.
  FusedOpKind Op = getFusedOpKind(SemaRef, S);
  if (Op == FOK_Unknown)
    return ConstSyntaxVisitor<NodeLabeler>::Visit(S->getCallee());

  // If there are no arguments, there's nothing more to do.
  if (S->getNumArguments() == 0) {
    insertChild(S, Label++);
    return;
  }

  ParentRAII AddParent(S, InteriorNodes, NodeParents);
  unsigned I = 0;
  ConstSyntaxVisitor<NodeLabeler>::Visit(S->getArgument(I));
  insertParent(S, Label++);
  for (++I; I < S->getNumArguments(); ++I)
    ConstSyntaxVisitor<NodeLabeler>::Visit(S->getArgument(I));
}

void DeclaratorBuilder::NodeLabeler::VisitGoldElemSyntax(const ElemSyntax *S) {
  ParentRAII AddParent(S, InteriorNodes, NodeParents);
  ConstSyntaxVisitor<NodeLabeler>::Visit(S->getObject());
  insertParent(S, Label++);
  for (const Syntax *Arg : S->getArguments()->children())
    ConstSyntaxVisitor<NodeLabeler>::Visit(Arg);
}

void DeclaratorBuilder::NodeLabeler::VisitGoldAtomSyntax(const AtomSyntax *S) {
  insertChild(S, Label++);
}

void DeclaratorBuilder::NodeLabeler::VisitGoldListSyntax(const ListSyntax *S) {
  // Only label this if it is the LHS of an operator"->" call.
  if (!InteriorNodes.empty() && isa<CallSyntax>(InteriorNodes.top())) {
    const CallSyntax *Call = cast<CallSyntax>(InteriorNodes.top());
    // Determine if we're on the LHS of the parent. If we are, the parent will
    // not have its label entered yet.
    auto It = NodeLabels.find(Call);
    bool ParentSeen = It != NodeLabels.end();
    if (getFusedOpKind(SemaRef, Call) == FOK_Arrow && !ParentSeen)
      insertChild(S, Label++);
  }

  // Elsewise, see through the list
  for (const Syntax *Param : S->children())
    ConstSyntaxVisitor<NodeLabeler>::Visit(Param);
}

DeclaratorBuilder::NodeLabeler::ParentRAII::ParentRAII(const Syntax *S,
                                      std::stack<const Syntax *> &InteriorNodes,
                                                       ParentMapTy &NodeParents)
  : InteriorNodes(InteriorNodes), NodeParents(NodeParents)
{
  if (!InteriorNodes.empty())
    NodeParents.insert({S, InteriorNodes.top()});
  InteriorNodes.push(S);
}

DeclaratorBuilder::NodeLabeler::ParentRAII::~ParentRAII() {
  InteriorNodes.pop();
}

bool DeclaratorBuilder::NodeLabeler::insertParent(const Syntax *S,
                                                 unsigned Label) {
  return NodeLabels.insert({S, Label}).second;
}

bool DeclaratorBuilder::NodeLabeler::insertChild(const Syntax *S,
                                                 unsigned Label) {
  return insertChild(S, InteriorNodes.top(), Label);
}

bool DeclaratorBuilder::NodeLabeler::insertChild(const Syntax *C,
                                                 const Syntax *P,
                                                 unsigned Label) {
  NodeParents.insert({C, P});
  return NodeLabels.insert({C, Label}).second;
}

const Syntax *DeclaratorBuilder::getParent(const Syntax *S) {
  auto Res = NodeParents.find(S);
  if (Res == NodeParents.end())
    return nullptr;
  return Res->second;
}

} // end namespace gold
