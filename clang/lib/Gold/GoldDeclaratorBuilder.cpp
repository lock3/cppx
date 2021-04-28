#include "clang/AST/ExprCppx.h"

#include "clang/Gold/GoldDeclaratorBuilder.h"
#include "clang/Gold/GoldDeclarationBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSymbol.h"

namespace gold {

using ParentMapTy = DeclaratorBuilder::ParentMapTy;

Declarator *DeclaratorBuilder::operator()(const Syntax *S) {
  NodeLabeler LabelNodes(SemaRef, NodeLabels, NodeParents);
  LabelNodes(S);

  gold::Scope *CurrentScope = SemaRef.getCurrentScope();
  gold::Declarator *Dcl = nullptr;
  switch(CurrentScope->getKind()) {
  case SK_Namespace:
    return handleNamespaceScope(S);
  case SK_Parameter:
    return handleParameterScope(S);
  case SK_Template:
    return handleTemplateScope(S);
  case SK_Function:
    return handleFunctionScope(S);
  case SK_Block:
    return handleBlockScope(S);
  case SK_Class:
    return handleClassScope(S);
  case SK_Control:
    return handleControlScope(S);
  case SK_Catch:
    return handleCatchScope(S);
  case SK_Enum:
    return handleEnumScope(S);
  }

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

static inline bool isPostfixCaret(const CallSyntax *S) {
  if (!S->getCallee() || !isa<AtomSyntax>(S->getCallee()))
    return false;
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  return Callee->getSpelling() == "postfix'^'";
}

static ListSyntax *constructList(SyntaxContext &Ctx, const Syntax *Arg) {
  if (isa<ListSyntax>(Arg))
    return const_cast<ListSyntax *>(cast<ListSyntax>(Arg));
  Syntax *NonConstArg = const_cast<Syntax *>(Arg);
  Syntax **Args = new (Ctx) Syntax *[1];
  Args[0] = NonConstArg;
  return new (Ctx) ListSyntax(Args, 1);
}

static inline bool isTypeOperator(const FusedOpKind K) {
  switch (K) {
  case FOK_Const:
  case FOK_RRef:
  case FOK_Ref:
    return true;
  default:
    return false;
  }
}

static inline bool isBuiltinFunctionName(tok::TokenKind K) {
  switch (K) {
  case tok::StaticCastKeyword:
  case tok::DynamicCastKeyword:
  case tok::ReinterpretCastKeyword:
  case tok::ConstCastKeyword:
  case tok::ConstExprKeyword:
  case tok::AlignOfKeyword:
  case tok::SizeOfKeyword:
  case tok::DeclTypeKeyword:
  case tok::TypeidKeyword:
  case tok::TypeIdKeyword:
    return true;
  default:
    return false;
  }

}

void DeclaratorBuilder::VisitGoldCallSyntax(const CallSyntax *S) {
  const AtomSyntax *Callee = dyn_cast<AtomSyntax>(S->getCallee());
  FusedOpKind Op = getFusedOpKind(SemaRef, S);

  if (Op == FOK_Preattr) {
    Owner.Attrs.insert(S->getArgument(0));
    return VisitSyntax(S->getArgument(1));
  }

  if (Op == FOK_Postattr) {
    Owner.Attrs.insert(S->getArgument(1));
    return VisitSyntax(S->getArgument(0));
  }

  // A normal function declaration.
  if (Op == FOK_Unknown && !isPostfixCaret(S)) {
    if (Callee) {
      if (Callee->getSpelling() == "...")
        return buildType(S);

      Token Tok = Callee->getToken();
      if (isBuiltinFunctionName(Tok.getKind()))
        return buildType(S);
      if (Tok.isFused() && Callee->getFusionBase() == tok::Conversion) {
        Owner.ConversionTypeSyntax = Callee->getFusionArg();
      }
    }

    VisitSyntax(S->getCallee());
    buildFunction(S);
    // This might be a conversion function.
    if (Owner.ConversionTypeSyntax)
      buildType(Owner.ConversionTypeSyntax);
    return;
  }

  if (Op == FOK_MemberAccess) {
    bool MethodType = false;
    const Syntax *AccessParent = getParent(S);
    if (AccessParent && !isLeftOfRoot(NodeLabels, S)) {
      if (const CallSyntax *ParentCall = dyn_cast<CallSyntax>(AccessParent))
        MethodType = getFusedOpKind(SemaRef, ParentCall) == FOK_Arrow;

      if (!MethodType)
        return buildType(S);
    }

    if (S->getNumArguments() == 1) {
      buildGlobalNameSpecifier(S);
      return VisitSyntax(S->getArgument(0));
    }

    if (isa<AtomSyntax>(S->getArgument(0))) {
      buildNestedNameSpecifier(cast<AtomSyntax>(S->getArgument(0)));
    } else {
      ExprElaborator::BooleanRAII B(NestedTemplateName, true);
      VisitSyntax(S->getArgument(0));
    }

    if (MethodType) {
      if (!isa<ListSyntax>(S->getArgument(1))) {
        unsigned DiagID =
          SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                        "expected parameter list");
        SemaRef.Diags.Report(S->getArgument(1)->getLoc(), DiagID);
        return;
      }

      return buildFunction(cast<ListSyntax>(S->getArgument(1)));
    }

    VisitSyntax(S->getArgument(1));
    Name = End;
    return;
  }

  if (Op == FOK_Equals) {
    // This checks if a declaration already exists in a parent scope.
    // For example, we are in a member function and are accessing a member.
    if (const AtomSyntax *LHS = dyn_cast<AtomSyntax>(S->getArgument(0))) {
      clang::DeclarationNameInfo DNI({
          &Context.CxxAST.Idents.get(LHS->getSpelling())
        }, S->getLoc());
      if (!Owner.ContextDeclaresNewName &&
          !SemaRef.checkUnqualifiedNameIsDecl(DNI)) {
        // this may need an error message depending on context.
        return;
      }
    }

    Owner.InitExpr = S->getArgument(1);
    Owner.InitOperatorUsed = IK_Equals;
    return VisitSyntax(S->getArgument(0));
  } else if (Op == FOK_Exclaim) {
    Owner.InitExpr = S->getArgument(1);
    Owner.InitOperatorUsed = IK_Exclaim;
    return VisitSyntax(S->getArgument(0));
  } else if (Op == FOK_In) {
    return VisitSyntax(S->getArgument(0));
  } else if (Op == FOK_Colon) {
    Owner.RequiresDeclOrError = true;
  }

  if (isTypeOperator(Op))
    return buildType(S);

  // This is an array prefix.
  if (Op == FOK_Brackets) {
    bool IsPartialSpecialization = false;
    if (!isLeftOfRoot(NodeLabels, S)) {
      SuppressDiagnosticsRAII Suppressor(SemaRef.getCxxSema());
      if (const Syntax *Arg = S->getArgument(0)) {
        // if we have something like []type, this is an implicit sized array.
        if (const ListSyntax *ArgList = dyn_cast<ListSyntax>(Arg))
          // if (!ArgList->getNumChildren())
            goto END;

        ExprElaborator Elab(Context, SemaRef);
        clang::Expr *E = Elab.elaborateExpr(Arg);
        // We don't know what we got, so we're gonna assume it's a partial
        // specialization.
        if (!E || (E && E->getType()->isTypeOfTypes()))
          IsPartialSpecialization = true;
      }
    }

  END:
    if (IsPartialSpecialization) {
      buildPartialSpecialization(constructList(Context, S->getArgument(0)));
      // Result->recordAttributes(S);
      return VisitSyntax(S->getArgument(1));
    } else {
      buildArray(S->getArgument(0));
      return VisitSyntax(S->getArgument(1));
    }
  }

  if (Op == FOK_Map) {
    if (!isa<ListSyntax>(S->getArgument(0))) {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "unexpected template parameter list");
      SemaRef.Diags.Report(S->getArgument(0)->getLoc(), DiagID);
    } else {
      buildTemplateParams(cast<ListSyntax>(S->getArgument(0)));
    }

    return VisitSyntax(S->getArgument(1));
  }

  bool LeftOfRoot = isLeftOfRoot(NodeLabels, S);
  if (!LeftOfRoot && (isPostfixCaret(S) || Op == FOK_Caret))
    buildPointer(S->getArgument(0));

  for (const Syntax *Arg : S->getArguments()->children())
    VisitSyntax(Arg);

  if (LeftOfRoot && (isPostfixCaret(S) || Op == FOK_Caret))
    buildPointer(S->getArgument(0));
}

void DeclaratorBuilder::VisitGoldElemSyntax(const ElemSyntax *S) {
  if (isLeftOfRoot(NodeLabels, S) && isa<ElemSyntax>(S->getObject())) {
    Visit(S->getObject());
    return buildPartialSpecialization(S);
  }

  // Check if we are more than one element deep and on the left,
  // thus certainly declaring a specialization.
  if (const Syntax *Parent = getParent(S)) {
    if (isLeftOfRoot(NodeLabels, S) && isa<ElemSyntax>(Parent)) {
      buildName(S->getObject());
      return buildTemplateParams(cast<ListSyntax>(S->getArguments()));
    }
  }

  // Check if this is a specialization as a type.
  if (getParent(S) && !isLeftOfRoot(NodeLabels, S)) {
    SuppressDiagnosticsRAII Suppressor(SemaRef.getCxxSema());
    clang::Expr *E = ExprElaborator(Context, SemaRef).elaborateExpr(S->getObject());
    // We know this is a specialization if the base of the element is a type.
    // A specialization is just a type.
    if (E &&
        (isa<clang::CppxDeclRefExpr>(E) || E->getType()->isTypeOfTypes()))
      return buildType(S);
  }

  VisitSyntax(S->getObject());
  if (isLeftOfRoot(NodeLabels, S) || !getParent(S)) {
    const ListSyntax *Args = dyn_cast<ListSyntax>(S->getArguments());
    if (!Args)
      return;

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
      if (!E || E->getType()->isTypeOfTypes()) {
        if (isa<ElemSyntax>(S->getObject())) {
          ExprElaborator::BooleanRAII T(ExplicitTemplateSpecialization, true);
          return buildTemplate(S);
        }

        return buildTemplate(S);
      }
    }

    ExprElaborator BaseElab(Context, SemaRef);
    clang::Expr *Base = BaseElab.elaborateExpr(S->getObject());

    if (!Args->getNumChildren() && !Base)
      return buildArray(Args);

    if (Base)
      return buildSpecialization(S);
  }

  buildArray(S->getArguments());
}

void DeclaratorBuilder::VisitGoldMacroSyntax(const MacroSyntax *S) {
  if (const AtomSyntax *Call = dyn_cast<AtomSyntax>(S->getCall()))
    if (Call->getToken().hasKind(tok::UsingKeyword))
      return buildUsingDirectiveDeclarator(S);

  if (Owner.RequiresDeclOrError)
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_declaration_kind)
      << 1;
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
  if (isLeftOfRoot(NodeLabels, S) || !getParent(S)) {
    if (NestedTemplateName)
      return buildNestedNameSpecifier(S);
    return buildIdentifier(S);
  }

  buildType(S);
}

void DeclaratorBuilder::VisitGoldLiteralSyntax(const LiteralSyntax *S) {
  tok::TokenKind K = S->getToken().getKind();
  if (K >= tok::VoidKeyword && K < tok::AnonymousKeyword)
    buildType(S);
}

void DeclaratorBuilder::VisitGoldErrorSyntax(const ErrorSyntax *S) {
  buildError(S);
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

  if (const AtomSyntax *Name = dyn_cast<AtomSyntax>(S)) {
    if (NestedTemplateName)
      return VisitGoldAtomSyntax(Name);

    return buildIdentifier(Name);
  }

    unsigned ErrorIndicator = 0;
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, dyn_cast<CallSyntax>(S));
    switch(OpKind) {
      case FOK_MemberAccess:{
        if (const auto *IdName = dyn_cast<AtomSyntax>(Call->getArgument(1)))
          return;

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
  if (Owner.RequiresDeclOrError)
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_declaration_kind)
                         << ErrorIndicator;
}

void DeclaratorBuilder::buildError(const ErrorSyntax *S) {
  push(new ErrorDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildGlobalNameSpecifier(const CallSyntax *S) {
  auto Result = new GlobalNameSpecifierDeclarator(S, nullptr);
  // Result->recordAttributes(S);
  push(Result);
}

void DeclaratorBuilder::buildNestedNameSpecifier(const AtomSyntax *S) {
  auto Result = new NestedNameSpecifierDeclarator(S, nullptr);
  // Result->recordAttributes(S);
  push(Result);
}

void DeclaratorBuilder::buildArray(const Syntax *S) {
  push(new ArrayDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildPointer(const Syntax *S) {
  push(new PointerDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildType(const Syntax *S) {
  push(new TypeDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildFunction(const CallSyntax *S) {
  push(new FunctionDeclarator(S->getArguments(), nullptr));
}

void DeclaratorBuilder::buildFunction(const ListSyntax *S) {
  push(new FunctionDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildTemplateParams(const ListSyntax *S) {
  push(new TemplateParamsDeclarator(S, nullptr));
  // Cur->recordAttributes(S);
}

void DeclaratorBuilder::buildTemplateParams(const ElemSyntax *S) {
  buildTemplateParams(cast<ListSyntax>(S->getArguments()));
  // Result->recordAttributes(S);
}

void DeclaratorBuilder::buildSpecialization(const ElemSyntax *S) {
  if (!ExplicitTemplateSpecialization)
    push(new ImplicitEmptyTemplateParamsDeclarator(S, nullptr));
  push(new SpecializationDeclarator(cast<ListSyntax>(S->getArguments()),
                                    nullptr));
  // Attach these attributes to the name declarator!
  // Result->recordAttributes(S);
  // Cur->recordAttributes(S);
}

void DeclaratorBuilder::buildPartialSpecialization(const ListSyntax *S) {
  push(new SpecializationDeclarator(S, nullptr));
}

void DeclaratorBuilder::buildPartialSpecialization(const ElemSyntax *S) {
  push(new SpecializationDeclarator(cast<ListSyntax>(S->getArguments()),
                                    nullptr));
  // Result->recordAttributes(S);
}

void DeclaratorBuilder::buildUsingDirectiveDeclarator(const MacroSyntax *S) {
  Owner.InitExpr = S;
  push (new UsingDirectiveDeclarator(S->getCall()->getLoc(), S));
}

Declarator *DeclaratorBuilder::handleNamespaceScope(const Syntax *S) {
  Owner.EnableFunctions = true;
  Owner.EnableNamespaceDecl = true;
  Owner.EnableTags = true;
  Owner.EnableAliases = true;
  Owner.EnableTemplateParameters = false;
  Owner.RequireTypeForVariable = false;
  Owner.EnableNestedNameSpecifiers = true;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  Owner.ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleParameterScope(const Syntax *S) {
  Owner.EnableFunctions = false;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = false;
  Owner.EnableAliases = false;  // TODO: This may need to be true in the future.
                          // But I'm not sure how we could pass a namespace as
                          // a parameter yet.
  Owner.EnableTemplateParameters = false;
  Owner.RequireTypeForVariable = true;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  Owner.ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleTemplateScope(const Syntax *S) {
  // This is for template parameters.
  Owner.EnableFunctions = false;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = false;
  Owner.EnableAliases = false;
  // Template parameters cannot have template parameters unless they
  // are template template parameters, in which case they should be specified
  // differently.
  Owner.EnableTemplateParameters = false;
  Owner.RequireTypeForVariable = true;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleFunctionScope(const Syntax *S) {
  Owner.EnableFunctions = true;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = true;
  Owner.EnableAliases = true;
  Owner.EnableTemplateParameters = false;
  Owner.RequireTypeForVariable = false;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = true;
  Owner.RequireTypeForFunctions = true;
  Owner.RequiresDeclOrError = false;
  Owner.ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleBlockScope(const Syntax *S) {
  Owner.EnableFunctions = true;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = true;
  Owner.EnableAliases = true;
  Owner.EnableTemplateParameters = false;
  Owner.RequireTypeForVariable = false;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = true;
  Owner.RequireTypeForFunctions = true;
  Owner.RequiresDeclOrError = false;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleClassScope(const Syntax *S) {
  Owner.EnableFunctions = true;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = true;
  Owner.EnableAliases = true;
  Owner.EnableTemplateParameters = true;
  Owner.RequireTypeForVariable = true;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  Owner.AllowShortCtorAndDtorSyntax = true;
  Owner.ContextDeclaresNewName = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleControlScope(const Syntax *S) {
  Owner.EnableFunctions = false;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = false;
  Owner.EnableAliases = false;
  Owner.RequireTypeForVariable = false;
  Owner.EnableTemplateParameters = false;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = false;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleEnumScope(const Syntax *S) {
  Owner.EnableFunctions = false;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = false;
  Owner.EnableAliases = false;
  Owner.RequireTypeForVariable = false;
  Owner.EnableTemplateParameters = false;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  Owner.IsInsideEnum = true;
  Owner.ContextDeclaresNewName = true;
  // Special case where enum values are allowed to just be names.
  if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
    buildIdentifier(Name);
    return Result;
  }
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::handleCatchScope(const Syntax *S) {
  Owner.EnableFunctions = false;
  Owner.EnableNamespaceDecl = false;
  Owner.EnableTags = false;
  Owner.EnableAliases = false;
  Owner.RequireTypeForVariable = true;
  Owner.EnableTemplateParameters = false;
  Owner.EnableNestedNameSpecifiers = false;
  Owner.RequireAliasTypes = false;
  Owner.RequireTypeForFunctions = false;
  Owner.RequiresDeclOrError = true;
  return makeDeclarator(S);
}

Declarator *DeclaratorBuilder::makeDeclarator(const Syntax *S) {
  // Handle a special case of an invalid enum identifier `.[name]`
  // without an assignment operator.
  if (Owner.IsInsideEnum) {
    if (const auto *Name = dyn_cast<AtomSyntax>(S)) {
      buildIdentifier(Name);
      return Result;
    }
  }

  if (isa<MacroSyntax>(S)) {
    VisitSyntax(S);
    return Result;
  }

  const auto *Call = dyn_cast<CallSyntax>(S);
  if (!Call) {
    if (Owner.RequiresDeclOrError)
      SemaRef.Diags.Report(S->getLoc(),
                           clang::diag::err_invalid_declaration_kind)
                           << 2;
    return nullptr;
  }


  // None of these operators can be the root of a declaration,
  // with the exception of very specific contexts.
  switch(getFusedOpKind(SemaRef, Call)) {
  case FOK_Colon:
    Owner.RequiresDeclOrError = true;
    break;
  case FOK_Postattr:
  case FOK_Preattr:
    if (Owner.IsInsideEnum)
      break;
    LLVM_FALLTHROUGH;
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
  case FOK_DotCaret: {
    if (Owner.RequiresDeclOrError) {
      if (const AtomSyntax *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
        if (Owner.AllowShortCtorAndDtorSyntax &&
              (Callee->getSpelling() == "constructor"
               || Callee->getSpelling() == "destructor"
               || (Callee->isFused() &&
                   Callee->getFusionBase() == tok::Conversion))) {
          VisitSyntax(Call);
          return Result;
        }
      }

      SemaRef.Diags.Report(Call->getCallee()->getLoc(),
                           clang::diag::err_invalid_declaration_kind)
                           << 2;
    }

    return nullptr;
  }

  default:
    break;
  } // switch (getFusedOpKind(SemaRef, Call)


  VisitSyntax(S);
  return Result;
}

void DeclaratorBuilder::buildIdentifier(const AtomSyntax *S) {
  // Don't bother with the unnamed name ("_")
  if (S->getToken().hasKind(tok::AnonymousKeyword)) {
    auto *D = new IdentifierDeclarator(S, nullptr);
    // D->recordAttributes(S);
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
      Owner.ConversionTypeSyntax = S->getFusionArg();
    }
  }

  auto *D = new IdentifierDeclarator(S, nullptr);
  // D->recordAttributes(S);
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
  if (insertParent(S, Label))
    NodeLabels.RootLabel = Label++;
  ConstSyntaxVisitor<NodeLabeler>::Visit(Op->getArgument(1));
}

void DeclaratorBuilder::NodeLabeler::VisitGoldCallSyntax(const CallSyntax *S) {
  // If we don't know what the callee is, we need to label it.
  FusedOpKind Op = getFusedOpKind(SemaRef, S);
  if (Op == FOK_Unknown && !isPostfixCaret(S))
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
