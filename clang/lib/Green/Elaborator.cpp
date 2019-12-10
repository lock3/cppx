#include "clang/AST/Stmt.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/SyntaxContext.h"
#include "clang/Lex/Preprocessor.h"

namespace green {

using namespace clang;

Elaborator::Elaborator(SyntaxContext &Context, GreenSema &SemaRef)
  : Context(Context), SemaRef(SemaRef), PP(SemaRef.getPP())
{
  Decl *TUDecl = Context.ClangContext.getTranslationUnitDecl();
  GreenScope *TUScope = new (Context) GreenScope(TUDecl, nullptr);
  SemaRef.PushScope(TUScope);
}

Decl *
Elaborator::elaborateDecl(const Syntax *S) {
  if (isa<CallSyntax>(S))
    return elaborateDeclForCall(cast<CallSyntax>(S));

  return nullptr;
}

// Get the clang::QualType described by an operator':' call.
static QualType
getOperatorColonType(Elaborator const &E, const CallSyntax *S) {
  // Get the argument list of an operator':' call. This should have
  // two arguments, the entity (argument 1) and its type (argument 2).
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

  // Right now this has to be an explicitly named type.
  if (!isa<AtomSyntax>(ArgList->Elems[1]))
    assert(false && "Evaluated types not supported yet");
  const AtomSyntax *Typename = cast<AtomSyntax>(ArgList->Elems[1]);
 
  auto BuiltinMapIter = E.BuiltinTypes.find(Typename->Tok.getSpelling());
  if (BuiltinMapIter == E.BuiltinTypes.end())
    assert(false && "Only builtin types are supported right now.");

  return BuiltinMapIter->second;
}

// Create a Clang Declaration for a call to operator':', in other words,
// a variable with an explicit type. Note that this function does /not/
// handle an operator':' on a function abstract. 
// Called from Elaborator::elaborateDeclForCall()
static Decl *
handleOperatorColon(SyntaxContext &Context, GreenSema &SemaRef,
                    Preprocessor &PP, Elaborator const &E,
                    const CallSyntax *S) {
  ASTContext &ClangContext = Context.ClangContext;

  QualType DeclType = getOperatorColonType(E, S);
  TypeSourceInfo *TInfo = ClangContext.CreateTypeSourceInfo(DeclType);

  DeclContext *TUDC =
    Decl::castToDeclContext(ClangContext.getTranslationUnitDecl());

  // We have a type and a name, so create a declaration.
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
  const AtomSyntax *Declarator = cast<AtomSyntax>(ArgList->Elems[0]);
  IdentifierInfo *II = PP.getIdentifierInfo(Declarator->Tok.getSpelling());
  if (Declarator->isParam()) {
    ParmVarDecl *PVD =
      ParmVarDecl::Create(ClangContext, TUDC, SourceLocation(),
                          SourceLocation(), II, TInfo->getType(), TInfo,
                          SC_Extern, /*DefaultArg=*/nullptr);
    // We'll add these to the function scope later.
    PVD->dump();
    return PVD;
  } else {
    VarDecl *VD =
      VarDecl::Create(ClangContext, TUDC, SourceLocation(),
                      SourceLocation(), II, TInfo->getType(), TInfo,
                      SC_Extern);
    SemaRef.getCurScope()->addDecl(VD);
    VD->getDeclContext()->addDecl(VD);
    VD->dump();
    return VD;
  }
}

// Create a Clang Declaration for a call to operator'!', in other words,
// a function with a definition.
// Called from Elaborator::elaborateDeclForCall()
static Decl *
handleOperatorExclaim(SyntaxContext &Context, Preprocessor &PP,
                      GreenSema &SemaRef, Elaborator &E, const CallSyntax *S) {
  ASTContext &ClangContext = Context.ClangContext;

  // Get the args for an operator'!' call. This should always have two
  // arguments: a (possibly typed) function declarator and a function
  // definition. We are not concerned with the defintion here.
  const ListSyntax *Args = cast<ListSyntax>(S->Args());

  const CallSyntax *Declarator = cast<CallSyntax>(Args->Elems[0]);
  const AtomSyntax *DeclaratorCallee
    = cast<AtomSyntax>(Declarator->Callee());
  IdentifierInfo *DeclaratorSpelling =
    PP.getIdentifierInfo(DeclaratorCallee->Tok.getSpelling());
  IdentifierInfo *Name = nullptr;

  // The parameters of the declared function.
  const ArraySyntax *Parameters;

  QualType ReturnType;
  // If the declarator is an operator':' call, we have an explicit return
  // type.
  if (DeclaratorSpelling == SemaRef.OperatorColonII) {
    const CallSyntax *OperatorColonCall = cast<CallSyntax>(Declarator);

    ReturnType = getOperatorColonType(E, OperatorColonCall);

    // Let's try to wrestle the parameters out of this operator':' call.
    const ListSyntax *OperatorColonArgList =
      cast<ListSyntax>(OperatorColonCall->Args());
    // The first argument of the operator':' call is the function itself.
    const CallSyntax *TheCall =
      cast<CallSyntax>(OperatorColonArgList->Elems[0]);
    // Now let's get the array of parameters from the function.
    Parameters = cast<ArraySyntax>(TheCall->Args());

    // Let's get the name of the function while we're here.
    Name =  PP.getIdentifierInfo(
      cast<AtomSyntax>(TheCall->Callee())->Tok.getSpelling());
  } else {
    // Otherwise, we have a bare function definition.
    // Just use an auto return type.
    ReturnType = ClangContext.getAutoDeductType();
    Parameters = cast<ArraySyntax>(Declarator->Args());
    Name = DeclaratorSpelling;
  }

  // Make some preparations to create an actual FunctionDecl.
  llvm::SmallVector<ParmVarDecl *, 4> ParameterDecls;
  llvm::SmallVector<QualType, 4> ParameterTypes;
  const ListSyntax *ParameterList = cast<ListSyntax>(Parameters->Elems[0]);
  for (const Syntax *Param : ParameterList->children()) {
    ParmVarDecl *PVD = cast<ParmVarDecl>(E.elaborateDecl(Param));
    ParameterDecls.push_back(PVD);
    ParameterTypes.push_back(PVD->getType());
  }

  // Create the FunctionDecl.
  FunctionProtoType::ExtProtoInfo EPI;
  QualType FnTy =
    ClangContext.getFunctionType(ReturnType, ParameterTypes, EPI);
  TypeSourceInfo *FnTInfo = ClangContext.CreateTypeSourceInfo(FnTy);
  DeclContext *TUDC =
    Decl::castToDeclContext(ClangContext.getTranslationUnitDecl());

  FunctionDecl *FD =
    FunctionDecl::Create(ClangContext, TUDC, SourceLocation(),
                         SourceLocation(), DeclarationName(Name),
                         FnTInfo->getType(), FnTInfo, SC_Extern);
  FD->setParams(ParameterDecls);
  FD->getDeclContext()->addDecl(FD);

  GreenScope *Scope = new (Context) GreenScope(FD, SemaRef.getCurScope());
  SemaRef.PushScope(Scope);

  // The parameters are currently owned by the translation unit, so let's
  // move them to the function itself.
  for (auto Param : FD->parameters()) {
    Param->setOwningFunction(FD);
    SemaRef.getCurScope()->addDecl(Param);
    Param->getDeclContext()->addDecl(Param);
  }

  // Now let's elaborate the function body.
  // elaborateDecl(Args->Elems[1]);

  FD->dump();
  return FD;
}

// Create a clang::Decl for a call to operator'=', i.e., an initialized variable
// or single-line function.
// We need to elaborate the initializer here as well.
// Called from Elaborator::elaborateDeclForCall()
static Decl *
handleOperatorEquals(SyntaxContext &Context, GreenSema &SemaRef,
                     Preprocessor &PP, Elaborator &E,
                     const CallSyntax *S) {
  ASTContext &ClangContext = Context.ClangContext;

  // Get the args for the operator'=' call. As usual, we expect binary
  // operands here: some sort of named entity and an expression.
  const ListSyntax *Args = cast<ListSyntax>(S->Args());

  VarDecl *EntityVD = nullptr;

  // If the named declaration is a call, then it is an operator':' call;
  // we have some sort of explicitly typed  entity.
  if (isa<CallSyntax>(Args->Elems[0])) {
    const CallSyntax *Entity = cast<CallSyntax>(Args->Elems[0]);

    Decl *EntityDecl = handleOperatorColon(Context, SemaRef, PP, E, Entity);

    // FIXME: Single-line functions are unimplemented. I don't think
    // default arguments are supported by the language.
    if (isa<ParmVarDecl>(EntityDecl) || !isa<VarDecl>(EntityDecl))
      assert(false && "Unsupported declaration.");

    EntityVD = cast<VarDecl>(EntityDecl);
  }

  // Now let's elaborate the initializer as a clang::Expr.
  ExprElaborator ExprElab(ClangContext, SemaRef);
  const Syntax *Init = Args->Elems[1];

  // FIXME: make this non-atom-specific
  Expr *InitExpr = ExprElab.elaborateExpr(Init, EntityVD->getType());
  EntityVD->setInit(InitExpr);
  EntityVD->dump();
  return EntityVD;
}

Decl *
Elaborator::elaborateDeclForCall(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) && "Unknown call format.");

  const AtomSyntax *Callee = cast<AtomSyntax>(S->Callee());
  const IdentifierInfo *Spelling =
    PP.getIdentifierInfo(Callee->Tok.getSpelling());

  if (Spelling == SemaRef.OperatorColonII)
    return handleOperatorColon(Context, SemaRef, PP, *this, S);
  else if (Spelling == SemaRef.OperatorExclaimII)
    return handleOperatorExclaim(Context, PP, SemaRef, *this, S);
  else if (Spelling == SemaRef.OperatorEqualsII)
    return handleOperatorEquals(Context, SemaRef, PP, *this, S);
  return nullptr;
}

} // namespace green
