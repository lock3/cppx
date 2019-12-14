#include "clang/AST/Stmt.h"
#include "clang/Sema/Sema.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/StmtElaborator.h"
#include "clang/Green/SyntaxContext.h"
#include "clang/Lex/Preprocessor.h"

namespace green {

using namespace clang;

Elaborator::Elaborator(SyntaxContext &Context, GreenSema &SemaRef)
  : Context(Context), SemaRef(SemaRef), PP(SemaRef.getPP())
{
  Decl *TUDecl = Context.CxxAST.getTranslationUnitDecl();
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
QualType
Elaborator::getOperatorColonType(const CallSyntax *S) const {
  // Get the argument list of an operator':' call. This should have
  // two arguments, the entity (argument 1) and its type (argument 2).
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

  // Right now this has to be an explicitly named type.
  if (!isa<AtomSyntax>(ArgList->Elems[1]))
    assert(false && "Evaluated types not supported yet");
  const AtomSyntax *Typename = cast<AtomSyntax>(ArgList->Elems[1]);

  auto BuiltinMapIter = BuiltinTypes.find(Typename->Tok.getSpelling());
  if (BuiltinMapIter == BuiltinTypes.end())
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
  ASTContext &CxxAST = Context.CxxAST;

  QualType DeclType = E.getOperatorColonType(S);
  TypeSourceInfo *TInfo = CxxAST.CreateTypeSourceInfo(DeclType);

  DeclContext *TUDC =
    Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());

  // We have a type and a name, so create a declaration.
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
  const AtomSyntax *Declarator = cast<AtomSyntax>(ArgList->Elems[0]);
  IdentifierInfo *II = PP.getIdentifierInfo(Declarator->Tok.getSpelling());
  if (Declarator->isParam()) {
    ParmVarDecl *PVD =
      ParmVarDecl::Create(CxxAST, TUDC, SourceLocation(),
                          SourceLocation(), II, TInfo->getType(), TInfo,
                          SC_Extern, /*DefaultArg=*/nullptr);
    // We'll add these to the function scope later.
    return PVD;
  } else {
    VarDecl *VD =
      VarDecl::Create(CxxAST, TUDC, SourceLocation(),
                      SourceLocation(), II, TInfo->getType(), TInfo,
                      SC_Extern);
    SemaRef.getCurScope()->addDecl(VD);
    VD->getDeclContext()->addDecl(VD);
    return VD;
  }
}

// Create a Clang Declaration for a call to operator'!', in other words,
// a function with a definition.
// Called from Elaborator::elaborateDeclForCall()
static Decl *
handleOperatorExclaim(SyntaxContext &Context, Preprocessor &PP,
                      GreenSema &SemaRef, Elaborator &E, const CallSyntax *S) {
  ASTContext &CxxAST = Context.CxxAST;

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

    ReturnType = E.getOperatorColonType(OperatorColonCall);

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
    ReturnType = CxxAST.getAutoDeductType();
    Parameters = cast<ArraySyntax>(Declarator->Args());
    Name = DeclaratorSpelling;
  }

  // Make some preparations to create an actual FunctionDecl.
  llvm::SmallVector<ParmVarDecl *, 4> ParameterDecls;
  llvm::SmallVector<QualType, 4> ParameterTypes;

  // If we have parameters, create clang ParmVarDecls. 
  if (Parameters->NumElems) {
    const ListSyntax *ParameterList = cast<ListSyntax>(Parameters->Elems[0]);
    for (const Syntax *Param : ParameterList->children()) {
      ParmVarDecl *PVD = cast<ParmVarDecl>(E.elaborateDecl(Param));
      ParameterDecls.push_back(PVD);
      ParameterTypes.push_back(PVD->getType());
    }
  }

  // Create the FunctionDecl.
  FunctionProtoType::ExtProtoInfo EPI;
  QualType FnTy =
    CxxAST.getFunctionType(ReturnType, ParameterTypes, EPI);
  TypeSourceInfo *FnTInfo = CxxAST.CreateTypeSourceInfo(FnTy);
  DeclContext *TUDC =
    Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());

  FunctionDecl *FD =
    FunctionDecl::Create(CxxAST, TUDC, SourceLocation(),
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
  StmtElaborator BodyElaborator(CxxAST, SemaRef);
  Stmt *Body = BodyElaborator.elaborateBlock(Args->Elems[1]);
  // elaborateDecl(Args->Elems[1]);
  FD->setBody(Body);

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
  ASTContext &CxxAST = Context.CxxAST;

  // Get the args for the operator'=' call. As usual, we expect binary
  // operands here: some sort of named entity and an expression.
  const ListSyntax *Args = cast<ListSyntax>(S->Args());

  VarDecl *EntityVD = nullptr;

  // True if this variable is declared with auto type.
  bool AutoType = false;

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
  } else if (isa<AtomSyntax>(Args->Elems[0])) {
    AutoType = true;

    // Perform a lookup on this name. If we didn't find anything, declare
    // it as an auto type.

    // Declare the variable.
    const AtomSyntax *Declarator = cast<AtomSyntax>(Args->Elems[0]);
    IdentifierInfo *II =
      SemaRef.getPP().getIdentifierInfo(Declarator->Tok.getSpelling());
    DeclContext *TUDC =
      Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());
    TypeSourceInfo *TInfo =
      CxxAST.CreateTypeSourceInfo(CxxAST.getAutoDeductType(), 0);

    EntityVD =
      VarDecl::Create(CxxAST, TUDC, SourceLocation(),
                      SourceLocation(), II, TInfo->getType(), TInfo,
                      SC_Extern);
    SemaRef.getCurScope()->addDecl(EntityVD);
    EntityVD->getDeclContext()->addDecl(EntityVD);
  }

  // Now let's elaborate the initializer as a clang::Expr.
  ExprElaborator ExprElab(CxxAST, SemaRef);
  const Syntax *Init = Args->Elems[1];

  Expr *InitExpr = ExprElab.elaborateExpr(Init);
  if (!InitExpr)
    return nullptr;

  if (AutoType) {
    clang::Sema &CxxSema = SemaRef.getCxxSema();

    QualType DeducedType;
    if (CxxSema.DeduceAutoType(EntityVD->getTypeSourceInfo(),
                                 InitExpr, DeducedType) == Sema::DAR_Failed) {
      llvm::errs() << "Failed to deduce type of expression.\n";
      return nullptr;
    }
    EntityVD->setType(DeducedType);
  }

  EntityVD->setInit(InitExpr);
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
