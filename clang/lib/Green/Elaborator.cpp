#include "clang/AST/Stmt.h"
#include "clang/Sema/Sema.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/StmtElaborator.h"
#include "clang/Green/SyntaxContext.h"

namespace green {

Elaborator::Elaborator(SyntaxContext &Context, GreenSema &SemaRef)
  : Context(Context), SemaRef(SemaRef)
{
}

clang::Decl *Elaborator::elaborateFile(const Syntax *S) {
  assert(isa<FileSyntax>(S) && "S is not a file");
  startFile(S);
  const FileSyntax *File = cast<FileSyntax>(S);

  // Pass 1. identify declarations in scope.
  // FIXME: Implement this.

  // Pass 2: elaborate top-level declarations and their definitions.
  for (const Syntax *SS : File->children())
    elaborateTopLevelDecl(SS);

  finishFile(S);

  return Context.CxxAST.getTranslationUnitDecl();
}

void Elaborator::startFile(const Syntax *S) {
  clang::Decl *TU = Context.CxxAST.getTranslationUnitDecl();
  SemaRef.enterScope(S, TU);
}

void Elaborator::finishFile(const Syntax *S) {
  SemaRef.leaveScope(S);
  // TODO: Any pending semantic analysis to do here?
}

clang::Decl *Elaborator::elaborateTopLevelDecl(const Syntax *S) {
  // TODO: Look for module-related declarations.
  llvm::errs() << "TOP LEVEL\n";
  S->dump();
  return elaborateDecl(S);
}

clang::Decl *Elaborator::elaborateDecl(const Syntax *S) {
  assert(isa<CallSyntax>(S));
  return elaborateDeclForCall(cast<CallSyntax>(S));
}

// Get the clang::QualType described by an operator':' call.
clang::QualType Elaborator::getOperatorColonType(const CallSyntax *S) const {
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
static clang::Decl *handleOperatorColon(SyntaxContext &Context,
                                        GreenSema &SemaRef,
                                        Elaborator const &E,
                                        const CallSyntax *S) {
  clang::ASTContext &CxxAST = Context.CxxAST;

  clang::QualType DeclType = E.getOperatorColonType(S);
  clang::TypeSourceInfo *TInfo = CxxAST.CreateTypeSourceInfo(DeclType);

  clang::DeclContext *TUDC =
    clang::Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());

  // We have a type and a name, so create a declaration.
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
  const AtomSyntax *Declarator = cast<AtomSyntax>(ArgList->Elems[0]);

  clang::IdentifierInfo *II = &CxxAST.Idents.get(Declarator->Tok.getSpelling());
  if (Declarator->isParam()) {
    clang::ParmVarDecl *PVD =
      clang::ParmVarDecl::Create(CxxAST, TUDC, clang::SourceLocation(),
                                 clang::SourceLocation(), II, TInfo->getType(),
                                 TInfo, clang::SC_Extern,
                                 /*DefaultArg=*/nullptr);
    // We'll add these to the function scope later.
    return PVD;
  } else {
    clang::VarDecl *VD =
      clang::VarDecl::Create(CxxAST, TUDC, clang::SourceLocation(),
                             clang::SourceLocation(), II, TInfo->getType(),
                             TInfo, clang::SC_Extern);
    SemaRef.getCurrentScope()->addDecl(VD);
    VD->getDeclContext()->addDecl(VD);
    return VD;
  }
}

// Create a Clang Declaration for a call to operator'!', in other words,
// a function with a definition.
// Called from Elaborator::elaborateDeclForCall()
static clang::Decl *handleOperatorExclaim(SyntaxContext &Context,
                                          GreenSema &SemaRef,
                                          Elaborator &E,
                                          const CallSyntax *S) {
  clang::ASTContext &CxxAST = Context.CxxAST;

  // Get the args for an operator'!' call. This should always have two
  // arguments: a (possibly typed) function declarator and a function
  // definition. We are not concerned with the defintion here.
  const ListSyntax *Args = cast<ListSyntax>(S->Args());

  const CallSyntax *Declarator = cast<CallSyntax>(Args->Elems[0]);
  const AtomSyntax *DeclaratorCallee
    = cast<AtomSyntax>(Declarator->Callee());
  clang::IdentifierInfo *DeclaratorSpelling =
    &CxxAST.Idents.get(DeclaratorCallee->Tok.getSpelling());
  clang::IdentifierInfo *Name = nullptr;

  // The parameters of the declared function.
  const ArraySyntax *Parameters;

  clang::QualType ReturnType;
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
    Name = &CxxAST.Idents.get(
      cast<AtomSyntax>(TheCall->Callee())->Tok.getSpelling());
  } else {
    // Otherwise, we have a bare function definition.
    // Just use an auto return type.
    ReturnType = CxxAST.getAutoDeductType();
    Parameters = cast<ArraySyntax>(Declarator->Args());
    Name = DeclaratorSpelling;
  }

  // Make some preparations to create an actual FunctionDecl.
  llvm::SmallVector<clang::ParmVarDecl *, 4> ParameterDecls;
  llvm::SmallVector<clang::QualType, 4> ParameterTypes;

  // If we have parameters, create clang ParmVarDecls.
  if (Parameters->NumElems) {
    const ListSyntax *ParameterList = cast<ListSyntax>(Parameters->Elems[0]);
    for (const Syntax *Param : ParameterList->children()) {
      clang::ParmVarDecl *PVD = cast<clang::ParmVarDecl>(E.elaborateDecl(Param));
      ParameterDecls.push_back(PVD);
      ParameterTypes.push_back(PVD->getType());
    }
  }

  // Create the FunctionDecl.
  clang::FunctionProtoType::ExtProtoInfo EPI;
  clang::QualType FnTy =
    CxxAST.getFunctionType(ReturnType, ParameterTypes, EPI);
  clang::TypeSourceInfo *FnTInfo = CxxAST.CreateTypeSourceInfo(FnTy);
  clang::DeclContext *TUDC =
    clang::Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());

  clang::FunctionDecl *FD =
    clang::FunctionDecl::Create(CxxAST, TUDC, clang::SourceLocation(),
                                clang::SourceLocation(),
                                clang::DeclarationName(Name),
                                FnTInfo->getType(),
                                FnTInfo,
                                clang::SC_Extern);
  FD->setParams(ParameterDecls);
  FD->getDeclContext()->addDecl(FD);

  // FIXME: Is this the right syntax for the scope?
  SemaRef.enterScope(S, FD);

  // The parameters are currently owned by the translation unit, so let's
  // move them to the function itself.
  for (auto Param : FD->parameters()) {
    Param->setOwningFunction(FD);
    SemaRef.getCurrentScope()->addDecl(Param);
    Param->getDeclContext()->addDecl(Param);
  }

  // FIXME: Start a new scope for the function definition?

  // Now let's elaborate the function body.
  StmtElaborator BodyElaborator(CxxAST, SemaRef);
  clang::Stmt *Body = BodyElaborator.elaborateBlock(Args->Elems[1]);
  FD->setBody(Body);

  // Leave the scope of the function declaration.
  SemaRef.leaveScope(S);

  // FD->dump();
  return FD;
}

// Create a clang::Decl for a call to operator'=', i.e., an initialized variable
// or single-line function.
// We need to elaborate the initializer here as well.
// Called from Elaborator::elaborateDeclForCall()
static clang::Decl *handleOperatorEquals(SyntaxContext &Context,
                                         GreenSema &SemaRef,
                                         Elaborator &E,
                                         const CallSyntax *S) {
  clang::ASTContext &CxxAST = Context.CxxAST;

  // Get the args for the operator'=' call. As usual, we expect binary
  // operands here: some sort of named entity and an expression.
  const ListSyntax *Args = cast<ListSyntax>(S->Args());

  clang::VarDecl *EntityVD = nullptr;

  // True if this variable is declared with auto type.
  bool AutoType = false;

  // If the named declaration is a call, then it is an operator':' call;
  // we have some sort of explicitly typed  entity.
  if (isa<CallSyntax>(Args->Elems[0])) {
    const CallSyntax *Entity = cast<CallSyntax>(Args->Elems[0]);

    clang::Decl *EntityDecl = handleOperatorColon(Context, SemaRef, E, Entity);

    // FIXME: Single-line functions are unimplemented. I don't think
    // default arguments are supported by the language.
    if (isa<clang::ParmVarDecl>(EntityDecl) || !isa<clang::VarDecl>(EntityDecl))
      assert(false && "Unsupported declaration.");

    EntityVD = cast<clang::VarDecl>(EntityDecl);
  } else if (isa<AtomSyntax>(Args->Elems[0])) {
    AutoType = true;

    // Perform a lookup on this name. If we didn't find anything, declare
    // it as an auto type.

    // Declare the variable.
    const AtomSyntax *Declarator = cast<AtomSyntax>(Args->Elems[0]);
    clang::IdentifierInfo *II =
      &CxxAST.Idents.get(Declarator->Tok.getSpelling());
    clang::DeclContext *TUDC =
      clang::Decl::castToDeclContext(CxxAST.getTranslationUnitDecl());
    clang::TypeSourceInfo *TInfo =
      CxxAST.CreateTypeSourceInfo(CxxAST.getAutoDeductType(), 0);

    EntityVD =
      clang::VarDecl::Create(CxxAST, TUDC, clang::SourceLocation(),
                             clang::SourceLocation(), II, TInfo->getType(),
                             TInfo, clang::SC_Extern);
    SemaRef.getCurrentScope()->addDecl(EntityVD);
    EntityVD->getDeclContext()->addDecl(EntityVD);
  }

  // Now let's elaborate the initializer as a clang::Expr.
  ExprElaborator ExprElab(CxxAST, SemaRef);
  const Syntax *Init = Args->Elems[1];

  clang::Expr *InitExpr = ExprElab.elaborateExpr(Init);
  if (!InitExpr)
    return nullptr;

  if (AutoType) {
    clang::Sema &CxxSema = SemaRef.getCxxSema();

    clang::QualType DeducedType;
    if (CxxSema.DeduceAutoType(EntityVD->getTypeSourceInfo(),
                               InitExpr, DeducedType) == clang::Sema::DAR_Failed) {
      llvm::errs() << "Failed to deduce type of expression.\n";
      return nullptr;
    }
    EntityVD->setType(DeducedType);
  }

  EntityVD->setInit(InitExpr);
  return EntityVD;
}

clang::Decl *Elaborator::elaborateDeclForCall(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) && "Unknown call format.");

  const AtomSyntax *Callee = cast<AtomSyntax>(S->Callee());
  const clang::IdentifierInfo *Spelling =
    &Context.CxxAST.Idents.get(Callee->Tok.getSpelling());

  if (Spelling == SemaRef.OperatorColonII)
    return handleOperatorColon(Context, SemaRef, *this, S);
  else if (Spelling == SemaRef.OperatorExclaimII)
    return handleOperatorExclaim(Context, SemaRef, *this, S);
  else if (Spelling == SemaRef.OperatorEqualsII)
    return handleOperatorEquals(Context, SemaRef, *this, S);
  return nullptr;
}

} // namespace green
