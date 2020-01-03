#include "clang/AST/Stmt.h"
#include "clang/Sema/Sema.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/StmtElaborator.h"
#include "clang/Green/SyntaxContext.h"

namespace green {

Elaborator::Elaborator(SyntaxContext &Context, GreenSema &SemaRef)
  : Context(Context), SemaRef(SemaRef) {}

clang::Decl *Elaborator::elaborateFile(const Syntax *S) {
  assert(isa<FileSyntax>(S) && "S is not a file");

  startFile(S);

  const FileSyntax *File = cast<FileSyntax>(S);

  // FIXME: Shouldn't we be using PushDecl?
  SemaRef.getCxxSema().CurContext = Context.CxxAST.getTranslationUnitDecl();

  // Pass 1. identify declarations in scope.
  for (const Syntax *SS : File->children())
    identifyDecl(SS);

  // Pass 2: elaborate the types.
  for (const Syntax *SS : File->children())
    elaborateDeclType(SS);

  // Pass 3: elaborate definitions.
  for (const Syntax *SS : File->children())
    elaborateDeclInit(SS);

  finishFile(S);

  return Context.CxxAST.getTranslationUnitDecl();
}

void Elaborator::startFile(const Syntax *S) {
  // Enter the global scope.
  SemaRef.enterScope(SK_Namespace, S);

  /// Build the declaration for the global namespace.
  Declaration *D = new Declaration(S);
  D->SavedScope = SemaRef.getCurrentScope();
  D->Cxx = Context.CxxAST.getTranslationUnitDecl();
  SemaRef.pushDecl(D);
}

void Elaborator::finishFile(const Syntax *S) {
  SemaRef.popDecl();
  SemaRef.leaveScope(S);

  // TODO: Any pending semantic analysis to do here?
}

clang::Decl *Elaborator::elaborateDeclType(const Syntax *S) {
  // TODO: Can we elaborate top-level statements? What would they do?
  // Would these equivalent to directives?
  //
  // TODO: Look for module-related declarations.
  //
  // TODO: What should we find for a list of declarators?
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    return nullptr;

  return elaborateDecl(D);
}

clang::Decl *Elaborator::elaborateDecl(Declaration *D) {
  // FIXME: This almost certainly needs its own elaboration context
  // because we can end up with recursive elaborations of declarations,
  // possibly having cyclic dependencies.
  if (D->declaresFunction())
    return elaborateFunctionDecl(D);
  else
    return elaborateVariableDecl(D);

  // TODO: We should be able to elaborate definitions at this point too.
  // We've already loaded salient identifier tables, so it shouldn't any
  // forward references should be resolvable.
}

// The parameter scope of a function declaration is always found in the
// second declarator.
static Declarator *getFunctionDeclarator(Declarator *D) {
  assert(D->isIdentifier());
  assert(D->Next->isFunction());
  return D->Next;
}

// Returns the function declarator part of D.
static Declarator *getFunctionDeclarator(Declaration *D) {
  assert(D->Decl);
  return getFunctionDeclarator(D->Decl);
}

// Get the Clang parameter declarations for D
static void getFunctionParameters(Declaration *D,
                          llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params) {
  Declarator *FnDecl = getFunctionDeclarator(D);
  const Syntax *ParamList = FnDecl->Data.ParamInfo.Params;
  GreenScope *ParamScope = FnDecl->Data.ParamInfo.Scope;
  for (const Syntax *P : ParamList->children()) {
    Declaration *PD = ParamScope->findDecl(P);
    assert(PD->Cxx && "No corresponding declaration");
    Params.push_back(cast<clang::ParmVarDecl>(PD->Cxx));
  }
}

clang::Decl *Elaborator::elaborateFunctionDecl(Declaration *D) {
  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::QualType Ty = elaborateType(D->Decl);
  clang::TypeSourceInfo *TSI = Context.CxxAST.CreateTypeSourceInfo(Ty);
  clang::DeclarationName Name = D->getId();
  clang::SourceLocation Loc;

  // FIXME: Make sure we have the right storage class.
  clang::FunctionDecl *FD = clang::FunctionDecl::Create(Context.CxxAST, Owner,
                                                        Loc, Loc, Name, Ty,
                                                        TSI, clang::SC_Extern);

  // Update the function parameters.
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  getFunctionParameters(D, Params);
  FD->setParams(Params);

  // Add the declaration and update bindings.
  Owner->addDecl(FD);
  D->Cxx = FD;
  return FD;
}

static clang::StorageClass getStorageClass(Elaborator &Elab) {
  // FIXME: What is the storage class for a variable? Computed from scope
  // and specifiers probably. We don't have specifiers yet.
  return Elab.SemaRef.getCurrentScope()->isBlockScope()
    ? clang::SC_Auto
    : clang::SC_Extern;
}

clang::Decl *Elaborator::elaborateVariableDecl(Declaration *D) {
  if (SemaRef.getCurrentScope()->isParameterScope())
    return elaborateParameterDecl(D);

  // Get the type of the entity.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::QualType Ty = elaborateType(D->Decl);
  clang::TypeSourceInfo *TSI = Context.CxxAST.CreateTypeSourceInfo(Ty);
  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc;
  clang::StorageClass SC = getStorageClass(*this);

  // Create the variable and add it to it's owning context.
  clang::VarDecl *VD = clang::VarDecl::Create(Context.CxxAST, Owner, Loc, Loc,
                                              Id, Ty, TSI, SC);
  Owner->addDecl(VD);
  D->Cxx = VD;
  return VD;
}

clang::Decl *Elaborator::elaborateParameterDecl(Declaration *D) {
  // Get type information.
  clang::DeclContext *Owner = SemaRef.getCurrentCxxDeclContext();
  clang::QualType Ty = elaborateType(D->Decl);
  clang::TypeSourceInfo *TSI = Context.CxxAST.CreateTypeSourceInfo(Ty);
  clang::IdentifierInfo *Id = D->getId();
  clang::SourceLocation Loc;

  // Just return the parameter. We add it to it's function later.
  clang::ParmVarDecl *P = clang::ParmVarDecl::Create(Context.CxxAST, Owner, Loc,
                                                     Loc, Id, Ty, TSI,
                                                     clang::SC_None,
                                                     /*DefaultArg=*/nullptr);
  D->Cxx = P;
  return P;
}

clang::Decl *Elaborator::elaborateDeclSyntax(const Syntax *S) {
  // Identify this as a declaration first.
  identifyDecl(S);

  // Elaborate the declaration.
  if (Declaration *D = SemaRef.getCurrentScope()->findDecl(S))
    return elaborateDecl(D);

  // TODO: Elaborate the definition or initializer?

  return nullptr;
}

void Elaborator::elaborateDeclInit(const Syntax *S) {
  // TODO: See elaborateDeclType. We have the same kinds of concerns.
  Declaration *D = SemaRef.getCurrentScope()->findDecl(S);
  if (!D)
    return;
  elaborateDef(D);
}

void Elaborator::elaborateDef(Declaration *D) {
  if (D->declaresFunction())
    elaborateFunctionDef(D);
  else
    elaborateVariableInit(D);
}

void Elaborator::elaborateFunctionDef(Declaration *D) {
  clang::FunctionDecl *FD = cast<clang::FunctionDecl>(D->Cxx);

  if (!D->Init)
    return;

  SemaRef.enterScope(SK_Function, D->Init);

  // Elaborate the function body.
  StmtElaborator BodyElaborator(Context.CxxAST, SemaRef);
  clang::Stmt *Body = BodyElaborator.elaborateBlock(D->Init);
  FD->setBody(Body);

  SemaRef.leaveScope(D->Init);
}

void Elaborator::elaborateVariableInit(Declaration *D) {
  clang::VarDecl *VD = cast<clang::VarDecl>(D->Cxx);

  if (!D->Init) {
    // FIXME: We probably want to synthesize some kind of initializer here.
    // Not quite sure how we want to do this.
    //
    // FIXME: What if D has type auto? Surely this is an error. For example:
    //
    //    x : auto
    //
    // declares an undeduced-type variable with no initializer. Presumably
    // this should be an error.
    return;
  }

  // Elaborate the initializer.
  ExprElaborator ExprElab(Context.CxxAST, SemaRef);
  clang::Expr *Init = ExprElab.elaborateExpr(D->Init);
  if (!Init)
    return;

  // Perform auto deduction.
  if (VD->getType()->isUndeducedType()) {
    clang::Sema &CxxSema = SemaRef.getCxxSema();
    clang::QualType Ty;
    auto Result = CxxSema.DeduceAutoType(VD->getTypeSourceInfo(), Init, Ty);
    if (Result == clang::Sema::DAR_Failed) {
      // FIXME: Make this a real diagnostic.
      llvm::errs() << "Failed to deduce type of expression.\n";
      return;
    }
    VD->setType(Ty);
  }

  // FIXME: Are we actually checking the type of the initializer? There
  // should be a single function to do all of this.

  // Update the initializer.
  VD->setInit(Init);
}

// Get the clang::QualType described by an operator':' call.
clang::QualType Elaborator::getOperatorColonType(const CallSyntax *S) const {
  // Get the argument list of an operator':' call. This should have
  // two arguments, the entity (argument 1) and its type (argument 2).
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

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
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());
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
    // SemaRef.getCurrentScope()->addDecl(VD);
    // VD->getDeclContext()->addDecl(VD);
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
#if 0

  clang::ASTContext &CxxAST = Context.CxxAST;

  // Get the args for an operator'!' call. This should always have two
  // arguments: a (possibly typed) function declarator and a function
  // definition. We are not concerned with the defintion here.
  const ListSyntax *Args = cast<ListSyntax>(S->getArguments());

  const CallSyntax *Declarator = cast<CallSyntax>(Args->Elems[0]);
  const AtomSyntax *DeclaratorCallee
    = cast<AtomSyntax>(Declarator->getCallee());
  clang::IdentifierInfo *DeclaratorSpelling =
    &CxxAST.Idents.get(DeclaratorCallee->Tok.getSpelling());
  clang::IdentifierInfo *Name = nullptr;

  // The parameters of the declared function.
  //
  // FIXME: This could actually be an array.
  const ListSyntax *Parameters;

  clang::QualType ReturnType;
  // If the declarator is an operator':' call, we have an explicit return
  // type.
  if (DeclaratorSpelling == SemaRef.OperatorColonII) {
    const CallSyntax *OperatorColonCall = cast<CallSyntax>(Declarator);

    ReturnType = E.getOperatorColonType(OperatorColonCall);

    // Let's try to wrestle the parameters out of this operator':' call.
    const ListSyntax *OperatorColonArgList =
      cast<ListSyntax>(OperatorColonCall->getArguments());
    // The first argument of the operator':' call is the function itself.
    const CallSyntax *TheCall =
      cast<CallSyntax>(OperatorColonArgList->Elems[0]);
    // Now let's get the array of parameters from the function.
    Parameters = cast<ListSyntax>(TheCall->getArguments());

    // Let's get the name of the function while we're here.
    Name = &CxxAST.Idents.get(
      cast<AtomSyntax>(TheCall->getCallee())->Tok.getSpelling());
  } else {
    // Otherwise, we have a bare function definition.
    // Just use an auto return type.
    ReturnType = CxxAST.getAutoDeductType();
    Parameters = cast<ListSyntax>(Declarator->getArguments());
    Name = DeclaratorSpelling;
  }

  // Make some preparations to create an actual FunctionDecl.
  llvm::SmallVector<clang::ParmVarDecl *, 4> ParameterDecls;
  llvm::SmallVector<clang::QualType, 4> ParameterTypes;

  // If we have parameters, create clang ParmVarDecls.
  if (Parameters->NumElems) {
    for (const Syntax *Param : Parameters->children()) {
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

  SemaRef.enterScope(S, FD);
  clang::Sema::ContextRAII(SemaRef.getCxxSema(), FD);

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
  #endif
  return nullptr;
}

// Create a clang::Decl for a call to operator'=', i.e., an initialized variable
// or single-line function.
// We need to elaborate the initializer here as well.
// Called from Elaborator::elaborateDeclForCall()
static clang::Decl *handleOperatorEquals(SyntaxContext &Context,
                                         GreenSema &SemaRef,
                                         Elaborator &E,
                                         const CallSyntax *S) {
#if 0
  clang::ASTContext &CxxAST = Context.CxxAST;

  // Get the args for the operator'=' call. As usual, we expect binary
  // operands here: some sort of named entity and an expression.
  const ListSyntax *Args = cast<ListSyntax>(S->getArguments());

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
#endif
  return nullptr;
}

clang::Decl *Elaborator::elaborateDeclForCall(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->getCallee()) && "Unknown call format.");

  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
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

// Get a vector of declarators.
static void getDeclarators(Declarator *D, llvm::SmallVectorImpl<Declarator *> &Decls) {
  while (D) {
    Decls.push_back(D);
    D = D->Next;
  }
}

clang::QualType Elaborator::elaborateType(Declarator *D) {
  // The type of a declarator is constructed back-to-front.
  llvm::SmallVector<Declarator *, 4> Decls;
  getDeclarators(D, Decls);

  // The type is computed from back to front. Start by assuming the type
  // is auto. This will be replaced if an explicit type specifier is given.
  clang::QualType Ty = Context.CxxAST.getAutoDeductType();
  for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
    D = *Iter;
    switch (D->Kind) {
    case DK_Identifier:
      // The identifier is not part of the type.
      break;

    case DK_Pointer:
      Ty = elaboratePointerType(D, Ty);
      break;

    case DK_Array:
      Ty = elaborateArrayType(D, Ty);
      break;

    case DK_Function:
      Ty = elaborateFunctionType(D, Ty);
      break;

    case DK_Type:
      Ty = elaborateExplicitType(D, Ty);
      break;

    default:
      llvm_unreachable("Invalid declarator");
    }
  }
  return Ty;
}

clang::QualType Elaborator::elaboratePointerType(Declarator *D, clang::QualType T) {
  llvm_unreachable("Pointers not supported");
}

clang::QualType Elaborator::elaborateArrayType(Declarator *D, clang::QualType T) {
  llvm_unreachable("Arrays not supported");
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
clang::QualType Elaborator::elaborateFunctionType(Declarator *D, clang::QualType T) {
  const auto Call = cast<CallSyntax>(D->Call);

  // FIXME: Handle array-based arguments.
  assert(isa<ListSyntax>(D->Data.ParamInfo.Params) && "Array parameters not supported");
  const Syntax *Args = D->Data.ParamInfo.Params;

  // Elaborate the parameter declarations in order to get their types, and save
  // the resulting scope with the declarator.
  llvm::SmallVector<clang::QualType, 4> Types;
  SemaRef.enterScope(SK_Parameter, Call);
  for (const Syntax *P : Args->children()) {
    clang::ValueDecl *VD = cast<clang::ValueDecl>(elaborateDeclSyntax(P));
    Types.push_back(VD->getType());
  }
  D->Data.ParamInfo.Scope = SemaRef.saveScope(Call);

  // FIXME: We probably need to configure parts of the prototype (e.g.,
  // make this noexcept by default).
  clang::FunctionProtoType::ExtProtoInfo EPI;
  return Context.CxxAST.getFunctionType(T, Types, EPI);
}

clang::QualType Elaborator::elaborateExplicitType(Declarator *D, clang::QualType T) {
  assert(isa<clang::AutoType>(T));
  assert(D->Kind == DK_Type);

  // FIXME: We should really elaborate the entire type expression. We're
  // just cheating for now. It will be interesting to square that with the
  // current expression elaborator.
  if (const auto *Atom = dyn_cast<AtomSyntax>(D->Data.Type)) {
    auto BuiltinMapIter = BuiltinTypes.find(Atom->getSpelling());
    if (BuiltinMapIter == BuiltinTypes.end()) {
      // FIXME: This requires a type lookup.
      assert(false && "User-defined types not supported.");
    }
    return BuiltinMapIter->second;
  }

  llvm_unreachable("Unknown type specification");
}

static Declarator *buildIdDeclarator(const AtomSyntax *S, Declarator *Next) {
  Declarator *D = new Declarator(DK_Identifier, Next);
  D->Data.Id = S;
  return D;
}

static Declarator *buildTypeDeclarator(const CallSyntax *S, Declarator *Next) {
  Declarator *D = new Declarator(DK_Type, Next);
  D->Call = S;
  D->Data.Type = S->getArgument(1);
  return D;
}

static Declarator *buildFunctionDeclarator(const CallSyntax *S, Declarator *Next) {
  // FIXME: Store the parameter list.
  Declarator *D = new Declarator(DK_Function, Next);
  D->Call = S;
  D->Data.ParamInfo.Params = S->getArguments();
  return D;
}

/// Analyze and decompose the declarator.
///
/// This is a recursive walk through a series of call nodes. In each step,
/// we build a declarator fragment.
static Declarator* makeDeclarator(const Syntax *S) {
  Declarator* D = nullptr;

  while (true) {
    // If we find an atom, then we're done.
    if (const auto *Atom = dyn_cast<AtomSyntax>(S)) {
      D = buildIdDeclarator(Atom, D);
      break;
    }

    if (const auto *Call = dyn_cast<CallSyntax>(S)) {
      const Syntax *Callee = Call->getCallee();
      if (const auto *Atom = dyn_cast<AtomSyntax>(Callee)) {
        // Check for "builtin" operators in the declarator.
        if (Atom->getSpelling() == "operator':'") {
          D = buildTypeDeclarator(Call, D);
          S = Call->getArgument(0);
          continue;
        }

        // Otherwise, this appears to be a function declarator.
        D = buildFunctionDeclarator(Call, D);
        S = Callee;
        continue;
      }
    }

    // FIXME: Is there anything else we can get here?
    return nullptr;
  }

  return D;
}

static clang::IdentifierInfo *getIdentifier(Elaborator &Elab,
                                            const Declarator *D) {
  if (const auto *Atom = dyn_cast_or_null<AtomSyntax>(D->getId()))
    return &Elab.Context.CxxAST.Idents.get(Atom->getSpelling());
  return nullptr;
}

void Elaborator::identifyDecl(const Syntax *S) {
  // Declarations only appear in calls.
  if (const auto *Call = dyn_cast<CallSyntax>(S)) {
    if (const auto *Callee = dyn_cast<AtomSyntax>(Call->getCallee())) {
      llvm::StringRef Op = Callee->getToken().getSpelling();

      // Unpack the declarator.
      const Syntax *Decl;
      const Syntax *Init;
      if (Op == "operator'='") {
        const auto *Args = cast<ListSyntax>(Call->getArguments());
        Decl = Args->getChild(0);
        Init = Args->getChild(1);
      } else if (Op == "operator'!'") {
        const auto *Args = cast<ListSyntax>(Call->getArguments());

        // Disallow definitions here.
        //
        // FIXME: This should be an error, not an assertion.
        if (SemaRef.getCurrentScope()->isParameterScope())
          assert(false && "Function definition");

        Decl = Args->getChild(0);
        Init = Args->getChild(1);
      } else if (Op == "operator':'") {
        Decl = S;
        Init = nullptr;
      } else {
        // Syntactically, this is not a declaration.
        return;
      }

      // FIXME: I think we can filter out some syntactic forms as
      // non-declarations. For example, the following look like definitions
      // but are actually assignments.
      //
      //    f(x) = 4
      //    a[3] = 5
      //
      // The array case might be tricky to disambiguate, and requires
      // a lookup. If it's the first initialization of the variable, then
      // it must be a declaration. See below.

      // Try to build a declarator for the declaration.
      Declarator *Dcl = makeDeclarator(Decl);
      if (!Dcl)
        return;

      // Parameters can only be declared as x, x:T, or :T. The full range
      // of declarator syntax is not supported.
      //
      // FIXME: Emit an error instead of a diagnostic.
      if (SemaRef.getCurrentScope()->isParameterScope() && !Dcl->isIdentifier())
        assert(false && "Invalid parameter declaration");

      clang::IdentifierInfo* Id = getIdentifier(*this, Dcl);

      // FIXME: We could mis-identify this as a declaration. For example:
      //
      //    x = 3
      //    x = 4
      //
      // The first statement is a declaration. The second is an assignment.

      // Create a declaration for this node.
      //
      // FIXME: Do a better job managing memory.
      Declaration *ParentDecl = SemaRef.getCurrentDecl();
      Declaration *TheDecl = new Declaration(ParentDecl, S, Dcl, Init);
      TheDecl->Id = Id;
      SemaRef.getCurrentScope()->addDecl(TheDecl);
    }

  }

  // FIXME: What other kinds of things are declarations?
  //
  // TODO: If S is a list, then we might be looking at one of these
  //
  //    x, y : int
  //    x, y = foo()
  //
  // We need to elaborate each declarator in the list, and then propagate
  // type information backwards.

  return;
}

} // namespace green
