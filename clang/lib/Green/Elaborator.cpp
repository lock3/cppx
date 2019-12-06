#include "clang/Green/GreenSema.h"
#include "clang/Green/Elaborator.h"
#include "clang/Green/SyntaxContext.h"
#include "clang/Lex/Preprocessor.h"

namespace green {

using namespace clang;

Elaborator::Elaborator(SyntaxContext &Context, GreenSema &GSemaRef)
  : Context(Context), GSemaRef(GSemaRef), PP(GSemaRef.getPP())
{}

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
handleOperatorColon(ASTContext &ClangContext, Preprocessor &PP,
                    Elaborator const &E, const CallSyntax *S) {
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
    PVD->dump();
    return PVD;
  } else {
    VarDecl *VD =
      VarDecl::Create(ClangContext, TUDC, SourceLocation(),
                      SourceLocation(), II, TInfo->getType(), TInfo,
                      SC_Extern);
    VD->dump();
    return VD;
  }
}

// Create a Clang Declaration for a call to operator'!', in other words,
// a function with a definition.
// Called from Elaborator::elaborateDeclForCall()
static Decl *
handleOperatorExclaim(ASTContext &ClangContext, Preprocessor &PP,
                      GreenSema &GSemaRef, Elaborator &E, const CallSyntax *S) {
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
  if (DeclaratorSpelling == GSemaRef.OperatorColonII) {
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

  // The parameters are currently owned by the translation unit, so let's
  // move them to the function itself.
  for (auto Param : FD->parameters())
    Param->setOwningFunction(FD);
  FD->dump();
  return FD;
}

Decl *
Elaborator::elaborateDeclForCall(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->Callee()) && "Unknown call format.");

  const AtomSyntax *Callee = cast<AtomSyntax>(S->Callee());
  const IdentifierInfo *Spelling =
    PP.getIdentifierInfo(Callee->Tok.getSpelling());

  if (Spelling == GSemaRef.OperatorColonII)
    return handleOperatorColon(Context.ClangContext, PP, *this, S);
  else if (Spelling == GSemaRef.OperatorExclaimII)
    return handleOperatorExclaim(Context.ClangContext, PP, GSemaRef, *this, S);
  return nullptr;
}

} // namespace green
