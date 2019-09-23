#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/GreenSema/GreenSema.h"
#include "clang/GreenSema/IdentifierMapper.h"
#include "clang/GreenAST/Syntax.h"
#include "clang/GreenParse/SymbolTable.h"

using namespace clang;
using namespace usyntax;

void
IdentifierMapper::MapSyntaxes(const SyntaxVector &Inputs) {
  for (const Syntax *S : Inputs)
    MapSyntax(S)->dump();
}

Decl *
IdentifierMapper::MapSyntax(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::SK_Macro:
    return MapMacro(clang::cast<SyntaxMacro>(S));
  case Syntax::SK_Call:
    return MapCall(clang::cast<SyntaxCall>(S));
  case Syntax::SK_Ident: {
    QualType AutoTy = Context.ClangContext.getAutoDeductType();
    return MapIdentifier(clang::cast<SyntaxIdent>(S), AutoTy);
  }
  default:
    return nullptr;
  }
}

Decl *
IdentifierMapper::MapMacro(const SyntaxMacro *S) {
  // FIXME: Look through the attributes and use them in the variable
  // construction. This will include things like public and private.

  if (SyntaxIdent *SubMacro = dyn_cast<SyntaxIdent>(S->macro)) {
    // FIXME: find a better way to switch here! Maybe an enum or stringswitch.

    std::string MacroName = SubMacro->name;
    /// Macro Operators
    if (MacroName == "operator\'=\'")
      return HandleOperatorEquals(S);
  }

  return nullptr;
}


/// Get the types of the parameter variables in a function declaration.
/// Parameters can have explicitly declared types (e.g. x:int), or automatically
/// deduced types. In the latter case, we need to create an implicit type
/// template  parameter type for each deduced variable.
///
/// \param Mapper - Reference to the identifier mapper.
/// \param Call - The function whose parameters we're mapping.
/// \param Types - the vector where we'll store each parameter type.
/// \param TemplateParams - the vector where we'll store only the template types
/// we created.
static bool
GetFunctionParameterTypes(IdentifierMapper &Mapper,
                          const SyntaxCall *Call,
                          llvm::SmallVectorImpl<QualType> &Types,
                          llvm::SmallVectorImpl<NamedDecl *> &TemplateTypes) {
  ASTContext &ClangContext = Mapper.Context.ClangContext;

  for (const Syntax *Parm : Call->call_parameters) {
    // This is just an untyped identifier, so we are going to have
    // to create a TemplateTypeParmDecl for its type.
    if (isa<SyntaxIdent>(Parm)) {
      DeclContext *TUDecl = ClangContext.getTranslationUnitDecl();
      unsigned Index = TemplateTypes.size();
      // FIXME: what is the template parameter depth?

      // We add the template parm to the TU temporarily, until we create the
      // template.
      TemplateTypeParmDecl *TheType =
        TemplateTypeParmDecl::Create(ClangContext, TUDecl, SourceLocation(),
                                     SourceLocation(), /*Depth*/0, Index,
                                     /*Identifier*/nullptr, /*Typename*/false,
                                     /*ParameterPack*/false);
      TheType->setImplicit();
      TemplateTypes.push_back(TheType);
      Types.push_back(QualType(TheType->getTypeForDecl(), 0));
    } else {
      // The type we're trying to find.
      QualType TheType;

      // Look through the children of S. If one is a prefix':', then this
      // identifier's type was explicitly stated.
      // FIXME: This is really ugly and hard to read. Fix it somehow.
      if (Parm && isa<SyntaxMacro>(Parm)) {
        const SyntaxMacro *ParmMacro = cast<SyntaxMacro>(Parm);
        const SyntaxIdent *ParmMacroIdent =
          dyn_cast<SyntaxIdent>(ParmMacro->macro);
        // We have an operator'=', which could possibly be a type assignment.
        if (ParmMacroIdent && ParmMacroIdent->name == "operator\'=\'") {
          // FIXME: Could a prefix be in a different clause? Find a better way
          // to iterate over clauses.

          // Look for a prefix':' within the assignment.
          for (const Syntax *Child : ParmMacro->clauses.front().body) {
            if (Child && isa<SyntaxMacro>(Child)) {
              const SyntaxMacro *ChildMacro = cast<SyntaxMacro>(Child);
              const SyntaxIdent *ChildMacroIdent =
                dyn_cast<SyntaxIdent>(ChildMacro->macro);

              // We found the prefix, so figure out its type.
              if (ChildMacroIdent && ChildMacroIdent->name == "prefix\':\'") {
                TheType = Mapper.HandlePrefixColon(ChildMacro);
                // The type will be null at this point if it wasn't declared.
                if (TheType.isNull()) {
                  // Replace with error.
                  llvm::errs() << "Bad parameter type.\n";
                  return true;
                }

                // We're done.
                break;
              }
            }
          }
        }
      }

      Types.push_back(TheType);
    }
  }

  return false;
}

static void
CreateFunctionParameters(IdentifierMapper &Mapper,
                         FunctionDecl *Fn,
                         llvm::ArrayRef<Syntax *> CallParams,
                         llvm::ArrayRef<QualType> ParamTypes) {
  llvm::SmallVector<ParmVarDecl *, 4> CreatedParameters;
  unsigned Index = 0;
  for (const Syntax *Parm : CallParams) {
    IdentifierMapper::VariableContextRAII
      VCRAII(Mapper.VarContext, IdentifierMapper::FunctionProto);

    ParmVarDecl *CreatedParm = cast<ParmVarDecl>(Mapper.MapSyntax(Parm));

    // If parameter is a pure identifier, it has abbreviated template type.
    if (isa<SyntaxIdent>(Parm))
      CreatedParm->setType(ParamTypes[Index]);

    CreatedParameters.push_back(CreatedParm);
    ++Index;
  }

  Fn->setParams(CreatedParameters);
}

/// Creates a function template out of a function and various template type
/// parameter declarations. Also makes sure all parameters are owned by the
/// template's DeclContext and not the Translation Unit.
///
/// \param ClangContext - Reference to the Clang ASTContext.
/// \param Fn - the FunctionDecl we are going to describe as a template.
/// \param TemplateTypes - A vector of TemplateParmTypeDecls we previously
/// created, each representing an auto-deduced parameter variable's type.
static void
CreateFunctionTemplate(ASTContext &ClangContext,
                       FunctionDecl *Fn,
                       llvm::SmallVectorImpl<NamedDecl *> &TemplateTypes) {
  // Create an actual TemplateParameterList
  TemplateParameterList *TPL =
    TemplateParameterList::Create(ClangContext, SourceLocation(),
                                  SourceLocation(), TemplateTypes,
                                  SourceLocation(), /*requires=*/nullptr);

  // Create the function template in the context that owns Fn.
  DeclContext *ParentContext = Decl::castToDeclContext(Fn)->getParent();

  FunctionTemplateDecl *const TemplateFn =
    FunctionTemplateDecl::Create(ClangContext, ParentContext,
                                 Fn->getLocation(), Fn->getDeclName(),
                                 TPL, Fn);
  Fn->setDescribedFunctionTemplate(TemplateFn);

  // Make sure no parameters are still owned by the TranslationUnit.
  for (auto Parm : Fn->parameters())
    Parm->setOwningFunction(Fn);
}

Decl *
IdentifierMapper::MapCall(const SyntaxCall *S) {
  assert(isa<SyntaxIdent>(S->call_function)
         && "Function name not an identifier!");

  SyntaxIdent *FnName = cast<SyntaxIdent>(S->call_function);
  ASTContext &ClangContext = Context.ClangContext;

  // This vector stores the template type parameter decls that we create for
  // type-deduced function parameters.
  llvm::SmallVector<NamedDecl *, 4> TemplateTypes;

  // We need to get the types of each parameter to create a proper
  // function type.
  llvm::SmallVector<QualType, 4> ParamTypes;
  ParamTypes.reserve(S->call_parameters.size());

  if (GetFunctionParameterTypes(*this, S, ParamTypes, TemplateTypes))
    return nullptr;

  FunctionProtoType::ExtProtoInfo EPI;
  // Explicit function return types get set by the type-assignment operator.
  // For now, just assume we're returning auto.
  QualType ReturnType = ClangContext.getAutoDeductType();
  QualType T = ClangContext.getFunctionType(ReturnType, ParamTypes, EPI);
  TypeSourceInfo *TSI = ClangContext.CreateTypeSourceInfo(T);

  IdentifierInfo *II = &PP.getIdentifierTable().get(FnName->name);
  DeclarationName DeclName(II);

  // TODO: can functions in Green be inline, constexpr, or have written
  // prototypes?
  FunctionDecl *Fn =
    FunctionDecl::Create(ClangContext, GSemaRef.CurContext,
                         SourceLocation(), SourceLocation(), DeclName,
                         TSI->getType(), TSI, SC_Static);
  GSemaRef.CurContext = Fn;

  // Actually create the parameters inside of the Fn DeclContext.
  CreateFunctionParameters(*this, Fn, S->call_parameters, ParamTypes);

  // If we have template parameters, create a function template.
  if (TemplateTypes.size())
    CreateFunctionTemplate(ClangContext, Fn, TemplateTypes);

  return Fn;
}

Decl *
IdentifierMapper::MapIdentifier(const SyntaxIdent *S, QualType Ty) {
  // This is an identifier with children, so it might be a
  // declaration.
  if (!(*S->children().begin())) {
    IdentifierInfo *II = &PP.getIdentifierTable().get(S->name);
    DeclarationName Name(II);

    // This just a keyword. Nothing to do.
    if (IsAnyKeyword(Name.getAsString()))
      return nullptr;

    // We already declared this, don't declare it again.
    // auto It = CurrentSDM.getPointer()->find(Name);
    // if (It != CurrentSDM.getPointer()->end())
    //   return nullptr;

    ASTContext &ClangContext = Context.ClangContext;
    TypeSourceInfo *TSI =
      ClangContext.CreateTypeSourceInfo(Ty);

    // If we're in global scope, make the variable have static storage.
    StorageClass SC =
      (GSemaRef.CurContext == ClangContext.getTranslationUnitDecl())
      ? SC_Static : SC_Auto;

    if (VarContext == Normal) {
      // This is a variable created in a normal context.
      VarDecl *Var =
        VarDecl::Create(ClangContext, GSemaRef.CurContext, SourceLocation(),
                        SourceLocation(), II, TSI->getType(), TSI, SC);
      return Var;
    } else if (VarContext == FunctionProto) {
      // This is a variable created in a function signature.
      ParmVarDecl *Parm =
        ParmVarDecl::Create(ClangContext, GSemaRef.CurContext, SourceLocation(),
                            SourceLocation(), II, TSI->getType(), TSI, SC,
                            nullptr);
      return Parm;
    }
  }

  return nullptr;
}

Decl *
IdentifierMapper::HandleOperatorEquals(const SyntaxMacro *S) {
  // Note: operator'=' should be visited from right to left.

  const std::vector<Clause> &Clauses = S->clauses;

  Syntax *LHS = Clauses.front().body.front();
  Syntax *RHS = Clauses.front().body.back();

  // Case 1: this is a type assignment.
  if (isa<SyntaxMacro>(RHS)) {
    const SyntaxMacro *RHSMacro = cast<SyntaxMacro>(RHS);
    if (const SyntaxIdent *RHSIdent = dyn_cast<SyntaxIdent>(RHSMacro->macro)) {
      if (RHSIdent->name == "prefix\':\'") {
        QualType TheType = HandlePrefixColon(RHSMacro);

        // Easy case - assigning a type to an identifier.
        if (isa<SyntaxIdent>(LHS))
          return MapIdentifier(cast<SyntaxIdent>(LHS), TheType);

        // Assigning a return type to a function definition.
        else if (isa<SyntaxCall>(LHS)) {
          FunctionDecl *Fn =
            cast<FunctionDecl>(MapCall(cast<SyntaxCall>(LHS)));

          FunctionProtoType::ExtProtoInfo EPI;
          llvm::SmallVector<QualType, 4> ParamTypes;
          for (const ParmVarDecl *P : Fn->parameters())
            ParamTypes.push_back(P->getType());

          TheType =
            Context.ClangContext.getFunctionType(TheType, ParamTypes, EPI);
          Fn->setType(TheType);

          return Fn;
        }
      }
    }
  }

  llvm_unreachable("Unimplemented use of operator\'=\'");
}

QualType
IdentifierMapper::HandlePrefixColon(const SyntaxMacro *S) {
  // This is a "1" macro, so its body is its first clause.
  std::vector<Syntax *> Body = S->clauses.front().body;

  // There might be nullptrs in its body from parsing, get rid of them.
  std::remove_if(Body.begin(), Body.end(),
                 [](const Syntax *C) { return !C; });

  Syntax *Operand = Body.front();

  // If this isn't an identifier, something has gone wrong.
  if (!isa<SyntaxIdent>(Operand))
    return QualType();

  // We have an identifier naming some type.
  SyntaxIdent *TypeName = cast<SyntaxIdent>(Operand);

  auto It = BuiltInTypes.find(TypeName->name);
  if (It == BuiltInTypes.end())
    return QualType();
  // TODO: Add support for user-defined types.

  return It->second;
}
