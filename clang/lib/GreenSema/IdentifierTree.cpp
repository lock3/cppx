#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/Type.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/GreenSema/GreenSema.h"
#include "clang/GreenSema/IdentifierTree.h"


#include <algorithm>

using namespace clang;

namespace usyntax {

void
IdentifierTreeAnalyzer::Visit(const Syntax *S) {
  ConstSyntaxVisitor<IdentifierTreeAnalyzer>::Visit(S);
}

bool
IdentifierTreeAnalyzer::HandleOperatorEquals(const SyntaxMacro *S) {
  const std::vector<Clause> &Clauses = S->clauses;

  Syntax *LHS = Clauses.front().body.front();
  Syntax *RHS = Clauses.front().body.back();

  // Operator'=' should be visited from right to left.
  this->Visit(RHS);
  this->Visit(LHS);

  return false;
}

/// Macro Prefixes
bool
IdentifierTreeAnalyzer::HandlePrefixColon(const SyntaxMacro *S) {
  // This is a "1" macro, so its body is its first clause.
  std::vector<Syntax *> Body = S->clauses.front().body;

  // There might be nullptrs in its body from parsing, get rid of them.
  std::remove_if(Body.begin(), Body.end(),
                 [](const Syntax *C) { return !C; });

  Syntax *Operand = Body.front();

  // If this isn't an identifier, something has gone wrong.
  if (!isa<SyntaxIdent>(Operand))
    return true;

  // We have an identifier naming some type.
  SyntaxIdent *TypeName = cast<SyntaxIdent>(Operand);

  auto It = BuiltInTypes.find(TypeName->name);
  if (It == BuiltInTypes.end())
    return true;
  // TODO: Add support for user-defined types.

  CurrentType = It->second;
  return false;
}

void
IdentifierTreeAnalyzer::VisitSyntaxMacro(const SyntaxMacro *S) {
  // FIXME: Look through the attributes and use them in the variable
  // construction. This will include things like public and private.

  if (SyntaxIdent *SubMacro = dyn_cast<SyntaxIdent>(S->macro)) {
  // FIXME: find a better way to switch here! Maybe an enum or stringswitch.
    std::string MacroName = SubMacro->name;
    /// Macro Operators
    if (MacroName == "operator\'=\'")
      HandleOperatorEquals(S);
    /// Macro prefixes
    else if (MacroName == "prefix\':\'")
      HandlePrefixColon(S);
  }
}


// FIXME: This will declare a function decl for a function with no definition.
void
IdentifierTreeAnalyzer::VisitSyntaxCall(const SyntaxCall *S) {
  assert(isa<SyntaxIdent>(S->call_function) && "Function name not an identifier!");

  SyntaxIdent *FnName = cast<SyntaxIdent>(S->call_function);
  ASTContext &ClangContext = Context.ClangContext;
  IdentifierInfo *II = &PP.getIdentifierTable().get(FnName->name);
  DeclarationName DeclName(II);

  // We need to get the types of each parameter to create a proper
  // function type.
  std::vector<QualType> ParamTypes;
  std::vector<TemplateTypeParmDecl *> TemplateParams;
  ParamTypes.reserve(S->call_parameters.size());
  for (const Syntax *Parm : S->call_parameters) {
    // We just want the type, don't create any variables.
    VarContext = Analysis;

    // This is just an untyped identifier, so we are going to have
    // to create a TemplateTypeParmDecl for its type.
    if (isa<SyntaxIdent>(Parm)) {
      // Create a name like __fn3 for the 4th template type in function fn.
      // std::string ID = "__" + FnName->name + std::itos(TemplateParms++);
      // IdentifierInfo *TemplateTypeII = &PP.getIdentifierTable.get(ID);
      DeclContext *TUDecl = ClangContext.getTranslationUnitDecl();
      unsigned Index = ParamTypes.size();
      // FIXME: what is the template parameter depth?

      // We add the template parm to the TU temporarily, until we create the
      // template.
      TemplateTypeParmDecl *TheType =
        TemplateTypeParmDecl::Create(ClangContext, TUDecl, SourceLocation(),
            SourceLocation(), /*Depth*/0, Index, /*Identifier*/nullptr,
            /*Typename*/false, /*ParameterPack*/false);
      TheType->setImplicit();
      TemplateParams.push_back(TheType);
      ParamTypes.push_back(QualType(TheType->getTypeForDecl(), 0));
    } else {
      Visit(Parm);
      ParamTypes.push_back(CurrentType);
    }
  }

  // Make sure we reset the current type.
  CurrentType = Context.ClangContext.getAutoDeductType();


  FunctionProtoType::ExtProtoInfo EPI;
  QualType T = ClangContext.getFunctionType(CurrentType, ParamTypes, EPI);
  TypeSourceInfo *TSI = ClangContext.CreateTypeSourceInfo(T);

  // TODO: can these be inline, constexpr, or have written prototypes?
  FunctionDecl *Fn =
    FunctionDecl::Create(ClangContext, GSema.CurContext,
                         SourceLocation(), SourceLocation(), DeclName,
                         TSI->getType(), TSI, SC_Static);
  GSema.CurContext = Fn;

  // Actually create the parameters inside of the Fn DeclContext.
  unsigned Index = 0;
  for (const Syntax *Parm : S->call_parameters) {
    VarContext = FunctionProto;

    // If parameter is a pure identifier, it has abbreviated template type.
    if (isa<SyntaxIdent>(Parm))
      CurrentType = ParamTypes[Index];
    Visit(Parm);
    ++Index;
  }

  Fn->setParams(CreatedParameters);

  VarContext = Normal;
  CreatedParameters.clear();

  Fn->dump();
}

void
IdentifierTreeAnalyzer::VisitSyntaxIdent(const SyntaxIdent *S) {
  // An identifier is a variable declaration if it has no parents.
  if (!getDepth()) {
    IdentifierInfo *II = &PP.getIdentifierTable().get(S->name);
    ASTContext &ClangContext = Context.ClangContext;

    // This identifier has no children, so we are ready to declare it.
    if (!(*S->children().begin()) && VarContext != Analysis) {
      TypeSourceInfo *TSI =
        ClangContext.CreateTypeSourceInfo(CurrentType);

      // If we're in global scope, make the variable have static storage.
      StorageClass SC =
        (GSema.CurContext == ClangContext.getTranslationUnitDecl())
        ? SC_Static : SC_Auto;

      if (VarContext == Normal) {
        // This is a variable created in a normal context.
        VarDecl *Var =
          VarDecl::Create(ClangContext, GSema.CurContext, SourceLocation(),
                          SourceLocation(), II, TSI->getType(), TSI, SC);
      } else if (VarContext == FunctionProto) {
        // This is a variable created in a function signature.
        ParmVarDecl *Parm =
          ParmVarDecl::Create(ClangContext, GSema.CurContext, SourceLocation(),
                              SourceLocation(), II, TSI->getType(), TSI, SC,
                              nullptr);
        CreatedParameters.push_back(Parm);
      }

      // Reset CurrentType
      CurrentType = ClangContext.getAutoDeductType();
    }
  }
}

} // namespace usyntax
