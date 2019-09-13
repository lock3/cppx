#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"

#include "clang/GreenAST/IdentifierTree.h"

using namespace clang;

namespace usyntax {

void
IdentifierTreeAnalyzer::Visit(const Syntax *S) {
  ConstSyntaxVisitor<IdentifierTreeAnalyzer>::Visit(S);
}

void
IdentifierTreeAnalyzer::VisitSyntaxConstInt(const SyntaxConstInt *S) {
}

void
IdentifierTreeAnalyzer::VisitSyntaxIdent(const SyntaxIdent *S) {
  // if (identifier) create vardecl;
  // If an identifier has no parents, it's a global.
  if (!getDepth()) {
    IdentifierInfo *II = &PP.getIdentifierTable().get(S->name);
    DeclarationName Name(II);

    ASTContext &ClangContext = Context.ClangContext;

    // The easy case: if a global doesn't have any children,
    // it is a variable with no initializer.
    // This is just an uninitialized VarDecl.
    if (!(*S->children().begin())) {
      TypeSourceInfo *TSI =
        ClangContext.CreateTypeSourceInfo(ClangContext.getAutoDeductType());
      VarDecl *Global =
        VarDecl::Create(ClangContext,
                        Decl::castToDeclContext(ClangContext.getTranslationUnitDecl()),
                        SourceLocation(), SourceLocation(), II, TSI->getType(), TSI, SC_Static);
      Global->dump();
      llvm::outs() << S->name << " is a bare identifier.\n";
    } else {
      // This could be an initialized variable or almost literally anything
      // else. Let's figure out if it is a variable.

      // pseudo code:
      // if (identifier already exists)
      //   this is simply an assignment expression
      // else
      //   create the global and initialize.
    }
  }
}

} // namespace usyntax
