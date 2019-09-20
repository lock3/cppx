#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/GreenAST/Syntax.h"
#include "clang/GreenSema/GreenSema.h"
#include "clang/GreenSema/IdentifierTree.h"


namespace usyntax {

using namespace llvm;

GreenSema::GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, 
                     clang::Sema &ClangSema)
  : Context(Context), PP(PP), ClangSema(ClangSema) {
  CurContext = Context.ClangContext.getTranslationUnitDecl();
}

void DumpClause(const Clause &C) {
  /* llvm::outs() << "KEYWORD: " << C.keyword << '\n'; */

  llvm::outs() << "ATTRIBUTES:\n";
  for (const Syntax *S: C.attrs)
    S->dump();

  llvm::outs() << "BODY\n";
  for (const Syntax *S: C.body) {
    S->dump();

    if (S && isa<SyntaxMacro>(S)) {
      llvm::outs() << "BODY MACRO:\n";
      llvm::outs() << "CLAUSES:\n";
      for (const Clause &C : cast<SyntaxMacro>(S)->clauses) {
        DumpClause(C);
      }
      llvm::outs() << "END BODY MACRO\n";
    }
  }
}

void DumpFunction(const SyntaxCall *S) {
  llvm::outs() << "parms:\n";
  for (auto Parm : S->call_parameters) {
    Parm->dump();
    // if (Parm && isa<SyntaxMacro>(Parm)) {
    //   llvm::outs() << "BODY MACRO:\n";
    //   llvm::outs() << "CLAUSES:\n";
    //   for (const Clause &C : cast<SyntaxMacro>(Parm)->clauses) {
    //     DumpClause(C);
    //   }
    //   llvm::outs() << "END BODY MACRO\n";
    // }
  }
}

void
GreenSema::FindIdentifiers(SyntaxVector &Syn) {
  for (const auto &S : Syn) {
    // llvm::outs() << "ANALYZING SYNTAX...\n";
    // S->dump();
    // if (const SyntaxMacro *Macro = dyn_cast<SyntaxMacro>(S)) {
    //   llvm::outs() << "ANALYZING MACRO...\n";
    //   IdentifierTransform T;
    //   T.TransformMacro(Macro);
    // }
  //   llvm::outs() << "ANALYZING SYNTAX...\n";
  //   S->dump();
  //   if (const SyntaxMacro *Macro = dyn_cast<SyntaxMacro>(S)) {
  //     llvm::outs() << "ANALYZING MACRO...\n";
  //     Macro->dump();
      // llvm::outs() << "Kind: " << Macro->getKind() << '\n';
      // llvm::outs() << "Subsyntax kind: " << Macro->macro->getKind() << '\n';
      // llvm::outs() << "First child kind: " << Macro->children().begin()->getKind() << '\n';
      // if (const SyntaxIdent *Ident = dyn_cast<SyntaxIdent>(Macro))
      //   Identifiers.insert({Ident->name, S});
  //   }
    // llvm::outs() << "ANALYZING SYNTAX...\n";
    // S->dump();
    // if (isa<SyntaxCall>(S))
    //   DumpFunction(cast<SyntaxCall>(S));
    // if (const SyntaxMacro *Macro = dyn_cast<SyntaxMacro>(S)) {
    //   outs() << "CLAUSES:\n";
    //   for (const auto &C: Macro->clauses)
    //     DumpClause(C);
    // }
    IdentifierTreeTraverser Traverser(Context, PP, *this, ClangSema);
    Traverser.Visit(S);
  }
}

} // namespace usyntax

