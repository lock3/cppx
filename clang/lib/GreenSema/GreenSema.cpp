#include "clang/Lex/Preprocessor.h"
#include "clang/GreenAST/IdentifierTree.h"
#include "clang/GreenSema/GreenSema.h"
#include "llvm/Support/raw_ostream.h"

namespace usyntax {

using namespace llvm;
void
GreenSema::FindIdentifiers(SyntaxVector &Syn) {
  for (const auto &S : Syn) {
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
    llvm::outs() << "ANALYZING SYNTAX...\n";
    S->dump();
    IdentifierTreeTraverser Traverser(Context, PP);
    Traverser.Visit(S);
  }
}

} // namespace usyntax
