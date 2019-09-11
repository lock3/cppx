#include "clang/GreenSema/GreenSema.h"
#include "llvm/Support/raw_ostream.h"

namespace usyntax {

using namespace llvm;
void
GreenSema::FindIdentifiers(SyntaxVector &Syn) {
  for (const auto &S : Syn)
    if (const SyntaxMacro *Macro = dyn_cast<SyntaxMacro>(S)) {
      llvm::outs() << "ANALYZING MACRO...\n";
      Macro->dump();
      if (const SyntaxIdent *Ident = dyn_cast<SyntaxIdent>(Macro))
        Identifiers.insert({Ident->name, S});
    }
}

} // namespace usyntax
