#ifndef LLVM_GREEN_GREENSEMA_H
#define LLVM_GREEN_GREENSEMA_H

#include "clang/GreenAST/Syntax.h"
#include "llvm/ADT/StringMap.h"

#include <memory>
#include <vector>

namespace usyntax {

using SyntaxVector = std::vector<Syntax *>;

// Semantic actions for the Green language.
class GreenSema {
  // A mapping of identifiers as strings to syntaxes.
  llvm::StringMap<Syntax *> Identifiers;

public:
  // Look through a translation unit and create the Identifiers map.
  void FindIdentifiers(SyntaxVector &Syn);
};

} // namespace usyntax

#endif
