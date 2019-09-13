#ifndef LLVM_GREEN_GREENSEMA_H
#define LLVM_GREEN_GREENSEMA_H

#include "clang/GreenAST/Syntax.h"
#include "llvm/ADT/StringMap.h"

#include <memory>
#include <vector>

namespace clang {
class Preprocessor;
} // namespace clang

namespace usyntax {

using SyntaxVector = std::vector<Syntax *>;

// Semantic actions for the Green language.
class GreenSema {
  // A mapping of identifiers as strings to syntaxes.
  llvm::StringMap<Syntax *> Identifiers;

  // The context
  SyntaxContext &Context;

  // The clang preprocessor, for access to IdentifierInfos.
  clang::Preprocessor &PP;

public:
  GreenSema(SyntaxContext &Context, clang::Preprocessor &PP)
    : Context(Context), PP(PP)
    {}

  // Look through a translation unit and create the Identifiers map.
  void FindIdentifiers(SyntaxVector &Syn);
};

} // namespace usyntax

#endif
