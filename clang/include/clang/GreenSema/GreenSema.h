#ifndef LLVM_GREEN_GREENSEMA_H
#define LLVM_GREEN_GREENSEMA_H

#include "llvm/ADT/StringMap.h"

#include "clang/GreenAST/Syntax.h"

#include <memory>
#include <vector>

namespace clang {
class DeclContext;
class Preprocessor;
class Sema;
} // namespace clang

namespace usyntax {

using SyntaxVector = std::vector<Syntax *>;

class SyntaxContext;

// Semantic actions for the Green language.
class GreenSema {
  // A mapping of identifiers as strings to syntaxes.
  llvm::StringMap<Syntax *> Identifiers;

  // The context
  SyntaxContext &Context;

  // The clang preprocessor, for access to IdentifierInfos.
  clang::Preprocessor &PP;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &ClangSema;
public:
  // The current DeclContext in which we are allocating declarations.
  clang::DeclContext *CurContext;

public:
  GreenSema(SyntaxContext &Context, clang::Preprocessor &PP, clang::Sema &S);

  // Look through a translation unit and create the Identifiers map.
  void FindIdentifiers(SyntaxVector &Syn);
};

} // namespace usyntax

#endif
