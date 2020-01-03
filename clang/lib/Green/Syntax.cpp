#include "clang/Green/Syntax.h"

namespace green {

Syntax *Syntax::error = new ErrorSyntax;

const char *
Syntax::getSyntaxKindName() const {

  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return # K;
#include "clang/Green/Syntax.def"
  }
}

Syntax::child_range
Syntax::children() {
  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return static_cast<K ## Syntax *>(this)->children();
#include "clang/Green/Syntax.def"
  }
}

std::size_t CallSyntax::getNumArguments() const {
  if (auto *List = dyn_cast<ListSyntax>(getArguments()))
    return List->getNumChildren();
  if (auto *Array = dyn_cast<ArraySyntax>(getArguments()))
    return Array->getNumChildren();
  llvm_unreachable("Invalid argument list");
}

Syntax* CallSyntax::getArgument(std::size_t N) {
  if (auto *List = dyn_cast<ListSyntax>(getArguments()))
    return List->getChild(N);
  if (auto *Array = dyn_cast<ArraySyntax>(getArguments()))
    return Array->getChild(N);
  llvm_unreachable("Invalid argument list");
}

} // namespace green
