#include "clang/GreenAST/Syntax.h"

namespace green {

Syntax *Syntax::error = new ErrorSyntax;

const char *
Syntax::getSyntaxKindName() const {

  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return # K;
#include "clang/GreenAST/Syntax.def"
  }
}

Syntax::child_range
Syntax::children() {
  switch (getKind()) {
#define def_syntax(K) \
    case SK_ ## K: return static_cast<K ## Syntax *>(this)->children();
#include "clang/GreenAST/Syntax.def"
  }
}

#undef def_syntax

} // namespace green
