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

#undef def_syntax

} // namespace green
