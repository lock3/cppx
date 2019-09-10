#include "clang/GreenAST/Syntax.h"
#include "llvm/Support/ErrorHandling.h"

namespace usyntax {

Syntax::child_range
Syntax::children() {
  switch (getKind()) {
  case SK_ConstInt:
    return static_cast<SyntaxConstInt *>(this)->children();
  case SK_ConstString:
    return static_cast<SyntaxConstString *>(this)->children();
  case SK_ConstPath:
    return static_cast<SyntaxConstPath *>(this)->children();
  case SK_Ident:
    return static_cast<SyntaxIdent *>(this)->children();
  case SK_Call:
    return static_cast<SyntaxCall *>(this)->children();
  case SK_Attr:
    return static_cast<SyntaxAttr *>(this)->children();
  case SK_Macro:
    return static_cast<SyntaxMacro *>(this)->children();
  case SK_Escape:
    return static_cast<SyntaxEscape *>(this)->children();
  }

  llvm_unreachable("Unknown syntax kind!");
}

const char *
Syntax::getSyntaxKindName() const {
  switch (Kind) {
  case SK_ConstInt:
    return "Const Int";
  case SK_ConstString:
    return "Const String";
  case SK_ConstPath:
    return "Const Path";
  case SK_Ident:
    return "Ident";
  case SK_Call:
    return "Call";
  case SK_Attr:
    return "Attr";
  case SK_Macro:
    return "Macro";
  case SK_Escape:
    return "Escape";
  }

  llvm_unreachable("Unknown syntax kind!");
}

} // namespace usyntax
