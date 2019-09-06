#ifndef CLANG_GREEN_SYNTAX_H
#define CLANG_GREEN_SYNTAX_H

#include "clang/GreenParse/SymbolTable.h"
#include "llvm/Support/Casting.h"

#include <memory>
#include <string>
#include <vector>

namespace usyntax {

// Location identification.
struct Locus {
  std::shared_ptr<std::string> filename;
  int64_t startline, startpos, endline, endpos;
};

// Syntax types.
struct Syntax {
  Locus whence;

  enum SyntaxKind {
    SK_ConstInt,
    SK_ConstString,
    SK_ConstPath,
    SK_Ident,
    SK_Call,
    SK_Attr,
    SK_Macro,
    SK_Escape,
  };

public:
  Syntax(SyntaxKind SK, const Locus &_whence) noexcept :
    whence(_whence), Kind(SK) {}
  virtual ~Syntax() {}

  SyntaxKind getKind() const { return Kind; }

  const char *getSyntaxKindName() const {
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
  }

  void dump() const;

private:
  const SyntaxKind Kind;
};

struct SyntaxConstInt : Syntax {
  int64_t value;

  SyntaxConstInt(const Locus &_whence, int64_t _value) noexcept
    : Syntax(SK_ConstInt, _whence), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstInt;
  }
};

struct SyntaxConstString : Syntax {
  std::string value;

  SyntaxConstString(const Locus &_whence, const std::string &_value) noexcept
    : Syntax(SK_ConstString, _whence), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstString;
  }
};

struct SyntaxConstPath : Syntax {
  std::string value;

  SyntaxConstPath(const Locus &_whence, const std::string &_value) noexcept
    : Syntax(SK_ConstPath, _whence), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstPath;
  }
};

struct SyntaxIdent : Syntax {
  std::shared_ptr<Syntax> qualifier;
  std::string name;

  SyntaxIdent(const Locus &_whence, const std::shared_ptr<Syntax> &_qualifier,
               const std::string &_name) noexcept
    : Syntax(SK_Ident, _whence), qualifier(_qualifier), name(_name) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Ident;
  }
};

struct SyntaxCall : Syntax {
  bool may_fail;

  std::shared_ptr<Syntax> call_function;
  std::vector<std::shared_ptr<Syntax>> call_parameters;

  SyntaxCall(
      const Locus &_whence, bool _may_fail,
      const std::shared_ptr<Syntax> &_call_function,
      const std::vector<std::shared_ptr<Syntax>> &_call_parameters) noexcept
    : Syntax(SK_Call, _whence), may_fail(_may_fail),
      call_function(_call_function),
      call_parameters(_call_parameters)
    {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Call;
  }
};

struct SyntaxAttr : Syntax {
  std::shared_ptr<Syntax> base, attr;

  SyntaxAttr(const Locus &_whence, const std::shared_ptr<Syntax> &_base,
              const std::shared_ptr<Syntax> &_attr) noexcept
    : Syntax(SK_Attr, _whence), base(_base), attr(_attr) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Attr;
  }
};

struct SyntaxMacro : Syntax {
  struct clause {
    usyntax::res_t keyword;
    std::vector<std::shared_ptr<Syntax>> attrs, body;
  };

  std::shared_ptr<Syntax> macro;
  std::vector<clause> clauses;

  SyntaxMacro(const Locus &_whence, const std::shared_ptr<Syntax> &_macro,
               const std::vector<clause> &_clauses)
    : Syntax(SK_Macro, _whence), macro(_macro), clauses(_clauses) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Macro;
  }
};

struct SyntaxEscape : Syntax {
  std::shared_ptr<Syntax> escaped;

  SyntaxEscape(const Locus &_whence,
                const std::shared_ptr<Syntax> &_escaped) noexcept
    : Syntax(SK_Escape, _whence), escaped(_escaped) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Escape;
  }
};

} // namespace usyntax

#endif
