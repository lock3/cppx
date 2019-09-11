#ifndef CLANG_GREEN_SYNTAX_H
#define CLANG_GREEN_SYNTAX_H

#include "clang/GreenAST/SyntaxIterator.h"
#include "clang/GreenParse/SymbolTable.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
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
  Syntax() = delete;
  Syntax(SyntaxKind SK, const Locus &_whence) noexcept :
    whence(_whence), Kind(SK) {}
  virtual ~Syntax() {}

  SyntaxKind getKind() const { return Kind; }
  const char *getSyntaxKindName() const;

  void dump() const;

  using child_iterator = SyntaxIterator;
  using const_child_iterator = ConstSyntaxIterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using const_child_range = llvm::iterator_range<const_child_iterator>;

  child_range children();
  const_child_range children() const {
    auto Children = const_cast<Syntax *>(this)->children();
    return const_child_range(Children.begin(), Children.end());
  }

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

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

};

struct SyntaxConstString : Syntax {
  std::string value;

  SyntaxConstString(const Locus &_whence, const std::string &_value) noexcept
    : Syntax(SK_ConstString, _whence), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstString;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

struct SyntaxConstPath : Syntax {
  std::string value;

  SyntaxConstPath(const Locus &_whence, const std::string &_value) noexcept
    : Syntax(SK_ConstPath, _whence), value(_value) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_ConstPath;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }
};

struct SyntaxIdent : Syntax {
  Syntax *qualifier;
  std::string name;

  SyntaxIdent(const Locus &_whence, Syntax *_qualifier,
               const std::string &_name) noexcept
    : Syntax(SK_Ident, _whence), qualifier(_qualifier), name(_name) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Ident;
  }

  child_range children() {
    return child_range(&qualifier, &qualifier + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxIdent *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxCall : Syntax {
  bool may_fail;

  // TODO: Make this a trailing object?
  Syntax *call_function;
  std::vector<Syntax *> call_parameters;

  SyntaxCall(
      const Locus &_whence, bool _may_fail,
      Syntax *_call_function,
      const std::vector<Syntax *> &_call_parameters) noexcept
    : Syntax(SK_Call, _whence), may_fail(_may_fail),
      call_function(_call_function),
      call_parameters(_call_parameters)
    {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Call;
  }

  child_range children() {
    return child_range(&call_function, &call_function + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxCall *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxAttr : Syntax {
private:
  enum {BASE, ATTR, END};

  Syntax *SubSyntaxes[END];
public:
  SyntaxAttr(const Locus &_whence, Syntax *_base,
             Syntax *_attr) noexcept
    : Syntax(SK_Attr, _whence) {
    SubSyntaxes[BASE] = _base;
    SubSyntaxes[ATTR] = _attr;
  }

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Attr;
  }

  child_range children() {
    return child_range(&SubSyntaxes[0], &SubSyntaxes[0] + END);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxAttr *>(this)->children();
    return const_child_range(Children);
  }

  const Syntax *getBase() const {
    return SubSyntaxes[BASE];
  }

  const Syntax *getAttr() const {
    return SubSyntaxes[ATTR];
  }
};

struct Clause {
  usyntax::res_t keyword;
  std::vector<Syntax *> attrs, body;
};

struct SyntaxMacro : Syntax {
  Syntax *macro;
  std::vector<Clause> clauses;

  SyntaxMacro(const Locus &_whence, Syntax *_macro,
               const std::vector<Clause> &_clauses)
    : Syntax(SK_Macro, _whence), macro(_macro), clauses(_clauses) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Macro;
  }

  child_range children() {
    return child_range(&macro, &macro + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxMacro *>(this)->children();
    return const_child_range(Children);
  }
};

struct SyntaxEscape : Syntax {
  Syntax *escaped;

  SyntaxEscape(const Locus &_whence,
                Syntax *_escaped) noexcept
    : Syntax(SK_Escape, _whence), escaped(_escaped) {}

  static bool classof(const Syntax *S) {
    return S->getKind() == SK_Escape;
  }

  child_range children() {
    return child_range(&escaped, &escaped + 1);
  }
  const_child_range children() const {
    auto Children = const_cast<SyntaxEscape *>(this)->children();
    return const_child_range(Children);
  }
};

} // namespace usyntax

#endif
