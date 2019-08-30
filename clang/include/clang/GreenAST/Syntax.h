#ifndef CLANG_GREEN_SYNTAX_H
#define CLANG_GREEN_SYNTAX_H

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
struct syntax {
  Locus whence;

  syntax(const Locus &_whence) noexcept : whence(_whence) {}

  // allow for dynamic casting
  virtual void ClaimPolymorphic() noexcept {}
};

struct SyntaxConstInt : syntax {
  int64_t value;

  SyntaxConstInt(const Locus &_whence, int64_t _value) noexcept
    : syntax(_whence), value(_value) {}
};

struct SyntaxConstString : syntax {
  std::string value;

  SyntaxConstString(const Locus &_whence, const std::string &_value) noexcept
    : syntax(_whence), value(_value) {}
};

struct SyntaxConstPath : syntax {
  std::string value;

  SyntaxConstPath(const Locus &_whence, const std::string &_value) noexcept
    : syntax(_whence), value(_value) {}
};

struct SyntaxIdent : syntax {
  std::shared_ptr<syntax> qualifier;
  std::string name;

  SyntaxIdent(const Locus &_whence, const std::shared_ptr<syntax> &_qualifier,
               const std::string &_name) noexcept
    : syntax(_whence), qualifier(_qualifier), name(_name) {}
};

struct SyntaxCall : syntax {
  bool may_fail;

  std::shared_ptr<syntax> call_function;
  std::vector<std::shared_ptr<syntax>> call_parameters;

  SyntaxCall(
      const Locus &_whence, bool _may_fail,
      const std::shared_ptr<syntax> &_call_function,
      const std::vector<std::shared_ptr<syntax>> &_call_parameters) noexcept
    : syntax(_whence), may_fail(_may_fail), call_function(_call_function),
        call_parameters(_call_parameters) {}
};

struct SyntaxAttr : syntax {
  std::shared_ptr<syntax> base, attr;

  SyntaxAttr(const Locus &_whence, const std::shared_ptr<syntax> &_base,
              const std::shared_ptr<syntax> &_attr) noexcept
    : syntax(_whence), base(_base), attr(_attr) {}
};

struct SyntaxMacro : syntax {
  struct clause {
    usyntax::res_t keyword;
    std::vector<std::shared_ptr<syntax>> attrs, body;
  };

  std::shared_ptr<syntax> macro;
  std::vector<clause> clauses;

  SyntaxMacro(const Locus &_whence, const std::shared_ptr<syntax> &_macro,
               const std::vector<clause> &_clauses)
    : syntax(_whence), macro(_macro), clauses(_clauses) {}
};

struct SyntaxEscape : syntax {
  std::shared_ptr<syntax> escaped;

  SyntaxEscape(const Locus &_whence,
                const std::shared_ptr<syntax> &_escaped) noexcept
    : syntax(_whence), escaped(_escaped) {}
};

// Constants.
auto NativePathSyntax = std::make_shared<SyntaxConstPath>(Locus{}, "@P");

} // namespace usyntax

#endif
