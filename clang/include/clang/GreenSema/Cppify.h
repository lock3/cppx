#ifndef CLANG_GREEN_CPPIFY_H
#define CLANG_GREEN_CPPIFY_H

#include "clang/GreenAST/Syntax.h"
#include "clang/GreenBasic/GreenBasic.h"
#include "clang/GreenParse/GreenParser.h"

#include <iostream>
#include <fstream>

namespace usyntax {

using namespace llvm;

// Helper functions.
[[noreturn]] inline void Error(const std::string &message) {
  std::cout << message << "\r\n";
  system("pause");
  exit(1);
}

[[noreturn]] inline void Error(const Locus &whence, const char *code,
                        const std::string &message) {
  std::cout << *whence.filename << "(" << whence.startline << ","
            << whence.startpos << ") : error " << code << ": " << message
            << "\r\n";
  system("pause");
  exit(1);
}

// Constants.
static auto NativePathSyntax = std::make_shared<SyntaxConstPath>(Locus{}, "@P");

// Syntax generation.
struct GenerateSyntax {
  // Types.
  using string_t = std::string;
  using syntax_t = std::shared_ptr<Syntax>;
  using array_t = std::vector<syntax_t>;
  using macro_t = std::vector<SyntaxMacro::clause>;
  using file_t = array_t;

  // Collection management.
  string_t StringNew() const noexcept { return std::string(); }

  void StringConcat(string_t &s, usyntax::Text t) const noexcept {
    s.append((const char *)t.start, t.stop - t.start);
  }

  array_t ArrayNew(int64_t reserve = 0) const noexcept {
    return std::vector<syntax_t>(reserve);
  }

  void ArrayAppend(array_t &as, const syntax_t &a) const noexcept {
    as.push_back(a);
  }

  // Generation operations.
  syntax_t NumHex(const usyntax::snippet_t &snip, usyntax::Text digits) const
      noexcept {
    int64_t D = 0;

    for (auto d : digits) {
      int64_t D0 = D;
      auto n = usyntax::chars.hexval[d];
      D = D * 16 + n;
      if ((D - n) / 16 != D0)
        Error("NumHex: integer overflow");
    }

    return std::make_shared<SyntaxConstInt>(LocusOf(snip), D);
  }

  syntax_t NumRational(const usyntax::snippet_t &snip, usyntax::Text digits,
                        usyntax::Text frac, usyntax::Text units) const
      noexcept {
    int64_t D = 0;

    for (auto d : digits) {
      int64_t D0 = D;
      auto n = d - '0';
      D = D * 10 + n;
      if ((D - n) / 10 != D0)
        Error("NumRational: integer overflow");
    }

    return std::make_shared<SyntaxConstInt>(LocusOf(snip), D);
    // FIXME: why is this even here?
    if (!frac && !units)
      return std::make_shared<SyntaxConstInt>(LocusOf(snip), D);
    Error("NumRational: unsupported");
  }
  
  syntax_t NumFloat(const usyntax::snippet_t &snip, usyntax::Text digits,
                     usyntax::Text frac, bool exp_neg, usyntax::Text exp,
                     char8 format) const noexcept {
    Error("NumFloat: unsupported");
  }

  syntax_t CharCode(const usyntax::snippet_t &snip, char8 v) const noexcept {
    Error("CharCode: unsupported");
  }

  syntax_t Unichar(const usyntax::snippet_t &snip, uint32_t v) const noexcept {
    Error("Unichar: unsupported");
  }

  syntax_t Charset(const usyntax::snippet_t &snip, const string_t &s) const
      noexcept {
    Error("charset: unsupported");
  }

  syntax_t ConstString(const usyntax::snippet_t &snip, const string_t &s) const
      noexcept {
    return std::make_shared<SyntaxConstString>(LocusOf(snip), s);
  }

  syntax_t Native(const usyntax::Text &s) const noexcept {
    return std::make_shared<SyntaxIdent>(Locus{}, NativePathSyntax,
                                         StringOf(s));
  }

  syntax_t SyntaxArray(const usyntax::snippet_t &snip,
                        const array_t &elements) const noexcept {
    return Macro(
        snip, Native("array"),
        macro_t{SyntaxMacro::clause{usyntax::res_none, {}, elements}});
  }

  syntax_t Ident(const usyntax::snippet_t &snip,
                 const usyntax::Text &name) const noexcept {
    return std::make_shared<SyntaxIdent>(LocusOf(snip), nullptr,
                                          StringOf(name));
  }

  syntax_t Qualident(const usyntax::snippet_t &snip, const syntax_t &qualifier,
                     const usyntax::Text &name) const noexcept {
    return std::make_shared<SyntaxIdent>(LocusOf(snip), qualifier,
                                          StringOf(name));
  }

  syntax_t Call(const usyntax::snippet_t &snip, int64_t ext,
                const syntax_t &call_function,
                const array_t &call_parameters) const noexcept {
    return std::make_shared<SyntaxCall>(LocusOf(snip), ext, call_function,
                                        call_parameters);
  }

  syntax_t Attr(const usyntax::snippet_t &snip, const syntax_t &base,
                const syntax_t &at) const noexcept {
    return std::make_shared<SyntaxAttr>(LocusOf(snip), base, at);
  }

  syntax_t Path(const usyntax::snippet_t &snip, usyntax::Text value) const
      noexcept {
    return std::make_shared<SyntaxConstPath>(LocusOf(snip),
                                             StringOf(value));
  }

  syntax_t Escape(const usyntax::snippet_t &snip, const syntax_t &escaped) const
      noexcept {
    return std::make_shared<SyntaxEscape>(LocusOf(snip), escaped);
  }

  syntax_t Parentheses(const usyntax::snippet_t &snip, const array_t &ys) const
      noexcept {
    return ys.size() != 1 ? SyntaxArray(snip, ys) : ys[0];
  }

  file_t File(const usyntax::snippet_t &snip, const array_t &ys) const
      noexcept {
    return ys;
  }

  [[noreturn]] void Err(const usyntax::snippet_t &snip, const char *code,
                        std::initializer_list<usyntax::Text> items) const {
    std::string msg;
    for (auto i : items)
      msg.append((const char *)i.start, i.stop - i.start);
    Error(LocusOf(snip), code, msg);
  }

  macro_t MacroStart(const syntax_t &macro, int64_t reserve = 0) const
      noexcept {
    return macro_t();
  }

  void MacroClause(macro_t &mac, usyntax::res_t res, const array_t &attrs,
                    const array_t &body) const noexcept {
    mac.push_back(SyntaxMacro::clause{res, attrs, body});
  }

  syntax_t Macro(const usyntax::snippet_t &snip, const syntax_t &macro,
                 const macro_t &clauses) const noexcept {
    return std::make_shared<SyntaxMacro>(LocusOf(snip), macro, clauses);
  }

  // Internal.
  std::shared_ptr<std::string> filename;
  std::string StringOf(const usyntax::Text &Text) const noexcept {
    return std::string((const char *)Text.start, Text.stop - Text.start);
  }

  Locus LocusOf(const usyntax::snippet_t &snip) const noexcept {
    return Locus{filename, snip.start_line, snip.start_ofs, snip.end_line,
                 snip.end_ofs};
  }
};

struct GenerateCpp {
  int64_t line;
  std::ofstream &dst_file;
  void GenerateFile(const std::vector<std::shared_ptr<Syntax>> &ys) {
    for (auto y : ys) {
      if (SyntaxMacro *ym = dyn_cast<SyntaxMacro>(y.get())) {
        // want IsNative(const char*) to simplify this
        // - support include{"filename"}
        // - support operator'=', generate_definition(bool in_code)
        // - support operator'!'
        if (SyntaxIdent *ym0 = dyn_cast<SyntaxIdent>(ym->macro.get()))
          dst_file << ym0->name << "\n";
        else
          dst_file << "macro\n";
      } else
        dst_file << "not_macro\n";
    }
  }
};

} // namespace usyntax

#endif
