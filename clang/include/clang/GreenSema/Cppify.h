//===- Cppify.h - Functions for Interfacing Usyntax and C++ ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the interface for interfacing Usyntax and C++ abstract
//  syntax trees.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_CPPIFY_H
#define CLANG_GREEN_CPPIFY_H

#include "llvm/Support/raw_ostream.h"

#include "clang/GreenAST/Syntax.h"
#include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenBasic/GreenBasic.h"
#include "clang/GreenParse/GreenParser.h"

#include <fstream>

namespace usyntax {

using namespace llvm;
using namespace clang;

// Helper functions.
[[noreturn]] inline void Error(const std::string &message) {
  llvm::outs() << message << "\r\n";
  system("pause");
  exit(1);
}

[[noreturn]] inline void Error(SourceLocation Loc, SourceManager &SrcMgr,
                               const char *code, const std::string &message) {
  std::string LocString = Loc.printToString(SrcMgr);
  llvm::outs() << LocString << " error: " << code << ": " << message << "\r\n";
  system("pause");
  exit(1);
}

// Constants.
static auto NativePathSyntax = new SyntaxConstPath(SourceLocation(), "@P");

// Syntax generation.
struct GenerateSyntax {
  // Types.
  using string_t = std::string;
  using syntax_t = Syntax *;
  using array_t = std::vector<syntax_t>;
  using macro_t = std::vector<Clause>;
  using file_t = array_t;

  GenerateSyntax(SyntaxContext &Context,
                 SourceManager &SrcMgr,
                 FileID FID)
    : Context(Context), SrcMgr(SrcMgr), FID(FID)
    {}

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

    return new (Context) SyntaxConstInt(LocOf(snip), D);
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

    return new (Context) SyntaxConstInt(LocOf(snip), D);
    // FIXME: why is this even here?
    if (!frac && !units)
      return new (Context) SyntaxConstInt(LocOf(snip), D);
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
    return new (Context) SyntaxConstString(LocOf(snip), s);
  }

  syntax_t Native(const usyntax::Text &s) const noexcept {
    return new (Context) SyntaxIdent(SourceLocation(), NativePathSyntax,
                                     StringOf(s));
  }

  syntax_t SyntaxArray(const usyntax::snippet_t &snip,
                        const array_t &elements) const noexcept {
    return Macro(
        snip, Native("array"),
        macro_t{Clause{usyntax::res_none, {}, elements}});
  }

  syntax_t Ident(const usyntax::snippet_t &snip,
                 const usyntax::Text &name) const noexcept {
    return new (Context) SyntaxIdent(LocOf(snip), nullptr,
                                     StringOf(name));
  }

  syntax_t Qualident(const usyntax::snippet_t &snip, const syntax_t qualifier,
                     const usyntax::Text &name) const noexcept {
    return new (Context) SyntaxIdent(LocOf(snip), qualifier,
                                     StringOf(name));
  }

  syntax_t Call(const usyntax::snippet_t &snip, int64_t ext,
                const syntax_t call_function,
                const array_t &call_parameters) const noexcept {
    return new (Context) SyntaxCall(LocOf(snip), ext, call_function,
                                    call_parameters);
  }

  syntax_t Attr(const usyntax::snippet_t &snip, const syntax_t base,
                const syntax_t at) const noexcept {
    return new (Context) SyntaxAttr(LocOf(snip), base, at);
  }

  syntax_t Path(const usyntax::snippet_t &snip, usyntax::Text value) const
      noexcept {
    return new (Context) SyntaxConstPath(LocOf(snip),
                                         StringOf(value));
  }

  syntax_t Escape(const usyntax::snippet_t &snip, const syntax_t escaped) const
      noexcept {
    return new (Context) SyntaxEscape(LocOf(snip), escaped);
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
    Error(LocOf(snip), SrcMgr, code, msg);
  }

  macro_t MacroStart(const syntax_t macro, int64_t reserve = 0) const
      noexcept {
    return macro_t();
  }

  void MacroClause(macro_t &mac, usyntax::res_t res, const array_t &attrs,
                    const array_t &body) const noexcept {
    mac.push_back(Clause{res, attrs, body});
  }

  Syntax *Macro(const usyntax::snippet_t &snip, syntax_t macro,
                 const macro_t &clauses) const noexcept {
    return new (Context) SyntaxMacro(LocOf(snip), macro, clauses);
  }

  // Internal.
  SyntaxContext &Context;
  SourceManager &SrcMgr;
  FileID FID;

  std::string StringOf(const usyntax::Text &Text) const noexcept {
    return std::string((const char *)Text.start, Text.stop - Text.start);
  }

  SourceLocation LocOf(const usyntax::snippet_t &snip) const {
    const FileEntry *File = SrcMgr.getFileEntryForID(FID);
    return SrcMgr.translateFileLineCol(File, snip.start_line, snip.start_ofs);
  }

  SourceRange RangeOf(const snippet_t &snip) const {
    const FileEntry *File = SrcMgr.getFileEntryForID(FID);
    SourceLocation EndLoc =
      SrcMgr.translateFileLineCol(File, snip.end_line, snip.end_ofs);
    return SourceRange(LocOf(snip), EndLoc);
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
        if (SyntaxIdent *ym0 = dyn_cast<SyntaxIdent>(ym->macro))
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
