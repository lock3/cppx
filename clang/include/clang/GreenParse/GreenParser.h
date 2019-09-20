//===- GreenParser.h - Parser for the Green language ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Green Parser.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENPARSER_H
#define CLANG_GREEN_GREENPARSER_H

#include "clang/GreenBasic/GreenBasic.h"
#include "clang/GreenParse/TokenInfo.h"
#include "clang/GreenParse/SymbolTable.h"
#include "llvm/Support/Compiler.h"

#include <cassert>

namespace usyntax {

//------------------------------------------------------------------------------
// Text.
struct TextPos {
  char8 *pos;       // Pointer to current parse position.
  char8 *LineStart; // Pointer to start of line.
  intp linecount;           // Zero-based line number.
};

struct GreenParserContext {
  nat8 top;                  // Whether we're at the top of the File.
  char8 *blockstart; // Pointers to starts of the respective lines.
                             // lineind should be redundent.
  bool lineprefix; // Whether subsequent scankey and new expr lines should be
                   // prefixed with '&'.
  GreenParserContext() : top(1), blockstart{}, lineprefix(0) {}
};

inline constexpr intp Length(const char8 *s) noexcept {
  intp n = 0;
  while (s[n])
    n++;
  return n;
}

struct Text {
  const char8 *start, *stop;
  constexpr Text() noexcept : start(nullptr), stop(nullptr) {}
  constexpr Text(const char8 *_start, intp _length) noexcept
      : start(_start), stop(_start + _length) {}
  constexpr Text(const char8 *_start,
                 const char8 *_stop) noexcept
      : start(_start), stop(_stop) {}
  constexpr Text(const char8 *s) noexcept : Text(s, Length(s)) {}
  constexpr Text(const char *s) noexcept : Text((const char8 *)s) {}
  constexpr char8 operator[](intp i) const noexcept {
    const char8 *p = start + i;
    assert(p >= start && p < stop);
    return start[i];
  }

  constexpr auto begin() const noexcept { return start; }
  constexpr auto end() const noexcept { return stop; }

  explicit operator bool() const noexcept { return start != stop; }
};

inline constexpr intp Length(const Text &a) noexcept {
  return a.stop - a.start;
}

inline bool operator==(const Text &as, const Text &bs) {
  if (Length(as) != Length(bs))
    return 0;
  for (intp i = 0; i < Length(as); i++)
    if (as.start[i] != bs.start[i])
      return 0;
  return 1;
}

//------------------------------------------------------------------------------
// Parser.

template <class gen_t> struct GreenParser : TextPos {
  using syntax_t = typename gen_t::syntax_t;
  using string_t = typename gen_t::string_t;
  using macro_t = typename gen_t::macro_t;
  using array_t = typename gen_t::array_t;

  GreenParser(const gen_t &_gen, const char8 *_base,
         const char8 *_end) noexcept
      : TextPos{}, gen(_gen), context() {
    intp len = _end - _base;
    base = pos = LineStart = new char8[len + 4];
    end = base + len;
    for (intp i = 0; i < len; i++)
      base[i] = _base[i];
    base[len] = base[len + 1] = base[len + 2] = base[len + 3] = 0;
  }
  ~GreenParser() { delete base; }

  const gen_t &gen;
  char8 *base, *end;
  nat8 tok;
  GreenParserContext context;

  void Next(intp n) noexcept {
    assert(pos[0] != 0 || n == 0);
    pos += n;
  }

  void Restore(const TextPos &sv) noexcept {
    // Should only be called by backtracking scankey and for error message
    // context.
    pos = sv.pos, LineStart = sv.LineStart, linecount = sv.linecount;
    tok = Token(pos);
  }

  void Eat(char) noexcept;
  bool Eat(nat8 token_index) noexcept {
    assert(tok == Token(pos));
    if (tok == token_index)
      return Next(tokens[tok].length), true;
    else
      return false;
  }

  bool Eat(const char *o) noexcept {
    if (!o[1])
      return pos[0] == char8(o[0]) ? (Next(1), 1) : 0;
    else
      return pos[0] == char8(o[0]) && pos[1] == char8(o[1])
                 ? (Next(2), 1)
                 : 0;
  }

  bool Eat(Text o) noexcept {
    if (Length(o) == 1)
      return pos[0] == char8(o[0]) ? (Next(1), 1) : 0;
    else
      return pos[0] == char8(o[0]) && pos[1] == char8(o[1])
                 ? (Next(2), 1)
                 : 0;
  }

  void Word() noexcept {
    assert(chars.flags[pos[0]] & cf_idlead);
    Next(1);
    while (chars.flags[pos[0]] & cf_idtail)
      Next(1);
    Space();
  }

  snippet_t Snip(const TextPos &start, const TextPos &end) const noexcept {
    return snippet_t{start.linecount + 1, start.pos - start.LineStart + 1,
                     end.linecount + 1, end.pos - end.LineStart + 1};
  }

  snippet_t Snip(const TextPos &start) const noexcept {
    return Snip(start, *this);
  }

  snippet_t Snip() const noexcept { return Snip(*this, *this); }

  // Data structures.
  struct list_t {
    explicit list_t(const array_t &_Content, bool _semicolons = 0) noexcept
        : Content(_Content), semicolons(_semicolons) {}
    array_t Content;
    bool semicolons;
  };

  struct markup_ex {
    syntax_t value;
  };

  // Errors.
  void Require(Text value, void (GreenParser::*errfunc)(Text what)) {
    if (!Eat(value))
      (this->*errfunc)(value);
  }

  Text PosQuote() noexcept {
    static const Text quote[2] = {"", "\""};
    return quote[(pos[0] > 0x20) & (pos[0] != '"') & (pos[0] < 0x7F)];
  }

  Text PosText() noexcept {

    // Quoted.
    if ((pos[0] == '#' && pos[1] == '>') || (pos[0] == '<' && pos[1] == '#'))
      return Text(pos, pos + 2);
    if (chars.flags[pos[0]] & cf_idlead) {
      intp n = 1;
      while (chars.flags[pos[n]] & cf_idtail)
        n++;
      return Text(pos, pos + n);
    }
    if (pos[0] > 0x20 && pos[0] <= 0x7E)
      return Text(pos, pos + 1);

    // Not quoted.
    if (pos[0] == '"')
      return Text("'\"'");
    else if (pos[0] >= 128 &&
             chars.EncodedLength<filter_utf8>(pos[0], pos[1], pos[2], pos[3]))
      return Text("unicode character");
    else if (pos[0] >= 128)
      return Text("non-unicode character sequence");
    else if (pos[0] == '\r' || pos[0] == '\n')
      return Text("end of line");
    else if (pos[0] == '\t')
      return Text("tab");
    else if (pos[0] == ' ')
      return Text("space");
    else if (pos == end)
      return Text("end of File");
    else
      return Text("ASCII control character");
  }

  // Comment and character set errors:
  [[noreturn]] void S01() {
    gen.Err(Snip(), "S01", {"Source must be ASCII or Unicode UTF-8 format"});
  }

  [[noreturn]] void S02() {
    gen.Err(
        Snip(), "S02",
        {"Unexpected ", PosQuote(), PosText(), PosQuote(), " in comment"});
  }

  [[noreturn]] void S04() {
    gen.Err(Snip(), "S04", {"Block comment beginning at \"<#\" never ends"});
  }

  [[noreturn]] void S05() {
    gen.Err(Snip(), "S05",
            {"Ending block comment \"#>\" is outside of comment"});
  }


  // Constant errors.
  [[noreturn]] void S15() {
    gen.Err(Snip(), "S15",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " following hexidecimal digits"});
  }

  [[noreturn]] void S16() {
    gen.Err(Snip(), "S16",
            {"Unicode character constant must be in range 0u0 to 0u10FFFF"});
  }

  [[noreturn]] void S17() {
    gen.Err(Snip(), "S17",
            {"Exponent is only allowed in floating point numbers ending in "
             "\"h\", \"f\", \"d\", \"q\""});
  }

  [[noreturn]] void S18() {
    gen.Err(
        Snip(), "S18",
        {"Character code unit must be 1-2 digits in the range 0c0 to 0cFF"});
  }

  [[noreturn]] void S19() {
    gen.Err(
        Snip(), "S19",
        {"Unicode code point must be 1-6 digits in the range 0u0 to 0c10FFFF"});
  }


  // Identifier errors:
  [[noreturn]] void S20(Text what) {
    gen.Err(Snip(), "S20",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing identifier following \"", what, "\""});
  }

  [[noreturn]] void S21(Text what) {
    gen.Err(Snip(), "S21",
            {"Reserved word \"", what, "\" is not allowed here"});
  }

  [[noreturn]] void S23(Text what) {
    gen.Err(Snip(), "S23",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in qualifier"});
  }

  [[noreturn]] void S24(Text what) {
    gen.Err(Snip(), "S24",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in quoted identifier"});
  }

  [[noreturn]] void S25(Text what) {
    gen.Err(Snip(), "S25",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in path literal"});
  }

  [[noreturn]] void S26(Text what) {
    gen.Err(Snip(), "S26", {"Missing label in path following \"", what, "\""});
  }

  // Text errors.
  [[noreturn]] void S31(Text what) {
    gen.Err(Snip(), "S31",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in character literal"});
  }

  [[noreturn]] void S32(Text) {
    gen.Err(Snip(), "S32",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing end quote in string literal"});
  }

  [[noreturn]] void S34() {
    gen.Err(Snip(), "S34",
            {"Bad character escape \"\\\" followed by ", PosQuote(),
             PosText(), PosQuote()});
  }

  [[noreturn]] void S36() {
    gen.Err(Snip(), "S36",
            {"Constant string interpolations must be blank or unspaced "
             "character constants"});
  }

  // Markup and documentation attribute delimiting errors.
  [[noreturn]] void S41() {
    gen.Err(Snip(), "S41",
            {"Bad markup expression or missing markup tag preceding ",
             PosQuote(), PosText(), PosQuote()});
  }

  [[noreturn]] void S42() {
    gen.Err(Snip(), "S42",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " following markup tag expression"});
  }

  [[noreturn]] void S43(Text tag, Text id) {
    gen.Err(Snip(), "S43",
            {"Markup started with \"<", tag,
             ">\" tag but ended in mismatched \"</", id, ">\" tag"});
  }

  [[noreturn]] void S44(Text what) {
    gen.Err(Snip(), "S44",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in markup end tag"});
  }

  [[noreturn]] void S45() {
    gen.Err(Snip(), "S45", {"Documentation attribute must not be empty"});
  }

  [[noreturn]] void S46() {
    gen.Err(Snip(), "S46",
            {"Expected indented markup following \":>\" but got ", PosQuote(),
             PosText(), PosQuote()});
  }

  // Markup content errors.
  [[noreturn]] void S51(Text what) {
    gen.Err(Snip(), "S51",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in markup"});
  }

  [[noreturn]] void S52(Text what) {
    gen.Err(Snip(), "S52",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in markup"});
  }

  [[noreturn]] void S53(Text what) {
    gen.Err(Snip(), "S53",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing \"", what, "\" in documentation attribute"});
  }

  [[noreturn]] void S54(Text) {
    gen.Err(Snip(), "S54",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " in indented markup"});
  }

  [[noreturn]] void S57() {
    gen.Err(Snip(), "S57",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " or missing ending \";\" or newline following \"&\" markup "
             "escape expression"});
  }

  [[noreturn]] void S58() {
    gen.Err(Snip(), "S58",
            {"Markup list separator \"~\" is only allowed in markup beginning "
             "with \"~\"; elsewhere escape it using \"\\~\""});
  }


  // Precedence errors.
  [[noreturn]] void S60(Text op, Text what) {
    gen.Err(Snip(), "S60",
            {"Precedence doesn't allow \"", op, "\" following \"", what, "\""});
  }

  [[noreturn]] void S62(Text op, Text) {
    gen.Err(
        Snip(), "S62",
        {"Precedence doesn't allow \"", op, "\" following prefix attribute"});
  }

  [[noreturn]] void S63(Text op, Text) {
    gen.Err(Snip(), "S63",
            {"Precedence doesn't allow \"", op,
             "\" following documentation attribute"});
  }

  [[noreturn]] void S64(Text op, Text) {
    gen.Err(Snip(), "S64",
            {"Precedence doesn't allow \"", op, "\" in markup tag expression"});
  }

  [[noreturn]] void S65(Text what) {
    gen.Err(
        Snip(), "S65",
        {"Precedence doesn't allow prefix attribute following \"", what, "\""});
  }


  // Bad or missing expression, block, keyword errors.
  [[noreturn]] void S70(Text) {
    gen.Err(Snip(), "S70",
            {"Expected expression, got ", PosQuote(), PosText(), PosQuote(),
             " at top level of program"});
  }

  [[noreturn]] void S71(Text what) {
    gen.Err(Snip(), "S71",
            {"Expected expression, got ", PosQuote(), PosText(), PosQuote(),
             " following \"", what, "\""});
  }

  [[noreturn]] void S72(Text) {
    gen.Err(Snip(), "S72",
            {"Expected expression, got ", PosQuote(), PosText(), PosQuote(),
             " following prefix attribute"});
  }

  [[noreturn]] void S73(Text) {
    gen.Err(Snip(), "S73",
            {"Expected expression, got ", PosQuote(), PosText(), PosQuote(),
             " following documentation attribute"});
  }

  [[noreturn]] void S74(Text) {
    gen.Err(Snip(), "S74",
            {"Expected markup tag expression, got ", PosQuote(), PosText(),
             PosQuote()});
  }

  [[noreturn]] void S76(Text what) {
    gen.Err(Snip(), "S76",
            {"Expected block or keyword, got ", PosQuote(), PosText(),
             PosQuote(), " following \"", what, "\""});
  }

  [[noreturn]] void S77(Text what) {
    gen.Err(
        Snip(), "S77",
        {"Reserved word \"", what, "\" is not allowed following expression"});
  }

  [[noreturn]] void S78() {
    gen.Err(Snip(), "S78",
            {"Unexpected ", PosQuote(), PosText(), PosQuote(),
             " following expression"});
  }

  // Expression grouping errors:
  [[noreturn]] void S81(Text what) {
    gen.Err(Snip(), "S81",
            {"Expected expression or \"", what, "\", got ", PosQuote(),
             PosText(), PosQuote(), " in Parentheses"});
  }

  [[noreturn]] void S82(Text what) {
    gen.Err(Snip(), "S82",
            {"Expected expression or \"", what, "\", got ", PosQuote(),
             PosText(), PosQuote(), " in parenthesized parameter list"});
  }

  [[noreturn]] void S83(Text what) {
    gen.Err(Snip(), "S83",
            {"Expected expression or \"", what, "\", got ", PosQuote(),
             PosText(), PosQuote(), " in bracketed block"});
  }

  [[noreturn]] void S84(Text what) {
    gen.Err(Snip(), "S84",
            {"Expected expression or \"", what, "\", got ", PosQuote(),
             PosText(), PosQuote(), " in braced block"});
  }

  [[noreturn]] void S85(Text what) {
    gen.Err(Snip(), "S85",
            {"Expected \"", what, "\", got ", PosQuote(), PosText(),
             PosQuote(), " in prefix brackets"});
  }

  [[noreturn]] void S86(Text what) {
    gen.Err(Snip(), "S86",
            {"Expected expression or \"", what, "\", got ", PosQuote(),
             PosText(), PosQuote(), " in string interpolation"});
  }

  [[noreturn]] void S88(Text) {
    gen.Err(Snip(), "S88",
            {"Expected expression, got ", PosQuote(), PosText(), PosQuote(),
             " in indented block"});
  }

  [[noreturn]] void S89() {
    gen.Err(Snip(), "S89",
            {"Indentation mismatch: expected ",
             context.blockstart[pos - LineStart] == ' ' ? "space" : "tab",
             ", got ", PosQuote(), PosText(), PosQuote()});
  }

  // Should never happen.
  [[noreturn]] void S99(Text) {
    gen.Err(Snip(), "S99", {"Unexpected error"});
  }

  // Element parsers.
  void HandleNewline() noexcept {
    assert(pos[0] == 0x0D || pos[0] == 0x0A);
    Next(1 + ((pos[0] == 0x0D) & (pos[1] == 0x0A)));
    linecount++;
    LineStart = pos;
  }

  bool Newline() noexcept {
    // newline = 0x0D [0x0A] | 0x0A
    if ((pos[0] == 0x0D) | (pos[0] == 0x0A))
      return HandleNewline(), 1;
    return 0;
  }

  template <nat8 filter> void printables() noexcept {
    for (;;) {
      if (intp n =
              chars.EncodedLength<filter>(pos[0], pos[1], pos[2], pos[3])) {
        pos += n;
        continue;
      }
      return;
    }
  }

  void HandleBadCmt() { Eat("#>") ? S05() : S02(); }
  void HandleBlockCmt() {
    // blockcmt = '<#' !'>' {printable|newline} '#>
    assert(pos[0] == '<' && pos[1] == '#' && pos[2] != '>');
    TextPos saved(*this);
    Next(2);
    for (;;) {
      printables<filter_blockcmt>();
      switch (pos[0]) {
      case '#':
        assert(pos[1] == '>');
        Next(2);
        return;
      case '\r':
      case '\n':
        HandleNewline();
        continue;
      case '<':
        assert(pos[1] == '#');
        if (pos[2] != '>') {
          HandleBlockCmt();
          continue;
        } else {
          Next(3);
          continue;
        }
      case 0:
        Restore(saved), S04();
      default:
        HandleBadCmt();
        continue;
      }
    }
  }

  void HandleIndCmt() {
    // comment = .. | '<#>' {printable} ind {printable|line} ded | ..
    assert(pos[0] == '<' && pos[1] == '#' && pos[2] == '>');
    Next(3);
    HandleLineCmt();
    ind(
        [&] {
          for (;;) {
            printables<filter_indcmt>();
            switch (pos[0]) {
            case '<':
              assert(pos[1] == '#');
              if (pos[2] != '>') {
                HandleBlockCmt();
                continue;
              } else {
                Next(3);
                continue;
              }
            case '\r':
            case '\n':
              if (Line())
                continue;
            LLVM_FALLTHROUGH;
            case 0:
              return 0;
            default:
              HandleBadCmt();
            }
          }
          return 0;
        },
        &GreenParser::S99);
  }

  void HandleLineCmt() {
    // comment = '#' !'>' {printable} ending | ..
    for (;;) {
      printables<filter_linecmt>();
      switch (pos[0]) {
      case '<':
        assert(pos[1] == '#');
        if (pos[2] != '>') {
          HandleBlockCmt();
          continue;
        } else {
          Next(3);
          continue;
        }
      case '\r':
      case '\n':
      case '\0':
        return;
      default:
        HandleBadCmt();
      }
    }
  }

  void Space() {
    // space = {0x09 | 0x20 | comment}
    for (;;)
      switch (pos[0]) {
      case 0x09:
      case 0x20:
        Next(1);
        continue;
      case '#':
        if (pos[1] != '>') {
          Next(1), HandleLineCmt();
          continue;
        }
        S05();
      case '<':
        if (pos[1] == '#') {
          if (pos[2] != '>') {
            HandleBlockCmt();
            continue;
          } else
            HandleIndCmt();
        }
      LLVM_FALLTHROUGH;
      default:
        tok = Token(pos);
        return;
      }
  }

  bool Ending() noexcept {
    // ending = peek(end|newline)
    return (pos == end) | (pos[0] == 0x0D) | (pos[0] == 0x0A);
  }

  template <class u> auto ind(const u &lambda, void (GreenParser::*errfunc)(Text)) {
    // ind = space ending push top=0 blockind=lineind lineprefix=''
    // ded = ending pop
    assert(Ending());
    auto SavedContext = context;
    context.top = 0;
    context.blockstart = LineStart;
    auto result = lambda();
    context = SavedContext;
    if (!Ending())
      (this->*errfunc)("");
    tok = Token(pos);
    return result;
  }

  auto IndList() {
    return ind([&] { return List("", &GreenParser::S88); }, &GreenParser::S88);
  }

  bool IsSpace(char c) noexcept { return (c == ' ') | (c == '\t'); }
  bool Line() {
    // Parse a new line that is more indented than blockstart.
    // line = newline i={0x09|0x20} space (ending | (top | i>blockind) lineind=i
    // | !(i<=blockind) error)
    TextPos saved_line_end(*this);
    if (!Newline())
      return 0;
    if (!context.top) { // Look ahead at leading spaces in this line for as long
                        // as they match blockstart.
      while (IsSpace(pos[0]) & (pos[0] == context.blockstart[pos - LineStart]))
        Next(1);
      bool line_continues = IsSpace(pos[0]);
      bool block_continues = IsSpace(context.blockstart[pos - LineStart]);
      TextPos saved_stop(*this);
      Space();
      if (Ending() |
          (line_continues & !block_continues)) // Blank or more indented.
        return 1;
      if (!line_continues) // Not more indented than block start, so don't
                           // consume anything.
        return Restore(saved_line_end), 0;
      Restore(saved_stop), S89();
    }
    return Space(), 1; // At top, nothing can go wrong.
  }

  void Scan() {
    // scan = space {line}
    Space();
    while (Line())
      ;
  }

  void Rescan() {
    // if(pos!=saved_scan_start)
    Scan();
    // else
    // Restore(saved_scan);
  }

  bool KeywordAccept() noexcept {
    constexpr intp accept = ~((1LL << Token("=")) | (1LL << Token(".")));
    return (1LL << tok) & accept;
  }

  bool ScanBrace() {
    Space();
    TextPos saved(*this);
    while (Line())
      ;
    if (pos[0] == '{')
      return true;
    return Restore(saved), false;
    // Avoid repeated scanning by having space&restore maintain saved-scan-pos
    // or nullptr.
  }

  res_t Scankey() noexcept {
    // With backtracking to latest trailing line ending.
    // scankey = space [line {line} lineprefix space]
    Space();
    TextPos saved(*this);
    bool same_line = 1;
    while (Line())
      same_line = 0;
    if ((same_line | !context.lineprefix) || (Eat("&") && (Space(), 1)))
      if (auto ri = Reserved[pos]) {
        Word();
        if (KeywordAccept())
          return ri;
      }
    return Restore(saved), res_none;
  }

  intp ParseHex(intp maxdigs, intp maxval, void (GreenParser::*errfunc)()) {
    intp i = 0;
    while (chars.flags[pos[0]] & cf_hex) {
      if (--maxdigs >= 0) {
        i = i * 16 + chars.hexval[pos[0]], Next(1);
        continue;
      }
      (this->*errfunc)();
    }
    if (chars.flags[pos[0]] & cf_idtail)
      S15();
    if (i > maxval)
      (this->*errfunc)();
    return i;
  }

  char8 Parse0c() {
    return char8(ParseHex(2, 0xFF, &GreenParser::S18));
  }

  nat32 Parse0u() {
    return nat32(ParseHex(6, 0x10FFFF, &GreenParser::S19));
  }

  syntax_t Num() {
    // num      = ('0x' hex {hex} | !(('0x'|'0u'|'0c') hex) [{digit} '.'] digit
    // {digit} [units]) !(alpha|digit|'_') units    = 'e' ['+'|'-'] digit
    // {digit} ('h'|'f'|'d'|'q') | !('e' digit) (alpha|'_') {alpha|digit|'_'} |
    // '%' charcode = '0c' hex [hex] | '0u' ('10' | hex) [hex] [hex] [hex] [hex]
    TextPos num_start(*this);
    assert(pos[0] >= '0' && pos[0] <= '9' ||
             pos[0] == '.' && pos[1] >= '0' && pos[1] <= '9');
    if (pos[0] == '0') {
      Next(1);
      char c0 = pos[0];
      if (((c0 == 'u') | (c0 == 'c') | (c0 == 'x')) &&
          (chars.flags[pos[1]] & cf_hex)) {
        Next(1);
        if (c0 == 'u') {
          auto n = Parse0u();
          auto locus = Snip(num_start);
          return Space(), gen.Unichar(locus, n);
        }
        if (c0 == 'x') {
          do {
            pos++;
          } while (chars.flags[pos[0]] & cf_hex);
          if (chars.flags[pos[0]] & cf_idtail)
            S15();
          auto num_end = pos;
          auto num_snip = Snip(num_start);
          return Space(),
                 gen.NumHex(num_snip, Text(num_start.pos + 2, num_end));
        } else {
          char8 n = Parse0c();
          auto locus = Snip(num_start);
          return Space(), gen.CharCode(locus, n);
        }
      }
    }

    while (chars.flags[pos[0]] & cf_digit)
      Next(1);

    Text digits(num_start.pos, pos), frac(pos + 1, pos + 1);
    if (pos[0] == '.' && (chars.flags[pos[1]] & cf_digit)) {
      Next(2);
      while (chars.flags[pos[0]] & cf_digit)
        Next(1);
      frac.stop = pos;
    }

    bool exp_neg = false;
    Text exp(nullptr, nullptr);
    if (pos[0] == 'e') {
      exp_neg = (pos[1] == '-');
      bool exp_sgn = exp_neg | (pos[1] == '+');
      if (chars.flags[pos[1 + exp_sgn]] & cf_digit) {
        Next(1 + exp_sgn);
        exp.start = pos;
        while (chars.flags[pos[0]] & cf_digit)
          Next(1);
        exp.stop = pos;
      }
    }

    auto format = pos[0];
    if (((format == 'h') | (format == 'f') |
         (format == 'd') | (format == 'q')) &&
        !(chars.flags[pos[1]] & cf_idtail)) {
      Next(1);
      auto locus = Snip(num_start);
      return Space(), gen.NumFloat(locus, digits, frac, exp_neg, exp, format);
    }

    if (exp)
      S17();

    Text units{};
    if (chars.flags[pos[0]] & cf_idlead) {
      auto pos0 = pos;
      do
        Next(1);
      while (chars.flags[pos[0]] & cf_idtail);
      units = Text(pos0, pos);
    }

    auto snippet = Snip(num_start);
    return Space(), gen.NumRational(snippet, digits, frac, units);
  }

  syntax_t RequirePath() {
    // path = ['/' label] '@' label {'/' ['(' path ')'] ident } !('/'|'&'')
    TextPos start(*this);
    if (Eat("/"))
      RequireLabel("/");
    Require("@", &GreenParser::S25);
    RequireLabel("@");

    while (Eat("/")) {
      Text what = "/";
      if (Eat("("))
        RequirePath(), Require(")", &GreenParser::S25), what = ")";
      if (chars.flags[pos[0]] & cf_idlead) {
        Ident();
        continue;
      }

      S20(what);
    }

    if ((pos[0] != '/') & (pos[0] != '&')) {
      auto snippet = Snip(start);
      return Space(), gen.Path(snippet, Text(start.pos, pos));
    }

    S25("/");
  }

  Text RequireLabel(Text what) {
    // label = (alpha|digit|'_') {alpha|digit|'_'|'-'|'.'}
    // !(alpha|digit|'_'|'-'|'.')
    auto pos0 = pos;
    if (chars.flags[pos[0]] & cf_idtail) {
      Next(1);
      while ((chars.flags[pos[0]] & cf_idtail) | (pos[0] == '-') |
             (pos[0] == '.'))
        Next(1);
      return Text(pos, pos0);
    }

    S26(what);
  }

  Text Ident() {
    // ident = (alpha|'_') {alpha|digit|'_'} !(alpha|digit|'_') ["'"
    // {!('\'|'{'|'}'|'''|'"'|'<#'|'#>') 0x20-0x7E} "'"]
    assert(chars.flags[pos[0]] & cf_idlead);

    auto pos0 = pos;
    do
      Next(1);
    while (chars.flags[pos[0]] & cf_idtail);
    if (!Eat("'"))
      return Text(pos0, pos);
    while (chars.flags[pos[0]] & cf_idquote)
      Next(1);

    Require("'", &GreenParser::S24);
    return Text(pos0, pos);
  }

  Text IdentNonreserved() {
    bool ri = Reserved[pos];
    if (!ri) {
      auto id = Ident();
      return Space(), id;
    }

    TextPos start(*this);
    auto id = Ident();
    Space();

    if (KeywordAccept())
      return Restore(start), "";
    return id;
  }

  syntax_t RequireQualident(Text what) {
    // qualident = ['(' list ')' space] !reserved ident
    TextPos start(*this);
    if (chars.flags[pos[0]] & cf_idlead) {
      if (auto id = IdentNonreserved())
        return gen.Ident(Snip(start), id);
      S21(Reserved[Reserved[pos]]);
    } else if (pos[0] == '(') {
      Next(1), Space();
      auto li = List("(", &GreenParser::S81);
      Require(")", &GreenParser::S23);

      if (chars.flags[pos[0]] & cf_idlead) {
        if (auto id = IdentNonreserved())
          return gen.Qualident(Snip(start),
                               gen.Parentheses(snippet_t{}, li.Content), id);
        S21(Reserved[Reserved[pos]]);
      }
    }

    S20(what);
  }

  template <nat8 filter, bool AllowFullInterpolation>
  string_t LiteralChars() {
    // printable = 0x09                 | !'<#' !'#>' 0x20-0x7E | blockcmt |
    // '<#>'     | 0xC2-0xDF ux
    //           | 0xE0 0xA0-0xBF ux    | 0xE1-0xEC ux ux       | 0xED 0x80-0x9F
    //           ux    | 0xEE-0xEF ux ux | 0xF0 0x90-0xBF ux ux | 0xF1-0xF3 ux
    //           ux ux    | 0xF4 0x80-0x8F ux ux
    // char      = ''' {charconst | !('\'|'{'|'}'|''') printable} ''' | ..
    // string    = '"' {special   | !('\'|'{'|'}'|'"') printable} '"'
    // element   = special | markup | escape | comment | line |
    // !('\'|'{'|'}'|'#'|'<'|'>'|'&'|'~') printable charconst = '{' scan
    // [charcode scan] '}' | '\' !'<#' !'#>'
    // ('r'|'n'|'t'|'''|'"'|'\'|'{'|'}'|'#'|'<'|'>'|'&'|'~')
    auto s = gen.StringNew();
    for (;;) {
      auto pos0 = pos;
      printables<filter>();
      if (pos0 != pos)
        gen.StringConcat(s, Text(pos0, pos));
      switch (pos[0]) {
      case '\r':
      case '\n':
        if (filter == filter_element)
          if (Line())
            continue;
        return s;
      case '<':
        return s;
      case '\\': {
        TextPos saved(*this);
        Next(1);
        if ((chars.flags[pos[0]] & cf_backslash) ||
            (pos[0] == '<' && pos[1] != '#') ||
            (pos[0] == '#' && pos[1] != '>')) {
          char8 ch = char8(
              pos[0] * ((pos[0] != 'n') & (pos[0] != 'r') & (pos[0] != 't')) +
              '\r' * (pos[0] == 'r') + '\n' * (pos[0] == 'n') +
              '\t' * (pos[0] == 't'));
          gen.StringConcat(s, Text(&ch, &ch + 1));
          Next(1);
          continue;
        }
        return Restore(saved), s;
      }
      case '{': {
        TextPos saved(*this);
        Next(1), Scan();
        if (Eat("0u")) {
          if (chars.flags[pos[0]] & cf_hex) {
            auto n = Parse0u();
            Scan();
            if (Eat("}")) {
              char8 buf[4];
              auto c = chars.MakeUtf8(buf, n);
              gen.StringConcat(s, Text(buf, buf + c));
              continue;
            }
          }
        } else if (Eat("0c")) {
          if (chars.flags[pos[0]] & cf_hex) {
            char8 ch = Parse0c();
            Scan();
            if (Eat("}")) {
              gen.StringConcat(s, Text(&ch, &ch + 1));
              continue;
            }
          } else
            S18();
        } else if (Eat("}")) {
          pos0 = pos;
          continue;
        }
        if (!AllowFullInterpolation)
          S36();
        return Restore(saved), s;
      }

      default:
        return s;
      }
    }

    return s;
  }

  template <nat8 filter> syntax_t TextSplice() {
    // char      = ''' {charconst | !('\'|'{'|'}'|''') printable} ''' | ..
    // string    = '"' {special   | !('\'|'{'|'}'|'"') printable} '"'
    // element   = special | markup | escape | comment | line |
    // !('\'|'{'|'}'|'#'|'<'|'>'|'&'|'~') printable special   = charconst |
    // !charconst ('{' list '}' | '\' (alpha|'_'))

    // TextPos whole_start(*this);
    auto splices = gen.ArrayNew();
    intp splicec = 0;
    for (;;) {
      TextPos start(*this);
      auto s = LiteralChars<filter, 1>();
      if (pos != start.pos)
        splicec++, gen.ArrayAppend(splices, gen.ConstString(Snip(start), s));
      switch (pos[0]) {
      case '{': {
        Next(1);
        auto li = List("}", &GreenParser::S86);
        Require("}", &GreenParser::S86);
        gen.ArrayAppend(
            splices,
            gen.Call(Snip(start), 0, gen.Native("stringify"), li.Content));
        continue;
      }
      case '#':
        Next(1), HandleLineCmt();
        continue;
      case '&': {
        // escape = '&' push lineprefix='&' space expr (';' | ending) pop
        Next(1), Space();
        auto SavedContext = context;
        context.lineprefix = true;
        TextPos esc_start(*this);
        auto parms = gen.ArrayNew(1);
        gen.ArrayAppend(parms, Expr(prec_expr, "&"));
        gen.ArrayAppend(splices, gen.Call(Snip(esc_start), 0,
                                           gen.Native("markupify"), parms));
        context = SavedContext;
        if (!Ending() && !Eat(Token(";")))
          S57();
        continue;
      }
      case '\\':
        Next(1);
        if (chars.flags[pos[0]] & cf_idlead) {
          char8 ident[12];
          for (intp i = 0; i < 12; i++)
            ident[i] = "backslash' '"[i];
          ident[10] = pos[0];
          Next(1);
          gen.ArrayAppend(splices,
                           SimpleMacro(Snip(start),
                                        gen.Native(Text{ident, ident + 12}),
                                        gen.ArrayNew()));
          continue;
        } else
          S34();
      case '<':
        if (pos[1] == '#') {
          if (pos[2] != '>') {
            HandleBlockCmt();
            continue;
          } else {
            HandleIndCmt();
            continue;
          }
        }
        if (pos[1] != '/') {
          gen.ArrayAppend(splices, RequireMarkup());
          continue;
        }
      LLVM_FALLTHROUGH;
      default:
        return splicec == 0
                   ? gen.ConstString(snippet_t{}, "")
                   : splicec == 1 ? gen.Parentheses(snippet_t{}, splices)
                                  : gen.Call(Snip(start), 0,
                                             gen.Native("concat"), splices);
      }
    }
  }

  syntax_t Content() {
    // content = space {line} ('~' {element} {'~' {element}} | {element})
    Space();
    while (Line())
      ;

    if (pos[0] != '~') {
      syntax_t rc = TextSplice<filter_element>();
      if (pos[0] == '~')
        S58();
      return rc;
    } else {
      Next(1);

      auto rcs = gen.ArrayNew();
      do
        gen.ArrayAppend(rcs, TextSplice<filter_element>());
      while (Eat("~"));

      return gen.SyntaxArray(snippet_t{}, rcs);
    }
  }

  string_t RequireChar() {
    // char = ''' {charconst | !('\'|'{'|'}'|''') printable} ''' | ..
    string_t s = LiteralChars<filter_char, 0>();
    return Require("'", &GreenParser::S31), Space(), s;
  }

  syntax_t MakeMarkup(const snippet_t &snip, const syntax_t &left,
                                  array_t *contentsp,
                                  const syntax_t &elements) {
    auto content = SimpleMacro_2(snippet_t{}, gen.Native("operator'='"),
                                  gen.Native("content"), elements);
    if (!contentsp) {
      auto contents = gen.ArrayNew(1);
      gen.ArrayAppend(contents, content);
      return SimpleMacro(snip, left, contents);
    } else {
      gen.ArrayAppend(*contentsp, content);
      return SimpleMacro(snip, left, *contentsp);
    }
  }

  void CheckMarkup(const TextPos &start, syntax_t &left,
                               link_t<Text> *marks, Text tag, bool brace,
                               array_t *contentsp) {
    // markup = '<' space tags (':>' ind content ded | ':' content '>' | '>'
    // content '<' {'/' ident space} '>')
    switch (tok) {
    case Token("{"):
      if (brace) {
      got_brace:
        ParseMacro(start, left, prec_attr, false, marks, tag, "");
      }
      return;
    case Token("\r"):
      if (brace && ScanBrace())
        goto got_brace;
      return;
    case Token(":"): {
      Next(1);
      /*auto r = */Content();
      Require(">", &GreenParser::S51);
      marks->value = tag;
      // throw markup_ex{MakeMarkup(snippet_t{}, left, contentsp, r)};
    }
    LLVM_FALLTHROUGH;
    case Token(">"): {
      Next(1);
      /*auto r = */Content();
      Require("</", &GreenParser::S52);
      Space();
      marks->value = tag;
      for (auto *expect_mark = marks; expect_mark;
           expect_mark = expect_mark->next)
        if (chars.flags[pos[0]] & cf_idlead) {
          auto endtag = Ident();
          if (!(endtag == expect_mark->value))
            S43(expect_mark->value, endtag);
          Space();
          if (expect_mark->next)
            Require("/", &GreenParser::S44);
        } else
          GreenParser::S44(expect_mark->value);
      Require(">", &GreenParser::S44);
      // throw markup_ex{MakeMarkup(snippet_t{}, left, contentsp, r)};
    }
    LLVM_FALLTHROUGH;
    case Token(":>"): {
      marks->value = tag;
      Next(2);
      Space();
      if (Ending()) {
        // throw markup_ex{
        //     MakeMarkup(snippet_t{}, left, contentsp,
        //                 ind([&] { return Content(); }, &GreenParser::S54))};
      }
      S46();
    }
    case Token(","): {
      Next(1), Scan();
      marks->value = tag;
      /*auto right = */MoreMarkupTags(TextPos{}, marks);
      // throw markup_ex{MakeMarkup(snippet_t{}, left, contentsp, right)};
    }
    LLVM_FALLTHROUGH;
    default:
      return;
    }
  }

  syntax_t MoreMarkupTags(TextPos start,
                                       link_t<Text> *outer_marks) {
    // tags = [base {postfix | {calls} brace} '.' scan] qualident space [brace]
    // [',' scan tags]
    // try {
      link_t<Text> marks{"", outer_marks};
      Expr(prec_attr, "", &GreenParser::S74, &GreenParser::S64, &marks);
      S41();
    // } catch (markup_ex m) {
    //   return m.value;
    // }
  }

  syntax_t RequireMarkup() {
    // markup = '<' space tags (':>' ind content ded | ':' content '>' | '>'
    // content '<' {'/' ident space} '>')
    assert(pos[0] == '<');
    TextPos start(*this);
    Next(1), Space();
    return MoreMarkupTags(start, nullptr);
  }

  syntax_t SimpleMacro(const snippet_t &snip, const syntax_t &mac,
                        const array_t &invoke_body) noexcept {
    auto m = gen.MacroStart(mac, 1);
    gen.MacroClause(m, res_none, gen.ArrayNew(0), invoke_body);
    return gen.Macro(snip, mac, m);
  }

  // FIXME: what do these do? Perhaps SimpleMacro_1 is unary whereas 2
  // is binary?
  syntax_t SimpleMacro_1(const snippet_t &snip, const syntax_t &mac,
                         const syntax_t &invoke_body_0) noexcept {
    auto body = gen.ArrayNew(1);
    gen.ArrayAppend(body, invoke_body_0);
    return SimpleMacro(snip, mac, body);
  }

  syntax_t SimpleMacro_2(const snippet_t &snip, const syntax_t &mac,
                         const syntax_t &invoke_body_0,
                         const syntax_t &invoke_body_1) noexcept {
    auto body = gen.ArrayNew(2);
    gen.ArrayAppend(body, invoke_body_0);
    gen.ArrayAppend(body, invoke_body_1);
    return SimpleMacro(snip, mac, array_t{invoke_body_0, invoke_body_1});
  }

  void ParseMacro(const TextPos &start, syntax_t &left, prec p,
                  bool allow_else, link_t<Text> *marks, Text tag,
                  Text require_macro) {
    // brace = scan '{' list '}' space
    // block = brace | ':' ind list ded
    // imm   = def lookahead(ending|')'|']'|'}'|','|';'|'where'|'=>'|'<|')
    // key   = scankey ('do'|'in'|'otherwise'|'returns'|'then'|'until'|'using')
    // words macro = {calls} (key imm | [key] block {key block} [key imm])
    // [catch macro] !key !catch calls = ('(' list ')' | '<' space base {postfix
    // | {calls} brace} '>') space if    = 'if' word macro (scankey 'else' word
    // (block | if | !('if' word) imm) | !(scankey 'else')) call  =
    // (base|path|if) {postfix|macro|of | ('&') scan  call } | prefix (brace |
    // char | !char call)
    struct calls_t {
      nat8 ext;
      list_t parms;
      TextPos end;
    };
    link_t<calls_t> *startlink = nullptr;
    auto m = gen.MacroStart(left);
    bool got_macro = false;
    auto add_clause = [&](res_t keyword, const array_t &body) noexcept -> void {
      auto attrs = gen.ArrayNew();
      if (!got_macro)
        for (auto p = startlink; p; p = p->next) {
          // Translate sequence of calls and attributes into macro clauses,
          // possibly leaving leftover attrs.
          auto &call = p->value; 
          if (call.ext == 2)
            gen.ArrayAppend(attrs,
                             gen.Parentheses(snippet_t{}, call.parms.Content));
          else
            gen.MacroClause(m, !call.parms.semicolons ? res_of : res_where,
                             attrs, call.parms.Content),
                attrs = gen.ArrayNew();
        }
      got_macro = true;
      gen.MacroClause(m, keyword, attrs, body);
    };
    auto recurse = [&](const auto &self, link_t<calls_t> **prevlink) -> void {
      assert(tok == Token(pos));
      auto add_call = [&](nat8 ext, const list_t &parms) -> void {
        link_t<calls_t> local_calls{{0, parms, *this}, nullptr};
        *prevlink = &local_calls;
        Space();
        return self(self, &local_calls.next);
      };
      switch (tok) {
      case Token("("): {
        Next(1);
        auto as = List(")", &GreenParser::S82);
        Require(")", &GreenParser::S82);
        return add_call(0, as);
      }
      case Token("["): {
        Next(1);
        auto as = List("]", &GreenParser::S83);
        Require("]", &GreenParser::S83);
        return add_call(1, as);
      }
      case Token("<"): {
        TextPos saved(*this);
        Next(1);
        Space();
        if (tokens[tok].pre_left >= prec_attr) {
          auto e = Expr(prec_attr, "<");
          if (Eat(">")) {
            auto as = gen.ArrayNew(1);
            gen.ArrayAppend(as, e);
            return add_call(2, list_t(as));
          }
        }
        Restore(saved);
        goto finished;
      }
      case Token("{"):
      got_brace : {
        Next(1);
        auto ali = List("}", &GreenParser::S84);
        Require("}", &GreenParser::S84);
        Space();
        if (!marks) {
        not_markup:
          add_clause(res_none, ali.Content);
          goto tail;
        }
        CheckMarkup(start, left, marks, tag, false, &ali.Content);
        goto not_markup;
      }
      case Token(":"): {
        if (p <= prec_call) {
          TextPos saved(*this);
          Next(1);
          Space();
          if (Ending()) {
            add_clause(res_none, IndList().Content);
            goto tail;
          }
          Restore(saved);
        }
        goto finished;
      }
      case Token("\n"):
        if (ScanBrace())
          goto got_brace;
      LLVM_FALLTHROUGH;
      default:
      tail:
        // macro = {calls} (key imm | [key] block {key block} [key imm]) [catch
        // macro] !key !catch
        if (p <= prec_call) {
          TextPos keyword_start(*this);
          auto ri = Scankey();
          switch (ri) {
          // case Reserved["do"_u8]:
          // case Reserved["in"_u8]:
          // case Reserved["otherwise"_u8]:
          // case Reserved["returns"_u8]:
          // case Reserved["then"_u8]:
          // case Reserved["until"_u8]: {
          case res_do:
          case res_in:
          case res_otherwise:
          case res_returns:
          case res_then:
          case res_until: {
            auto body = gen.ArrayNew();
            Block(body, true, true, Reserved[ri]);
            add_clause(ri, body);
            goto tail;
          }
          // case Reserved["catch"_u8]: {
          case res_catch: {
            syntax_t key2_left = gen.Ident(Snip(keyword_start), Reserved[ri]);
            ParseMacro(keyword_start, key2_left, p, allow_else, nullptr, "",
                        Reserved[ri]);
            auto body = gen.ArrayNew(1);
            gen.ArrayAppend(body, key2_left);
            add_clause(res_then, body);
            goto finished;
          }
          // case Reserved["else"_u8]:
          case res_else:
            // if = 'if' word macro (scankey 'else' word (block | if | !('if'
            // word) imm) | !(scankey 'else'))
            if (allow_else) {
              if (((pos[0] == 'i') & (pos[1] == 'f')) &&
                  !(chars.flags[pos[2]] & cf_idtail)) {
                TextPos if_start(*this);
                Next(2);
                auto snippet = Snip(if_start);
                Space();
                if (KeywordAccept()) {
                  syntax_t key2_left = gen.Ident(snippet, "if");
                  ParseMacro(keyword_start, key2_left, p, allow_else, nullptr,
                              "", Reserved[ri]);
                  auto body = gen.ArrayNew(1);
                  gen.ArrayAppend(body, key2_left);
                  add_clause(res_else, body);
                  goto finished;
                }
                Restore(if_start);
              }
              auto body = gen.ArrayNew();
              Block(body, true, true, Reserved[ri]);
              add_clause(ri, body);
              goto finished;
            }
          LLVM_FALLTHROUGH;
          default:
            Restore(keyword_start); // Don't consume other reserved words.
            goto finished;
          }
        }
      finished:
        if (!got_macro) {
          // Produce a sequence of calls and attributes.
          if (!require_macro) {
            for (auto p = startlink; p; p = p->next) {
              // Translate sequence of calls and attributes into macro clauses,
              // possibly leaving leftover attrs.
              auto &call = p->value;
              if (call.ext != 2)
                left = gen.Call(Snip(start, call.end), call.ext, left,
                                call.parms.Content);
              else
                left =
                    gen.Attr(Snip(start, call.end), left,
                             gen.Parentheses(snippet_t{}, call.parms.Content));
            }
            return;
          }

          S76(require_macro);
        } else {
          // Produce a macro invocation.
          left = gen.Macro(Snip(start) /*includes trailing space*/, left, m);
          return;
        }
      }
    };

    recurse(recurse, &startlink);
  }

  bool Block(array_t &body, bool ind, bool imm, Text what) {
    switch (tok) {
    case Token("{"):
    brace : {
      // brace = scan '{' list '}' space
      Next(1);
      auto li = List("}", &GreenParser::S84);
      body = li.Content;
      return Require("}", &GreenParser::S84), Space(), true;
    }
    case Token("\r"): {
      if (ScanBrace())
        goto brace;
      goto got_imm;
    }
    case Token(":"): {
      // block = .. | ':' ind list ded
      if (ind) {
        TextPos start(*this);
        Next(1);
        Space();
        if (Ending())
          return IndList(), true;
        Restore(start);
      }
    }
    LLVM_FALLTHROUGH;
    default:
    got_imm : {
      // imm = def lookahead(ending|')'|']'|'}'|','|';'|'where'|'=>'|'<|')
      if (imm)
        return gen.ArrayAppend(body, Expr(prec_def, what)), true;
      return false;
    }
    }
  }

  syntax_t ParsePrefix(const TextPos &start, const syntax_t &func,
                                   const syntax_t *domo, const char *what) {
    // .. (braces | char | !''' open)
    TextPos local_start(*this);
    auto body = gen.ArrayNew();
    bool special = Block(body, false, false, what);
    auto parms = gen.ArrayNew(1 + (domo != nullptr));
    if (domo)
      gen.ArrayAppend(parms, *domo);
    gen.ArrayAppend(
        parms, special ? SimpleMacro(snippet_t{}, gen.Native("set"), body)
                       : Eat("'") ? (special = 1, gen.Charset(Snip(local_start),
                                                              RequireChar()))
                                  : Expr(prec_call, what));
    auto e = gen.Call(Snip(start), 0, func, parms);
    if (special)
      return SimpleMacro_1(Snip(start), gen.Native("prefix':'"), e);
    return e;
  }

  syntax_t docattr() {
    // docattr = '<|' !'>' {element} '>' space
    assert(tok == Token("<|"));
    TextPos start(*this);
    Next(2);
    if (pos[0] != '>') {
      auto content = TextSplice<filter_element>();
      Require(">", &GreenParser::S53);
      auto snippet = Snip(start);
      return Space(), MakeMarkup(snippet, gen.Native("Doc"), nullptr, content);
    }

    S45();
  }

  syntax_t RequireInequalityExpr(TokenInfo ti0) {
    Space();
    auto start = TextPos(*this);
    auto right = Expr(ti0.post_right, ti0.cs);
    auto ti1 = tokens[tok];
    if (ti1.cmp_priority <= ti0.cmp_nochain)
      return right;
    if (ti1.cmp_priority == ti0.cmp_priority) {
      Next(ti1.length), Space();
      auto parms = gen.ArrayNew(2);
      gen.ArrayAppend(parms, right);
      gen.ArrayAppend(parms, RequireInequalityExpr(ti0));
      return gen.Call(Snip(start), ti1.post_mode, gen.Native(ti1.posts), parms);
    }

    S60(ti1.cs, ti0.cs);
  }

  syntax_t ExprBase(prec p, Text what, void (GreenParser::*badtok)(Text),
                                void (GreenParser::*badprec)(Text, Text),
                                link_t<Text> *marks) {
    // prefix = ('[' space expr ']' | '?' | '^' | '+' | '-' | '*') space
    // base   = ('(' list       ')' | num | char | string | markup | qualident)
    // space
    //
    // tags   = [base {postfix | {calls} brace} '.' scan] qualident space
    // [brace] [',' scan tags] macro  = {calls} ([key] block {key block} [key
    // def] !key catch | key def) if     = 'if' word macro (scankey 'else' word
    // (block | if | !('if' word) imm) | !(scankey 'else'))
    //
    // call   = (base|path|if) .. | prefix (brace | char | !char call)
    // to     = ..                | (':') space to
    // cmp    = ..                | ('&'|'..'|'!' | 'not' word) space cmp
    // expr   = ..                | (docattr|preattr) scan expr
    assert(tok == Token(pos));
    auto tok0 = tok;
    TokenInfo ti = tokens[tok0];
    if (p <= ti.pre_left) {
      TextPos start(*this);
      switch (tok0) {
      case Token("_"): {
        auto ri = Reserved[pos]; // can/should this incorporate KeywordAccept
                                 // to avoid backtracking?
        switch (ri) {
        not_reserved:
          Restore(start);
        LLVM_FALLTHROUGH;
        case 0: {
          // base = (.. | qualident) space
          auto id = Ident();
          auto result = gen.Ident(Snip(start), id);
          Space();
          if (!marks)
            return result;
          return CheckMarkup(start, result, marks, id, true, nullptr), result;
        }
        // case Reserved["if"_u8]: {
        case res_if: {
          // if = 'if' word macro (scankey 'else' word (block | if | !('if'
          // word) imm) | !(scankey 'else'))
          Word();
          if (KeywordAccept()) {
            if (p <= prec_call) {
              syntax_t left = gen.Ident(snippet_t{}, "if");
              ParseMacro(start, left, p, true, nullptr, "", "if");
              return left;
            }
            (this->*badprec)("if", what);
          }
          goto not_reserved;
        }
        // case Reserved["not"_u8]: {
        case res_not: {
          // cmp = .. | 'not' word space cmp
          Word();
          if (KeywordAccept()) {
            if (p <= prec_cmp) {
              auto ye = Expr(prec_cmp, "not");
              return SimpleMacro_1(
                  Snip(start), gen.Ident(snippet_t{}, "operator'not'"), ye);
            }
            (this->*badprec)("not", what);
          }
          goto not_reserved;
        }
        default:
          Word();
          if (KeywordAccept())
            S21(Reserved[ri]);
          goto not_reserved;
        }
      }
      case Token("0"):
      case Token(".0"):
        return Num();

      case Token("\""): {
        Next(1);
        auto sc = TextSplice<filter_string>();
        return Require("\"", &GreenParser::S32), Space(), sc;
      }
      case Token("\'"): {
        Next(1);
        auto cy = RequireChar();
        auto c0 = start.pos[1], c1 = start.pos[2], c2 = start.pos[3],
             c3 = start.pos[4];
        auto un = chars.EncodedLength<filter_char>(c0, c1, c2, c3);
        if (pos == start.pos + un + 2)
          return gen.Unichar(Snip(start),
                             chars.EncodedUnichar(c0, c1, c2, c3, un));
        return SimpleMacro_1(Snip(start), gen.Native("prefix':'"),
                              gen.Charset(Snip(start), cy));
      }
      case Token("("): {
        Next(1);
        auto li = List(")", &GreenParser::S81);
        auto e0 = gen.Parentheses(snippet_t{}, li.Content);
        Require(")", &GreenParser::S81), Space();
        if (!(chars.flags[pos[0]] & cf_idlead))
          return e0;
        auto id = IdentNonreserved();
        if (!id)
          return e0;
        auto e1 = gen.Qualident(Snip(start), e0, id);
        if (!marks)
          return e1;
        return CheckMarkup(start, e1, marks, id, true, nullptr), e1;
      }
      case Token("<|"): {
        // cond = {docattr|..} ..
        auto parms = gen.ArrayNew(1);
        gen.ArrayAppend(parms, docattr());
        return gen.Call(Snip(start), 2,
                        Expr(ti.pre_left, "", &GreenParser::S73, &GreenParser::S63),
                        parms);
      }
      case Token("<"): {
        auto y = RequireMarkup();
        return Space(), y;
      }
      case Token("/"):
      case Token("@"):
        return RequirePath();
      case Token(".."):
      case Token("!"):
      case Token(":"):
        return Next(ti.length), Space(),
               SimpleMacro_1(Snip(start), gen.Native(ti.pres),
                              Expr(ti.pre_left, ti.cs));
      case Token("&"):
        return Next(1), Space(),
               gen.Escape(Snip(start), Expr(ti.pre_left, "&"));
      case Token("&&"):
        return Next(2), Space(),
               gen.Escape(
                   Snip(start),
                   gen.Escape(Snip(TextPos{start.pos + 1, start.LineStart,
                                           start.linecount}),
                              Expr(ti.pre_left, "&")));
      case Token("?"):
      case Token("^"):
      case Token("+"):
      case Token("-"):
      case Token("*"):
        // prefix = (.. | '?' | '^' | '+' | '-' | '*') space
        // call   = .. | prefix (brace | char | !char call)
        return Next(ti.length), Space(),
               ParsePrefix(start, gen.Native(ti.pres), nullptr, ti.cs);

      case Token("["): {
        Next(1), Space();
        if (Eat(Token("]")))
          return Space(),
                 ParsePrefix(start, gen.Native(ti.pres), nullptr, "[]");
        auto e = Expr(prec_expr, "[");
        Require("]", &GreenParser::S85);
        Space();
        if (!Ending()) {
          // prefix = ('[' space expr ']' | | ..) space
          // call   = .. | prefix (brace | char | !char call)
          return ParsePrefix(start, gen.Native(ti.posts), &e, "[]");
        } else if (p <= prec_expr) {
          // preattr = '[' space expr ']' space ending
          // expr    = .. | (docattr|preattr) scan expr
          Scan();
          syntax_t right = Expr(prec_expr, "", &GreenParser::S72, &GreenParser::S62);
          auto parms = gen.ArrayNew(1);
          gen.ArrayAppend(parms, e);
          return gen.Call(Snip(start), 2, right, parms);
        } else
          S65(what);
      }
      }

      assert(
          0); // Should never occur due to structure of the precedence table.
    }

    if (tokens[tok].pre_left == prec_never)
      (this->*badtok)(what);
    else
      (this->*badprec)(ti.cs, what);

    S99(Text{});
  }

  syntax_t Expr(prec p, Text what, void (GreenParser::*badtok)(Text) = &GreenParser::S71,
                void (GreenParser::*badprec)(Text, Text) = &GreenParser::S60,
                link_t<Text> *marks = nullptr) {
    // of      = 'of' word (block|imm)
    // calls   = ('(' list ')' | '<' space base {{calls} brace | postfix} '>')
    // space postfix = ('[' list ']' | '?' | '^' | '@' | calls !macro | '.' scan
    // qualident) space macro   = {calls} (key imm | [key] block {key block}
    // [key imm]) [catch macro] !key !catch
    //
    // call      = (base|path|if) {postfix|macro|of | ('&') scan  call } | ..
    // mul       = call { ('*' | '/'                      ) scan  call }
    // add       = mul  { ('+' | '-'                      ) scan  mul  }
    // to        = add  { ('..') scan add |          ('->') scan  to   } | ..
    // cmp       = to   { (':')      space to} {('>='|'>' ) space to   }
    //                  { ('<='|'<') space to} {('=='|'<>') space to   } | ..
    // and       = cmp  { ('and' word | '&&'              ) scan  cmp  }
    // or        = and  { ('or'  word | '||' | '|'        ) scan  or   }
    // def       = or   { ('='|':='|'+='|'-='|'*='|'/='   ) scan  def  | '!'
    // space [brace | ind list ded ]     } expr      = def  { docattr space |
    // '=>' space (brace | ind list ded | expr)
    //           | scankey 'where' word (block | def {',' scan def} !',')} |
    //           (docattr|preattr) scan expr
    TextPos start(*this);
    auto left = ExprBase(p, what, badtok, badprec, marks);
    for (;;) {
      auto tok0 = tok;
      assert(tok0 == Token(pos));
      res_t ri;
      TokenInfo ti = tokens[tok];
      if (p > ti.post_left)
        return left;
      switch (tok0) {
      case Token("_"): {
        ri = Reserved[pos];
        switch (ri) {
        // case Reserved["do"_u8]:
        // case Reserved["in"_u8]:
        // case Reserved["otherwise"_u8]:
        // case Reserved["returns"_u8]:
        // case Reserved["then"_u8]:
        // case Reserved["until"_u8]:
        case res_do:
        case res_in:
        case res_otherwise:
        case res_returns:
        case res_then:
        case res_until:
          goto got_macro;
        case res_and:
          // and = cmp { ('and' word | ..) scan cmp}
          if (p <= prec_and) {
            Word();
            auto right = Expr(prec_and, "and");
            left = SimpleMacro_2(Snip(start), gen.Native("operator'and'"),
                                  left, right);
            continue;
          }
          return left;
        case res_or:
          // or = and { ('or' word | ..) scan or}
          if (p <= prec_or) {
            Word();
            auto right = Expr(prec_or, "or");
            left = SimpleMacro_2(Snip(start), gen.Native("operator'or'"), left,
                                  right);
            continue;
          }
          return left;
        case res_of: {
          // of = 'of' word (block|imm)
          if (p <= prec_call) {
            Word();
            auto body = gen.ArrayNew();
            Block(body, true, true, Reserved[ri]);
            left = gen.Call(Snip(start), 0, left, body);
            continue;
          }
          return left;
        }
        case res_where:
        got_where:
          // expr = def { .. | scankey 'where' word (block | def {',' scan def}
          // !',')} | ..
          if (p <= prec_def) {
            Word();
            auto body = gen.ArrayNew();
            if (!Block(body, true, true, "where"))
              for (const char *what = "where";; what = ",") {
                gen.ArrayAppend(body, Expr(prec_def, what));
                if (!Eat(Token(",")))
                  break;
                Scan();
              }
            auto m = gen.Native("operator'where'");
            auto lefts = gen.ArrayNew(1);
            gen.ArrayAppend(lefts, left);
            auto clauses = gen.MacroStart(m, 2);
            gen.MacroClause(clauses, res_of, gen.ArrayNew(0), lefts);
            gen.MacroClause(clauses, res_none, gen.ArrayNew(0), body);
            left = gen.Macro(Snip(start), m, clauses);
            continue;
          }
          return left;
        case 0:
          S78();
        default:
          return left;
        }
      }
      case Token(".."):
      case Token("+"):
      case Token("-"):
      case Token("*"):
      case Token("/"): {
        Next(ti.length), Scan();
        auto parms = gen.ArrayNew(2);
        gen.ArrayAppend(parms, left);
        gen.ArrayAppend(parms, Expr(ti.post_right, ti.cs));
        left = gen.Call(Snip(start), ti.post_mode, gen.Native(ti.posts), parms);
        continue;
      }
      case Token("+="):
      case Token("-="):
      case Token("*="):
      case Token("/="):
      case Token(":="):
      case Token("&"):
      case Token("&&"):
      case Token("|"):
      case Token("||"):
      case Token("="):
      case Token("->"): {
        Next(ti.length), Scan();
        auto right = Expr(ti.post_right, ti.cs);
        left = SimpleMacro_2(Snip(start), gen.Native(ti.posts), left, right);
        continue;
      }
      case Token("?"):
      case Token("^"):
      case Token("@"): {
        Next(ti.length), Space();
        left = SimpleMacro_1(Snip(start), gen.Native(ti.posts), left);
        continue;
      }
      case Token("."): {
        // '.' scan ident
        Next(1), Scan();
        auto pos0 = pos;
        auto id = RequireQualident(".");
        left = SimpleMacro_2(Snip(start), gen.Native(ti.posts), left, id);
        auto pos1 = pos;
        Space();
        if (!marks)
          continue;
        CheckMarkup(start, left, marks, Text(pos0, pos1), true, nullptr);
        continue;
      }
      case Token(".0"):
        Next(1), S20(".");
      case Token("<|"): {
        Next(2);
        auto parms = gen.ArrayNew(1);
        gen.ArrayAppend(parms, docattr());
        left = gen.Call(Snip(start), 2, left, parms);
        continue;
      }
      case Token("\n"): {
        TextPos saved(*this);
        while (Line())
          ;
        if (pos[0] == '{')
          goto got_macro;
        if (Reserved[pos] == res_where)
          goto got_where;
        return Restore(saved), left;
      }
      case Token("["):
      case Token(":"):
      case Token("<"):
      case Token("{"):
      case Token("("):
      got_macro : {
        ParseMacro(start, left, p, false, nullptr, "", "");
        Space();
        if ((tok != Token(":")) & (tok != Token("<")))
          continue;
        // Hypothesis: Could move all of this logic to macro based on precedence
        // p, and eliminate backtracking there.
        if (tok == Token(":")) {
          if (p <= prec_cmp) { // Handle "x:t" form.
            Next(1), Space();
            left = SimpleMacro_2(Snip(start), gen.Native("operator'='"), left,
                                 SimpleMacro_1(Snip(start),
                                               gen.Native("prefix':'"),
                                               Expr(prec_to, ":")));
            continue;
          }
          return left;
        } else {
          if (p <= prec_cmp)
            goto inequality;
          return left;
        }
      }
      case Token("=="):
      case Token("<>"):
      case Token("<="):
      case Token(">"):
      case Token(">="):
      inequality : {
        // cmp = fn { (':')      space fn} {('>='|'>' ) space fn   }
        //          { ('<='|'<') space fn} {('=='|'<>') space fn   } | ..
        Next(ti.length);
        auto parms = gen.ArrayNew(2);
        gen.ArrayAppend(parms, left);
        gen.ArrayAppend(parms, RequireInequalityExpr(ti));
        left = gen.Call(Snip(start), ti.post_mode, gen.Native(ti.posts), parms);
        continue;
      }
      case Token("!"):
      case Token("=>"): {
        // '!'  space [block0 |        ind list ded]
        // '=>' space (block0 | expr | ind list ded)
        Next(ti.length), Space();
        auto bodys = gen.ArrayNew();
        if (!Block(bodys, false, false, ti.cs)) {
          if (Ending())
            bodys = IndList().Content;
          else if (tok0 == Token("=>"))
            gen.ArrayAppend(bodys, Expr(prec_expr, "=>"));
        }
        auto func = gen.Native(ti.posts);
        auto parms = gen.ArrayNew(1);
        gen.ArrayAppend(parms, left);
        auto m = gen.MacroStart(func, 2);
        gen.MacroClause(m, res_of, gen.ArrayNew(0), parms);
        gen.MacroClause(m, res_none, gen.ArrayNew(0), bodys);
        left = gen.Macro(Snip(start), func, m);
        continue;
      }
      default:
        assert(0);
      }
    }
  }

  template <bool initial>
  intp Commas(const TextPos &start, array_t &es, Text what,
              void (GreenParser::*badtok)(Text)) {
    // Commas = lookahead(';') | expr {',' scan expr}
    constexpr intp not_expr = (1LL << Token(";")) | (1LL << Token("]")) |
                              (1LL << Token(")")) | (1LL << Token("}")) |
                              (1LL << Token("\r"));
    if (!(((1LL << tok) & not_expr) | (pos == end))) {
      auto e = Expr(prec_expr, what, badtok);
      if (!Eat(Token(","))) {
        gen.ArrayAppend(es, e);
        return 1;
      }

      intp n = 0;
      if (initial) {
        gen.ArrayAppend(es, e);
        for (;;) {
          ++n;
          Scan();
          gen.ArrayAppend(es, Expr(prec_expr, what, badtok));
          if (Eat(Token(",")))
            continue;
          return n;
        }
      } else {
        auto gs = gen.ArrayNew(1);
        gen.ArrayAppend(gs, e);
        for (;;) {
          ++n;
          Scan();
          gen.ArrayAppend(gs, Expr(prec_expr, what, badtok));
          if (Eat(Token(",")))
            continue;
          return gen.ArrayAppend(es, gen.SyntaxArray(Snip(start), gs)), n;
        }
      }
    } else if (tok == Token(";")) {
      return 0;
    }

    return -1;
  }

  list_t List(Text what, void (GreenParser::*badtok)(Text)) {
    // list = push lineprefix='' scan [Commas {(';'|ending) scan Commas}] pop
    constexpr intp stop = (1LL << Token("]")) | (1LL << Token(")")) |
                          (1LL << Token("}")) | (1LL << Token("\r"));
    auto SavedContext = context;
    context.lineprefix = false;
    bool any_semi = false;
    TextPos start0(*this);
    Rescan();
    auto es0 = gen.ArrayNew();
    intp n0 = Commas<1>(start0, es0, what, badtok);
    if (n0 >= 0) {
      if ((any_semi |= Eat(Token(";"))) || tok == Token("\n")) {
        Scan();
        if (any_semi | !(((1LL << tok) & stop) | (pos == end))) {
          if (n0 != 1) {
            auto ga = gen.SyntaxArray(Snip(start0), es0);
            es0 = gen.ArrayNew();
            gen.ArrayAppend(es0, ga);
          }

          for (;;) {
            TextPos start1(*this);
            intp n1 = Commas<0>(start1, es0, what, badtok);
            if (n1 >= 0) {
              if ((any_semi |= Eat(Token(";"))) || tok == Token("\n")) {
                Scan();
                if (!(((1LL << tok) & stop) | (pos == end)))
                  continue;
              }
            }

            if (((1LL << tok) & stop) | (pos == end))
              return context = SavedContext, list_t(es0, any_semi);
            goto err;
          }
        }
      }
    }

    if (((1LL << tok) & stop) | (pos == end))
      return context = SavedContext, list_t(es0, any_semi);
  err:
    if (auto r = Reserved[pos])
      S77(Reserved[r]);
    else
      S78();
  }

  auto File() {
    // File = [0xEF 0xBB 0xBF] top=1 blockind='' lineind='' list end
    if (Eat("\xEF") && (!Eat("\xBB") || !Eat("\xBF")))
      S01();

    TextPos start(*this);
    auto li = List("", &GreenParser::S70);
    auto result = gen.File(Snip(start), li.Content);
    if (pos[0] != 0) {
      // log("The program so far: ",codify(result));
      S70("");
    }

    return result;
  }
};

} // namespace usyntax

#endif
