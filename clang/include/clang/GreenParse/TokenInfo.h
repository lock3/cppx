//===- TokenInfo.h - Definitions of various Token information -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines information pertaining to tokens used by the GreenParser.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_TOKENINFO_H
#define CLANG_GREEN_TOKENINFO_H

#include "clang/GreenBasic/GreenBasic.h"

#include <cassert>

namespace usyntax {

// Precedence.
//------------------------------------------------------------------------------
enum prec : nat8 {
  prec_never,
  prec_list,
  prec_expr,
  prec_def,
  prec_or,
  prec_and,
  prec_cmp,
  prec_to,
  prec_add,
  prec_mul,
  prec_call,
  prec_attr,
  prec_base
};

// Tokens.
//------------------------------------------------------------------------------

struct TokenInfo {
  nat8 pre_mode;
  prec pre_left;
  nat8 post_mode;
  prec post_left, post_right;
  nat8 cmp_priority, cmp_nochain, length;
  const char *cs, *pres, *posts;
};

constexpr TokenInfo tokens[] = {
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 0, "", nullptr, nullptr},
    {0, prec_never, 0, prec_expr, prec_never, 0, 0, 0, "\r \n", nullptr, nullptr},
    {0, prec_base, 0, prec_expr, prec_never, 0, 0, 0,
     "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j "
     "k l m n o p q r s t u v w x y z _",
     nullptr, nullptr},
    {0, prec_base, 0, prec_never, prec_never, 0, 0, 0, "0 1 2 3 4 5 6 7 8 9",
     nullptr, nullptr},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 1, ",", nullptr, nullptr},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 1, ";", nullptr, nullptr},
    {2, prec_cmp, 2, prec_def, prec_never, 0, 0, 1, "!", "prefix'!'",
     "operator'!'"},
    {0, prec_base, 0, prec_never, prec_never, 0, 0, 1, "\"", nullptr, nullptr},
    {2, prec_cmp, 2, prec_call, prec_call, 0, 0, 1, "&", nullptr,
     "operator'&'"},
    {2, prec_cmp, 2, prec_and, prec_cmp, 0, 0, 1, "&&", nullptr,
     "operator'&&'"},
    {0, prec_base, 0, prec_never, prec_never, 0, 0, 1, "'", nullptr, nullptr},
    {0, prec_base, 0, prec_attr, prec_never, 0, 0, 1, "(", nullptr, nullptr},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 1, ")", nullptr, nullptr},
    {0, prec_call, 0, prec_mul, prec_call, 0, 0, 1, "*", "prefix'*'",
     "operator'*'"},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 2, "*=", nullptr,
     "operator'*='"},
    {0, prec_call, 0, prec_add, prec_mul, 0, 0, 1, "+", "prefix'+'",
     "operator'+'"},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 2, "+=", nullptr,
     "operator'+='"},
    {0, prec_call, 0, prec_add, prec_mul, 0, 0, 1, "-", "prefix'-'",
     "operator'-'"},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 2, "-=", nullptr,
     "operator'-='"},
    {0, prec_never, 2, prec_to, prec_to, 0, 0, 2, "->", nullptr,
     "operator'->'"},
    {0, prec_never, 2, prec_attr, prec_base, 0, 0, 1, ".", nullptr,
     "operator'.'"},
    {0, prec_base, 0, prec_attr, prec_never, 0, 0, 0,
     ".0 .1 .2 .3 .4 .5 .6 .7 .8 .9 0 1 2 3 4 5 6 7 8 9", nullptr, nullptr},
    {2, prec_cmp, 0, prec_to, prec_add, 0, 0, 2, "..", "prefix'..'",
     "operator'..'"},
    {0, prec_attr, 1, prec_mul, prec_call, 0, 0, 1, "/", nullptr,
     "operator'/'"},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 2, "/=", nullptr,
     "operator'/='"},
    {2, prec_to, 2, prec_call, prec_to, 4, 4, 1, ":", "prefix':'",
     nullptr}, // postfix min(prec_cmp for x:t, prec_call for :ind)
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 2, ":=", nullptr,
     "operator':='"},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 2, ":>", nullptr, nullptr},
    {0, prec_base, 1, prec_attr, prec_to, 2, 1, 1, "<", nullptr,
     "operator'<'"}, // postfix min(prec_cmp for a<b, prec_attr for a<b>)
    {0, prec_never, 1, prec_cmp, prec_to, 2, 1, 2, "<=", nullptr,
     "operator'<='"},
    {0, prec_never, 0, prec_cmp, prec_to, 1, 1, 2, "<>", nullptr,
     "operator'<>'"},
    {0, prec_expr, 0, prec_expr, prec_never, 0, 0, 2, "<|", nullptr, nullptr},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 1, "=", nullptr,
     "operator'='"},
    {0, prec_never, 1, prec_cmp, prec_to, 1, 1, 2, "==", nullptr,
     "operator'=='"},
    {0, prec_never, 2, prec_expr, prec_expr, 0, 0, 2, "=>", nullptr,
     "operator'=>'"},
    {0, prec_never, 1, prec_cmp, prec_to, 3, 2, 1, ">", nullptr, "operator'>'"},
    {0, prec_never, 1, prec_cmp, prec_to, 3, 2, 2, ">=", nullptr,
     "operator'<='"},
    {0, prec_call, 2, prec_attr, prec_never, 0, 0, 1, "?", "prefix'?'",
     "operator'?'"},
    {0, prec_attr, 2, prec_attr, prec_never, 0, 0, 1, "@", nullptr,
     "operator'@'"},
    {0, prec_call, 0, prec_attr, prec_attr, 0, 0, 1, "[", "prefix'[]'",
     "operator'[]'"},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 1, "]", nullptr, nullptr},
    {0, prec_call, 2, prec_attr, prec_never, 0, 0, 1, "^", "prefix'^'",
     "operator'^'"},
    {0, prec_never, 0, prec_attr, prec_never, 0, 0, 1, "{", nullptr, nullptr},
    {0, prec_never, 2, prec_def, prec_def, 0, 0, 1, "|", nullptr,
     "operator'|'"},
    {0, prec_never, 2, prec_or, prec_and, 0, 0, 2, "||", nullptr,
     "operator'||'"},
    {0, prec_never, 0, prec_never, prec_never, 0, 0, 1, "}", nullptr, nullptr},
};

constexpr nat8 Token(char8 c0, char8 c1 = 0) noexcept {
  struct token_helper {
    nat8 second_map[256];
    nat16 first_map[256];
    nat8 popcnt[128];

    constexpr token_helper() : second_map{}, first_map{}, popcnt{} {
      for (intp i = 0, n = array_size(popcnt); i < n; i++)
        popcnt[i] = (i & 1) + ((i & 2) != 0) + ((i & 4) != 0) + ((i & 8) != 0) +
                    ((i & 16) != 0) + ((i & 32) != 0) + ((i & 64) != 0);

      for (nat8 i = '0'; i <= '9'; i++)
        second_map[i] = 1;

      nat8 second_bit = 2;
      for (intp i = 0, n = array_size(tokens); i < n; i++) {
        auto t = tokens[i];
        intp j = 0;

        while (t.cs[j]) {
          if (first_map[t.cs[j]] == 0)
            first_map[t.cs[j]] = nat16(i) << 8;
          intp two_chars = t.cs[j + 1] && t.cs[j + 1] != ' ';

          if (two_chars) {
            nat8 &m1 = second_map[t.cs[j + 1]];
            if (!m1)
              m1 = second_bit, second_bit *= 2;
            first_map[t.cs[j]] |= m1;
          }

          if (!t.cs[j + 1 + two_chars])
            break;

          j += 2 + two_chars;
        }
      }
    }
  };

  constexpr token_helper helper;
  nat16 m0 = helper.first_map[c0];
  nat8 m1 = helper.second_map[c1];

  return nat8(m0 / 256 +
              (-((m0 & m1) != 0) & helper.popcnt[nat8(2 * m1 - 1) & nat8(m0)]));
}

constexpr nat8 Token(const char *op) noexcept { return Token(op[0], op[1]); }
constexpr nat8 Token(const char8 *op) noexcept {
  return Token(op[0], op[1]);
}

// Character characterization.

static const nat8 cf_idlead = 1, cf_digit = 2, cf_hex = 4, cf_string = 8,
                  cf_idquote = 16, cf_backslash = 32,
                  cf_idtail = cf_idlead | cf_digit;
static const nat8 cat_line = 0, cat_control = 1, cat_single_quote = 2,
                  cat_double_quote = 3, cat_lt = 4, cat_hash = 5,
                  cat_special = 6, cat_element = 7, cat_ascii_print = 8,
                  cat_u2 = 9, cat_u3a = 10, cat_u3b = 11, cat_u3c = 12,
                  cat_u3d = 13, cat_u4a = 14, cat_u4b = 15, cat_u4c = 16,
                  cat_ubad = 17, cat_max = 18;
static const nat8 filter_utf8 = 0, filter_blockcmt = 1, filter_linecmt = 2,
                  filter_indcmt = 3, filter_char = 4, filter_string = 5,
                  filter_element = 6, filter_max = 7;
struct chars_t {
  nat8 flags[256], hexval[256], category[256];
  nat32 filters[filter_max][cat_max];
  constexpr chars_t() noexcept
      : flags{}, hexval{}, category{},
        filters{
            /*filter_     line       control    '          "          < #
               \{}        >&~        ascii      0xC2-0xDF  0xE0       0xE1-0xEC
               0xED       0xEE-0xEF  0xF0       0xF1-0xF3  0xF4       ubad */
            /*utf8    */ {0x10000000, 0x10000000, 0x10000000, 0x10000000,
                          0x10000000, 0x10000000, 0x10000000, 0x10000000,
                          0x10000000, 0x200F0FF0, 0x30BF3FF0, 0x30BF0FF0,
                          0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0, 0x4BBF0FF0,
                          0x4BBFEFF0, 0},
            /*blockcmt*/
            {0x00000000, 0x00000000, 0x10000000, 0x10000000, 0x10000001,
             0x10000002, 0x10000000, 0x10000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
            /*linecmt */
            {0x00000000, 0x00000000, 0x10000000, 0x10000000, 0x10000001,
             0x10000002, 0x10000000, 0x10000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
            /*indcmt  */
            {0x00000000, 0x00000000, 0x10000000, 0x10000000, 0x10000001,
             0x10000002, 0x10000000, 0x10000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
            /*char    */
            {0x00000000, 0x00000000, 0x00000000, 0x10000000, 0x10000001,
             0x10000002, 0x00000000, 0x10000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
            /*string  */
            {0x00000000, 0x00000000, 0x10000000, 0x00000000, 0x10000001,
             0x10000002, 0x00000000, 0x10000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
            /*element */
            {0x00000000, 0x00000000, 0x10000000, 0x10000000, 0x00000000,
             0x10000002, 0x00000000, 0x00000000, 0x10000000, 0x200F0FF0,
             0x30BF3FF0, 0x30BF0FF0, 0x30BFCFF0, 0x30BF0FF0, 0x4BBF1FF0,
             0x4BBF0FF0, 0x4BBFEFF0, 0},
        } {
    // Rejection mask for '#'=0x1,
    // '>'=0x2,c1_flags=0xFFFF0,c2_flags=0xF00000,c3_flags=0xF000000; length
    // mask =0x70000000
    for (intp i = 0; i < 256; ++i) {
      flags[i] =
        cf_idlead *
        ((i >= 'A' && i <= 'Z') || (i >= 'a' && i <= 'z') || i == '_') +
        cf_digit * (i >= '0' && i <= '9') +
        cf_hex * ((i >= '0' && i <= '9') || (i >= 'A' && i <= 'F') ||
                  (i >= 'a' && i <= 'f')) +
        cf_string * (i >= 0x20 && i <= 0x7E && i != '{' && i != '}' &&
                     i != '"' && i != '\\' && i != '\r' && i != '\n') +
        cf_idquote * (i >= 0x20 && i <= 0x7E && i != '{' && i != '}' &&
                      i != '"' && i != '\'' && i != '\\' && i != '#') +
        cf_backslash * (i == 'n' || i == 'r' || i == 't' || i == '\\' ||
                        i == '"' || i == '\'' || i == '>' || i == '&' ||
                        i == '~' || i == '{' || i == '}');

      hexval[i] =
          nat8(i >= '0' && i <= '9'
                   ? i - '0'
                   : i >= 'A' && i <= 'F' ? i - 'A'
                                          : i >= 'a' && i <= 'f' ? i - 'a' : 0);

      category[i] =
        i=='\r'||i=='\n'? cat_line: ((i < 0x20 && i != 0x09) || i==0x7f) ? cat_control:
        i=='\''? cat_single_quote: i=='"'? cat_double_quote: i=='<'? cat_lt: i=='#'? cat_hash: 
        i=='\\'||i=='{'||i=='}'? cat_special: i=='>'||i=='&'||i=='~'? cat_element: i<0x7F? cat_ascii_print:
        i<0xC2? cat_ubad:
        i<0xE0? cat_u2:
        i<0xE1? cat_u3a: i<0xED? cat_u3b: i<0xEE? cat_u3c: i<0xF0? cat_u3d:
        i<0xF1? cat_u4a: i<0xF4? cat_u4b: i<0xF5? cat_u4c: cat_ubad;
    }
  }

  template <nat8 filter>
  intp EncodedLength(char8 c0, char8 c1, char8 c2,
                     char8 c3) const noexcept {
    nat32 value =
        filters[filter]
               [category[c0]]; // 256B category table + 4*18*4B filter table
    nat32 mask = (c1 == '#') + (c1 == '>') * 2 + (0x10 << c1 / 16) +
                 (0x100000 << c2 / 0x40) + (0x1000000 << c3 / 0x40);

    auto length = value / 0x10000000 & -!(value & mask);

    return length;
  }

  nat32 EncodedUnichar(char8 c0, char8 c1, char8 c2,
                        char8 c3, intp length) const noexcept {
    static const nat8 shifts[5] = {0, 18, 12, 6, 0};
    static const nat32 masks[5] = {0, 0x3F, 0x7FF, 0xFFFF, 0x1FFFFF};

    return ((nat32(c0) * 0x40000 + nat32(c1 & 0x3F) * 0x1000 +
             nat32(c2 & 0x3F) * 0x40 + nat32(c3 & 0x3F)) >>
            shifts[length]) &
           masks[length];
  }

  intp MakeUtf8(char8 *cs, nat32 ch) const noexcept {
    assert(ch < 0x10FFFF);

    if (ch <= 0x007F)
      return cs[0] = char8((ch)), 1;
    else if (ch <= 0x07FF)
      return cs[0] = char8((ch >> 6) | 0xC0),
        cs[1] = char8(((ch)&0x3F) | 0x80), 2;
    else if (ch <= 0xFFFF)
      return cs[0] = char8((ch >> 12) | 0xE0),
        cs[1] = char8(((ch >> 6) & 0x3F) | 0x80),
        cs[2] = char8((ch & 0x3F) | 0x80), 3;
    else
      return cs[0] = char8((ch >> 18) | 0xF0),
        cs[1] = char8(((ch >> 12) & 0x3F) | 0x80),
        cs[2] = char8(((ch >> 6) & 0x3F) | 0x80),
        cs[3] = char8((ch & 0x3F) | 0x80), 4;
  }
};

constexpr auto chars = chars_t();
//------------------------------------------------------------------------------

} // namespace usyntax

#endif
