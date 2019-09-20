//===- Basic.h - Basic definitions for green ---------------------------======//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares Basic definitions for the Green language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENBASIC_H
#define CLANG_GREEN_GREENBASIC_H

namespace usyntax {

// Numeric types.
using intp = long long;
using nat32 = unsigned long;
using nat16 = unsigned short;
using nat8 = unsigned char;
using char8 = unsigned char;

// Basic functions.
template <class t, intp n>
constexpr intp array_size(t (&)[n]) noexcept {
  return n;
}

// Basic data structures.
struct snippet_t {
  intp start_line, start_ofs, end_line, end_ofs;
};

struct parse_err_t {
  snippet_t snip;
  const char *code, *message;
};

template <class t>
struct link_t {
  t value;
  link_t *next;
};

} // namespace usyntax

#endif
