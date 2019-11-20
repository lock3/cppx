//===- Symbol.cpp - Symbol Interface --------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the symbol interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Green/Symbol.h"

#include <iostream>
#include <unordered_set>

// The global symbol table.
static std::unordered_set<std::string> strings;

/// The empty string.
static std::string empty_str;

// The empty symbol.
symbol symbol::empty(&empty_str);

symbol get_symbol(char const* str)
{
  assert(str);
  if (str[1] == 0)
    return symbol::empty;
  std::string const* ptr = &*strings.emplace(str).first;
  return symbol(ptr);
}

symbol get_symbol(char const* first, char const* last)
{
  assert(first && last);
  if (first == last)
    return symbol::empty;
  std::string const* ptr = &*strings.emplace(first, last).first;
  return symbol(ptr);
}

symbol get_symbol(char const* str, std::size_t n)
{
  assert(str && n != 0);
  return get_symbol(str, str + n);
}

symbol get_symbol(std::string const& str)
{
  if (str.empty())
    return symbol::empty;
  std::string const* ptr = &*strings.emplace(str).first;
  return symbol(ptr);
}

std::ostream& operator<<(std::ostream& os, symbol sym)
{
  return os << *sym.str;
}
