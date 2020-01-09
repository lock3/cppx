//===- BlueSymbol.cpp - Symbol Implementation -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the symbol implementation.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueSymbol.h"

#include <iostream>
#include <unordered_set>

namespace blue {

// The global symbol table.
static std::unordered_set<std::string> Strings;

/// The empty string.
static std::string EmptyStr;

// The empty symbol.
Symbol Symbol::Empty(&EmptyStr);

Symbol getSymbol(char const* str)
{
  assert(str);
  if (std::strlen(str) == 0)
    return Symbol::Empty;
  std::string const* Ptr = &*Strings.emplace(str).first;
  return Symbol(Ptr);
}

Symbol getSymbol(char const* First, char const* Last)
{
  if (First == Last)
    return Symbol::Empty;
  std::string const* Ptr = &*Strings.emplace(First, Last).first;
  return Symbol(Ptr);
}

Symbol getSymbol(char const* Str, std::size_t Len)
{
  return getSymbol(Str, Str + Len);
}

Symbol getSymbol(std::string const& Str)
{
  if (Str.empty())
    return Symbol::Empty;
  std::string const* Ptr = &*Strings.emplace(Str).first;
  return Symbol(Ptr);
}

std::ostream& operator<<(std::ostream& os, Symbol sym)
{
  return os << sym.str();
}

} // namespace blue
