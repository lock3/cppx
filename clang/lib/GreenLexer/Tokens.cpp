//===- Tokens.cpp - Lock3 Token Interface ---------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the base class for Tokens used in Lock3 compilers.
//
//===----------------------------------------------------------------------===//

#include "clang/GreenBasic/Tokens.h"

char const* token::spelling() const
{
  if (kind == 0)
    return "<end of input>";
  return sym.data();
}

