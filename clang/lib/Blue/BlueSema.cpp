//===- BlueSema.cpp - Semantic Analysis of Blue ASTs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Blue::Sema class, which performs semantic analysis
//  for the Blue language.
//
//===----------------------------------------------------------------------===//
#include "clang/Blue/BlueSema.h"

namespace blue {

Sema::Sema(SyntaxContext &Context, clang::Sema &CxxSema)
  : CxxSema(CxxSema), CxxContext(Context)
{ }

} // end namespace Blue