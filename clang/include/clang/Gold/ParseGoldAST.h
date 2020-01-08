//===- ParseGoldAST.h - Define the ParseGoldAST method --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the gold::ParseGoldAST method.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_PARSEGOLDAST
#define CLANG_GOLD_PARSEGOLDAST

namespace clang {
class ASTContext;
class Preprocessor;
class Sema;
}

namespace gold {

void ParseGoldAST(clang::ASTContext &ClangContext, clang::Preprocessor &PP,
                  clang::Sema &ClangSema);

} // namespace gold

#endif
