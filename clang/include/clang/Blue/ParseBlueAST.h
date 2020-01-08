//===- ParseBlueAST.h - Define the ParseBlueAST method --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the blue::ParseBlueAST method.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_PARSEBLUEAST
#define CLANG_BLUE_PARSEBLUEAST

namespace clang {

class ASTContext;
class Preprocessor;
class Sema;

} // namespace clang


namespace blue {

void ParseBlueAST(clang::ASTContext &ClangContext, clang::Preprocessor &PP,
                  clang::Sema &ClangSema);

} // namespace blue

#endif
