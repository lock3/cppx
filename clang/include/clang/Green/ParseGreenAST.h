//===- ParseGreenAST.h - Define the ParseGreenAST method ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the usyntax::ParseGreenAST method.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENPARSE_PARSEGREENAST
#define CLANG_GREEN_GREENPARSE_PARSEGREENAST

namespace clang {
class ASTContext;
class Preprocessor;
class Sema;
}

namespace lock3 {

void ParseGreenAST(clang::ASTContext &ClangContext, clang::Preprocessor &PP,
                   clang::Sema &ClangSema);

} // namespace lock3

#endif
