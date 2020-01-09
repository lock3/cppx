//===- ParseBlueAST.cpp - Implement the ParseBlueAST method ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the blue::ParseBlueAST method.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Blue/ParseBlueAST.h"

using namespace clang;

namespace blue {

void ParseBlueAST(ASTContext &ClangContext, Preprocessor &PP,
                  Sema &ClangSema) {
  llvm::errs() << "Parsing blue AST...\n";
}

}
