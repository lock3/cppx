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

#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenParse/ParseGreenAST.h"
#include "clang/GreenParse/GreenParser.h"
#include "clang/GreenSema/Cppify.h"
#include "clang/GreenSema/GreenSema.h"

#include <string>
#include <cstdlib>
#include <fstream>

using namespace clang;

namespace lock3 {

inline void ParseGreenAST(ASTContext &ClangContext, Preprocessor &PP,
                          Sema &ClangSema) {
  using namespace std;
  using namespace usyntax;

  // Use the Green Parser to create a syntax vector.
  string src_filename{"../usyntax/test.usyntax"};
  ifstream src_file{src_filename};
  string src_text{istreambuf_iterator<char>(src_file),
                  istreambuf_iterator<char>()};

  SyntaxContext Context(ClangContext);
  GenerateSyntax Generator(make_shared<string>(src_filename),
                           Context);

  auto src_syntaxs =
    GreenParser<GenerateSyntax>(
      Generator, (char8 *)&src_text[0], (char8 *)&src_text[src_text.size()])
    .File();

  GreenSema Actions(Context, PP, ClangSema);

  // Map the identifiers to clang constructs.
  Actions.MapIdentifiers(src_syntaxs);
}

} // namespace lock3

#endif
