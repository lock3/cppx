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
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenParse/ParseGreenAST.h"
#include "clang/GreenParse/GreenParser.h"
#include "clang/GreenSema/Cppify.h"
#include "clang/GreenSema/GreenSema.h"

using namespace clang;

namespace lock3 {

inline void ParseGreenAST(ASTContext &ClangContext, Preprocessor &PP,
                          Sema &ClangSema) {
  using namespace std;
  using namespace usyntax;

  FileID MainFID = PP.getSourceManager().getMainFileID();
  const FileEntry *File = PP.getSourceManager().getFileEntryForID(MainFID);
  SyntaxContext Context(ClangContext);
  GenerateSyntax Generator(make_shared<string>(File->getName()),
                           Context);

  // Use the Green Parser to create a syntax vector.
  GreenParser<GenerateSyntax> TheParser(Generator, PP.getSourceManager(),
                                        MainFID);
  auto SourceSyntaxes = TheParser.File();

  GreenSema Actions(Context, PP, ClangSema);

  // Map the identifiers to clang constructs.
  Actions.MapIdentifiers(SourceSyntaxes);
}

} // namespace lock3

#endif
