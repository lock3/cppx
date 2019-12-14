//===- GreenFrontend.cpp - Green Frontend Action Interface ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GreenSyntaxAction interface, a frontend action for
//  parsing Green Syntax only.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Parse/ParseAST.h"

#include "clang/Green/GreenFrontend.h"
#include "clang/Green/ParseGreenAST.h"

namespace lock3 {

void GreenSyntaxAction::ExecuteAction() {
  llvm::outs() << "egsecuding agjion :-DDDD\n";
  llvm::outs() << getCurrentFile() << '\n';

  CompilerInstance &CI = getCompilerInstance();
  if (!CI.hasPreprocessor())
    return;

  if (!CI.hasASTContext())
    return;

  if (!CI.hasSema())
    CI.createSema(getTranslationUnitKind(), nullptr);

  switch (getCurrentFileKind().getLanguage()) {
  case clang::Language::Green:
    ParseGreenAST(CI.getASTContext(), CI.getPreprocessor(), CI.getSema());
    break;

  default:
    // FIXME: Why is the default to parse C++? This should be an error.
    clang::ParseAST(CI.getSema(), CI.getFrontendOpts().ShowStats,
                    CI.getFrontendOpts().SkipFunctionBodies);
    break;
  }
}


} // namespace lock3
