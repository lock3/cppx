//===- GoldFrontend.cpp - Gold Frontend Action Interface ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the GoldSyntaxAction interface, a frontend action for
//  parsing Gold Syntax only.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Parse/ParseAST.h"

#include "clang/Gold/GoldFrontend.h"
#include "clang/Gold/ParseGoldAST.h"

namespace gold {

void GoldSyntaxAction::ExecuteAction() {
  clang::CompilerInstance &CI = getCompilerInstance();
  if (!CI.hasPreprocessor())
    return;

  if (!CI.hasASTContext())
    return;

  if (!CI.hasSema())
    CI.createSema(getTranslationUnitKind(), nullptr);

  switch (getCurrentFileKind().getLanguage()) {
  case clang::Language::Gold:
    CI.getLangOpts().CPlusPlus = true;
    CI.getLangOpts().CPlusPlus11 = true;
    CI.getLangOpts().CPlusPlus14 = true;
    CI.getLangOpts().CPlusPlus17 = true;
    ParseGoldAST(CI.getASTContext(), CI.getPreprocessor(), CI.getSema());
    break;

  default:
    // FIXME: Why is the default to parse C++? This should be an error.
    clang::ParseAST(CI.getSema(), CI.getFrontendOpts().ShowStats,
                    CI.getFrontendOpts().SkipFunctionBodies);
    break;
  }
}


} // namespace gold
