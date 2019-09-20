//===- GreenFrontend.cpp - Green Frontend Action Interface ---------------===//
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

#include "clang/GreenFront/GreenFrontend.h"
#include "clang/GreenParse/ParseGreenAST.h"

namespace lock3 {

void GreenSyntaxAction::ExecuteAction() {
  CompilerInstance &CI = getCompilerInstance();
  if (!CI.hasPreprocessor())
    return;

  if (!CI.hasASTContext())
    return;

  if (!CI.hasSema())
    CI.createSema(getTranslationUnitKind(), nullptr);

  ParseGreenAST(CI.getASTContext(), CI.getPreprocessor(), CI.getSema());
}


} // namespace lock3
