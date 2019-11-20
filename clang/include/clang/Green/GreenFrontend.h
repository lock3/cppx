//===- GreenFrontend.h - Green Frontend Action Interface ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GreenSyntaxAction interface, a frontend action for
//  parsing Green Syntax only.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_GREENFRONT_GREENFRONTEND_H
#define CLANG_GREEN_GREENFRONT_GREENFRONTEND_H

#include "clang/Frontend/FrontendAction.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include <memory>

using namespace clang;

namespace lock3 {

class GreenSyntaxAction : public FrontendAction {
protected:
  void ExecuteAction() override;

public:
  GreenSyntaxAction() {}

  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI, StringRef InFile) override {
    return std::make_unique<ASTConsumer>();
  }
  bool usesPreprocessorOnly() const override { return false; }
  bool hasCodeCompletionSupport() const override { return false; }
};

} // namespace lock3

#endif
