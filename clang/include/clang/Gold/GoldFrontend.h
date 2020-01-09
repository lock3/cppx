//===- GoldFrontend.h - Gold Frontend Action Interface --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GoldSyntaxAction interface, a frontend action for
//  parsing Gold Syntax only.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDFRONTEND_H
#define CLANG_GOLD_GOLDFRONTEND_H

#include "clang/Frontend/FrontendAction.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include <memory>

namespace gold {

class GoldSyntaxAction : public clang::FrontendAction {
protected:
  void ExecuteAction() override;

public:
  GoldSyntaxAction() {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    llvm::StringRef InFile) override {
    return std::make_unique<clang::ASTConsumer>();
  }
  bool usesPreprocessorOnly() const override { return false; }
  bool hasCodeCompletionSupport() const override { return false; }
};

} // namespace gold

#endif
