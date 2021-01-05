//===- BlueFrontend.h - Blue Frontend Action Interface ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the BlueSyntaxAction interface, a frontend action for
//  parsing Blue Syntax only.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUEFRONT_BLUEFRONTEND_H
#define CLANG_BLUE_BLUEFRONT_BLUEFRONTEND_H

#include "clang/Frontend/FrontendAction.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include <memory>

using namespace clang;

namespace blue {

class BlueSyntaxAction : public clang::ASTFrontendAction {
protected:
  void ExecuteAction() override;

public:
  BlueSyntaxAction() {}
  BlueSyntaxAction(clang::ast_matchers::MatchFinder* Finder)
    :Matcher(Finder)
  { }

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    llvm::StringRef InFile) override {
    if(Matcher) {
      return Matcher->newASTConsumer();
    } else {
      return std::make_unique<clang::ASTConsumer>();
    }
  }

  bool usesPreprocessorOnly() const override { return false; }
  bool hasCodeCompletionSupport() const override { return false; }

  bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
    if (!clang::ASTFrontendAction::BeginSourceFileAction(CI))
      return false;
    return true;
  }

  void EndSourceFileAction() override {
    clang::ASTFrontendAction::EndSourceFileAction();
  }

private:
  clang::ast_matchers::MatchFinder *Matcher = nullptr;
};

class BlueSyntaxActionDumper : public clang::ASTFrontendAction {
protected:
  void ExecuteAction() override;

public:
  BlueSyntaxActionDumper() {}

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI,
                    llvm::StringRef InFile) override {
    return std::make_unique<clang::ASTConsumer>();
  }
  bool usesPreprocessorOnly() const override { return false; }
  bool hasCodeCompletionSupport() const override { return false; }

  bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
    if (!clang::ASTFrontendAction::BeginSourceFileAction(CI))
      return false;
    return true;
  }

  void EndSourceFileAction() override {
    clang::ASTFrontendAction::EndSourceFileAction();
  }
};

} // namespace blue

#endif
