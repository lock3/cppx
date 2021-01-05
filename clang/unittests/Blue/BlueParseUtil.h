//===- unittest/Blue/BlueParseUtil.h ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_UNITTESTS_PARSE_UTIL_H
#define CLANG_BLUE_UNITTESTS_PARSE_UTIL_H

#include "gtest/gtest.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueFile.h"
#include "clang/Blue/BlueLexer.h"
#include "clang/Blue/BlueParser.h"
#include "clang/Blue/BlueSema.h"
#include "clang/Blue/ParseBlueAST.h"
#include "clang/Blue/BlueSyntaxContext.h"
#include "clang/Blue/BlueSyntax.h"

#include "clang/Blue/BlueSyntaxVisitor.h"
#include "clang/Tooling/Tooling.h"


#include "clang/AST/Mangle.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/LLVM.h"
#include "llvm/IR/DataLayout.h"
#include "clang/Blue/BlueFrontend.h"

namespace blue {

inline void SimpleBlueParseTest(llvm::StringRef Code) {
  // using namespace clang::ast_matchers;
  using namespace clang::tooling;
  using namespace clang;
  // MatchFinder Finder;
  std::unique_ptr<FrontendActionFactory> Factory(
      newFrontendActionFactory<BlueSyntaxAction>());
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "blue", "-c"},
                            "temp.usyntax")) {
    ASSERT_FALSE(true) << "Parsing error in \"" << Code.str() << "\"";
  }
}

/// Test that something doesn't compile and returns an error
inline void BlueFailureTest(llvm::StringRef Code) {
  using namespace clang::tooling;
  using namespace clang;
  std::unique_ptr<FrontendActionFactory> Factory(
      newFrontendActionFactory<BlueSyntaxAction>());
  if (runToolOnCodeWithArgs(Factory->create(), Code,
                            {"-x", "blue", "-c", "-Wall", "-Werror"},
                            "temp.usyntax")) {
    ASSERT_FALSE(true) << "Unexpected success \"" << Code.str() << "\"";
  }
}

}

#endif