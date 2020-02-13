#ifndef CLANG_GOLD_UNITTESTS_PARSE_UTIL_H
#define CLANG_GOLD_UNITTESTS_PARSE_UTIL_H

#include "gtest/gtest.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldFile.h"
#include "clang/Gold/GoldLexer.h"
#include "clang/Gold/GoldParser.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/ParseGoldAST.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldSyntax.h"

#include "clang/Gold/GoldSyntaxVisitor.h"
#include "clang/Tooling/Tooling.h"


#include "clang/AST/Mangle.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/LLVM.h"
#include "llvm/IR/DataLayout.h"
#include "clang/Gold/GoldFrontend.h"

namespace gold {

inline void SimpleGoldParseTest(llvm::StringRef Code) {
  // using namespace clang::ast_matchers;
  using namespace clang::tooling;
  using namespace clang;
  // MatchFinder Finder;
  std::unique_ptr<FrontendActionFactory> Factory(
      newFrontendActionFactory<GoldSyntaxAction>());
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "gold"},
                            "temp.usyntax")){
    ASSERT_FALSE(true) << "Parsing error in \"" << Code.str() << "\"";
  }
}

}

#endif