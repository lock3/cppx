#ifndef LLVM_CLANG_GREENFRONT_GREENFRONTEND_H
#define LLVM_CLANG_GREENFRONT_GREENFRONTEND_H

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
