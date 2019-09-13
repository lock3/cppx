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

  ParseGreenAST(CI.getASTContext(), CI.getPreprocessor());
}


} // namespace lock3
