#include "clang/Sema/Sema.h"
namespace clang {
void Sema::ActOnStartOfBlueLambdaDefinition(
  blue::Sema &BlueSema, LambdaIntroducer &Intro,
  llvm::SmallVectorImpl<clang::ParmVarDecl *> &EParams,
  Scope *CurScope, QualType TrailingReturn, bool IsMutable) {

}
}