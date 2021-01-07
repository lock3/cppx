#include "clang/AST/Decl.h"

#include "clang/Blue/BlueDeclaration.h"
#include "clang/Blue/BlueSema.h"

namespace blue {


llvm::StringRef phaseToStr(Phase p) {
  switch(p) {
  case Phase::Unprocessed:
    return "Unprocessed";
  case Phase::Identification:
    return "Identification";
  case Phase::Typing:
    return "Typing";
  case Phase::Initialization:
    return "Initialization";
  }
  llvm_unreachable("Invalid Phase");
}

void Declaration::setCxx(Sema &SemaRef, clang::Decl *Cxx) {
  SemaRef.addDeclToDecl(Cxx, this);
  this->Cxx = Cxx;
}

bool Declaration::IsVariableDecl() const {
  return Cxx && isa<clang::VarDecl>(Cxx);
}

Phase phaseOf(Declaration *D) {
  return D->CurrentPhase;
}

} // end namespace blue
