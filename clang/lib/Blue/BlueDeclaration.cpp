#include "clang/AST/Decl.h"

#include "clang/Blue/BlueDeclaration.h"
#include "clang/Blue/BlueSema.h"

namespace blue {

void Declaration::setCxx(Sema &SemaRef, clang::Decl *Cxx) {
  SemaRef.addDeclToDecl(Cxx, this);
  this->Cxx = Cxx;
}

} // end namespace blue
