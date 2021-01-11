#include "clang/AST/Decl.h"

#include "clang/Blue/BlueDeclaration.h"
#include "clang/Blue/BlueSema.h"
#include "clang/Blue/BlueSyntax.h"

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

bool Declaration::hasInitializer() const {
  if (auto DS = dyn_cast_or_null<DefSyntax>(Def)) {
    return DS->hasInitializer();
  }
  return false;
}

const Syntax *Declaration::getInitializer() const {
  if (auto DS = dyn_cast_or_null<DefSyntax>(Def)) {
    return DS->getInitializer();
  }
  return nullptr;
}

const DefSyntax *Declaration::asDef() const {
  assert(isa<DefSyntax>(Def));
  return cast<DefSyntax>(Def);
}

const IdentifierSyntax *Declaration::asId() const {
  assert(isa<IdentifierSyntax>(Def));
  return cast<IdentifierSyntax>(Def);
}

Phase phaseOf(Declaration *D) {
  return D->CurrentPhase;
}

} // end namespace blue
