#if 0

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

Declarator *Declaration::getFirstDeclarator(Declarator::Kind DeclKind) const {
  Declarator *D = Decl;
  while(D) {
    if (D->getKind() == DeclKind)
      return D;
    D = D->getNext();
  }
  return nullptr;  
}

bool Declaration::declaratorContains(Declarator::Kind DeclKind) const {
  Declarator *D = Decl;
  while(D) {
    if (D->getKind() == DeclKind)
      return true;
    D = D->getNext();
  }
  return false;
}

bool Declaration::declaratorContainsClass() const {
  return declaratorContains(Declarator::Class);
}

bool Declaration::declaratorContainsFunction() const {
  return declaratorContains(Declarator::Function);
}

bool Declaration::declaratorContainsTemplate() const {
  return declaratorContains(Declarator::Template);
}

bool Declaration::declaratorContainsTag() const {
  return declaratorContains(Declarator::Class);
}

bool Declaration::declaratorContainsClassTemplate() const {
  return declaratorContainsClass() && declaratorContainsTemplate();
}

bool Declaration::declaredWithinClassBody() const {
  assert(ScopeForDecl && "Invalid scope for declaration.");
  return ScopeForDecl->isClassScope();
}

bool Declaration::isVariableDecl() const {
  return Cxx && isa<clang::VarDecl>(Cxx);
}

bool Declaration::isFunctionDecl() const {
  return Cxx && isa<clang::FunctionDecl>(Cxx);
}

bool Declaration::isClassDecl() const {
  return isDecl<clang::CXXRecordDecl>();
}

bool Declaration::isTypeAliasDecl() const {
  return isDecl<clang::TypeAliasDecl>();
}

bool Declaration::isFieldDecl() const {
  return isDecl<clang::FieldDecl>();
}

bool Declaration::isFunctionTemplate() const {
  if (!Cxx)
    return false;
  if (auto Fn = dyn_cast<clang::FunctionDecl>(Cxx)) {
    return Fn->getDescribedFunctionTemplate();
  }
  return false;
}

bool Declaration::isTypeTemplate() const {
  return Cxx && Cxx->getDescribedTemplate() && isa<clang::TypeDecl>(Cxx);
}

bool Declaration::isDeclaredInClass() const {
  return ScopeForDecl->isClassScope();
}

bool Declaration::declaresInitializedVariable() const {
  return (isDecl<clang::VarDecl>() || isDecl<clang::FieldDecl>())
      && hasInitializer();
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

#endif
