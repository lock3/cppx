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

bool Declaration::declIsStatic() const {
  // Checking to see if we already have created our declaration.
  if (auto curFnDecl = dyn_cast_or_null<clang::FunctionDecl>(Cxx))
    return curFnDecl->getStorageClass() == clang::SC_Static;

  // Checking scope to make sure that the current scope is inside of a class.
  if (ScopeForDecl) {
    if(!ScopeForDecl->isClassScope())
      return false;
  } else {
    return false;
  }
  auto FnDcl = getFirstDeclarator(Declarator::Function);
  if (!FnDcl)
    return false;
  auto ParamEnc = dyn_cast_or_null<EnclosureSyntax>(FnDcl->getInfo());
  if (ParamEnc) {
    if (auto ParamList = dyn_cast_or_null<ListSyntax>(ParamEnc->getOperand())) {
      const Syntax *FirstArg = ParamList->getOperand(0);
      if (auto FirstParm = dyn_cast<DeclarationSyntax>(FirstArg)) {
        if (auto FirstIdParm = dyn_cast<IdentifierSyntax>(FirstParm->getDeclarator())) {
          return FirstIdParm->getSpelling() != "this";
        }
      } else if (auto FirstIdParm = dyn_cast<IdentifierSyntax>(FirstArg)) {
        return FirstIdParm->getSpelling() != "this";
      }
      return false;
    } else {
      // Static method, basically without a this parameter.
      return true;
    }
  }

  return false;
}

bool Declaration::hasInitializer() const {
  return bool(Init);
}

const Syntax *Declaration::getInitializer() const {
  return Init;
}

const IdentifierSyntax *Declaration::asId() const {
  assert(isa<IdentifierSyntax>(Def));
  return cast<IdentifierSyntax>(Def);
}

const DeclarationSyntax *Declaration::asDef() const {
  assert(isa<DeclarationSyntax>(Def));
  return cast<DeclarationSyntax>(Def);
}

clang::SourceLocation Declaration::getErrorLocation() const {
  if (auto DS = dyn_cast_or_null<DeclarationSyntax>(Def)) {
    return DS->getErrorLocation();
  }
  if (auto IS = dyn_cast_or_null<IdentifierSyntax>(Def)) {
    return IS->getLocation();
  }
  return clang::SourceLocation();
}

Phase phaseOf(Declaration *D) {
  return D->CurrentPhase;
}

bool deduceDeclKindFromSyntax(Declaration *D) {
  assert(D && "Invalid declaration syntax");
  return true;
}

} // end namespace blue
