#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"

#include "clang/Gold/GoldScope.h"


namespace clang {

CppxNamespaceDecl *CppxNamespaceDecl::Create(const ASTContext &C,
                                             DeclContext *DC,
                                             SourceLocation L,
                                             IdentifierInfo *II,
                                             NamespaceDecl *NS,
                                             gold::Scope *Rep) {
  auto *Ret = new (C, DC) CppxNamespaceDecl(Decl::CppxNamespace,
                                            C, DC, L, II, Rep);
  // NS can be null in the case of the global scope specifier.
  if (!NS)
    return Ret;
  Ret->setTypeForDecl(C.getCppxNamespaceType(NS).getTypePtr());
  return Ret;
}

NamespaceDecl *CppxNamespaceDecl::getNamespace() {
  assert(isa<CppxNamespaceType>(getTypeForDecl()));
  auto *Ty = getTypeForDecl()->getAs<CppxNamespaceType>();
  return Ty->getDecl();
}

gold::Scope *CppxNamespaceDecl::getScopeRep() {
  return Rep;
}

} // namespace clang
