#include "clang/AST/Decl.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Type.h"

// #include "clang/Gold/GoldScope.h"

namespace clang {

CppxNamespaceDecl *
CppxNamespaceDecl::Create(ASTContext &C, DeclContext *DC, bool Inline,
                          SourceLocation StartLoc, SourceLocation IdLoc,
                          IdentifierInfo *Id, NamespaceDecl *PrevDecl,
                          gold::Scope *GScope) {
  return new (C, DC) CppxNamespaceDecl(C, DC, Inline, StartLoc, IdLoc, Id,
                                       PrevDecl, GScope);
}

CppxNamespaceDecl *
CppxNamespaceDecl::Create(ASTContext &C, DeclContext *DC, bool Inline,
                          SourceLocation StartLoc, SourceLocation IdLoc,
                          IdentifierInfo *Id, NamespaceDecl *PrevDecl,
                          blue::Scope *BScope) {
  return new (C, DC) CppxNamespaceDecl(C, DC, Inline, StartLoc, IdLoc, Id,
                                       PrevDecl, BScope);
}

gold::Scope *CppxNamespaceDecl::getScopeRep() {
  assert(!BlueScope);
  return Rep;
}

blue::Scope *CppxNamespaceDecl::getBlueScopeRep() {
  assert(!Rep);
  return BlueScope;
}

} // namespace clang
