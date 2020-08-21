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

gold::Scope *CppxNamespaceDecl::getScopeRep() {
  return Rep;
}

} // namespace clang
