//===- BlueSema.h - Blue sematic actions ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the sema class for the blue language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUE_SEMA_H
#define CLANG_BLUE_BLUE_SEMA_H

#include "clang/AST/ExprCppx.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Type.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "clang/Sema/Sema.h"

#include "clang/Blue/BlueSyntaxContext.h"
#include "clang/Blue/BlueScope.h"
#include <memory>
#include <vector>



namespace clang {

class CppxNamespaceDecl;
class Decl;
class DeclContext;
class DiagnosticsEngine;
class LookupResult;
class Preprocessor;
class CXXRecordDecl;
class Sema;
class Stmt;
class Type;
class CppxTypeLiteral;
class CppxDeclRefExpr;
class TypeSourceInfo;
class DeclContext;
class CppxDependentMemberAccessExpr;
} // namespace clang




namespace blue {

class Sema {
  /// Syntactic context
  blue::SyntaxContext &Context;

  /// The clang semantic object, allows to create various syntax nodes
  /// as well as perform important transformations on them.
  clang::Sema &CxxSema;

  clang::ASTContext &CxxAST;

  /// Stack of active Scopes.
  llvm::SmallVector<Scope *, 4> ScopeStack;

  /// The declaration context.
  Declaration *CurrentDecl = nullptr;

  /// A mapping of clang Decl nodes to Blue declarations.
  std::unordered_map<clang::Decl *, Declaration *> DeclToDecl;

public:
  Sema(SyntaxContext &Context, clang::Sema &S);
  ~Sema();

  llvm::StringMap<clang::BinaryOperatorKind> BinOpMap;
  llvm::StringMap<clang::UnaryOperatorKind> UnaryOpMap;

  clang::QualType DefaultCharTy;

  clang::DeclContext *CurContext = nullptr;

  clang::Sema &getCxxSema();
  clang::ASTContext &getCxxAST();

  // Perform unqualified lookup of a name in the current scope.
  bool lookupUnqualifiedName(clang::LookupResult &R);

  // Perform unqualified lookup of a name starting in S.
  bool lookupUnqualifiedName(clang::LookupResult &R, Scope *S);

  Scope *getCurrentScope();

  /// Enter a new scope corresponding to the syntax S.
  void enterScope(Scope::Kind K, const Syntax *S);

  /// Leave the current scope. The syntax S must match the syntax for
  /// which the scope was initially pushed.
  void leaveScope(const Syntax *S);

  /// Push a new scope.
  void pushScope(Scope *S);

  /// Pop the current scope, returning it.
  Scope *popScope();

  /// Returns the current DeclContext that's set within the clang::Sema.
  /// It's worth noting that getCurrentCxxDeclContext doesn't always equal
  /// getCurClangDeclContext.
  clang::DeclContext *getCurClangDeclContext() const;

  /// The current declaration.
  Declaration *getCurrentDecl() {
    return CurrentDecl;
  }

  /// Make D the current declaration.
  void pushDecl(Declaration *D);

  /// Sets the decl context without modifying the clang::Sema class
  void setCurrentDecl(Declaration *D);

  /// Make the owner of CurrentDecl current.
  void popDecl();

  // Dictionary of built in types.
  const llvm::StringMap<clang::QualType> BuiltinTypes;
  const llvm::StringMap<clang::QualType> createBuiltinTypeList();

  clang::CppxTypeLiteral *buildTypeExpr(clang::QualType Ty,
                                        clang::SourceLocation Loc);
  clang::CppxTypeLiteral *buildTypeExpr(clang::TypeSourceInfo *TInfo);
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::TypeSourceInfo *TInfo);
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::QualType Ty,
                                           clang::SourceLocation Loc);
  clang::CppxTypeLiteral *buildFunctionTypeExpr(clang::QualType FnTy,
                                                clang::SourceLocation BeginLoc,
                                                clang::SourceLocation LParenLoc,
                                                clang::SourceLocation RParenLoc,
                                                clang::SourceRange ExceptionSpecRange,
                                                clang::SourceLocation EndLoc,
                           llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params);

private:
  friend struct Declaration;
  void addDeclToDecl(clang::Decl *Cxx, Declaration *Blue);
  Declaration *getDeclaration(clang::Decl *Cxx);

//===----------------------------------------------------------------------===//
//                               RAII Objects                                 //
//===----------------------------------------------------------------------===//

public:
  // An RAII type for constructing scopes.
  struct ScopeRAII {
    ScopeRAII(Sema &S, Scope::Kind K, const Syntax *ConcreteTerm)
      : S(S), ConcreteTerm(ConcreteTerm) {
      S.enterScope(K, ConcreteTerm);
    }

    ~ScopeRAII() {
      S.leaveScope(ConcreteTerm);
    }

  private:
    Sema &S;
    const Syntax *ConcreteTerm;
  };
};

} // end namespace blue

#endif
