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
  // Syntactic context
  blue::SyntaxContext &Context;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &CxxSema;

  clang::ASTContext &CxxAST;

  // Stack of active Scopes.
  llvm::SmallVector<Scope *, 4> ScopeStack;


public:
  Sema(SyntaxContext &Context, clang::Sema &S);
  ~Sema();

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

  // Dictionary of built in types.
  const llvm::StringMap<clang::QualType> BuiltinTypes;
  const llvm::StringMap<clang::QualType> createBuiltinTypeList();

  clang::CppxTypeLiteral *buildTypeExpr(clang::QualType Ty,
                                        clang::SourceLocation Loc);
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::TypeSourceInfo *TInfo);
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::QualType Ty,
                                           clang::SourceLocation Loc);

//===----------------------------------------------------------------------===//
//                               RAII Objects                                 //
//===----------------------------------------------------------------------===//

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
