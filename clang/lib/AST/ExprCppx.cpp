//===- ExprCXX.cpp - (C++) Expression AST Node Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Expr class declared in ExprCXX.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ExprCppx.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Type.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstddef>
#include <cstring>
#include <memory>

namespace clang {


SourceLocation CppxTypeLiteral::getBeginLoc() const {
  return Loc;
}

SourceLocation CppxTypeLiteral::getEndLoc() const {
  return Loc;
}

SourceLocation CppxTypeLiteral::getLocation() const {
  return Loc;
}

CppxTypeLiteral* CppxTypeLiteral::create(clang::ASTContext &Context, 
                                         QualType KindTy,
                                         QualType Ty, SourceLocation Loc) {
  return new (Context) CppxTypeLiteral(KindTy, Ty, Loc);
}

CppxTypeLiteral* CppxTypeLiteral::create(clang::ASTContext &Context, 
                                         QualType KindTy,
                                         TypeSourceInfo *TInfo) {
  // TODO: This may not be the right way to get the location of/for a type. 
  return new (Context) CppxTypeLiteral(KindTy, TInfo->getType(),
                                       TInfo->getTypeLoc().getBeginLoc());
}

CppxNamespaceDeclRefExpr *
CppxNamespaceDeclRefExpr::create(clang::ASTContext &Context,
                                 ValueType *NsDecl, SourceLocation Loc) {
  return new (Context) CppxNamespaceDeclRefExpr(
      Context.getCppxNamespaceType(NsDecl->getNamespace()), NsDecl, Loc);
}
} // namespace clang