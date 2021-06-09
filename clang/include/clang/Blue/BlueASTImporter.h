//===- BlueASTImporter.h - Custom AST importer ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements a custom AST importer for importing C++ into the
//  blue language. This provides the means to create custom namespace inside
//  of the Blue AST so that we can maintain consistency with the current way
//  we generate the AST.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUE_AST_IMPORTER_H
#define CLANG_BLUE_BLUE_AST_IMPORTER_H

#include "llvm/ADT/StringRef.h"
#include "clang/AST/ASTImporter.h"
#include "clang/AST/APValue.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/TemplateName.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include <utility>
#include "clang/Blue/BlueSema.h"
namespace clang {

class ASTContext;
class ASTImporterSharedState;
class Attr;
class CXXBaseSpecifier;
class CXXCtorInitializer;
class Decl;
class DeclContext;
class Expr;
class FileManager;
class NamedDecl;
class Stmt;
class TagDecl;
class TranslationUnitDecl;
class TypeSourceInfo;
class BlueASTNodeImporter;
}
namespace blue {
class BlueASTImporter : public clang::ASTImporter {
  friend class clang::BlueASTNodeImporter;
  blue::Sema &SemaRef;

public:
  /// \param ToContext The context we'll be importing into.
  ///
  /// \param ToFileManager The file manager we'll be importing into.
  ///
  /// \param FromContext The context we'll be importing from.
  ///
  /// \param FromFileManager The file manager we'll be importing into.
  ///
  /// \param MinimalImport If true, the importer will attempt to import
  /// as little as it can, e.g., by importing declarations as forward
  /// declarations that can be completed at a later point.
  ///
  /// \param SharedState The importer specific lookup table which may be
  /// shared amongst several ASTImporter objects.
  /// If not set then the original C/C++ lookup is used.
  BlueASTImporter(Sema& BlueSema, clang::ASTContext &ToContext, clang::FileManager &ToFileManager,
                  clang::ASTContext &FromContext, clang::FileManager &FromFileManager,
                  bool MinimalImport,
                  std::shared_ptr<clang::ASTImporterSharedState> SharedState = nullptr);


protected:
  virtual llvm::Expected<clang::Decl *> ImportImpl(clang::Decl *From);
};
} // end namespace blue.
#endif