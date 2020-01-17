//=== TypeLocUtil.h - Interface for Building TypeLocs ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2020, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines an interface for building TypeLocs used in dragon
//  language elaboration.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_TYPELOCUTIL_H
#define CLANG_GOLD_TYPELOCUTIL_H

#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"

#include "llvm/ADT/SmallVector.h"

namespace clang {

class ASTContext;
class ParmVarDecl;
class TypeLocBuilder;

}  // namespace clang

namespace gold {

using clang::SourceLocation;

// Use the libsema TypeLocBuilder to create a TypeSourceInfo for any type.
// \param TLB is an already constructed TypeLocBuilder that we
//        want to carry through.
// \param T is the type we want to build a TypeSourceInfo out of.
// \param Loc is the source info.
clang::TypeSourceInfo *BuildAnyTypeLoc(clang::ASTContext &Context,
                                       clang::TypeLocBuilder &TLB,
                                       clang::QualType T, SourceLocation Loc);

// Same as above, but uses a single-instance TypeLocBuilder.
clang::TypeSourceInfo *BuildAnyTypeLoc(clang::ASTContext &Context,
                                       clang::QualType T, SourceLocation Loc);

// Use the libsema TypeLocBuilder to create a TypeSourceInfo for a specific
// kind of TypeLoc.
// Needs to be explicitly instantiated for every TypeLocType.
// \param TLB is a TypeLocBuilder we want to carry through.
// \param Ty is the type we want to build a TypeSourceInfo out of.
// \param Loc is the SourceLocation of the TypeSourceInfo.
template <typename TypeLocType>
clang::TypeSourceInfo *BuildTypeLoc(clang::ASTContext &Context,
                                    clang::TypeLocBuilder &TLB,
                                    clang::QualType Ty, SourceLocation Loc);

// Same as above, but uses a single-instance TypeLocBuilder.
template <typename TypeLocType>
clang::TypeSourceInfo *BuildTypeLoc(clang::ASTContext &Context,
                                    clang::QualType Ty, SourceLocation Loc);

/// ======================================================================== ///
/// The following functions are for special TypeLoc types that require more  ///
/// information to be constructed, and thus need their own functions.        ///
/// ======================================================================== ///

clang::TypeSourceInfo *BuildFunctionTypeLoc(clang::ASTContext &Context,
     clang::QualType Ty, SourceLocation BeginLoc, SourceLocation LParenLoc,
     SourceLocation RParenLoc, clang::SourceRange ExceptionSpecRange,
     SourceLocation EndLoc, llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params);

} // namespace gold

#endif
