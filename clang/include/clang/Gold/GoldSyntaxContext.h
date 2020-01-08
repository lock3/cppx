//===- GoldSyntaxContext.h - Context to hold long-lived Syntax nodes ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
// This file defines the gold::SyntaxContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_SYNTAXCONTEXT_H
#define CLANG_GOLD_SYNTAXCONTEXT_H

#include "clang/AST/DeclContextInternals.h"
#include "llvm/Support/Allocator.h"

#include "clang/Gold/GoldSyntaxContextAllocate.h"

namespace clang {
class ASTContext;
} // namespace clang

namespace gold {

/// Analogous to clang::ASTContext, keeps track of some information associated
/// with the syntax tree. For now it just manages memory.
/// TODO: this will contain maps associated with "declcontexts" too.
class SyntaxContext {
  /// The allocator used to create Syntax tree objects.
  ///
  /// Syntax tree objects are never destructed; rather, all memory associated
  /// with them will be released when the SyntaxContext itself is destroyed.
  mutable llvm::BumpPtrAllocator BumpAlloc;

public:
  /// The context for the Clang AST.
  clang::ASTContext &CxxAST;

public:
  SyntaxContext(clang::ASTContext &CxxAST)
    : CxxAST(CxxAST)
  {}

  llvm::BumpPtrAllocator &getAllocator() const {
    return BumpAlloc;
  }

  void *Allocate(size_t Size, unsigned Align = 8) const {
    return BumpAlloc.Allocate(Size, Align);
  }
  template <typename T> T *Allocate(size_t Num = 1) const {
    return static_cast<T *>(Allocate(Num * sizeof(T), alignof(T)));
  }
  void Deallocate(void *Ptr) const {}

  /// Return the total amount of physical memory allocated for representing
  /// AST nodes and type information.
  size_t getASTAllocatedMemory() const {
    return BumpAlloc.getTotalMemory();
  }
};

} // namespace gold


/// The following allow for allocation of memory in the SyntaxContext just
/// like in clang::ASTContext. We want to keep the Gold Syntax Tree and
/// C++ AST separate as they don't necessarily have the same lifetime, and
/// keeping them inside the C++ AST can confuse Clang.
inline void *operator new(size_t Bytes, const gold::SyntaxContext &C,
                          size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

inline void operator delete(void *Ptr, const gold::SyntaxContext &C, size_t) {
  C.Deallocate(Ptr);
}

inline void *operator new[](size_t Bytes, const gold::SyntaxContext& C,
                            size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

inline void
operator delete[](void *Ptr, const gold::SyntaxContext &C, size_t) {
  C.Deallocate(Ptr);
}



#endif
