#ifndef CLANG_GREEN_GREENAST_SYNTAXCONTEXT_H
#define CLANG_GREEN_GREENAST_SYNTAXCONTEXT_H

#include "clang/GreenAST/SyntaxContextAllocate.h"
#include "llvm/Support/Allocator.h"

namespace usyntax {

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

} // namespace usyntax


/// The following allow for allocation of memory in the SyntaxContext just
/// like in clang::ASTContext.
inline void *operator new(size_t Bytes, const usyntax::SyntaxContext &C,
                          size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

inline void operator delete(void *Ptr, const usyntax::SyntaxContext &C, size_t) {
  C.Deallocate(Ptr);
}

inline void *operator new[](size_t Bytes, const usyntax::SyntaxContext& C,
                            size_t Alignment /* = 8 */) {
  return C.Allocate(Bytes, Alignment);
}

inline void
operator delete[](void *Ptr, const usyntax::SyntaxContext &C, size_t) {
  C.Deallocate(Ptr);
}



#endif
