//===----------------------------------------------------------------------===//
//
//  This file declares SyntaxContext  allocation functions separate from the
//  main code in SyntaxContext.h
//
//===----------------------------------------------------------------------===//


#ifndef LLVM_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H
#define LLVM_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H

#include <cstddef>

namespace usyntax {

class SyntaxContext;

// We'll leave these in the usyntax namespace since we absolutely do not want
// them used globally.
void *operator new(size_t Bytes, const SyntaxContext &S,
                   size_t Alignment = 8);
void *operator new[](size_t Bytes, const SyntaxContext &C,
                     size_t Alignment = 8);

void operator delete(void *Ptr, const SyntaxContext &C, size_t);
void operator delete[](void *Ptr, const SyntaxContext &C, size_t);

} // namespace usyntax

#endif // LLVM_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H

