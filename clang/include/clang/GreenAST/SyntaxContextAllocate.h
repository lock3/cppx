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
}

void *operator new(size_t Bytes, const usyntax::SyntaxContext &S,
                   size_t Alignment = 8);
void *operator new[](size_t Bytes, const usyntax::SyntaxContext &C,
                     size_t Alignment = 8);

void operator delete(void *Ptr, const usyntax::SyntaxContext &C, size_t);
void operator delete[](void *Ptr, const usyntax::SyntaxContext &C, size_t);

#endif // LLVM_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H

