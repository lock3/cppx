//===- SyntaxContextAllocate.h - SyntaxContext allocate functions ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the SyntaxContext allocation functions separate from the
//  main code in SyntaxContext.h
//
//===----------------------------------------------------------------------===//


#ifndef CLANG_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H
#define CLANG_GREEN_GREENAST_SYNTAXCONTEXTALLOCATE_H

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

