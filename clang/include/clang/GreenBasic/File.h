//===- File.h - Files for the Green Lexer --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines a data structure around Files that are used by the
//  Green Lexer.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GREEN_FILE_HPP
#define CLANG_GREEN_FILE_HPP

#include "clang/Basic/SourceLocation.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"

#include <string>

namespace clang {
class SourceManager;
} // namespace clang

/// A source file.
struct file
{
  file(clang::SourceManager &SM, clang::FileID FID);

  char const* name() const
  {
    return path.empty() ? "<input>" : path.c_str();
  }

  char const* data() const
  {
    return text->getBufferStart();
  }

  std::size_t size() const
  {
    return text->getBufferSize();
  }

  const clang::FileEntry *FileEntry;
  const llvm::MemoryBuffer *text;
  std::string path;
};

#endif
