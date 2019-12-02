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
class File
{
public:
  File(clang::SourceManager &SM, clang::FileID FID);

  char const* getName() const
  {
    return Path.empty() ? "<input>" : Path.c_str();
  }

  char const* data() const
  {
    return Text->getBufferStart();
  }

  std::size_t size() const
  {
    return Text->getBufferSize();
  }

private:
  /// The entry loaded by the source manager.
  const clang::FileEntry *FileEntry;

  /// The text of the file.
  const llvm::MemoryBuffer *Text;

  /// The path to the file.
  ///
  /// FIXME: We should be able to get this from the source manager.
  std::string Path;
};

#endif
