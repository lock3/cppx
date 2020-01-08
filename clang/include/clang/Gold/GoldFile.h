//===- GoldFile.h - Files for the Gold Lexer ------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines a data structure around clang::Files that are used by the
//  Gold Lexer.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_FILE_HPP
#define CLANG_GOLD_FILE_HPP

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"

#include <string>

namespace gold {

/// A source file.
class File
{
public:
  File(clang::SourceManager &SM, clang::FileID ID)
    : SM(SM), ID(ID) {}

  clang::FileID getID() const {
    return ID;
  }

  llvm::StringRef getName() const {
    return SM.getFilename(SM.getLocForStartOfFile(ID));
  }

  llvm::StringRef getText() const {
    return SM.getBuffer(ID)->getBufferStart();
  }

private:
  /// The manager of the file.
  clang::SourceManager& SM;

  /// The file id.
  clang::FileID ID;
};

} // namespace gold

#endif
