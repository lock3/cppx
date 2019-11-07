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

#include "clang/Basic/SourceManager.h"

#include "clang/GreenBasic/File.h"

file::file(clang::SourceManager &SM, clang::FileID FID)
  : FileEntry(SM.getFileEntryForID(FID)),
    text(SM.getMemoryBufferForFile(FileEntry)),
    path(FileEntry->tryGetRealPathName())
{
}
