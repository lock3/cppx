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

#include "clang/Green/File.h"

File::File(clang::SourceManager &SM, clang::FileID FID)
  : FileEntry(SM.getFileEntryForID(FID)),
    Text(SM.getMemoryBufferForFile(FileEntry)),
    Path(FileEntry->tryGetRealPathName())
{
}
