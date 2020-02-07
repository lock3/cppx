//===- BlueDeclarator.cpp - Information about declarations ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Defines a structure used to represent declarations.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueDeclarator.h"
#include "clang/Blue/BlueSyntax.h"

namespace blue {

clang::SourceLocation Declarator::getLocation() const {
  return Info->getLocation();
}

void Declarator::dump() const {
  // FIXME: It would be nice to print something useful here.
  Info->dump();
}

} // namespace blue
