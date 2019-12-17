//===- GreenScope.cpp - Simple scope used in Green parsing ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Implements the Scope interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Green/GreenScope.h"
#include "clang/Green/Syntax.h"

namespace green {

// The identifier, if present, is the first node in the declarator.
const Syntax *Declarator::getId() const {
  if (Kind == DK_Identifier)
    return Data.Id;
  return nullptr;
}

// The type specifier, if present, is the last node in the declarator.
const Syntax *Declarator::getType() const {
  const Declarator *D = this;
  while (D->Next) {
    D = D->Next;
  }
  if (D->Kind == DK_Type)
    return D->Data.Type;
  return nullptr;
}


} // namespace green
