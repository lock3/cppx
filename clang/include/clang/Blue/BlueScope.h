//===- BlueElaborator.h - Scope Constructs --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Definition of scope facilities.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUESCOPE_H
#define CLANG_BLUE_BLUESCOPE_H

namespace blue
{

/// A region or program text in which all declared names have the same scope.
class Scope {
public:
  enum Kind {
    Namespace,
    Parameter,
    Function,
    Block,
    Class,
  };

  Scope(Kind K, Scope *P)
    : Which(K), Parent(P) {}

  Kind getKind() const {
    return Which;
  }

  const Scope *getParent() const {
    return Parent;
  }

  Scope *getParent() {
    return Parent;
  }

private:
  Kind Which;
  Scope *Parent;
};


} // namespace blue

#endif
