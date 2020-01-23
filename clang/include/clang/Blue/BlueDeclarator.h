//===- BlueDeclarator.h - Information about declarations ------------------===//
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

#ifndef CLANG_BLUE_BLUEDECLARATOR_H
#define CLANG_BLUE_BLUEDECLARATOR_H

namespace blue {

class Syntax;

/// A declarator is a linked list of structures that provide information
/// about the kind and type of a declaration.
class Declarator {
public:
  enum Kind {
    Name,
    Type,
    Pointer,
    Array,
    Function,
    Template,
  };

  Declarator(Kind K, Syntax *S)
    : Which(K), Info(S), Next(nullptr) {}

  Kind getKind() const {
    return Which;
  }

  Syntax *getInfo() {
    return Info;
  }

  const Syntax *getInfo() const {
    return Info;
  }

  Declarator *getNext() {
    return Next;
  }

  const Declarator *getNext() const {
    return Next;
  }

private:
  Kind Which;
  Syntax *Info;
  Declarator *Next;
};

} // namespace blue


#endif

