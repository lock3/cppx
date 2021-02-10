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
  // Info->dump();
  printSequence(llvm::errs());
}

void Declarator::printSequence(llvm::raw_ostream &os) const {
  const Declarator *D = this;
  do {
    os << D->getString();
    if (D->Next)
      os << " -> ";

    D = D->Next;
  }  while (D);

  os << '\n';
}

std::string Declarator::getString() const {
  switch(getKind()) {
  case Type: {
    std::string Ret = "[type] ";
    if (const AtomSyntax *A = dyn_cast<AtomSyntax>(getInfo()))
      Ret += A->spelling();
    else
      Ret += "complex";
    return Ret;
  }

  case ImplicitType:
    return "[implicit_type]";
  case Pointer:
    return "^";
  case Array: {
    return "[array]";
    // std::string Ret = "[";

    // if (const AtomSyntax *A = dyn_cast<AtomSyntax>(getInfo()))
    //   Ret += A->spelling();
    // else if (const ListSyntax *L = dyn_cast<ListSyntax>(getInfo())) {
    //   // if (L->getNumChildren() == 1) {
    //   //   if (const AtomSyntax *B = dyn_cast<AtomSyntax>(L->getChild(0)))
    //   //     Ret += B->spelling();
    //   //   else
    //   //     Ret += "expr";
    //   // } else {
    //   //   Ret += "expr";
    //   // }
    //   Ret += "LIST";
    // } else
    //   return "expr";

    // Ret += "]";
    // return Ret;
  }

  case Function: {
    // TODO: implement me
    std::string Ret = "[function] ()";
    return Ret;
  }

  case Template:
    // TODO: implement me
    return "[template]";

  case Class:
    // TODO: implement me
    return "[class]";

  }
  llvm_unreachable("Unimplemented kind of declarator.");
}

} // namespace blue

