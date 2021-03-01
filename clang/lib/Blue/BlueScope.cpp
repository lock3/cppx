//===- BlueElaborator.cpp - Scope Constructs ------------------------------===//
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

#include "clang/Blue/BlueScope.h"

namespace blue {

static const char *getScopeKindName(Scope::Kind K) {
  switch (K) {
  case Scope::Namespace:
    return "Namespace";
  case Scope::Parameter:
    return "Parameter";
  case Scope::Function:
    return "Function";
  case Scope::Block:
    return "Block";
  case Scope::Class:
    return "Class";
  case Scope::Control:
    return "Control";
  case Scope::Template:
    return "Template";
  }
}

void Scope::dump(llvm::raw_ostream &os) const {
  os << getScopeKindName(getKind()) << '\n';
  if (Entity) {
    if (Entity->getCxx()) {
      os << "Entity = \n";
      Entity->getCxx()->dump();
    } else {
      os << "Entity isn't set yet\n";
    }
  }
  if (getKind() == Scope::Template) {
    for(auto D : IdMap) {
      os << D.first->getName();
      if (D.second->getCxx())
        D.second->getCxx()->dump(os << " " << D.first << " ");
      else
        os << "\n";
    }
  } else
    for (auto D : IdMap)
      os << D.first->getName() << '\n';
}

void Scope::dump() const {
  dump(llvm::errs());
}

void Scope::dumpScopeChain() const {
  const Scope *Cur = this;
  while (Cur) {
    llvm::outs() << "-----------------------\n";
    Cur->dump();
    Cur = Cur->getParent();
  }
  llvm::outs() << "-----------------------\n";
}

} // end namespace blue
