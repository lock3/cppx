//===- GoldScope.cpp - Simple scope used in Gold parsing ----------------===//
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


#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

#include "llvm/Support/raw_ostream.h"

namespace gold {

static llvm::StringRef getScopeKindName(ScopeKind K) {
  switch (K) {
  case SK_Namespace:
    return "Namespace";

  case SK_Parameter:
    return "Parameter";

  case SK_Template:
    return "Template";

  case SK_Function:
    return "Function";

  case SK_Block:
    return "Block";

  case SK_Class:
    return "Class";

  case SK_Control:
    return "Control";

  case SK_Enum:
    return "Enum";
  }

  llvm_unreachable("invalid scope");
}

llvm::StringRef Scope::getKindStr() const {
  return getScopeKindName(getKind());
}
void Scope::dump(llvm::raw_ostream &os) const {
  os << getScopeKindName(getKind()) << '\n';
  if (Entity) {
    if (Entity->Cxx) {
      os << "Entity->Cxx = \n";
      Entity->Cxx->dump();
    } else{
      os << "Entity->Cxx isn't set yet\n";
    }
  }
  if (getKind() == SK_Template) {
    for(auto D : IdMap) {
      os << D.first->getName();
      if (D.second->Cxx)
        D.second->Cxx->dump(os << " " << D.first << " ");
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

Phase phaseOf(Declaration *D) {
  return D->CurrentPhase;
}

} // namespace gold
