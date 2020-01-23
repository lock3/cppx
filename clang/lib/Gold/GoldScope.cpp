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

#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"

namespace gold {

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

clang::SourceLocation Declarator::getLoc() const {
  switch (getKind()) {
  case DK_Identifier:
  case DK_Function:
    return getId()->getLoc();
  case DK_Type:
    return getType()->getLoc();
  default:
    return clang::SourceLocation();
  }
}

static llvm::StringRef getCallName(const CallSyntax *S) {
  // Get the bottom-left most element, which should be an
  // atom syntax naming the function.
  while (const Syntax *L = S->getArgument(0)) {
    if (const auto *Atom = dyn_cast<AtomSyntax>(L))
      return Atom->getSpelling();

    S = cast<CallSyntax>(L);
  }

  // This should never happen for a proper function declarator.
  return "";
}

llvm::StringRef Declarator::getString() const {
  if (getKind() == DK_Type) {
    return cast<AtomSyntax>(Data.Type)->getSpelling();
  } else if (isFunction()) {
    return getCallName(cast<CallSyntax>(Call));
  } else if (isIdentifier()) {
    return cast<AtomSyntax>(Data.Id)->getSpelling();
  } else if (getKind() == DK_Pointer) {
    return "^";
  } else {
    return "[unimplemented]";
  }
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

// A pointer is essentially a type, but we keep them separate for convenience
// in expression elaboration.
// const Syntax *Declarator::getPtrType() const {
//   const Declarator *D = this;
//   while (D->Next) {
//     D = D->Next;
//   }
//   if (D->Kind == DK_Pointer)
//     return D->Data.Type;
//   return nullptr;
// }

// A declarator declares a variable, if it does not declare a function.
bool Declaration::declaresVariable() const {
  return !declaresFunction();
}

// A declarator declares a function if it's first non-id declarator is
// declares parameters.
bool Declaration::declaresFunction() const {
  assert(Decl);
  const Declarator *D = Decl;
  if (D->Kind == DK_Identifier)
    D = D->Next;
  if (D)
    return D->Kind == DK_Function;
  return false;
}

clang::DeclContext *Declaration::getCxxContext() const {
  return clang::Decl::castToDeclContext(Cxx);
}

} // namespace gold
