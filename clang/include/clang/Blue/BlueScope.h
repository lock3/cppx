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

#include "llvm/ADT/DenseMap.h"

#include "clang/Blue/BlueDeclaration.h"

#include <map>
#include <unordered_map>
#include <set>

namespace blue
{

class Syntax;

template<typename K, typename V>
class IdMapRange : public std::pair<typename std::multimap<K, V>::iterator,
                                    typename std::multimap<K, V>::iterator> {
public:
  IdMapRange(typename std::multimap<K, V>::iterator f,
             typename std::multimap<K, V>::iterator s)
    : std::pair<typename std::multimap<K, V>::iterator,
                typename std::multimap<K, V>::iterator>(f, s)
    {}

  std::size_t size() const {
    return std::distance(this->first, this->second);
  }

  bool empty() const {
    return size() == 0;
  }

  bool single_result() const {
    return size() == 1;
  }

  bool overload_set() const {
    return size() > 1;
  }
};

template <typename K, typename V>
class IdMapType : public std::multimap<K, V> {
public:
  IdMapRange<K, V> find_range(K key) {
    auto range = this->equal_range(key);
    return IdMapRange<K, V>(range.first, range.second);
  }
};

/// A region or program text in which all declared names have the same scope.
class Scope {
public:
  enum Kind {
    Namespace,
    Parameter,
    Function,
    Block,
    Class,
    Control,
  };

  Scope(Kind K, const Syntax *S, Scope *P)
    : Which(K), ConcreteTerm(S), Parent(P) {}

  Kind getKind() const {
    return Which;
  }

  bool isBlockScope() const {
    return getKind() == Block;
  }

  bool isControlScope() const {
    return getKind() == Control;
  }

  const Scope *getParent() const {
    return Parent;
  }

  Scope *getParent() {
    return Parent;
  }

  const Syntax *getTerm() const {
    return ConcreteTerm;
  }

  /// Adds a declaration to this scope.
  void addDecl(Declaration *D) {
    // Store the declaration.
    assert(DeclMap.count(D->Def) == 0);
    DeclMap.try_emplace(D->Def, D);

    // If there's an id, then register it for lookup.
    if (D->Id)
      addDeclLookup(D);
  }

  void addDeclLookup(Declaration *D) {
    assert(D->Id);
    IdMap.emplace(D->Id, D);
  }

  /// Finds a declaration with the given name in this scope.
  std::set<Declaration *> findDecl(const clang::IdentifierInfo *Id) {
    assert(Id);
    auto Range = IdMap.find_range(Id);
    if (Range.empty())
      return std::set<Declaration *>();

    std::set<Declaration *> Ret;
    for (auto It = Range.first; It != Range.second; ++It)
      Ret.insert(It->second);
    return Ret;
  }

  /// Finds the declaration corresponding to the given syntax or null if
  /// the syntax does not form a declaration.
  Declaration *findDecl(const Syntax *S) const {
    const DefSyntax *Def = dyn_cast<DefSyntax>(S);
    if (!Def)
      return nullptr;

    auto Iter = DeclMap.find(Def);
    if (Iter == DeclMap.end())
      return nullptr;
    return Iter->second;
  }

  bool hasDeclaration(const DefSyntax *Def) const {
    return DeclMap.count(Def) != 0;
  }

private:
  Kind Which;
  const Syntax *ConcreteTerm;
  Scope *Parent;

  /// The mapping of original syntax to its construction.
  using DeclMapType = llvm::DenseMap<const DefSyntax *, Declaration *>;
  DeclMapType DeclMap;

  /// Mapping of an identifier to a set of Declarations.
  IdMapType<const clang::IdentifierInfo *, Declaration *> IdMap;
};


} // namespace blue

#endif
