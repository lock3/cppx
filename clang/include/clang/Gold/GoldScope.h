//===- GoldScope.h - Simple scope used in Gold parsing --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Scope interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDSCOPE_H
#define CLANG_GOLD_GOLDSCOPE_H

#include "clang/Gold/GoldDeclaration.h"

#include "llvm/ADT/SmallPtrSet.h"

#include <map>
#include <set>


namespace gold {

/// Different kinds of scope.
enum ScopeKind : unsigned {
  /// The scope associated with a namespace.
  SK_Namespace = 0x0,

  /// The scope associated with a function parameter list.
  SK_Parameter = 0x01,

  /// The scope associated with a template parameter list.
  SK_Template  = 0x02,

  /// The scope associated with a function definition.
  SK_Function  = 0x04,

  /// The scope associated with a compound statement.
  SK_Block     = 0x08,

  /// The scope associated with a class definition
  SK_Class     = 0x16,

  /// The scope associated with a control statement.
  SK_Control   = 0x32,

  SK_Enum      = 0x64,

  /// This is for catch blocks.
  SK_Catch     = 0x128,
};

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

/// Stores information about declarations and the scope they were declared in.
class Scope {
public:
  /// The kind of scope.
  ScopeKind Kind;

  /// The parent/enclosing scope of this scope.
  Scope *Parent = nullptr;

  /// The syntax associated with the scope.
  const Syntax *Term = nullptr;

  /// The mapping of original syntax to its construction.
  using DeclMapType = llvm::DenseMap<const Syntax *, Declaration *>;
  DeclMapType DeclMap;

  IdMapType<const clang::IdentifierInfo *, Declaration *> IdMap;

  // FIXME: Is there any purpose for this at all?
  unsigned Depth;

  Declaration *Entity = nullptr;
public:
  /// Creates a new scope.
  Scope(ScopeKind K, const Syntax *S, Scope *P, Declaration *D = nullptr)
    : Kind(K), Parent(P), Term(S), Entity(D) {
    Depth = Parent ? Parent->getDepth() + 1 : 0;
  }
  llvm::StringRef getKindStr() const;

  /// The kind of scope.
  ScopeKind getKind() const {
    return Kind;
  }

  bool isNamespaceScope() const {
    return Kind == SK_Namespace;
  }

  bool isParameterScope() const {
    return Kind == SK_Parameter;
  }

  bool isTemplateScope() const {
    return Kind == SK_Template;
  }

  bool isFunctionScope() const {
    return Kind == SK_Function;
  }

  bool isBlockScope() const {
    return Kind == SK_Block;
  }

  bool isClassScope() const {
    return Kind == SK_Class;
  }

  bool isControlScope() const {
    return Kind == SK_Control;
  }

  bool isEnumScope() const {
    return Kind == SK_Enum;
  }

  // True when a scope is the block of a lambda expression.
  bool isLambdaScope() const {
    return Kind == SK_Block && Lambda;
  }

  // True when a scope is the capture block of a lambda expression.
  bool isLambdaCaptureScope() const {
    return LambdaCaptureScope;
  }

  /// The parent of this scope.
  Scope *getParent() const {
    return Parent;
  }

  /// Set the parent of the scope; BE CAREFUL when using this.
  void setParent(Scope *S) {
    Parent = S;
  }

  /// The depth of the scope.
  unsigned getDepth() const {
    return Depth;
  }

  /// The original, concrete term associated with the scope.
  const Syntax *getConcreteTerm() const {
    return Term;
  }

  /// Adds a declaration to this scope. Declarations are added when they
  /// are first identified, not when their types are elaborated.
  void addDecl(Declaration *D) {
    // Store the declaration.
    assert(DeclMap.count(D->Op) == 0);
    DeclMap.try_emplace(D->Op, D);

    // If there's an id, then register it for lookup.
    if (D->Id)
      addDeclLookup(D);
  }

  void addDeclLookup(Declaration *D) {
    assert(D->Id);

    // FIXME: If D is overloaded, then we need to add this to the declaration
    // set instead of just forcing it into place.
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
    auto Iter = DeclMap.find(S);
    if (Iter == DeclMap.end())
      return nullptr;
    return Iter->second;
  }

  bool hasDeclaration(const Syntax *Op) const {
    return DeclMap.count(Op) != 0;
  }

  // Using Directives in this scope.
  llvm::SmallPtrSet<clang::UsingDirectiveDecl *, 4> UsingDirectives;

  // UsingDecls that get added to this scope.
  llvm::SmallPtrSet<clang::UsingShadowDecl *, 4> Shadows;

  // True when this is the block of a lambda scope.
  bool Lambda = false;

  // True when this is the capture block of a lambda.
  bool LambdaCaptureScope = false;

  void dump(llvm::raw_ostream &os) const;
  void dump() const;

  void dumpScopeChain() const;
};

} // namespace gold

#endif
