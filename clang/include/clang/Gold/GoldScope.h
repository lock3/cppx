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

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/iterator_range.h"

namespace llvm {

class raw_ostream;

}

namespace gold {

struct Syntax;
class Scope;

/// Kinds of declarations.
enum DeclaratorKind {
  /// Does not match the syntactic form of a declarator.
  DK_Unknown,

  /// The id of a declarator.
  DK_Identifier,

  /// Declares a pointer.
  DK_Pointer,

  /// Declares an array bound.
  DK_Array,

  /// Declares function parameters.
  DK_Function,

  /// Declares a type.
  DK_Type,
};

/// A declarator introduces the declaration of a value.
///
/// TODO: Represent multiple declarators whose syntax would be
/// something like: x, y : int -- maybe.
class Declarator {
public:
  Declarator(DeclaratorKind K, Declarator *P)
    : Kind(K), Next(P) { }

  /// The kind of declarator.
  DeclaratorKind getKind() const {
    return Kind;
  }

  bool isIdentifier() const {
    return Kind == DK_Identifier;
  }

  bool isFunction() const {
    return Kind == DK_Function;
  }

  /// Returns the identifier for the declarator, if given.
  const Syntax *getId() const;

  /// Returns the type for declarator, if given.
  const Syntax *getType() const;

  /// Get a SourceLocation representative of this declarator. 
  clang::SourceLocation getLoc() const;

  /// Returns a readable string representing this declarator.
  llvm::StringRef getString() const;

  /// Prints the declarator sequence.
  void printSequence(llvm::raw_ostream &os) const;

  /// The kind of declarator.
  DeclaratorKind Kind;

  /// The next declarator in the sequence.
  Declarator *Next = nullptr;

  /// For non-identifiers, the call representing the declarator component.
  const Syntax *Call = nullptr;

  /// TODO: What other information do we need here?
  union {
    /// For DK_Identifier, the id.
    const Syntax *Id;

    /// For DK_Function, information about parameters.
    struct ParamInfoType {
      /// The initial parameter list.
      const Syntax *Params;

      /// The scope constructed during elaboration.
      Scope *Scope;
    } ParamInfo;

    /// For DK_Type, the type in the call.
    const Syntax *Type;
  } Data;
};

/// A declaration is stores information about the declaration of an
/// identifier. It binds together the declaring operator, the declarator,
/// the definition, and the some corresponding C++ declaration.
class Declaration {
public:
  /// Use to create the initial file/global namespace.
  Declaration(const Syntax *File)
    : Cxt(), Op(), Decl(), Init(File)
  { }

  /// Creates a declaration.
  Declaration(Declaration *Cxt, const Syntax *Op, Declarator *Decl, const Syntax *Init)
    : Cxt(Cxt), Op(Op), Decl(Decl), Init(Init)
  { }

  /// The enclosing declaration.
  Declaration *getOwner() const {
    return Cxt;
  }

  /// True if this declares a variable.
  bool declaresVariable() const;

  /// True if this declares a function.
  bool declaresFunction() const;

  /// The identifier of the declaration, if any.
  clang::IdentifierInfo *getId() const {
    return Id;
  }

  /// The corresponding C++ declaration as a context.
  clang::DeclContext *getCxxContext() const;

  /// The owning context.
  Declaration *Cxt;

  /// The top-level operator that forms the declaration or definition.
  const Syntax *Op;

  /// The declarator of the declaration.
  Declarator *Decl;

  /// The initializer or definition.
  const Syntax *Init;

  /// The list of members associated with this declaration.
  Scope *SavedScope = nullptr;

  /// The identifier for the declaration.
  clang::IdentifierInfo *Id = nullptr;

  /// The corresponding C++ declaration.
  clang::Decl* Cxx = nullptr;
};

/// Different kinds of scope.
enum ScopeKind {
  /// The scope associated with a namespace.
  SK_Namespace,

  /// The scope associated with a function parameter list.
  SK_Parameter,

  /// The scope associated with a function definition.
  SK_Function,

  /// The scope associated with a compound statement.
  SK_Block,
};

/// Stores information about declarations and the scope they were declared in.
class Scope {
  /// The kind of scope.
  ScopeKind Kind;

  /// The parent/enclosing scope of this scope.
  Scope *Parent;

  /// The syntax associated with the scope.
  const Syntax *Term;

  /// The mapping of original syntax to its construction.
  using DeclMapType = llvm::DenseMap<const Syntax *, Declaration *>;
  DeclMapType DeclMap;

  /// The mapping of declarations to its construction.
  ///
  /// FIXME: For overloading a single identifier can refer to a set of
  /// declarations. We'll need to adjust this in order to make it work.
  using IdMapType = llvm::DenseMap<clang::IdentifierInfo const*, Declaration *>;
  IdMapType IdMap;

  // FIXME: Is there any purpose for this at all?
  unsigned Depth;

public:
  /// Creates a new scope.
  Scope(ScopeKind K, const Syntax *S, Scope *P)
    : Kind(K), Parent(P), Term(S) {
    Depth = Parent ? Parent->getDepth() + 1 : 0;
  }

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

  bool isFunctionScope() const {
    return Kind == SK_Function;
  }

  bool isBlockScope() const {
    return Kind >= SK_Block;
  }

  /// The parent of this scope.
  Scope *getParent() const {
    return Parent;
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
    IdMap.try_emplace(D->Id, D);
  }

  /// Finds a declaration with the given name in this scope.
  ///
  /// FIXME: This could return an overload set.
  Declaration *findDecl(const clang::IdentifierInfo *Id) const {
    assert(Id);
    auto Iter = IdMap.find(Id);
    if (Iter == IdMap.end())
      return nullptr;
    return Iter->second;
  }

  /// Finds the declaration corresponding to the given syntax or null if
  /// the syntax does not form a declaration.
  Declaration *findDecl(const Syntax *S) const {
    auto Iter = DeclMap.find(S);
    if (Iter == DeclMap.end())
      return nullptr;
    return Iter->second;
  }
};

} // namespace gold

#endif
