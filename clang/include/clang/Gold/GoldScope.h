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
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/iterator_range.h"

#include <map>
#include <set>

namespace clang {
class Scope;
}

namespace llvm {

class raw_ostream;

}

namespace gold {

struct Syntax;
class Scope;
struct Attribute;

/// Kinds of declarations.
enum DeclaratorKind {
  /// Does not match the syntactic form of a declarator.
  DK_Unknown,

  /// The id of a declarator.
  DK_Identifier,

  /// Template indication for classes. This part of the declarator is used to
  /// track if a templated type declaration is being given.
  DK_TemplateType,

  /// Declares a pointer.
  DK_Pointer,

  /// Declares an array bound.
  DK_Array,

  /// Declares function parameters.
  DK_Function,

  /// Declares a type.
  DK_Type,

  /// Declares const
  DK_Const,

  /// Declares L-value reference
  DK_Ref,

  /// Declares R-value reference
  DK_RRef,
};

using Attributes = llvm::SmallVector<const Syntax *, 16>;
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

  bool isType() const { 
    return Kind == DK_Type;
  }

  bool isFunction() const {
    return Kind == DK_Function;
  }

  bool isConst() const {
    return Kind == DK_Const;
  }

  /// Returns the identifier for the declarator, if given.
  const Syntax *getId() const;

  /// Returns the type for declarator, if given.
  const Syntax *getType() const;

  /// Get a SourceLocation representative of this declarator. 
  clang::SourceLocation getLoc() const;

  /// Returns a readable string representing this declarator.
  std::string getString() const;

  /// Prints the declarator sequence.
  void printSequence(llvm::raw_ostream &os) const;

  /// This sets the attribute node and records all attributes into the
  /// UnprocessedAttributes member.
  void recordAttributes(const Syntax* AttributeNode);

  /// The kind of declarator.
  DeclaratorKind Kind;

  /// The next declarator in the sequence.
  Declarator *Next = nullptr;

  /// For non-identifiers, the call representing the declarator component.
  const Syntax *Call = nullptr;

  // The tag's body.
  clang::Scope *TagScope = nullptr;

  /// TODO: What other information do we need here?
  union {
    /// For DK_Identifier, the id.
    const Syntax *Id;

    /// For DK_Function, information about parameters.
    struct ParamInfoType {
      /// The initial parameter list.
      const Syntax *Params;

      /// For DK_Function, the template parameter list.
      const Syntax *TemplateParams;

      /// The scope constructed during elaboration.
      Scope *ConstructedScope;

      /// The scope containing the template parameters
      Scope *TemplateScope;

      /// Whether or not this function has a variadic parameter.
      bool VariadicParam;
    } ParamInfo;

    /// For DK_Type, the type in the call.
    const Syntax *Type;

    /// For DK_Array, the array index.
    const Syntax *Index;

    /// For DK_TemplateType, for templated types.
    struct TemplateInfoStruct {
      /// A pointer to the template parameters within the declaration.
      const Syntax* Params;

      // /// The scope for the template parameters.
      // Scope *DeclScope;
      // /// This is the clang scope that's used for declaring template parameters.
      // clang::Scope *ClangScope;
    } TemplateInfo;
  } Data;

  /// This is optionally set for each piece of the declarator.
  const Syntax* AttributeNode = nullptr;
  llvm::Optional<Attributes> UnprocessedAttributes;
};

enum class Phase : std::size_t
{
  Unprocessed,
  Identification,
  Typing,
  Initialization
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

  ~Declaration();

  /// The enclosing declaration.
  Declaration *getOwner() const {
    return Cxt;
  }

  clang::SourceLocation getEndOfDecl() const;

  /// True if this declares a variable.
  bool declaresVariable() const;

  /// Declares variable with in body initialization.
  bool declaresInitializedVariable() const;

  /// Any template has default parameters. Either class or function.
  bool templateHasDefaultParameters() const;

  /// True if this is a type declaration.
  bool declaresType() const;

  /// Checks if the type declaration is declaring a record.
  bool declaresRecord() const;

  /// Checks if the type declaration is declaring a namespace.
  bool declaresNamespace() const;

  /// Checks if the declarator declares a template type or not.
  bool declaresTemplateType() const;

  /// True if this declares a function.
  bool declaresFunction() const;

  /// Returns true if the CXX type is a FieldDecl.
  /// This is true when declares variable is also true some times.
  bool declaresMemberVariable() const;

  /// Returns true if the declaration is inside of a class, and it's a member
  /// function.
  bool declaresMemberFunction() const;

  /// Returns true if the declaration is inside of a class, and it's a
  /// constructor
  bool declaresConstructor() const;

  /// Returns True if the given function is a destructor, and it's declared
  /// within a class.
  bool declaresDestructor() const;

  /// True if this declares a template.
  bool declaresFunctionTemplate() const;

  /// This is true iff isa<TypeAliasDecl>(Cxx)
  bool declaresTypeAlias() const;

  /// Checks if a declaration is static.
  bool declIsStatic() const;

  /// Checks if a decl is a declaration and it doesn't have a body.
  bool declaresFunctionDecl() const;

  /// checks if a function has a body.
  bool decalaresFunctionDef() const;

  template<typename T>
  bool defines() const {
    return Cxx && clang::isa<T>(Cxx);
  }
  /// Checks if the current Cxx decl is a static member variable of a class.
  bool declaresInlineInitializedStaticVarDecl() const;

  /// Get the template parameters for this declaration or null if none.
  const Syntax *getTemplateParams() const;

  /// The identifier of the declaration, if any.
  clang::IdentifierInfo *getId() const {
    return Id;
  }

  /// This looks for the first instance of DK_TemplateType and returns it.
  const Declarator *getFirstTemplateDeclarator() const;
  Declarator *getFirstTemplateDeclarator();
  
  const Declarator *getIdDeclarator() const;
  Declarator *getIdDeclarator();

  /// The corresponding C++ declaration as a context.
  clang::DeclContext *getCxxContext() const;

  /// Set the previous declaration in the redeclaration chain.
  void setPreviousDecl(Declaration *Prev);

  /// This function checks to see if the current scope was declared within
  // the scope of another class body.
  bool isDeclaredWithinClass() const;

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

  /// The list of template parameter declarations associated
  /// with this declaration.
  Scope *SavedTemplateScope = nullptr;

  /// The identifier for the declaration.
  clang::IdentifierInfo *Id = nullptr;

  /// The corresponding C++ declaration.
  clang::Decl* Cxx = nullptr;

  /// ====================================================================== ///
  /// Below are declarations pertaining to the redeclaration chain, which
  /// is a circularly linked list containing declarations with the same
  /// signature.

  /// The first declaration in the redeclaration chain.
  Declaration *First = this;

  /// The next decl in the redeclaration chain.
  Declaration *Next = this;

  /// ====================================================================== ///

  /// Decl phase completed. This is used to paint the declarations and
  /// avoid re-visitation during lookup/elaboration. This has a value from 0-3.
  /// 0 is unprocessed (the default value), 1 is identified, 2 is the declaration
  /// is elaborated and 3 is the definition is complete.
  Phase CurrentPhase = Phase::Unprocessed;

  /// This information is to aid with early elaboration. This allows the
  /// elabrotor to restore the state in which something was declared.
  ///
  /// This is the current clang scope that the clang declaration is part of.
  clang::Scope *ClangDeclaringScope = nullptr;

  /// This is the scope that this declaration is a member of.
  /// This is also the parent scope to the SavedScope, if set.
  gold::Scope *ScopeForDecl = nullptr;

  /// This is the gold DeclContext for a declaration.
  Declaration* ParentDecl = nullptr;

  /// This is the current DeclContext when the declaration was encountered.
  clang::DeclContext *DeclaringContext = nullptr;

  ///}
};

Phase phaseOf(Declaration *D);

/// Different kinds of scope.
enum ScopeKind {
  /// The scope associated with a namespace.
  SK_Namespace,

  /// The scope associated with a function parameter list.
  SK_Parameter,

  /// The scope associated with a template parameter list.
  SK_Template,

  /// The scope associated with a function definition.
  SK_Function,

  /// The scope associated with a compound statement.
  SK_Block,

  /// The scope associated with a class definition
  SK_Class,

  /// The scope associated with a control statement.
  SK_Control,
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
  Scope *Parent;

  /// The syntax associated with the scope.
  const Syntax *Term;

  /// The mapping of original syntax to its construction.
  using DeclMapType = llvm::DenseMap<const Syntax *, Declaration *>;
  DeclMapType DeclMap;

  using TypeDecls = llvm::DenseMap<llvm::StringRef, clang::QualType>;
  TypeDecls Types;

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

  void dump(llvm::raw_ostream &os) const;
  void dump() const;

  void dumpScopeChain() const;
};

} // namespace gold

#endif
