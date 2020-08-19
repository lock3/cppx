//=== GoldDeclarator.h ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Defines the delarator which are used to build a declaration.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_DECLARATOR_H
#define CLANG_GOLD_DECLARATOR_H

#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/TemplateBase.h"

#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/iterator_range.h"

namespace clang {
class Scope;
class TemplateParameterList;
}

namespace llvm {
class raw_ostream;
}

namespace gold {
class Scope;
struct Syntax;
class OpInfoBase;
struct CallSyntax;
struct AtomSyntax;
struct Attribute;
struct ListSyntax;

class UnknownDeclarator;
class ErrorDeclarator;
class GlobalNameSpecifierDeclarator;
class IdentifierDeclarator;
class NestedNameSpecifierDeclarator;
class FunctionDeclarator;
class TypeDeclarator;
class TemplateParamsDeclarator;
class ImplicitEmptyTemplateParamsDeclarator;
class ExplicitSpecializationDeclarator;
class PartialSpecializationDeclarator;

/// Kinds of declarations.
enum DeclaratorKind {
  /// This is the base class of the declarator hierarchy
  DK_DeclaratorBase,

  /// Does not match the syntactic form of a declarator.
  DK_Unknown,

  /// The id of a declarator.
  DK_Identifier,

  /// Declares function parameters.
  DK_Function,

  /// Declares a type.
  /// This is whatever is on the RHS of a :
  /// For example, x:int
  /// int is labeled as a DK_Type.
  DK_Type,

  /// This indicates the global namespace specifier was used to name the
  /// declaration.
  DK_GlobalNamespecifier,

  /// This is for things which are defined outside of their declared scope.
  /// Example: a.b. The name specifier would be a
  DK_NestedNameSpecifier,

  /// Template indication for classes. This part of the declarator is used to
  /// track if a templated type declaration is being given.
  DK_TemplateParams,

  /// This is a special declarator type that really doesn't do anything
  /// with the exception of constructing an empty template parameter list
  /// This occurs only during explicit specialization x[^int]
  DK_ImplicitEmptyTemplateParams,

  /// This is for when we have x[^int]
  DK_ExplicitSpecialization,

  /// This is for when we have x[T:type][^T]
  DK_PartialSpecialization,

  /// This declarator indicates that there was an error evaluating
  /// the declarator. This usually means that there is an ErrorSyntax node
  /// located within the tree.
  DK_Error,
};

/// The attribute collection type.
using Attributes = llvm::SmallVector<const Syntax *, 16>;

/// The structure associated with derived class, by tag name.
/// DK_Unknown
///   - has pointer to Syntax node indicating which node was unknown.
///
/// DK_Error
///   - has pointer to Syntax node indicating the location of the error node.
///
/// DK_GlobalNamespecifier
///   - Has pointer to the SyntaxCall containing the "operator'.'".
///
/// DK_Identifier
///   - Has pointer to AtomSyntax indicating the final part of the name.
///
/// DK_NestedNameSpecifier
///   - Has Has pointer AtomSyntax indicate the nested name specifier
///
/// DK_Function
///   - Has a pointer to a Syntax node that contains the function parameter
///   - gold::Scope - A pointer to the scope that the parameter were
///     constructed within.
///
/// DK_Type
///   - A pointer to the Syntax node for the root of the type expression.
///
/// DK_TemplateParams
///   - A Syntax node pointing to the parameters contained within an element node
///   - A gold::Scope that contains parameter declarations
///   - clang::TemplateArgumentListInfo
///
/// DK_ImplicitEmptyTemplateParams
///   - Pointer to syntax node that's associated with an explicit specialization.
///   - A pointer to a gold::Scope, that is empty.
///   - clang::TemplateParameterList
///
/// DK_ExplicitSpecialization
///   - A pointer to the node containing the specialization arguments.
///   - clang::TemplateArgumentListInfo
///
/// DK_PartialSpecialization
///   - A pointer to the node containing the specialization arguments.
///   - clang::TemplateArgumentListInfo
///
/// A declarator introduces the declaration of a value.
///
/// TODO: Represent multiple declarators whose syntax would be
/// something like: x, y : int -- maybe.
class Declarator {
public:
  Declarator(DeclaratorKind K, Declarator *P)
    : Kind(K), Next(P) { }

  virtual ~Declarator() {
    delete Next;
  }
  /// The kind of declarator.
  DeclaratorKind getKind() const {
    return Kind;
  }

  bool isIdentifier() const { return Kind == DK_Identifier; }
  bool isType() const { return Kind == DK_Type; }
  bool isFunction() const { return Kind == DK_Function; }
  bool isUnknown() const { return Kind == DK_Unknown; }
  bool isGlobalNameSpecifier() const { return Kind == DK_GlobalNamespecifier; }
  bool isNestedNameSpecifier() const { return Kind == DK_NestedNameSpecifier; }
  bool isTemplateParameters() const { return Kind == DK_TemplateParams; }
  bool isImplicitTemplateParameters() const { return Kind == DK_ImplicitEmptyTemplateParams; }
  bool isExplicitSpecialization() const { return Kind == DK_ExplicitSpecialization; }
  bool isPartialSpecialization() const { return Kind == DK_PartialSpecialization; }
  bool isError() const { return Kind == DK_Error; }

  UnknownDeclarator *getAsUnknown();
  const UnknownDeclarator *getAsUnknown() const;
  ErrorDeclarator *getAsError();
  const ErrorDeclarator *getAsError() const;
  GlobalNameSpecifierDeclarator *getAsGlobalNameSpecifier();
  const GlobalNameSpecifierDeclarator *getAsGlobalNameSpecifier() const;
  IdentifierDeclarator *getAsIdentifier();
  const IdentifierDeclarator *getAsIdentifier() const;
  NestedNameSpecifierDeclarator *getAsNestedNameSpecifier();
  const NestedNameSpecifierDeclarator *getAsNestedNameSpecifier() const;
  FunctionDeclarator *getAsFunction();
  const FunctionDeclarator *getAsFunction() const;
  TypeDeclarator *getAsType();
  const TypeDeclarator *getAsType() const;
  TemplateParamsDeclarator *getAsTemplateParams();
  const TemplateParamsDeclarator *getAsTemplateParams() const;
  ImplicitEmptyTemplateParamsDeclarator *getAsImplicitEmptyTemplateParams();
  const ImplicitEmptyTemplateParamsDeclarator *getAsImplicitEmptyTemplateParams() const;
  ExplicitSpecializationDeclarator *getAsExplicitSpecialization();
  const ExplicitSpecializationDeclarator *getAsExplicitSpecialization() const;
  PartialSpecializationDeclarator *getAsPartialSpecialization();
  const PartialSpecializationDeclarator *getAsPartialSpecialization() const;

  /// Get a SourceLocation representative of this declarator.
  virtual clang::SourceLocation getLoc() const = 0;

  /// Returns a readable string representing this declarator.
  virtual std::string getString(bool IncludeKind = false) const = 0;

  /// Prints the declarator sequence.
  void printSequence(llvm::raw_ostream &os) const;

  /// This sets the attribute node and records all attributes into the
  /// UnprocessedAttributes member.
  void recordAttributes(const Syntax* AttributeNode);

private:
  /// The kind of declarator.
  DeclaratorKind Kind;
public:
  /// The next declarator in the sequence.
  Declarator *Next = nullptr;

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_DeclaratorBase;
  }

  /// For non-identifiers, the call representing the declarator component.
  // const Syntax *Call = nullptr;

  // The tag's body.
  // clang::Scope *TagScope = nullptr;

  /// TODO: What other information do we need here?
  // union {
  //   /// For DK_Identifier, the id.
  //   const Syntax *Id;

  //   /// For DK_Function, information about parameters.
  //   struct ParamInfoType {
  //     /// The initial parameter list.
  //     const Syntax *Params;

  //     /// For DK_Function, the template parameter list.
  //     const Syntax *TemplateParams;

  //     /// The scope constructed during elaboration.
  //     Scope *ConstructedScope;

  //     /// The scope containing the template parameters
  //     Scope *TemplateScope;

  //     /// Whether or not this function has a variadic parameter.
  //     bool VariadicParam;
  //   } ParamInfo;

  //   /// For DK_Type, the type in the call.
  //   const Syntax *Type;

  //   /// For DK_TemplateParams, for templated types.
  //   struct TemplateInfoStruct {
  //     /// A pointer to the template parameters within the declaration.
  //     const Syntax* Params;
  //   } TemplateInfo;
  // } Data;

  /// This is optionally set for each piece of the declarator
  const Syntax* AttributeNode = nullptr;
  llvm::Optional<Attributes> UnprocessedAttributes;
};

class UnknownDeclarator : public Declarator {
  const Syntax *UnknownDeclSyntax;
public:
  UnknownDeclarator(const Syntax* InvalidSyntax, Declarator *Next)
    :Declarator(DK_Unknown, Next),
    UnknownDeclSyntax(InvalidSyntax)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const Syntax *getUnknownNode() const { return UnknownDeclSyntax; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_Unknown;
  }
};

class ErrorDeclarator : public Declarator {
  const Syntax *Err;
public:
  ErrorDeclarator(const Syntax* ErrNode, Declarator *Next)
    :Declarator(DK_Error, Next),
    Err(ErrNode)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const Syntax *getErrorNode() const { return Err; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_Error;
  }
};

class GlobalNameSpecifierDeclarator : public Declarator {
  const Syntax *Name;
public:
  GlobalNameSpecifierDeclarator(const Syntax *NameNode, Declarator *Next)
    :Declarator(DK_GlobalNamespecifier, Next),
    Name(NameNode)
  { }
  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const Syntax *getGlobalNameSpecifier() const { return Name; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_GlobalNamespecifier;
  }
};

class IdentifierDeclarator : public Declarator {
  const AtomSyntax *Name;
public:
  IdentifierDeclarator(const AtomSyntax *NameNode, Declarator *Next)
    :Declarator(DK_Identifier, Next),
    Name(NameNode)
  { }
  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const AtomSyntax *getIdentifier() const { return Name; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_Identifier;
  }
};

class NestedNameSpecifierDeclarator : public Declarator {
  const AtomSyntax *Name;
public:
  NestedNameSpecifierDeclarator(const AtomSyntax* NameNode, Declarator *Next)
    :Declarator(DK_NestedNameSpecifier, Next),
    Name(NameNode)
  { }
  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const AtomSyntax *getNestedName() const { return Name; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_NestedNameSpecifier;
  }
};

class FunctionDeclarator : public Declarator {
  const ListSyntax *Params;
  gold::Scope *Scope;
  bool HasVariadicParam = false;
public:
  FunctionDeclarator(const ListSyntax *ParamsNode, gold::Scope *ParamScope,
                     Declarator *Next, bool HasElipsis = false)
    :Declarator(DK_Function, Next),
    Params(ParamsNode),
    Scope(ParamScope),
    HasVariadicParam(HasElipsis)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  gold::Scope *getScope() const { return Scope; }
  void setScope(gold::Scope *NewScope) { Scope = NewScope; }
  gold::Scope *&getScopePtrRef() { return Scope;}
  const ListSyntax *getParams() const { return Params; }
  bool isVariadic() const { return HasVariadicParam; }
  void setIsVariadic(bool Val) { HasVariadicParam = Val; }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_Function;
  }
};

class TypeDeclarator : public Declarator {
  const Syntax *TyExpr;
public:
  TypeDeclarator(const Syntax *TypeRoot, Declarator *Next)
    :Declarator(DK_Type, Next),
    TyExpr(TypeRoot)
  { }
  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;
  const Syntax *getTyExpr() const { return TyExpr; }
  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_Type;
  }
};

class TemplateParamsDeclarator : public Declarator {
  const ListSyntax *Params;
  gold::Scope *Scope;
  clang::TemplateParameterList *ClangParamList;
public:
  TemplateParamsDeclarator(const ListSyntax *ParamsNode,
                           gold::Scope *ParamScope,
                           Declarator *Next)
    :Declarator(DK_TemplateParams, Next),
    Params(ParamsNode),
    Scope(ParamScope),
    ClangParamList(nullptr)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  gold::Scope *getScope() const { return Scope; }
  void setScope(gold::Scope *NewScope) { Scope = NewScope; }
  gold::Scope *&getScopePtrRef() { return Scope;}
  const ListSyntax *getParams() const { return Params; }

  clang::TemplateParameterList *getTemplateParameterList() const {
    return ClangParamList;
  }
  void setTemplateParameterList(clang::TemplateParameterList *ParamList) {
    ClangParamList = ParamList;
  }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_TemplateParams;
  }
};

class ImplicitEmptyTemplateParamsDeclarator : public Declarator {
  const Syntax *Owner;
  gold::Scope *Scope;
  clang::TemplateParameterList *ClangParamList;
public:
  ImplicitEmptyTemplateParamsDeclarator(const Syntax *ExplicitSpecialization,
                                       gold::Scope *ParamScope,
                                       Declarator *Next)
    :Declarator(DK_ImplicitEmptyTemplateParams, Next),
    Owner(ExplicitSpecialization),
    Scope(ParamScope),
    ClangParamList(nullptr)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  gold::Scope *getScope() const { return Scope; }
  void setScope(gold::Scope *NewScope) { Scope = NewScope; }
  gold::Scope *&getScopePtrRef() { return Scope;}
  const Syntax *getOwner() const { return Owner; }

  clang::TemplateParameterList *getTemplateParameterList() const {
    return ClangParamList;
  }
  void setTemplateParameterList(clang::TemplateParameterList *ParamList) {
    ClangParamList = ParamList;
  }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_ImplicitEmptyTemplateParams;
  }
};

class ExplicitSpecializationDeclarator : public Declarator {
  const ListSyntax *Args;
  clang::TemplateArgumentListInfo ArgListInfo;
public:
  ExplicitSpecializationDeclarator(const ListSyntax *SpecializationArgs,
                                   Declarator *Next)
    :Declarator(DK_ExplicitSpecialization, Next),
    Args(SpecializationArgs)
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const ListSyntax *getArgs() const { return Args; }
  const clang::TemplateArgumentListInfo &getArgList() const {
    return ArgListInfo;
  }

  clang::TemplateArgumentListInfo &getArgList() {
    return ArgListInfo;
  }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_ExplicitSpecialization;
  }
};

class PartialSpecializationDeclarator : public Declarator {
  const ListSyntax *Args;
  clang::TemplateArgumentListInfo ArgListInfo;
public:
  PartialSpecializationDeclarator(const ListSyntax *SpecializationArgs,
                                  Declarator *Next)
    :Declarator(DK_PartialSpecialization, Next),
    Args(SpecializationArgs),
    ArgListInfo()
  { }

  virtual clang::SourceLocation getLoc() const override;
  virtual std::string getString(bool IncludeKind = false) const override;

  const ListSyntax *getArgs() const { return Args; }
  const clang::TemplateArgumentListInfo &getArgList() const {
    return ArgListInfo;
  }

  clang::TemplateArgumentListInfo &getArgList() {
    return ArgListInfo;
  }

  static bool classof(const Declarator *Dcl) {
    return Dcl->getKind() == DK_PartialSpecialization;
  }
};

}

#endif