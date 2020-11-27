//===- ExprCppx.h - Classes for representing expressions --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines the clang::Expr interface and subclasses for cppx expressions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_EXPRCPPX_H
#define LLVM_CLANG_AST_EXPRCPPX_H

#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"
#include "clang/AST/UnresolvedSet.h"
#include "clang/Basic/ExceptionSpecificationType.h"
#include "clang/Basic/ExpressionTraits.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Basic/TypeTraits.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/TrailingObjects.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>

namespace gold {
  /// Forward declaration
  /// Add additional kinds of derived classes here.
  enum class PartialExprKind : std::size_t;

  using ExprList = llvm::SmallVectorImpl<clang::Expr *>;
  /// This is a solution, it may not be the best solution but it's one that
  /// will do what we need. It will hide the incomplete information
  /// needed to construct the expression as part of the AST.
  ///
  /// The main problem is that clang doesn't really lend it self to this kind
  /// of AST expression naturally, because it was never meant to do this.
  /// So that being said we have to create a new way to collect and apply arguments
  /// to an incomplete expression, in some way, while hiding what that expression
  /// actually is and perfoming actions when possible to actually create a
  /// partially evaluated expression.
  ///
  /// The draw back to using this is that we may have to create a hierarchy
  /// so we can figure out what the thing being created actually is.
  class CppxPartialExprBase {
  private:
    PartialExprKind Kind;
  public:
    CppxPartialExprBase(PartialExprKind PEK) :Kind(PEK) { }

    virtual ~CppxPartialExprBase() = default;
    clang::SourceLocation BeginLocation;
    clang::SourceLocation EndLocation;

    clang::SourceLocation beginLoc() const { return BeginLocation; }
    clang::SourceLocation endLoc() const { return EndLocation; }

    PartialExprKind getKind() const { return Kind; }

    /// Return true if the given arguments can be handled applied to the
    /// partial expression, this could be template parameters or array access.
    virtual bool canAcceptElementArgs(const ExprList &Args) const = 0;
    virtual void applyElementArgs(const ExprList &Args) = 0;

    /// Returns true if the partial expression would accept function style
    /// call next. meaning (args). Args can be empty.
    virtual bool canAcceptFunctionArgs(const ExprList &Args) const = 0;
    virtual void applyFunctionArgs(const ExprList &Args) = 0;

    /// Check if the expression is completable
    virtual bool isCompletable() const = 0;

    /// This is used to generate the complete expression that is represented
    /// by a derived version of this class.
    virtual clang::Expr *completeExpr() = 0;

    /// Function for reporting errors in the event that an expression can not be
    /// completed and it's needed.
    virtual void diagnoseIncompleteReason() = 0;

    /// This is always true.
    static bool classof(const CppxPartialExprBase *) { return true; }
  };
}

namespace clang {

class ASTContext;

/// This is any type expression.
class CppxTypeLiteral : public Expr {
public:
  using ValueType = clang::TypeSourceInfo *;
private:
  ValueType Value;

  explicit CppxTypeLiteral(EmptyShell Empty)
    : Expr(CppxTypeLiteralClass, Empty) {}

public:
  CppxTypeLiteral(QualType K, ValueType T)
    : Expr(CppxTypeLiteralClass, K, VK_RValue, OK_Ordinary),
      Value(T) {
    setDependence(computeDependence(this));
  }

  ValueType getValue() const LLVM_READONLY {
    return Value;
  }

  SourceLocation getBeginLoc() const LLVM_READONLY;

  SourceLocation getEndLoc() const LLVM_READONLY;

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxTypeLiteralClass;
  }
  static CppxTypeLiteral* create(ASTContext &Context, QualType KindTy,
                                 ValueType Ty);

};

/// This represents a reference to a namespace expression.
class CppxDeclRefExpr : public Expr {
public:
  /// The actual type denoted by the literal.
  using ValueType = clang::Decl *;
private:
  ValueType DeclRef;

  /// The location of the namespace.
  SourceLocation Loc;

  explicit CppxDeclRefExpr(EmptyShell Empty)
    : Expr(CppxDeclRefExprClass, Empty) {}

public:
  CppxDeclRefExpr(QualType KindTy, ValueType D, SourceLocation L)
    : Expr(CppxDeclRefExprClass, KindTy, VK_RValue, OK_Ordinary),
      DeclRef(D), Loc(L) {
    setDependence(computeDependence(this));
  }

  ValueType getValue() const LLVM_READONLY {
    return DeclRef;
  }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return Loc;
  }

  SourceLocation getEndLoc() const LLVM_READONLY {
    return Loc;
  }

  /// Retrieve the location of the literal.
  SourceLocation getLocation() const { return Loc; }

  void setLocation(SourceLocation Location) { Loc = Location; }

  // Iterators
  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxDeclRefExprClass;
  }

  static CppxDeclRefExpr *Create(ASTContext &Context, QualType KindTy,
                                 ValueType NsDecl, SourceLocation Loc);
};


/// This is a partial implementation class that provides an interface into
/// the clang AST for partial gold expressions.
class CppxPartialEvalExpr : public Expr {
  /// The location associated with the expression being constructed.
  // SourceLocation Loc;
  gold::CppxPartialExprBase *Impl = nullptr;
  SourceLocation Loc;

  explicit CppxPartialEvalExpr(EmptyShell Empty)
    : Expr(CppxPartialEvalExprClass, Empty)
  { }

public:
  CppxPartialEvalExpr(QualType ResultTy, gold::CppxPartialExprBase *E,
                      SourceLocation L)
    :Expr(CppxPartialEvalExprClass, ResultTy, VK_RValue, OK_Ordinary),
    Impl(E), Loc(L)
  { }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    assert(Impl && "Implementation not set.");
    return Impl->beginLoc();
  }

  SourceLocation getEndLoc() const LLVM_READONLY {
    assert(Impl && "Implementation not set.");
    return Impl->endLoc();
  }

  /// Retrieve the location of the literal.
  SourceLocation getLocation() const { return Loc;}
  void setLocation(SourceLocation Location) { Loc = Location; }

  gold::CppxPartialExprBase *getImpl() const { return Impl; }
  void setImpl(gold::CppxPartialExprBase *Base) { Impl = Base; }

  bool canAcceptElementArgs(const gold::ExprList &Args) const {
    assert(Impl && "Implementation not set.");
    return Impl->canAcceptElementArgs(Args);
  }

  bool canAcceptFunctionArgs(const gold::ExprList &Args) const {
    assert(Impl && "Implementation not set.");
    return Impl->canAcceptFunctionArgs(Args);
  }

  // Application functions.
  void applyElementArgs(const gold::ExprList &Args) {
    assert(Impl && "Implementation not set.");
    Impl->applyElementArgs(Args);
  }

  void applyFunctionArgs(const gold::ExprList &Args) {
    assert(Impl && "Implementation not set.");
    Impl->applyFunctionArgs(Args);
  }

  bool isCompletable() {
    assert(Impl && "Implementation not set.");
    return Impl->isCompletable();
  }
  clang::Expr *forceCompleteExpr() {
    assert(Impl && "Implementation not set.");
    if (Impl->isCompletable()){
      return Impl->completeExpr();
    }
    Impl->diagnoseIncompleteReason();
    return nullptr;
  }

  child_range children() {
    return child_range(child_iterator(), child_iterator());
  }
  const_child_range children() const {
    return const_child_range(const_child_iterator(), const_child_iterator());
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxPartialEvalExprClass;
  }

  static CppxPartialEvalExpr *Create(ASTContext &Ctx,
                                     gold::CppxPartialExprBase *E,
                                     SourceLocation Loc);
};

/// This class solves a problem within gold that has to do with template
/// instantiation and possible dependent names associated with them.
/// This could be 1 of 2 things:
/// 1) a type expression,
/// 3) a variable reference expression.
class CppxDependentMemberAccessExpr final
    : public Expr {
  friend class ASTStmtReader;
  friend class ASTStmtWriter;

  /// The expression for the base pointer or class reference,
  /// e.g., the \c x in x.f.  Can be null in implicit accesses.
  Stmt *Base = nullptr;

  Expr *NameSpecifier = nullptr;

  /// The type of the base expression.  Never null, even for
  /// implicit accesses.
  QualType BaseType;

  /// The member to which this member expression refers, which
  /// can be name, overloaded operator, or destructor.
  DeclarationNameInfo MemberNameInfo;

  CppxDependentMemberAccessExpr(const ASTContext &Ctx, Expr *Base, QualType BaseType,
                                SourceLocation OperatorLoc,
                                DeclarationNameInfo MemberNameInfo,
                                Expr *NameSpecExpr = nullptr);

  CppxDependentMemberAccessExpr(EmptyShell Empty);

public:
  static CppxDependentMemberAccessExpr *
    Create(const ASTContext &Ctx, Expr *Base, QualType BaseType,
           SourceLocation OperatorLoc, DeclarationNameInfo MemberNameInfo,
           Expr *NameSpecExpr = nullptr);

  static CppxDependentMemberAccessExpr *CreateEmpty(const ASTContext &Ctx);

  /// True if this is an implicit access, i.e. one in which the
  /// member being accessed was not written in the source.  The source
  /// location of the operator is invalid in this case.
  bool isImplicitAccess() const {
    if (!Base)
      return true;
    return cast<Expr>(Base)->isImplicitCXXThis();
  }

  /// Retrieve the base object of this member expressions,
  /// e.g., the \c x in \c x.m.
  Expr *getBase() const {
    assert(!isImplicitAccess());
    return cast<Expr>(Base);
  }

  Expr *getNameQualifierExpr() const {
    return NameSpecifier;
  }

  QualType getBaseType() const { return BaseType; }

  /// Retrieve the location of the '->' or '.' operator.
  SourceLocation getOperatorLoc() const {
    return CppxDependentMemberAccessExprBits.OperatorLoc;
  }

  /// Retrieve the name of the member that this expression refers to.
  const DeclarationNameInfo &getMemberNameInfo() const {
    return MemberNameInfo;
  }

  /// Retrieve the name of the member that this expression refers to.
  DeclarationName getMember() const { return MemberNameInfo.getName(); }

  // Retrieve the location of the name of the member that this
  // expression refers to.
  SourceLocation getMemberLoc() const { return MemberNameInfo.getLoc(); }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return MemberNameInfo.getBeginLoc();
  }

  SourceLocation getEndLoc() const LLVM_READONLY {
    return MemberNameInfo.getEndLoc();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxDependentMemberAccessExprClass;
  }

  // Iterators
  child_range children() {
    if (isImplicitAccess())
      return child_range(child_iterator(), child_iterator());
    return child_range(&Base, &Base + 1);
  }

  const_child_range children() const {
    if (isImplicitAccess())
      return const_child_range(const_child_iterator(), const_child_iterator());
    return const_child_range(&Base, &Base + 1);
  }
};

class CppxTemplateOrArrayExpr final
    : public Expr {
  friend class ASTStmtReader;
  friend class ASTStmtWriter;
  unsigned NumArgs = 0;

  Stmt **getTrailingStmts() {
    return reinterpret_cast<Stmt **>(reinterpret_cast<char *>(this) +
                                   TemplateOrArrayBits.OffsetToTrailingObjects);
  }

  Stmt *const *getTrailingStmts() const {
    return const_cast<CppxTemplateOrArrayExpr *>(this)->getTrailingStmts();
  }

  unsigned getSizeOfTrailingStmts() const {
    return (NumArgs + 1) * sizeof(Stmt *);
  }

protected:
  // Name of associated bitfields.
  // CppxTemplateOrArrayExprBitFields TemplateOrArrayBits;
  CppxTemplateOrArrayExpr(const ASTContext &Ctx, Stmt *Base,
                          ArrayRef<Expr *> Args);

  /// Build an empty call expression, for deserialization.
  CppxTemplateOrArrayExpr(const ASTContext &Ctx, unsigned ArgCount,
                          EmptyShell Empty);

  /// Return the size in bytes needed for the trailing objects.
  /// Used by the derived classes to allocate the right amount of storage.
  static unsigned sizeOfTrailingObjects(unsigned ArgsCount) {
    return (ArgsCount + 1) * sizeof(Stmt *);
  }

public:
  static CppxTemplateOrArrayExpr *
    Create(const ASTContext &Ctx, Expr *BaseExpr, ArrayRef<Expr *> Args);

  /// Create an empty call expression, for deserialization.
  static CppxTemplateOrArrayExpr *
    CreateEmpty(const ASTContext &Ctx, unsigned NumArgs,
                EmptyShell Empty);

  Expr *getBase() { return cast<Expr>(*getTrailingStmts()); }
  const Expr *getBase() const { return cast<Expr>(*getTrailingStmts()); }
  void setBase(Expr *E) { *getTrailingStmts() = E; }

  /// getNumArgs - Return the number of actual arguments to this call.
  unsigned getNumArgs() const { return NumArgs; }

  /// Retrieve the call arguments.
  Expr **getArgs() {
    return reinterpret_cast<Expr **>(getTrailingStmts() + 1);
  }

  Expr *const *getArgs() const {
    return reinterpret_cast<Expr *const *>(getTrailingStmts() + 1);
  }

  /// getArg - Return the specified argument.
  Expr *getArg(unsigned Arg) {
    assert(Arg < getNumArgs() && "Arg access out of range!");
    return getArgs()[Arg];
  }
  const Expr *getArg(unsigned Arg) const {
    assert(Arg < getNumArgs() && "Arg access out of range!");
    return getArgs()[Arg];
  }

  /// setArg - Set the specified argument.
  void setArg(unsigned Arg, Expr *ArgExpr) {
    assert(Arg < getNumArgs() && "Arg access out of range!");
    getArgs()[Arg] = ArgExpr;
  }

  typedef ExprIterator arg_iterator;
  typedef ConstExprIterator const_arg_iterator;
  typedef llvm::iterator_range<arg_iterator> arg_range;
  typedef llvm::iterator_range<const_arg_iterator> const_arg_range;

  arg_range arguments() { return arg_range(arg_begin(), arg_end()); }
  const_arg_range arguments() const {
    return const_arg_range(arg_begin(), arg_end());
  }

  arg_iterator arg_begin() {
    return getTrailingStmts() + 1;
  }
  arg_iterator arg_end() { return arg_begin() + getNumArgs(); }

  const_arg_iterator arg_begin() const {
    return getTrailingStmts();
  }
  const_arg_iterator arg_end() const { return arg_begin() + getNumArgs(); }

  /// This method provides fast access to all the subexpressions of
  /// a CallExpr without going through the slower virtual child_iterator
  /// interface.  This provides efficient reverse iteration of the
  /// subexpressions.  This is currently used for CFG construction.
  ArrayRef<Stmt *> getRawSubExprs() {
    return llvm::makeArrayRef(getTrailingStmts(), 1 + getNumArgs());
  }

  SourceLocation getBeginLoc() const LLVM_READONLY {
    return getBase()->getBeginLoc();
  }
  SourceLocation getEndLoc() const LLVM_READONLY {
    if (getNumArgs()) {
      return getArg(getNumArgs()-1)->getEndLoc();
    }
    return getBase()->getBeginLoc();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CppxTemplateOrArrayExprClass;
  }

  // Iterators
  child_range children() {
    return child_range(getTrailingStmts(), getTrailingStmts() + 1 + getNumArgs());
  }

  const_child_range children() const {
    return const_child_range(getTrailingStmts(),
                             getTrailingStmts() + 1 + getNumArgs());
  }
};

} // namespace clang

#endif // LLVM_CLANG_AST_EXPRCPPX_H


