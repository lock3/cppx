//===- GoldSema.h - Semantic Analysis of Gold ASTs ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file declares the gold::Sema class, which performs semantic analysis
//  for the Gold language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDSEMA_H
#define CLANG_GOLD_GOLDSEMA_H

#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "clang/Sema/Sema.h"

#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldLateElaboration.h"

#include <memory>
#include <vector>

namespace clang {

class CppxNamespaceDecl;
class Decl;
class DeclContext;
class DiagnosticsEngine;
class LookupResult;
class Preprocessor;
class CXXRecordDecl;
class Sema;
class Stmt;
class Type;
class CppxTypeLiteral;
class CppxDeclRefExpr;
class TypeSourceInfo;
} // namespace clang

namespace gold {

class Declarator;
class Declaration;
struct Syntax;
struct ArraySyntax;
class SyntaxContext;

/// Maintains the state of Gold-to-C++ translation for a
/// translation unit in the Gold Language.
class Sema {
  friend struct QualifiedLookupRAII;

  // The clang semantic object, allows to create various syntax nodes
  // as well as perform important transformations on them.
  clang::Sema &CxxSema;

  // Stack of active Scopes.
  llvm::SmallVector<Scope *, 4> ScopeStack;

  // The declaration context.
  Declaration *CurrentDecl = nullptr;

public:
  Sema(SyntaxContext &Context, clang::Sema &S);
  ~Sema();
  // Look through a translation unit and map the identifiers to Clang
  // constructs.
  void IdentifyDecls(const ArraySyntax *S);

  /// Check if, within the current scope a access specifier is valid,
  bool accessSpecifierIsValidInScope() const;
  // Scope management.

  /// Get the currently active Scope.
  Scope *getCurrentScope();

  /// Push a new scope.
  void pushScope(Scope *S);

  /// This is used in order to properly restore a scope stack that was prevously
  /// replace. This is done to allow for breaks in elaboration, and switching
  /// between valid lookup contexts while additional elaboration is completed.
  void setCurrentScope(Scope *S);
  
  /// Pop the current scope, returning it.
  Scope *popScope();

  /// Enter a new scope corresponding to the syntax S. This is primarily
  /// used for the elaboration of function and template parameters, which
  /// have no corresponding declaration at the point of elaboration.
  void enterScope(ScopeKind K, const Syntax *S, Declaration *D = nullptr);

  /// Leave the current scope. The syntax S must match the syntax for
  /// which the scope was initially pushed.
  void leaveScope(const Syntax *S);

  /// Leaves the current scope, but preserves the object for later use. This
  /// is primarily used to save lists of parameter declarations. The syntax
  /// S must match the syntax for which the scope was initially pushed.
  Scope *saveScope(const Syntax *S);

  // Name lookup

  // Perform unqualified lookup of a name in the current scope.
  bool lookupUnqualifiedName(clang::LookupResult &R);

  // Perform unqualified lookup of a name starting in S.
  bool lookupUnqualifiedName(clang::LookupResult &R, Scope *S);

  // Perform qualified lookup of a name starting in S.
  bool lookupQualifiedName(clang::LookupResult &R, Scope *S);
  bool lookupQualifiedName(clang::LookupResult &R);

  /// This checks to see if we are within a class body scope currently.
  bool scopeIsWithinClass();
  bool scopeIsWithinClass(Scope *S);

  /// Gets a declaration for a scope, if available.
  clang::Decl *getDeclForScope();
  clang::Decl *getDeclForScope(Scope *S);

  // Declaration context
  /// The current declaration.
  Declaration *getCurrentDecl() {
    return CurrentDecl;
  }

  /// The current C++ declaration.
  clang::DeclContext *getCurrentCxxDeclContext();

  /// Returns the current DeclContext that's set within the clang::Sema.
  /// It's worth noting that getCurrentCxxDeclContext doesn't always equal
  /// getCurClangDeclContext.
  clang::DeclContext *getCurClangDeclContext() const;

  /// Restore previously exited DeclContext
  void restoreDeclContext(Declaration *D);

  /// Make D the current declaration.
  void pushDecl(Declaration *D);

  /// Sets the decl context without modifying the clang::Sema class
  void setCurrentDecl(Declaration *D);

  /// Sets only the clang DeclContext.
  void setClangDeclContext(clang::DeclContext *DC);

  /// Make the owner of CurrentDecl current.
  void popDecl();

  // Iterate through the mapped identifiers and determine their type.
  void elaborateDecls();

  /// Iterate through a Declarations redecl chain and see if it has
  /// already been defined.
  /// \param Start - The decl we are beginning the search with.
  template <typename DeclType>
  bool checkForRedefinition(Declaration *Start) {
    using clang::cast;
    using clang::cast_or_null;

    Declaration *Iter = Start->First;
    do {
      DeclType *IterD = cast_or_null<DeclType>(Iter->Cxx);
      if (Iter != Start->First &&
          IterD && IterD->isThisDeclarationADefinition()) {
        DeclType *StartCxx = cast<DeclType>(Start->Cxx);
        Diags.Report(StartCxx->getBeginLoc(), clang::diag::err_redefinition)
          << StartCxx->getName();
        Diags.Report(IterD->getBeginLoc(), clang::diag::note_previous_decl)
          << IterD->getName();
        return true;
      }

      Iter = Iter->Next;
    } while (Iter != Start->First);

    return false;
  }

  clang::Sema &getCxxSema() { return CxxSema; }

  SyntaxContext &getContext() { return Context; }

  /// This is the clang processing scope. This is mostly for code GenPieces.
  clang::Scope *getCurClangScope();
  clang::Scope *enterClangScope(unsigned int ScopeFlags);
  clang::Scope *moveToParentScopeNoPop();
  void reEnterClangScope(clang::Scope* Scope);
  void leaveClangScope(clang::SourceLocation Loc);
  clang::Scope* saveCurrentClangScope();

  void dumpState(llvm::raw_ostream &out = llvm::outs());

  /// This is a stack of classes currently being elaborated.
  llvm::SmallVector<ElaboratingClass *, 6> ClassStack;

  /// Returns the top of the stack for a class currently being elaborated.
  ElaboratingClass &getCurrentElaboratingClass() {
    assert(!ClassStack.empty() && "No classes on stack!");
    return *ClassStack.back();
  }
  using ClassElaborationState = clang::Sema::DelayedDiagnosticsState;
  bool isElaboratingClass() const;
  ClassElaborationState pushElaboratingClass(Declaration *D,
                                             bool TopLevelClass);
  void deallocateElaboratingClass(ElaboratingClass *D);
  void popElaboratingClass(ClassElaborationState State);

  /// This attempts to check if declaration needs to be delayed during class
  /// elaboration.
  bool declNeedsDelayed(Declaration *D);

  /// Based on the current elaboration state read from class stack we compute
  /// the current depth of a template.
  ///
  /// \note This could be changed in the future in order to include ths current
  /// scope stack for elaboration.
  ///
  unsigned computeTemplateDepth() const;



  /// Members that allow construction of the CppxLiteralType
  ///{
  clang::CppxTypeLiteral *buildTypeExpr(clang::QualType Ty,
                                        clang::SourceLocation Loc);
  clang::CppxTypeLiteral *buildTypeExpr(clang::TypeSourceInfo *TInfo);
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::TypeSourceInfo *TInfo);

  
  clang::CppxTypeLiteral *buildAnyTypeExpr(clang::QualType KindTy,
                                           clang::QualType Ty,
                                           clang::SourceLocation Loc);

  clang::CppxTypeLiteral *buildFunctionTypeExpr(clang::QualType FnTy,
                                                clang::SourceLocation BeginLoc,
                                                clang::SourceLocation LParenLoc,
                                                clang::SourceLocation RParenLoc,
                                                clang::SourceRange ExceptionSpecRange,
                                                clang::SourceLocation EndLoc,
                           llvm::SmallVectorImpl<clang::ParmVarDecl *> &Params);
  clang::CppxTypeLiteral *buildTypeExprFromTypeDecl(
                      const clang::TypeDecl *TyDecl, clang::SourceLocation Loc);

  clang::CppxDeclRefExpr *buildTemplateType(clang::TemplateDecl *TD,
                                            clang::SourceLocation Loc);
  
  clang::Expr *addConstToTypeExpr(const clang::Expr *TyExpr,
                                  clang::SourceLocation Loc);
  clang::Expr *addRefToTypeExpr(const clang::Expr *TyExpr,
                                clang::SourceLocation Loc);
  clang::Expr *addRRefToTypeExpr(const clang::Expr *TyExpr,
                                 clang::SourceLocation Loc);
  ///}


  /// This simply checks and extracts the QualType from a type expression.
  /// This can return a QualType where .isNull() is true,
  clang::QualType getQualTypeFromTypeExpr(const clang::Expr *TyExpr);

  /// This functions will be responsible for converting an expression into
  /// a TInfo and reporting if it fails, it shall return nullptr in the
  /// event it fails.
  clang::TypeSourceInfo *getTypeSourceInfoFromExpr(const clang::Expr *TyExpr,
                             clang::SourceLocation Loc=clang::SourceLocation());

  /// These are stub implementations for now so that I can implement them at a
  /// later time with a later expression.
  // clang::TypeSourceInfo *getTypeSourceInfoForTemplateExpr(
  //     const clang::Expr *TemplateTy);
  // clang::TypeSourceInfo *getTypeSourceInfoForTemplateExpr(
  //     const clang::Expr *TemplateTy, clang::SourceLocation Loc);



  clang::CppxDeclRefExpr *buildNSDeclRef(clang::CppxNamespaceDecl *D,
                                         clang::SourceLocation Loc);

  clang::CppxDeclRefExpr *buildAnyDeclRef(clang::QualType KindTy,
                                          clang::Decl *D,
                                          clang::SourceLocation Loc);
  /// This function extracts a namespace from an expression and returns the
  /// resulting namespace or nullptr if invalid
  clang::Decl *getDeclFromExpr(const clang::Expr *DeclExpr,
                               clang::SourceLocation Loc);
private:
  /// =============== Members related to qualified lookup. ================= ///

  // The list of nested-name-specifiers to use for qualified lookup.
  // FIXME: make this a list, instead of a single NNS.
  clang::CppxNamespaceDecl *NNS;

public:
  bool isQualifiedLookupContext() const {
    return QualifiedLookupContext;
  }

  // True when lookups should be performed with a qualifier.
  bool QualifiedLookupContext = false;

  /// ============= Members related to NNS typo correction. =============== ///

  /// A C++ scope specifier that gets set during NNS so we can leverage Clang's
  /// typo correction.
  clang::CXXScopeSpec CurNNSContext;
public:
  // The context
  SyntaxContext &Context;

  clang::AttributeFactory AttrFactory;
  
  // The Clang diagnostics engine.
  clang::DiagnosticsEngine &Diags;

  // Tokenizations of commonly compared-against strings.
  const clang::IdentifierInfo *OperatorColonII;
  const clang::IdentifierInfo *OperatorExclaimII;
  const clang::IdentifierInfo *OperatorEqualsII;
  const clang::IdentifierInfo *OperatorIfII;
  const clang::IdentifierInfo *OperatorElseII;
  const clang::IdentifierInfo *OperatorReturnII;
  const clang::IdentifierInfo *OperatorReturnsII;
  const clang::IdentifierInfo *OperatorDotII;
  const clang::IdentifierInfo *OperatorForII;
  const clang::IdentifierInfo *OperatorWhileII;
  const clang::IdentifierInfo *OperatorInII;
  const clang::IdentifierInfo *OperatorDotDotII;
  const clang::IdentifierInfo *OperatorConstII;
  const clang::IdentifierInfo *OperatorCaretII;
  const clang::IdentifierInfo *OperatorRefII;
  const clang::IdentifierInfo *OperatorRRefII;
  
  /// These are the identifier names given to operators in C++.
  ///{
  const clang::IdentifierInfo *CPPOp_Plus;
  const clang::IdentifierInfo *CPPOp_Minus;
  const clang::IdentifierInfo *CPPOp_Mul;
  const clang::IdentifierInfo *CPPOp_Div;
  const clang::IdentifierInfo *CPPOp_Mod;
  const clang::IdentifierInfo *CPPOp_BitWiseXOr;
  const clang::IdentifierInfo *CPPOp_BitWiseOr;
  const clang::IdentifierInfo *CPPOp_BitWiseAnd;
  const clang::IdentifierInfo *CPPOp_BitWiseNot;
  const clang::IdentifierInfo *CPPOp_BitWiseLeftShift;
  const clang::IdentifierInfo *CPPOp_BitWiseRightShift;
  const clang::IdentifierInfo *CPPOp_LOr;
  const clang::IdentifierInfo *CPPOp_LAnd;
  const clang::IdentifierInfo *CPPOp_LNot;
  const clang::IdentifierInfo *CPPOp_Less;
  const clang::IdentifierInfo *CPPOp_Greater;
  const clang::IdentifierInfo *CPPOp_LessEqual;
  const clang::IdentifierInfo *CPPOp_GreaterEqual;
  const clang::IdentifierInfo *CPPOp_Equal;
  const clang::IdentifierInfo *CPPOp_NotEqual;
  const clang::IdentifierInfo *CPPOp_Assign;
  const clang::IdentifierInfo *CPPOp_PlusAssign;
  const clang::IdentifierInfo *CPPOp_MinusAssign;
  const clang::IdentifierInfo *CPPOp_MulAssign;
  const clang::IdentifierInfo *CPPOp_DivAssign;
  const clang::IdentifierInfo *CPPOp_ModAssign;
  const clang::IdentifierInfo *CPPOp_BitWiseXOrAssign;
  const clang::IdentifierInfo *CPPOp_BitWiseOrAssign;
  const clang::IdentifierInfo *CPPOp_BitWiseAndAssign;
  const clang::IdentifierInfo *CPPOp_BitWiseLeftShiftAssign;
  const clang::IdentifierInfo *CPPOp_BitWiseRightShiftAssign;
  const clang::IdentifierInfo *CPPOp_ArrayAccess;
  const clang::IdentifierInfo *CPPOp_FunctionCall;
  ///}

  // We don't expose this externally, we may need to provide a way to explicitly
  // invoke this in order to fully support C++.
  const clang::IdentifierInfo *CPPOp_Arrow;

  // An RAII type for constructing scopes.
  struct ScopeRAII {
    ScopeRAII(Sema &S, ScopeKind K, const Syntax *ConcreteTerm,
              Scope **SavedScope = nullptr)
      : S(S), SavedScope(SavedScope), ConcreteTerm(ConcreteTerm) {
      S.enterScope(K, ConcreteTerm);
    }

    ~ScopeRAII() {
      if (SavedScope)
        *SavedScope = S.saveScope(ConcreteTerm);
      else
        S.leaveScope(ConcreteTerm);
    }

  private:
    Sema &S;

    /// Optionally save this scope to be stored in the Declaration.
    Scope **SavedScope;

    const Syntax *ConcreteTerm;
  };

  struct ResumeScopeRAII {
    ResumeScopeRAII(Sema &S, gold::Scope *Sc, const Syntax *ConcreteTerm,
        bool PopOnExit = true)
      :SemaRef(S), ExitTerm(ConcreteTerm), PopOnExit(PopOnExit)
    {
      SemaRef.pushScope(Sc);
    }

    ~ResumeScopeRAII() {
      if (PopOnExit) {
        SemaRef.popScope();
      } else {
        SemaRef.leaveScope(ExitTerm);
      }
    }
  private:
    Sema &SemaRef;
    const Syntax *ExitTerm;
    bool PopOnExit;
  };


  struct ClangScopeRAII {
    ClangScopeRAII(Sema &S, unsigned ScopeKind, clang::SourceLocation ExitLoc,
        bool EnteringScope = true, bool BeforeCompoundStmt = false)
      : SemaPtr(&S), ExitingLocation(ExitLoc)
    {
      if (EnteringScope && !BeforeCompoundStmt)
        SemaPtr->enterClangScope(ScopeKind);
      else {
        if (BeforeCompoundStmt)
          SemaPtr->getCxxSema().incrementMSManglingNumber();

        SemaPtr = nullptr;
      }
    }

    ~ClangScopeRAII() {
      Exit();
    }

    void Exit() {
      if (SemaPtr) {
        SemaPtr->leaveClangScope(ExitingLocation);
        SemaPtr = nullptr;
      }
    }

  private:
    Sema *SemaPtr;
    clang::SourceLocation ExitingLocation;
  };

  /// This class provides RAII for keeping track of DeclContexts, even if 
  /// the DeclContext isn't set by us for clang::Sema.
  class DeclContextRAII {
    Sema &SemaRef;
    bool DoSetAndReset;
    Declaration *OriginalDecl;
  public:
    DeclContextRAII(Sema &S, Declaration *D,
        bool SetAndResetDeclarationsOnly = false)
      :SemaRef(S), OriginalDecl(SemaRef.CurrentDecl)
    {
      if (DoSetAndReset)
        SemaRef.CurrentDecl = D;
      else 
        SemaRef.pushDecl(D);
    }
    ~DeclContextRAII() {
      if (DoSetAndReset){
        SemaRef.setCurrentDecl(OriginalDecl);
      } else 
        SemaRef.popDecl();
    }
  };
  
  struct EnterNonNestedClassEarlyElaboration {
    EnterNonNestedClassEarlyElaboration(Sema& S, Declaration* Decl)
      :SemaRef(S),
      PrevClassStack(std::move(SemaRef.ClassStack)),
      D(Decl),
      GoldScopeResumer(SemaRef, Decl->ScopeForDecl, D->Op),
      PrevContext(SemaRef.getCurClangDeclContext()),
      PrevDeclaration(SemaRef.getCurrentDecl()),
      PrevClangScope(SemaRef.getCurClangScope())
    {
      SemaRef.reEnterClangScope(D->ClangDeclaringScope);
      SemaRef.setClangDeclContext(D->DeclaringContext);
      SemaRef.setCurrentDecl(D->ParentDecl);
    }

    ~EnterNonNestedClassEarlyElaboration() {
      // Moving the previous information back onto the stack.
      SemaRef.ClassStack = std::move(PrevClassStack);

      SemaRef.setCurrentDecl(PrevDeclaration);
      SemaRef.reEnterClangScope(PrevClangScope);
      SemaRef.setClangDeclContext(PrevContext);
    }
  private:
    Sema &SemaRef;
    llvm::SmallVector<ElaboratingClass *, 6> PrevClassStack;
    Declaration* D;
    ResumeScopeRAII GoldScopeResumer;
    clang::DeclContext* PrevContext = nullptr;
    Declaration* PrevDeclaration = nullptr;
    clang::Scope *PrevClangScope = nullptr;
  };

  struct ExprEvalRAII {
    ExprEvalRAII(Sema& S, clang::Sema::ExpressionEvaluationContext NewContext)
      :SemaRef(S)
    {
      SemaRef.getCxxSema().PushExpressionEvaluationContext(NewContext);
    }
    ~ExprEvalRAII() {
      SemaRef.getCxxSema().PopExpressionEvaluationContext();
    }
  private:
    Sema& SemaRef;
  };



  /// This class is an RAII that tracks the classes scope and current status
  /// during processing. This allows for us to more easily keep track of
  /// the class currently being elaborated and how we hande that particular
  /// classes elaboration.
  /// This helps keep track of classes that are currently being elaborated.
  class ElaboratingClassDefRAII {
    gold::Sema &SemaRef;
    bool WasPopped;
    ClassElaborationState State;
  public:
    ElaboratingClassDefRAII(Sema &S, Declaration *D, bool IsTopLevelClass,
        bool IsTemplate = false)
      :SemaRef(S), WasPopped(false),
      State(SemaRef.pushElaboratingClass(D, IsTopLevelClass)) { }

    ~ElaboratingClassDefRAII() {
      if (!WasPopped)
        pop();
    }

    void pop() {
      assert(!WasPopped && "Attempting to double exit class. "
          "Class already popped");
      WasPopped = true;
      SemaRef.popElaboratingClass(State);
    }
  };

  template<typename T>
  class OptionalInitScope {
    Sema &SemaRef;
    llvm::Optional<T> Opt;
  public:
    OptionalInitScope(Sema &S) :SemaRef(S) { }
    template<typename... Args>
    OptionalInitScope(Sema &S, Args&&... Arguments)
        :SemaRef(S), Opt()
    {
      Init(std::forward<Args>(Arguments)...);
    }

    template<typename... Args>
    void Init(Args&&... Arguments) {
      assert(!Opt && "Error attempting to enter scope twice.");
      Opt.emplace(SemaRef, std::forward<Args>(Arguments)...);
    }
  };

  struct QualifiedLookupRAII {
    QualifiedLookupRAII(Sema &SemaRef,
                        bool &QualifiedLookupContext,
                        clang::CppxNamespaceDecl **NNS)
      : SemaRef(SemaRef),
        QualifiedLookupContext(QualifiedLookupContext) {
      SemaRef.NNS = *NNS;
      QualifiedLookupContext = true;
    }

    ~QualifiedLookupRAII() {
      QualifiedLookupContext = false;
      SemaRef.NNS = nullptr;
    }

  private:
    Sema &SemaRef;
    bool &QualifiedLookupContext;
  };

  struct NNSRAII {
    NNSRAII(clang::CXXScopeSpec &SS)
      : SS(SS)
      {}

    ~NNSRAII() {
      SS.clear();
    }
  private:
    clang::CXXScopeSpec &SS;
  };

  /// This helps keep track of the scope associated with templated classes
  /// by providing optionally initialized behavior for a scope. This is done
  /// using llvm::optional.
  using OptionalScopeRAII = OptionalInitScope<ScopeRAII>;
  using OptioanlClangScopeRAII = OptionalInitScope<ClangScopeRAII>;

  clang::QualType NullTTy;

  clang::QualType CharTy;
  clang::QualType Char8Ty;
  clang::QualType Char16Ty;
  clang::QualType Char32Ty;

  clang::QualType IntTy;
  clang::QualType Int8Ty;
  clang::QualType Int16Ty;
  clang::QualType Int32Ty;
  clang::QualType Int64Ty;
  clang::QualType Int128Ty;

  clang::QualType UIntTy;
  clang::QualType UInt8Ty;
  clang::QualType UInt16Ty;
  clang::QualType UInt32Ty;
  clang::QualType UInt64Ty;
  clang::QualType UInt128Ty;
  
  clang::QualType Float16Ty;
  clang::QualType Float32Ty;
  clang::QualType Float64Ty;
  clang::QualType Float128Ty;

  // Dictionary of built in types.
  const llvm::StringMap<clang::QualType> BuiltinTypes;


  /// IsUnaryOperator
  /// Checks to see if a given unary operator is a know unary operator.
  bool IsUnaryOperator(llvm::StringRef OpName) const;
  
  /// GetUnaryOperatorKind
  /// @returns false if the operator was found and true if it wasn't.
  bool GetUnaryOperatorKind(llvm::StringRef OpName,
                            clang::UnaryOperatorKind &Kind) const;

  // Map of unary operators, this shouldn't have a static constructor
  // according to the LLVM documentation so it's stored here instead.
  const llvm::StringMap<clang::UnaryOperatorKind> UnaryOpNames;

  /// Checks to see if a given name is associated with a binary operator.
  bool IsBinaryOperator(llvm::StringRef OpName) const;

  /// GetBinaryOperatorKind
  /// Attempts to search for and return the binary operator associated with
  /// a given operator name.
  /// @returns false if the operator was found and true if it wasn't.
  bool GetBinaryOperatorKind(llvm::StringRef OpName,
      clang::BinaryOperatorKind &Kind) const;

  // Map of binary operator names to their clang operator kind.
  const llvm::StringMap<clang::BinaryOperatorKind> BinaryOpNames;
};

} // namespace gold

#endif
