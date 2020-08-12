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

#include "clang/Gold/GoldLateElaboration.h"
#include "clang/Gold/GoldOperatorInfo.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntaxContext.h"

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
class Elaborator;

/// This contains the status for the elaboration of method attributes.
/// Each flag is set to true/false depending on if that field has been
/// encountered yet or not.
struct AttrStatus {
  AttrStatus() {
    HasConstExpr = false;
    HasInLine = false;
    HasExtern = false;
    HasMemberAccessSpecifier = false;
    HasExceptionSpec = false;
    HasStatic = false;
    HasExplicit = false;
    HasVirtual = false;
    HasOverride = false;
    HasFinal = false;
    HasConst = false;
    HasThreadLocal = false;
    HasBits = false;
    HasAlignAs = false;
    HasRefQualifier = false;
  }
  bool HasConstExpr : 1;
  bool HasInLine : 1;
  bool HasExtern : 1;
  bool HasMemberAccessSpecifier : 1;
  bool HasExceptionSpec : 1;
  bool HasStatic : 1;
  bool HasThreadLocal : 1;
  bool HasExplicit : 1;
  bool HasVirtual : 1;
  bool HasOverride : 1;
  bool HasFinal : 1;
  bool HasConst : 1;
  bool HasBits : 1;
  bool HasAlignAs : 1;
  bool HasRefQualifier : 1;
};

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

  /// This is used to trigger a complete elaboration of a declaration during
  /// lookup. This is used to indicate that any definition needed must be
  /// fully elaborated before it can be used. Used during constant
  /// expression evaluation, but this is triggerred during lookup.
  bool ForceDeepElaborationDuringLookup = false;

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

  // Perform unqualified memberlooku
  bool unqualifiedMemberAccessLookup(clang::LookupResult &R,
                                     const clang::Expr *LHSResultExpr);

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

  LateElaboratedMethodDeclaration *CurrentLateMethodDecl = nullptr;
  class LateMethodRAII {
    Sema &SemaRef;
    LateElaboratedMethodDeclaration *Previous;
  public:
    LateMethodRAII(Sema &S, LateElaboratedMethodDeclaration *NextDecl)
      :SemaRef(S),
      Previous(SemaRef.CurrentLateMethodDecl)
    {
      SemaRef.CurrentLateMethodDecl = NextDecl;
    }
    ~LateMethodRAII() {
      SemaRef.CurrentLateMethodDecl = Previous;
    }
  };
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
  clang::CppxTypeLiteral *buildNsTypeExpr(clang::SourceLocation Loc);
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
  clang::ParsedType getParsedTypeFromExpr(const clang::Expr *TyExpr,
                             clang::SourceLocation Loc=clang::SourceLocation());

  clang::CppxDeclRefExpr *buildNSDeclRef(clang::CppxNamespaceDecl *D,
                                         clang::SourceLocation Loc);

  clang::CppxDeclRefExpr *buildAnyDeclRef(clang::QualType KindTy,
                                          clang::Decl *D,
                                          clang::SourceLocation Loc);

  clang::Decl *getDeclFromExpr(const clang::Expr *DeclExpr,
                               clang::SourceLocation Loc);
  /// This function extracts a namespace from an expression and returns the
  /// resulting namespace or nullptr if invalid
  clang::CppxNamespaceDecl *getNsDeclFromExpr(const clang::Expr *DeclExpr,
                                          clang::SourceLocation Loc);


private:
  /// =============== Members related to qualified lookup. ================= ///
  enum NNSKind {
    NNSK_Empty,
    NNSK_Global,
    NNSK_Namespace
  };
  struct GlobalNNS {
    gold::Scope *Scope;
    clang::DeclContext *DC;
  };
  union NNSLookupDecl {
    GlobalNNS Global;
    clang::CppxNamespaceDecl *NNS;
  };
  NNSKind CurNNSKind = NNSK_Empty;
  // The list of nested-name-specifiers to use for qualified lookup.

  // FIXME: make this a list, instead of a single NNS.
  NNSLookupDecl CurNNSLookupDecl;
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

  /// This class keeps track of the current nested namespace lookup state
  /// it provides a means of constructing things that are either a
  /// CppxNamespaceDecl, or the global namespace scope and DeclContext.
  struct QualifiedLookupRAII {
    // Constructor for non-global namespace specifier.
    QualifiedLookupRAII(Sema &SemaRef,
                        bool &QualifiedLookupContext,
                        clang::CppxNamespaceDecl *NS)
      : SemaRef(SemaRef),
        QualifiedLookupContext(QualifiedLookupContext),
        PreviousKind(SemaRef.CurNNSKind),
        PreviousLookup(SemaRef.CurNNSLookupDecl) {
      SemaRef.CurNNSLookupDecl.NNS = NS;
      SemaRef.CurNNSKind = NNSK_Namespace;
      QualifiedLookupContext = true;
    }

    // Constructor for global namespace specifier.
    QualifiedLookupRAII(Sema &SemaRef,
                        bool &QualifiedLookupContext,
                        gold::Scope *Scope, clang::DeclContext *DC)
      : SemaRef(SemaRef),
        QualifiedLookupContext(QualifiedLookupContext),
        PreviousKind(SemaRef.CurNNSKind),
        PreviousLookup(SemaRef.CurNNSLookupDecl) {
      SemaRef.CurNNSLookupDecl.Global.Scope = Scope;
      SemaRef.CurNNSLookupDecl.Global.DC = DC;
      SemaRef.CurNNSKind = NNSK_Global;
      QualifiedLookupContext = true;
    }

    ~QualifiedLookupRAII() {
      QualifiedLookupContext = false;
      SemaRef.CurNNSLookupDecl = PreviousLookup;
      SemaRef.CurNNSKind = PreviousKind;
    }

  private:
    Sema &SemaRef;
    bool &QualifiedLookupContext;
    NNSKind PreviousKind;
    NNSLookupDecl PreviousLookup;
  };
public:
  // The context
  SyntaxContext &Context;

  clang::AttributeFactory AttrFactory;

  // The Clang diagnostics engine.
  clang::DiagnosticsEngine &Diags;

  // Tokenizations of commonly compared-against strings.
  clang::IdentifierInfo *const OperatorColonII;
  clang::IdentifierInfo *const OperatorArrowII;
  clang::IdentifierInfo *const OperatorExclaimII;
  clang::IdentifierInfo *const OperatorEqualsII;
  clang::IdentifierInfo *const OperatorIfII;
  clang::IdentifierInfo *const OperatorElseII;
  clang::IdentifierInfo *const OperatorReturnII;
  clang::IdentifierInfo *const OperatorReturnsII;
  clang::IdentifierInfo *const OperatorDotII;
  clang::IdentifierInfo *const OperatorForII;
  clang::IdentifierInfo *const OperatorWhileII;
  clang::IdentifierInfo *const OperatorInII;
  clang::IdentifierInfo *const OperatorDotDotII;
  clang::IdentifierInfo *const OperatorConstII;
  clang::IdentifierInfo *const OperatorRefII;
  clang::IdentifierInfo *const OperatorRRefII;
  clang::IdentifierInfo *const OperatorBracketsII;
  clang::IdentifierInfo *const OperatorParensII;

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

  template<typename T>
  class OptionalInitClangRAII {
    clang::Sema &SemaRef;
    llvm::Optional<T> Opt;
  public:
    OptionalInitClangRAII(Sema &S) :SemaRef(S.getCxxSema()) { }
    template<typename... Args>
    OptionalInitClangRAII(Sema &S, Args&&... Arguments)
        :SemaRef(S.getCxxSema()), Opt()
    {
      Init(std::forward<Args>(Arguments)...);
    }

    template<typename... Args>
    void Init(Args&&... Arguments) {
      assert(!Opt && "Error attempting to enter scope twice.");
      Opt.emplace(SemaRef, std::forward<Args>(Arguments)...);
    }
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
  using OptionalResumeScopeRAII = OptionalInitScope<ResumeScopeRAII>;
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

  /// Contains a large amount of constant information about individual operators
  /// If it's not in here it cannot be overriden.
  const OperatorInfo OpInfo;


  using AttributeHandler = void(*)(Elaborator &, Declaration*,
                                         const Syntax*, AttrStatus &);
  using StringToAttrHandlerMap = llvm::StringMap<AttributeHandler>;

  /// MethodAttrHelper Contains mapings back to member functions that handle
  /// attributes processing of specific attributes. This is so we don't have to
  /// do a N^2 search of attribute names.
  /// The reason that this is here instead of inside the elaborator class
  /// is that the elaborator class gets constructed multiple times.
  const StringToAttrHandlerMap AttrHandlerMap;


  /// Deep elaboration mode functions.
  bool isInDeepElaborationMode() const;

  /// Sets Deep elaboration to true, returns the previous elaboration mode.
  bool setDeepElaborationMode(bool EnableDisable);

  struct EnterDeepElabRAII {
    Sema &SemaRef;
    bool PreviousValue;
    EnterDeepElabRAII(Sema &S)
      :SemaRef(S),
      PreviousValue(S.setDeepElaborationMode(true))
    {}
    ~EnterDeepElabRAII() {
      SemaRef.setDeepElaborationMode(PreviousValue);
    }
  };
public:
  using FunctionExtInfo = clang::FunctionProtoType::ExtInfo;
  using FunctionExtProtoInfo = clang::FunctionProtoType::ExtProtoInfo;
  using FunctionExceptionSpec = clang::FunctionProtoType::ExceptionSpecInfo;
  /// This does a rebuild the type of the function, in a single action without
  /// the need to rebuild the TypeLoc for the function more then a single time.
  /// This returns true if there was an error.
  /// In the event of an error no changes are made to the FD.
  bool rebuildFunctionType(clang::FunctionDecl *FD,
                           clang::SourceLocation Loc,
                           const clang::FunctionProtoType *FuncProtoType,
                           const FunctionExtInfo &ExtInfo,
                           const FunctionExtProtoInfo &ProtoTypeInfo,
                           const FunctionExceptionSpec &ExceptionSpecInfo);

  clang::CppxNamespaceDecl *ActOnStartNamespaceDef(clang::Scope *NamespcScope,
                                      clang::SourceLocation InlineLoc,
                                      clang::SourceLocation NamespaceLoc,
                                      clang::SourceLocation IdentLoc,
                                      clang::IdentifierInfo *II,
                                      clang::SourceLocation LBrace,
                                      const clang::ParsedAttributesView &AttrList,
                                      clang::UsingDirectiveDecl *&UD);
};

} // namespace gold

#endif
