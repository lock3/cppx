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

#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldLateElaboration.h"

#include <memory>
#include <vector>

namespace clang {

class Decl;
class DeclContext;
class DiagnosticsEngine;
class LookupResult;
class Preprocessor;
class CXXRecordDecl;
class Sema;
class Stmt;
class Type;

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

  /// Restore previously exited DeclContext
  void restoreDeclContext(Declaration *D);

  /// Make D the current declaration.
  void pushDecl(Declaration *D);

  /// Sets the decl context without modifying the clang::Sema class
  void setCurrentDecl(Declaration *D);

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

  // clang::QualType lookUpType(clang::IdentifierInfo *Id, Scope *S) const;

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
      :SemaRef(S), Scope(Sc), ExitTerm(ConcreteTerm), PopOnExit(PopOnExit)
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
    gold::Scope *Scope;
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

  class DeclContextRAII {
    Sema &SemaRef;
    bool DoSetAndReset;
    clang::DeclContext *OriginalDC;
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

  /// This helps keep track of the scope associated with templated classes
  /// by providing optionally initialized behavior for a scope. This is done
  /// using llvm::optional.
  using OptionalScopeRAII = OptionalInitScope<ScopeRAII>;
  using OptioanlClangScopeRAII = OptionalInitScope<ClangScopeRAII>;

  // Dictionary of built in types.
  //
  // FIXME: This should be initialized in the constructor.
  const llvm::StringMap<clang::QualType> BuiltinTypes = {
    {"void", Context.CxxAST.VoidTy},
    {"bool", Context.CxxAST.BoolTy},
    {"char", Context.CxxAST.CharTy},
    {"wchar_t", Context.CxxAST.WideCharTy},
    {"wint_t", Context.CxxAST.WIntTy},
    {"char8_t", Context.CxxAST.Char8Ty},
    {"char16_t", Context.CxxAST.Char16Ty},
    {"char32_t", Context.CxxAST.Char32Ty},
    {"signed char", Context.CxxAST.SignedCharTy},
    {"short", Context.CxxAST.ShortTy},
    {"short int", Context.CxxAST.ShortTy},
    {"int", Context.CxxAST.IntTy},
    {"long", Context.CxxAST.LongTy},
    {"long int", Context.CxxAST.LongTy},
    {"long long", Context.CxxAST.LongLongTy},
    {"long long int", Context.CxxAST.LongLongTy},
    {"int128_t", Context.CxxAST.Int128Ty},
    {"unsigned char", Context.CxxAST.UnsignedCharTy},
    {"unsigned short", Context.CxxAST.UnsignedShortTy},
    {"unsigned short int", Context.CxxAST.UnsignedShortTy},
    {"unsigned", Context.CxxAST.UnsignedIntTy},
    {"unsigned int", Context.CxxAST.UnsignedIntTy},
    {"unsigned long", Context.CxxAST.UnsignedLongTy},
    {"unsigned long int", Context.CxxAST.UnsignedLongTy},
    {"unsigned long long", Context.CxxAST.UnsignedLongLongTy},
    {"unsigned long long int", Context.CxxAST.UnsignedLongLongTy},
    {"uint128_t", Context.CxxAST.UnsignedInt128Ty},
    {"float", Context.CxxAST.FloatTy},
    {"double", Context.CxxAST.DoubleTy},
    {"long double", Context.CxxAST.LongDoubleTy},
    {"float128_t", Context.CxxAST.Float128Ty},
    {"type", Context.CxxAST.CppxKindTy}
  };
};

} // namespace gold

#endif
