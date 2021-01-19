//===- BlueSema.h - Blue sematic actions ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the sema class for the blue language.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_BLUE_BLUE_SEMA_H
#define CLANG_BLUE_BLUE_SEMA_H

#include "clang/AST/ExprCppx.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/Type.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "clang/Sema/Sema.h"

#include "clang/Blue/BlueLateElaboration.h"
#include "clang/Blue/BlueSyntaxContext.h"
#include "clang/Blue/BlueScope.h"
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
class DeclContext;
class CppxDependentMemberAccessExpr;
} // namespace clang




namespace blue {

class Sema {
  /// Syntactic context
  blue::SyntaxContext &Context;

  /// The clang semantic object, allows to create various syntax nodes
  /// as well as perform important transformations on them.
  clang::Sema &CxxSema;

  clang::ASTContext &CxxAST;

  /// Stack of active Scopes.
  llvm::SmallVector<Scope *, 4> ScopeStack;

  /// The declaration context.
  Declaration *CurrentDecl = nullptr;

  /// A mapping of clang Decl nodes to Blue declarations.
  std::unordered_map<clang::Decl *, Declaration *> DeclToDecl;

public:
  Sema(SyntaxContext &Context, clang::Sema &S);
  ~Sema();

  llvm::StringMap<clang::BinaryOperatorKind> BinOpMap;
  llvm::StringMap<clang::UnaryOperatorKind> UnaryOpMap;

  clang::QualType DefaultCharTy;

  clang::DeclContext *CurContext = nullptr;

  clang::Sema &getCxxSema();
  clang::ASTContext &getCxxAST();

  // Perform unqualified lookup of a name in the current scope.
  bool lookupUnqualifiedName(clang::LookupResult &R);

  // Perform unqualified lookup of a name starting in S.
  // Returns true if there's an error, and false if not.
  // In the event that it returns false but doesn't have any results
  // it means that the id looked up was a built in type that doesn't have
  // a declaration, and a 2nd level of access should get the type from
  // within the BuiltinTypes map.
  bool lookupUnqualifiedName(clang::LookupResult &R, Scope *S);

  Scope *getCurrentScope();

  /// Enter a new scope corresponding to the syntax S.
  void enterScope(Scope::Kind K, const Syntax *S);

  /// Leave the current scope. The syntax S must match the syntax for
  /// which the scope was initially pushed.
  void leaveScope(const Syntax *S);
  Scope *saveScope(const Syntax *S);

  /// Push a new scope.
  void pushScope(Scope *S);

  /// Pop the current scope, returning it.
  Scope *popScope();

  /// Returns the current DeclContext that's set within the clang::Sema.
  /// It's worth noting that getCurrentCxxDeclContext doesn't always equal
  /// getCurClangDeclContext.
  clang::DeclContext *getCurClangDeclContext() const;

  /// The current declaration.
  Declaration *getCurrentDecl() {
    return CurrentDecl;
  }

  /// Make D the current declaration.
  void pushDecl(Declaration *D);

  /// Sets the decl context without modifying the clang::Sema class
  void setCurrentDecl(Declaration *D);

  /// Sets only the clang DeclContext.
  void setClangDeclContext(clang::DeclContext *DC);

  /// Make the owner of CurrentDecl current.
  void popDecl();

  // Dictionary of built in types.
  const llvm::StringMap<clang::QualType> BuiltinTypes;
  const llvm::StringMap<clang::QualType> createBuiltinTypeList();

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

  clang::CppxTypeLiteral *buildTypeExprTypeFromExpr(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                  bool IsConstructExpr = false);

  clang::CppxTypeLiteral *buildTypeExprTypeFromExprLiteral(clang::Expr *E,
                                                    clang::SourceLocation Loc,
                                                  bool IsConstructExpr = false);

  clang::QualType buildQualTypeExprTypeFromExpr(clang::Expr *E,
                                                clang::SourceLocation Loc,
                                                bool IsConstructExpr = false);

  clang::CppxTypeLiteral *buildTypeExprFromTypeDecl(
                      const clang::TypeDecl *TyDecl, clang::SourceLocation Loc);

  clang::CppxDeclRefExpr *buildTemplateType(clang::TemplateDecl *TD,
                                            clang::SourceLocation Loc);

  /// This functions will be responsible for converting an expression into
  /// a TInfo and reporting if it fails, it shall return nullptr in the
  /// event it fails.
  clang::TypeSourceInfo *getTypeSourceInfoFromExpr(const clang::Expr *TyExpr,
                             clang::SourceLocation Loc=clang::SourceLocation());

  /// This simply checks and extracts the QualType from a type expression.
  /// This can return a QualType where .isNull() is true,
  clang::QualType getQualTypeFromTypeExpr(const clang::Expr *TyExpr);


  clang::ParsedType getParsedTypeFromExpr(const clang::Expr *TyExpr,
                             clang::SourceLocation Loc=clang::SourceLocation());

  clang::CppxDeclRefExpr *buildNSDeclRef(clang::CppxNamespaceDecl *D,
                                         clang::SourceLocation Loc);

  clang::CppxDeclRefExpr *buildNSDeclRef(clang::NamespaceAliasDecl *D,
                                         clang::SourceLocation Loc);
  clang::CppxDeclRefExpr *buildAnyDeclRef(clang::QualType KindTy,
                                          clang::Decl *D,
                                          clang::SourceLocation Loc);

  clang::Decl *getDeclFromExpr(const clang::Expr *DeclExpr,
                               clang::SourceLocation Loc);
  /// This function extracts a namespace from an expression and returns the
  /// resulting namespace or nullptr if invalid
  clang::CppxNamespaceDecl *getNSDeclFromExpr(const clang::Expr *DeclExpr,
                                              clang::SourceLocation Loc);

  /// This function dispatches to other functions to handle other declarations
  /// It is the job of this function to determine of the declaration should be
  /// merged with other declarations or is in conflict with other declarations.
  bool checkForRedeclaration(Declaration *D);


public:
  /// Clang scope management functions.
  ///@{
  clang::Scope *getCurClangScope();
  clang::Scope *enterClangScope(unsigned int ScopeFlags);
  clang::Scope *moveToParentScopeNoPop();
  void reEnterClangScope(clang::Scope* Scope);
  void leaveClangScope(clang::SourceLocation Loc);
  clang::Scope* saveCurrentClangScope();
  //@}

private:
  friend struct Declaration;
  void addDeclToDecl(clang::Decl *Cxx, Declaration *Blue);
  Declaration *getDeclaration(clang::Decl *Cxx);

//===----------------------------------------------------------------------===//
//                               RAII Objects                                 //
//===----------------------------------------------------------------------===//

public:
// An RAII type for constructing scopes.
  struct ScopeRAII {
    ScopeRAII(Sema &S, Scope::Kind K, const Syntax *ConcreteTerm,
              Scope **SavedScope = nullptr)
      : S(S), SavedScope(SavedScope), ConcreteTerm(ConcreteTerm) {
      S.enterScope(K, ConcreteTerm);
      if (SavedScope) {
        *SavedScope = S.getCurrentScope();
      }
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

  /// Deep epaboation is mainly used for constexpr, whenever we need to fully
  /// elaborate a declaration before it's used. If something is used within
  // a constant expression it must be fully defined and intialized before you
  /// can use it and evaluate the constant expression.
  bool DeepElaborationMode = false;

  struct DeepElaborationModeRAII {
    Sema &SemaRef;
    bool PreviousState = false;

    DeepElaborationModeRAII(Sema &S, bool Enable = true)
      :SemaRef(S),
      PreviousState(S.DeepElaborationMode)
    {
      if (Enable) {
        SemaRef.DeepElaborationMode = true;
      }
    }

    void setMode(bool Mode) {
      SemaRef.DeepElaborationMode = Mode;
    }
    ~DeepElaborationModeRAII() {
      SemaRef.DeepElaborationMode = PreviousState;
    }
  };
private:
  llvm::SmallVector<Declaration *, 64> DeclsBeingElaborated;
public:
  struct DeclarationElaborationRAII {
    Sema &SemaRef;
    bool DidRecordDecl = false;

    DeclarationElaborationRAII(Sema &S, Declaration *NewDecl)
      :SemaRef(S)
    {
      assert(NewDecl && "Invalid declaration given.");
      assert(!NewDecl->IsElaborating && "Declaration already being elaborated");
      DidRecordDecl = true;
      SemaRef.DeclsBeingElaborated.push_back(NewDecl);
      SemaRef.DeclsBeingElaborated.back()->IsElaborating = true;
    }

    DeclarationElaborationRAII(Sema &S) :SemaRef(S) { }

    void init(Declaration *NewDecl) {
      assert(NewDecl && "Invalid declaration given.");
      assert(!NewDecl->IsElaborating && "Declaration already being elaborated");
      DidRecordDecl = true;
      SemaRef.DeclsBeingElaborated.push_back(NewDecl);
      SemaRef.DeclsBeingElaborated.back()->IsElaborating = true;
    }

    ~DeclarationElaborationRAII()  {
      if (DidRecordDecl) {
        SemaRef.DeclsBeingElaborated.back()->IsElaborating = false;
        SemaRef.DeclsBeingElaborated.pop_back();
      }
    }
  };
  void diagnoseElabCycleError(Declaration *CycleTerminalDecl);

public:

  //===--------------------------------------------------------------------===//
  //                    Complete class parsing/elaboration                    //
  //===--------------------------------------------------------------------===//
  ///{
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
  ///}


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
    Declaration *OriginalDecl;
    bool DoSetAndReset;
  public:
    DeclContextRAII(Sema &S, Declaration *D,
        bool SetAndResetDeclarationsOnly = false)
      :SemaRef(S), OriginalDecl(SemaRef.CurrentDecl),
      DoSetAndReset(SetAndResetDeclarationsOnly)
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


  /// This class is an RAII that tracks the classes scope and current status
  /// during processing. This allows for us to more easily keep track of
  /// the class currently being elaborated and how we hande that particular
  /// classes elaboration.
  /// This helps keep track of classes that are currently being elaborated.
  class ElaboratingClassDefRAII {
    Sema &SemaRef;
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
  struct SaveAndRestoreClangDCAndScopeRAII {
    Sema &SemaRef;
    clang::Scope *ScopeOnEntry = nullptr;
    clang::DeclContext *DCOnEntry = nullptr;
    SaveAndRestoreClangDCAndScopeRAII(Sema &S)
      :SemaRef(S),
      ScopeOnEntry(S.getCxxSema().CurScope),
      DCOnEntry(S.getCxxSema().CurContext)
    { }
    ~SaveAndRestoreClangDCAndScopeRAII() {
      SemaRef.getCxxSema().CurScope = ScopeOnEntry;
      SemaRef.getCxxSema().CurContext = DCOnEntry;
    }
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


struct ResumeScopeRAII {
  ResumeScopeRAII(Sema &S, Scope *Sc, const Syntax *ConcreteTerm,
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

struct EnterNonNestedClassEarlyElaboration {
  EnterNonNestedClassEarlyElaboration(Sema& S, Declaration* Decl)
    :SemaRef(S),
    PrevClassStack(std::move(SemaRef.ClassStack)),
    D(Decl),
    ScopeResumer(SemaRef, Decl->ScopeForDecl, D->Def),
    PrevContext(SemaRef.getCurClangDeclContext()),
    PrevDeclaration(SemaRef.getCurrentDecl()),
    PrevClangScope(SemaRef.getCurClangScope())
  {
    SemaRef.reEnterClangScope(D->ClangDeclaringScope);
    SemaRef.setClangDeclContext(D->DeclaringContext);
    SemaRef.setCurrentDecl(D->Ctx);
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
  ResumeScopeRAII ScopeResumer;
  clang::DeclContext* PrevContext = nullptr;
  Declaration* PrevDeclaration = nullptr;
  clang::Scope *PrevClangScope = nullptr;
};

} // end namespace blue

#endif
