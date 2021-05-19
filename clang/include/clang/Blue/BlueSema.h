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
#include <utility>


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
class CppxPartialEvalExpr;
class CXXConstructorDecl;

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

  /// Translation unit decl.
  Declaration *TUDecl = nullptr;
public:
  clang::AttributeFactory AttrFactory;

  Sema(SyntaxContext &Context, clang::Sema &S);
  ~Sema();

  void setTUDecl(Declaration *TU) { TUDecl = TU; }
  Declaration *getTUDecl() const { return TUDecl; }

  llvm::StringMap<clang::BinaryOperatorKind> BinOpMap;
  llvm::StringMap<clang::UnaryOperatorKind> UnaryPrefixOpMap;
  llvm::StringMap<clang::UnaryOperatorKind> UnaryPostfixOpMap;

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

  // bool lookupQualifiedName(clang::LookupResult &R, Scope *S,
  //                          Declaration *NotThisOne = nullptr);
  bool lookupQualifiedName(clang::LookupResult &R);

  Scope *getCurrentScope() const;

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

  /// This checks to see if we are in a class body scope currently.
  bool scopeIsClass() const;

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

  /// Bitwise built-ins
  ///{
  /// This is a helper function that dumps ALL of the functions into the
  /// translation unit at once.
  void createBitwiseBuiltinFunctions();

private:
  bool DidLoadBWAnd = false;
  bool DidLoadBWOr = false;
  bool DidLoadBWXOr = false;
  bool DidLoadBWShl = false;
  bool DidLoadBWShr = false;
  bool DidLoadBWNot = false;
public:
  void buildBitAnd();
  void buildBitOr();
  void buildBitXOr();
  void buildBitShr();
  void buildBitShl();
  void buildBitNot();
  ///}

  /// Constructs inside of the global scope the at address function, and
  /// object that's needed to use it.
  ///{
  clang::FunctionDecl *getInplaceNewFn();
private:
  void buildInplaceNew();
  clang::IdentifierInfo *InplaceNewId;
  clang::FunctionDecl *InplaceNewFnDcl = nullptr;
public:
  ///}
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
  clang::TypeSourceInfo *getTypeSourceInfoFromExpr(clang::Expr *TyExpr,
                             clang::SourceLocation Loc=clang::SourceLocation());

  /// This simply checks and extracts the QualType from a type expression.
  /// This can return a QualType where .isNull() is true,
  // clang::QualType getQualTypeFromTypeExpr(clang::Expr *TyExpr);


  clang::ParsedType getParsedTypeFromExpr(clang::Expr *TyExpr,
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


  /// Based on the current elaboration state read from class stack we compute
  /// the current depth of a template.
  ///
  /// \note This could be changed in the future in order to include ths current
  /// scope stack for elaboration.
  ///
  unsigned computeTemplateDepth() const;

  // Counts the depth of the current generic lambda. Will be zero whenever we
  // are not currently elaborating a lambda expression.
  unsigned LambdaTemplateDepth = 0;

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
public:
  Declaration *getDeclaration(clang::Decl *Cxx);

  // clang::Expr *buildReferenceToDecl(clang::SourceLocation Loc,
  //                                   clang::LookupResult &R,
  //                                   bool IsKnownOverload);
//===----------------------------------------------------------------------===//
//                      Partial Expr Creation                                 //
//===----------------------------------------------------------------------===//
public:

  clang::CppxPartialEvalExpr *createPartialExpr(clang::SourceLocation Loc,
                                                bool IsWithinClass,
                                                bool allowImplicitThis,
                                                clang::Expr *BaseExpr,
                                    bool IsPartOfTemplateInstantiation = false);
  bool memberAccessNeedsPartialExpr(clang::Expr *LHS, clang::IdentifierInfo *Id,
                                    clang::SourceLocation IdLoc);
  bool isThisValidInCurrentScope();
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
private:
  /// =============== Members related to qualified lookup. ================= ///
  enum NNSKind {
    NNSK_Empty,
    NNSK_Global,
    NNSK_Namespace,
    NNSK_NamespaceAlias,
    NNSK_Record,

    /// Special context used for when we have a nested name specifier
    /// with template parameters. Beacuse if we simply re-enter the current
    /// scope we won't have the template parameters that we created before this
    /// in scope, instead we will have those originally declared within
    /// the class, struct, or union and none for those from the nested name
    /// specifier.
    // NNSK_RecordTemplate
  };
  struct GlobalNNS {
    blue::Scope *Scope;
    clang::DeclContext *DC;
  };
  union NNSLookupDecl {
    GlobalNNS Global;
    clang::CppxNamespaceDecl *NNS;
    clang::NamespaceAliasDecl *Alias;
    // clang::CXXRecordDecl *Record;
    Scope *RebuiltClassScope;
  };

  NNSKind CurNNSKind = NNSK_Empty;
  // The list of nested-name-specifiers to use for qualified lookup.
  // FIXME: make this a list, instead of a single NNS.
  NNSLookupDecl CurNNSLookupDecl;

  Scope *duplicateScopeForNestedNameContext(Declaration *D);
public:

  void setLookupScope(GlobalNNS GlobalNs) {
    CurNNSLookupDecl.Global = GlobalNs;
    CurNNSKind = NNSK_Global;
  }

  void setLookupScope(clang::CppxNamespaceDecl *NNS) {
    CurNNSLookupDecl.NNS = NNS;
    CurNNSKind = NNSK_Namespace;
  }

  void setLookupScope(clang::NamespaceAliasDecl *Alias) {
    CurNNSLookupDecl.Alias = Alias;
    CurNNSKind = NNSK_NamespaceAlias;
  }

  bool setLookupScope(clang::CXXRecordDecl *Record);

  Scope *getLookupScope();

  // Get the identifier of the current lookup scope, or "true" for a global NNS
  std::pair<bool, clang::IdentifierInfo *>
  getLookupScopeName(NNSLookupDecl const &D, NNSKind K) const;
  std::pair<bool, clang::IdentifierInfo *> getLookupScopeName() const;

  bool isQualifiedLookupContext() const {
    return QualifiedLookupContext;
  }

  // True when lookups should be performed with a qualifier.
  bool QualifiedLookupContext = false;

  /// ============= Members related to NNS typo correction. =============== ///
  Declaration *getLookupContextFromExpr(clang::Expr *E);
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
                        blue::Scope *Scope, clang::DeclContext *DC)
      : SemaRef(SemaRef),
        QualifiedLookupContext(QualifiedLookupContext),
        PreviousKind(SemaRef.CurNNSKind),
        PreviousLookup(SemaRef.CurNNSLookupDecl) {
      SemaRef.CurNNSLookupDecl.Global.Scope = Scope;
      SemaRef.CurNNSLookupDecl.Global.DC = DC;
      SemaRef.CurNNSKind = NNSK_Global;
      QualifiedLookupContext = true;
    }

    // Constructor for namespace aliases
    QualifiedLookupRAII(Sema &SemaRef,
                        bool &QualifiedLookupContext,
                        clang::NamespaceAliasDecl *Alias)
      : SemaRef(SemaRef),
        QualifiedLookupContext(QualifiedLookupContext),
        PreviousKind(SemaRef.CurNNSKind),
        PreviousLookup(SemaRef.CurNNSLookupDecl) {
      SemaRef.CurNNSLookupDecl.Alias = Alias;
      SemaRef.CurNNSKind = NNSK_NamespaceAlias;
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

  // Allows us to keep our nns context for a bit longer.
  struct ExtendQualifiedLookupRAII {
    ExtendQualifiedLookupRAII(Sema &SemaRef)
      : ExtendQualifiedLookup(SemaRef.ExtendQualifiedLookup),
        CurNNSContext(SemaRef.CurNNSContext)
      {
        SavedValue = ExtendQualifiedLookup;
        ExtendQualifiedLookup = true;
      }

    ~ExtendQualifiedLookupRAII() {
      ExtendQualifiedLookup = SavedValue;
      CurNNSContext.clear();
    }

  private:
    bool SavedValue;
    bool &ExtendQualifiedLookup;
    clang::CXXScopeSpec &CurNNSContext;
  };

  bool isExtendedQualifiedLookupContext() const {
    return ExtendQualifiedLookup;
  }

private:
  // True if we want to maintain the NNSContext after we are done with
  // qualified lookup.
  bool ExtendQualifiedLookup = false;
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

public:
  /// Transformation triggering expressions.
  clang::QualType TransformCppxTypeExprType(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::TypeLocBuilder &TLB, clang::CppxTypeExprTypeLoc TL);

  clang::Expr *TransformCppxDependentMemberAccessExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxDependentMemberAccessExpr *E);

  clang::Expr *TransformCppxTemplateOrArrayExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxTemplateOrArrayExpr *E);

  clang::Expr *TransformCppxCallOrConstructorExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxCallOrConstructorExpr *E);

  clang::Expr *TransformCppxDerefOrPtrExpr(
    const clang::MultiLevelTemplateArgumentList &TemplateArgs,
    clang::SourceLocation Loc, clang::DeclarationName Entity,
    clang::CppxDerefOrPtrExpr *E);

  clang::ParsedTemplateArgument convertExprToTemplateArg(clang::Expr *E);

}; // End of class blue::Sema


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

struct ClangDeclContextRAII {
  Sema &SemaRef;
  clang::DeclContext *PrevDC;
  ClangDeclContextRAII(Sema &S, clang::DeclContext *NextDC)
    :SemaRef(S),
    PrevDC(S.getCurClangDeclContext())
  {
    SemaRef.setClangDeclContext(NextDC);
  }
  ~ClangDeclContextRAII() {
    SemaRef.setClangDeclContext(PrevDC);
  }
};

// using OptionalScopeRAII = OptionalInitScope<Sema::
using OptionalScopeRAII = OptionalInitScope<Sema::ScopeRAII>;
using OptionalResumeScopeRAII = OptionalInitScope<ResumeScopeRAII>;
using OptionalClangScopeRAII = OptionalInitScope<Sema::ClangScopeRAII>;
struct ElabBalanceChecker {
  Sema &SemaRef;
  Declaration *PrevDecl = nullptr;
  clang::DeclContext *PrevDC = nullptr;
  clang::Scope *PrevClangScope = nullptr;
  Scope *PrevBlueScope = nullptr;
  ElabBalanceChecker(Sema &S)
    :SemaRef(S),
    PrevDecl(SemaRef.getCurrentDecl()),
    PrevDC(SemaRef.getCurClangDeclContext()),
    PrevClangScope(SemaRef.getCurClangScope()),
    PrevBlueScope(SemaRef.getCurrentScope())
  { }

  ~ElabBalanceChecker() {
    bool didError = false;
    if (PrevDecl != SemaRef.getCurrentDecl()) {
      didError = true;
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping current gold declaration\n";
      llvm::outs() << "=====================================================\n";
      SemaRef.getCurrentDecl()->dump();
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping previous gold declaration\n";
      llvm::outs() << "=====================================================\n";
      PrevDecl->dump();
      llvm::outs() << "=====================================================\n";
    }

    if (PrevClangScope != SemaRef.getCurClangScope()) {
      didError = true;
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping current clang scope\n";
      clang::Scope *CurScope = SemaRef.getCurClangScope();
      while(CurScope) {
        llvm::outs() << "=====================================================\n";
        CurScope->dump();
        if (CurScope->getEntity()) {
          llvm::outs() << "Dumping Entity = ";
          if (auto Ent = dyn_cast<clang::Decl>(CurScope->getEntity())) {
            Ent->dump();
          } else {
            llvm::outs() << "Entity isn't a declaration\n";
          }
        }
        CurScope = CurScope->getParent();
      }

      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping expected clang scope\n";
      CurScope = PrevClangScope;
      while(CurScope) {
        llvm::outs() << "=====================================================\n";
        CurScope->dump();
        if (CurScope->getEntity()) {
          llvm::outs() << "Dumping Entity = ";
          if (auto Ent = dyn_cast<clang::Decl>(CurScope->getEntity())) {
            Ent->dump();
          } else {
            llvm::outs() << "Entity isn't a declaration\n";
          }
        }
        CurScope = CurScope->getParent();
      }
      llvm::outs() << "=====================================================\n";
    }

    if (PrevDC != SemaRef.getCurClangDeclContext()) {
      didError = true;
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping current DeclContext\n";
      llvm::outs() << "=====================================================\n";
      SemaRef.getCurClangDeclContext()->dumpDeclContext();
      if (auto TempDcl = dyn_cast<clang::Decl>(SemaRef.getCurClangDeclContext())) {
        TempDcl->dump();
      } else {
        llvm::outs() << "Current decl context isn't a clang::decl.\n";
      }
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping expected DeclContext\n";
      llvm::outs() << "=====================================================\n";
      PrevDC->dumpDeclContext();
      if (auto TempDcl = dyn_cast<clang::Decl>(PrevDC)) {
        TempDcl->dump();
      } else {
        llvm::outs() << "Current decl context isn't a clang::decl.\n";
      }
      llvm::outs() << "=====================================================\n";
    }
    if (PrevBlueScope != SemaRef.getCurrentScope()) {
      didError = true;
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping current gold scope\n";
      llvm::outs() << "=====================================================\n";
      SemaRef.getCurrentScope()->dump();
      llvm::outs() << "=====================================================\n";
      llvm::outs() << "Dumping previous gold scope\n";
      llvm::outs() << "=====================================================\n";
      PrevBlueScope->dump();
      llvm::outs() << "=====================================================\n";
    }
    assert(!didError && "Pre/post/invariant condition violation");
  }
};
} // end namespace blue

#endif
