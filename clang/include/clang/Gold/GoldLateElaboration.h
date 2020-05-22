//=== Gold:ateElaboration.h - Elaboration for Gold Nodes ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Elaborator interface, which creates clang::Decl*
//  nodes out of Gold Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_LATE_ELABORATION_H
#define CLANG_GOLD_LATE_ELABORATION_H

#include "clang/AST/Type.h"
#include "clang/AST/Decl.h"

#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"

namespace clang {
class Stmt;
class CXXRecordDecl;
class Decl;
} // namespace clang

namespace gold {

class Declarator;
class Declaration;
class Sema;

class ElaboratingClass;

/// This is the base class that's used for elaborating the body of a class
/// in an out of order manner without causing errors.
/// This is to mimic the behaviors from LateParsedDeclaration
/// Declared in file clang/include/clang/Parse/Parser.h:1191
class LateElaboratedDecl {
public:
  virtual ~LateElaboratedDecl();

  virtual void ElaborateMethodDeclarations();
  virtual void ElaborateMemberInitializers();
  virtual void ElaborateMethodDefs();
  virtual void ElaborateAttributes(); 
};


/// Inner node of the LateElaboratedDecl tree that parses all its members
/// recursively.
class LateElaboratedClass : public LateElaboratedDecl {
public:
  LateElaboratedClass(Sema &S, SyntaxContext &Ctxt, ElaboratingClass *C);
  ~LateElaboratedClass() override;

  void ElaborateMemberInitializers() override;
  void ElaborateMethodDeclarations() override;
  void ElaborateMethodDefs() override;
  void ElaborateAttributes() override;

private:
  Sema &SemaRef;
  SyntaxContext &Context;
  ElaboratingClass *Class;
};

/// Contains the member variable that depend on other types within the class
/// body that may have not been processed yet. 
struct LateElaboratedAttributeDecl : public LateElaboratedDecl {
  Sema &SemaRef;
  SyntaxContext &Context;
  Declaration *Decl;
  clang::IdentifierInfo &Id;
  clang::SourceLocation NameLoc;
  // TODO: Not sure what this is for off hand.
  // IdentifierInfo *MacroII = nullptr; 
  llvm::SmallVector<Declaration *, 2> Decls;

  explicit LateElaboratedAttributeDecl(Sema &S, SyntaxContext &Ctxt,
                                   clang::IdentifierInfo &Name,
                                   clang::SourceLocation Loc)
    : SemaRef(S), Context(Ctxt), Id(Name), NameLoc(Loc) { }

  void ElaborateAttributes() override;

  /// This is because a single declarator could technically declare multiple
  /// variables. However this hasn't been added yet.
  void addDecl(Declaration *D) { Decls.push_back(D); }
};

/// Contains a member function definition that needs to be completed at the
/// end of the class after parsing all other member declarations.
struct LateElaboratedMethodDef : public LateElaboratedDecl {
  Sema &SemaRef;
  SyntaxContext &Context;
  Declaration *D;

  /// Whether this member function had an associated template scope. When true,
  /// D is a template declaration, otherwise, it is a member function
  /// declaration.
  bool TemplateScope;

  explicit LateElaboratedMethodDef(Sema &S, SyntaxContext &Ctxt,
      Declaration *MD)
    : SemaRef(S), Context(Ctxt), D(MD), TemplateScope(false) { }

  void ElaborateMethodDefs() override;
};

/// LateElaboratedDefaultArgument - Keeps track of a parameter that may
/// have a default argument that cannot be parsed yet because it
/// occurs within a member function declaration inside the class
/// (C++ [class.mem]p2).
struct LateElaboratedDefaultArgument {
  explicit LateElaboratedDefaultArgument(Declaration *P)
    : Param(P) { }

  /// Param - The parameter declaration for this parameter.
  Declaration *Param;
};

/// LateParsedMethodDeclaration - A method declaration inside a class that
/// contains at least one entity whose parsing needs to be delayed
/// until the class itself is completely-defined, such as a default
/// argument (C++ [class.mem]p2).
struct LateElaboratedMethodDeclaration : public LateElaboratedDecl {
  explicit LateElaboratedMethodDeclaration(Sema &S, SyntaxContext &Ctxt,
      Declaration *MD)
    : SemaRef(S), Context(Ctxt), D(MD), TemplateScope(false) { }

  void ElaborateMethodDeclarations() override;

  Sema &SemaRef;
  SyntaxContext &Context;

  /// Method - The method declaration.
  Declaration *D;

  /// Whether this member function had an associated template
  /// scope. When true, D is a template declaration.
  /// otherwise, it is a member function declaration.
  bool TemplateScope;

  /// DefaultArgs - Contains the parameters of the function and
  /// their default arguments. At least one of the parameters will
  /// have a default argument, but all of the parameters of the
  /// method will be stored so that they can be reintroduced into
  /// scope at the appropriate times.
  clang::SmallVector<LateElaboratedDefaultArgument, 8> DefaultArgs;
};

/// LateElaborateMemberInitializer - An initializer for a non-static class data
/// member whose parsing must to be delayed until the class is completely
/// defined (C++11 [class.mem]p2).
struct LateElaborateMemberInitializer : public LateElaboratedDecl {
  LateElaborateMemberInitializer(Sema &S, SyntaxContext &Ctxt,
      Declaration *FD)
    : SemaRef(S), Context(Ctxt), D(FD) { }

  void ElaborateMemberInitializers() override;

  Sema &SemaRef;
  SyntaxContext &Context;

  /// Field - The field declaration.
  Declaration *D;
};

/// LateElaboratedDecls - During parsing of a top (non-nested)
/// C++ class, its method declarations that contain parts that won't be
/// parsed until after the definition is completed (C++ [class.mem]p2),
/// the method declarations and possibly attached inline definitions are stored
/// here for final elaboration once the end of the class has actuall been reached.
/// This is taken from clang/include/clang/Parse/Parser.h:1370
using LateElaboratedDecls = llvm::SmallVector<LateElaboratedDecl*, 2>;

/// This is a representation of a class being elaborated. Including any member
/// functions, declarations, or definitions, that are incomplete after the initial
/// phases of elaboration. All instances of this class will be processed once
/// the top level classes level 1/3 elaboration has been completed, depending
/// on the type of declaration being processed.
///
/// This class also holds information about classes that are elaborated late
/// meaning they are created after the top-level class is completed.
///
/// This class mimics some of the behaviors associated with the ParsingClass
/// located in file clang/include/clang/Parse/Parser.h:1375
struct ElaboratingClass {
  ElaboratingClass(Declaration *TagOrTemplate, bool TopLevelClass);

  /// This indicates if this class is the top level class being elaborated
  /// or not.
  bool IsTopLevelClass : 1;

  /// This indicates if we were inside of template. Which can then be adjusted
  /// and renetered before final elaboration is complete.
  bool TemplateScope : 1;

  /// The decl or template that's being elaborated.
  Declaration *TagOrTemplate = nullptr;

  /// Incomplete/partially/unelaborated delcarations that need to be finished,
  /// after the body of the class has been completed.
  LateElaboratedDecls LateElaborations;
};

} // namespace gold
#endif