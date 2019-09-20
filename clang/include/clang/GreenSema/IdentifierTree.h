#ifndef CLANG_GREEN_IDENTIFIER_TREE_H
#define CLANG_GREEN_IDENTIFIER_TREE_H

//===----------------------------------------------------------------------===//
//
// This file implements a traverser of Syntax Trees that maintains some context
// about each node's depth. It is used to associate an identifier with a Clang
// construct.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTNodeTraverser.h"
#include "clang/AST/DeclContextInternals.h"
#include "llvm/ADT/SmallVector.h"

#include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenAST/SyntaxVisitor.h"

#include <functional>
#include <string>
#include <unordered_map>

namespace clang {
class ParmVarDecl;

class Preprocessor;
class Sema;
} // namespace clang

namespace usyntax {

class GreenSema;

class IdentifierTreeStructure {
  /// Pending[i] is an action to analyze an entity at level i.
  llvm::SmallVector<std::function<void(bool isLastChild)>, 32> Pending;

  /// Indicates if we're handling the first child after entering a new depth.
  bool FirstChild = true;

  /// Indicates whether we're at the top level.
  bool TopLevel = true;

  // The current depth in the tree of this traversal.
  std::size_t Depth = 0;

protected:
  std::size_t getDepth() const {
    return Depth;
  }

public:
  template <typename Fn> void AddChild(Fn DoAddChild) {
    if (TopLevel) {
      TopLevel = false;

      DoAddChild();
      while (!Pending.empty()) {
        Pending.back()(true);
        Pending.pop_back();
      }

      TopLevel = true;
      return;
    }

    auto ClassifyIdentifiers = [this, DoAddChild](bool IsLastChild) {
      FirstChild = true;
      Depth = Pending.size();

      DoAddChild();

      while (Depth < Pending.size()) {
        Pending.back()(true);
        this->Pending.pop_back();
      }
    };

    if (FirstChild) {
      Pending.push_back(std::move(ClassifyIdentifiers));
    } else {
      Pending.back()(false);
      Pending.back() = std::move(ClassifyIdentifiers);
    }
  }
};

class IdentifierTreeAnalyzer
    :  public IdentifierTreeStructure,
       public ConstSyntaxVisitor<IdentifierTreeAnalyzer> {
  SyntaxContext &Context;

  // Clang preprocessor. Gives access to Clang's identifier table.
  clang::Preprocessor &PP;

  // The parsed type of the identifier we are analyzing.
  // Defaults to auto.
  clang::QualType CurrentType;

  // A reference to the GreenSema object.
  GreenSema &GSema;

  // A reference to Clang's Sema object.
  clang::Sema &ClangSema;

  std::vector<clang::ParmVarDecl *> CreatedParameters;
public:
  IdentifierTreeAnalyzer(SyntaxContext &Context, clang::Preprocessor &PP,
                         GreenSema &GSema, clang::Sema &ClangSema)
    : Context(Context), PP(PP), GSema(GSema), ClangSema(ClangSema) {
    CurrentType = Context.ClangContext.getAutoDeductType();
    VarContext = Normal;
  }

  void Visit(const Syntax *S);

  void VisitSyntaxMacro(const SyntaxMacro *S);
  void VisitSyntaxCall(const SyntaxCall *S);
  void VisitSyntaxIdent(const SyntaxIdent *S);

private:
  bool HandleOperatorEquals(const SyntaxMacro *S);

  bool HandlePrefixColon(const SyntaxMacro *S);

private:
  /// Enumerations

  // There are different contexts in which we can create a variable: we might
  // be creating a regular VarDecl, or a ParmVarDecl while analyzing a function
  // declaration. In some cases, we might not want to create a variable at all.
  enum VariableCreationContext {
    // This is a normal context, create VarDecls.
    Normal,

    // We're analyzing a function prototype, create ParmVarDecls.
    FunctionProto,

    // Don't create a variable: we're just analyzing something.
    // Used in order to determine the parameter types in a function signature.
    Analysis
  } VarContext;

  /// Maps

  const std::unordered_map<std::string, clang::QualType> BuiltInTypes = {
    {"int", Context.ClangContext.IntTy},
    {"unsigned", Context.ClangContext.UnsignedIntTy},
    {"float", Context.ClangContext.FloatTy},
    {"double", Context.ClangContext.DoubleTy},
  };
};

class IdentifierTreeTraverser
  : public clang::ASTNodeTraverser<IdentifierTreeTraverser, IdentifierTreeAnalyzer> {
  IdentifierTreeAnalyzer Analyzer;

public:
  IdentifierTreeTraverser(SyntaxContext &Context, clang::Preprocessor &PP,
                          GreenSema &GSema, clang::Sema &ClangSema)
    : Analyzer(Context, PP, GSema, ClangSema)
    {}

  IdentifierTreeAnalyzer &doGetNodeDelegate() { return Analyzer; }
};

} // namespace usyntax

#endif
