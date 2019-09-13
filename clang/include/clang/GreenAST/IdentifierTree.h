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
#include "clang/GreenAST/SyntaxContext.h"
#include "clang/GreenAST/SyntaxVisitor.h"
#include "llvm/ADT/SmallVector.h"

#include <functional>

namespace clang {
class Preprocessor;
} // namespace clang

namespace usyntax {

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

  clang::Preprocessor &PP;

public:
  IdentifierTreeAnalyzer(SyntaxContext &Context, clang::Preprocessor &PP)
    : Context(Context), PP(PP)
    {}

  void Visit(const Syntax *S);

  void VisitSyntaxConstInt(const SyntaxConstInt *S);
  void VisitSyntaxIdent(const SyntaxIdent *S);
};

class IdentifierTreeTraverser
  : public clang::ASTNodeTraverser<IdentifierTreeTraverser, IdentifierTreeAnalyzer> {
  IdentifierTreeAnalyzer Analyzer;

  SyntaxContext &Context;
public:
  IdentifierTreeTraverser(SyntaxContext &Context, clang::Preprocessor &PP)
    : Analyzer(Context, PP), Context(Context)
    {}

  IdentifierTreeAnalyzer &doGetNodeDelegate() { return Analyzer; }
};

} // namespace usyntax

#endif
