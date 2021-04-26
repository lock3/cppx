#ifndef CLANG_GOLD_DECLARATOR_BUILDER_H
#define CLANG_GOLD_DECLARATOR_BUILDER_H

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Gold/GoldDeclarator.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSyntax.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldSyntaxVisitor.h"

#include <stack>

namespace gold {
class DeclarationBuilder;
class Sema;
class SyntaxContext;

class DeclaratorBuilder :
  public ConstSyntaxVisitor<DeclaratorBuilder> {
  friend class DeclarationBuilder; 

  SyntaxContext &Context;
  Sema &SemaRef;
  DeclarationBuilder &Owner;

  // The declarator we build up, and the actual output of this function object.
  Declarator *Result = nullptr;

  // The current working declarator chunk.
  Declarator *Cur = nullptr;

  // The most recently created declarator chunk.
  Declarator *End = nullptr;

  // The main identifier for the declaration.
  Declarator *Name = nullptr;
public:
  DeclaratorBuilder(SyntaxContext &Context, Sema &SemaRef,
                    DeclarationBuilder &Owner)
    : Context(Context), SemaRef(SemaRef), Owner(Owner) {}

  Declarator *operator()(const Syntax *S);

  // Visitor Functions
  void VisitSyntax(const Syntax *S);
  void VisitGoldCallSyntax(const CallSyntax *S);
  void VisitGoldElemSyntax(const ElemSyntax *S);
  void VisitGoldMacroSyntax(const MacroSyntax *S);
  void VisitGoldListSyntax(const ListSyntax *S);
  void VisitGoldAtomSyntax(const AtomSyntax *S);
  void VisitGoldLiteralSyntax(const LiteralSyntax *S);
  void VisitGoldErrorSyntax(const ErrorSyntax *S);

private:
  void push(Declarator *D) {
    if (!Result) {
      Name = End = Cur = Result = D;
    } else {
      End->Next = D;
      Cur = End;
      End = End->Next;
    }
  }

  // Methods
  // Special requirements.
  // - Can have scoped name declarations.
  // - Is a decl if it has an assignment, :, or !
  // - Can declare classes, enum, and unions,
  // - Can declare namespace, namespace aliases and template, template variables
  Declarator *handleNamespaceScope(const Syntax *S);

  // Special requirements
  // Must have a :
  // No function declarations
  // No namespace declarations alias or otherwise.
  // No declarations tags of any kind.
  // No template parameters/specializations for the parameter name
  // Cannot have a Nested name specifier for the name of the variable.
  Declarator *handleParameterScope(const Syntax *S);

  // Special requirements.
  // Same restrictions as handleParameterScope with the resulting
  // UnprocessedDeclKind not being different.
  Declarator *handleTemplateScope(const Syntax *S);

  // Special requirements
  // This is for a function body.
  // That means we can't have nested name specifiers on a declaration.
  //    - it implies that it's not a function declarations.
  // Any function declared within the body of a function must have a : operator
  // Cannot declare namespace declarations,
  // can declare namespace aliases.
  Declarator *handleFunctionScope(const Syntax *S);

  // Same as handleFunctionScope
  Declarator *handleBlockScope(const Syntax *S);

  // Special requirements
  // Names declared within the body of a class, or union cannot have a
  // nested name specifier(yet, they may need it for base class using access?)
  // Cannot declare a new namespace, aliases are alright.
  Declarator *handleClassScope(const Syntax *S);

  // Same as block scope
  // The declaration cannot be of a type, namespace, or tag
  Declarator *handleControlScope(const Syntax *S);

  // Special requirements
  // This allows the atom only identifier
  // No declarations other then name = value, or name
  Declarator *handleEnumScope(const Syntax *S);


  Declarator *handleCatchScope(const Syntax *S);

  /// Attempts to reach the end of a declarator chain an append a new
  /// declarator, specifically the type declarator, iff we are a conversion
  /// operator declaration.
  Declarator *appendConversionType(Declarator *CurDcl);

  /// This must be a call Either "operator'='", "operator':'", or "operator'in'"
  /// Operator in is a special case for us because it's just a ranged for loop.
  Declarator *makeDeclarator(const Syntax *S);
  Declarator *dispatchAndCreateDeclarator(const Syntax *S);
  Declarator *makeTopLevelDeclarator(const Syntax *S, Declarator *Next);
  Declarator *handleLHSElement(const CallSyntax *S, Declarator *Next);
  Declarator *handleLHSCaret(const CallSyntax *S, Declarator *Next);
  Declarator *handleSingleCaret(const CallSyntax *S, Declarator *Next);
  Declarator *buildTemplateFunctionOrNameDeclarator(const Syntax *S,
                                                    Declarator *Next);

  Declarator *buildNestedNameSpec(const CallSyntax *S, Declarator *Next);
  Declarator *buildNestedTemplate(const ElemSyntax *S, Declarator *Next);
  Declarator *buildNestedOrRegularName(const Syntax *S, Declarator *Next);
  Declarator *buildNestedName(const Syntax *S, Declarator *Next);
  Declarator *buildNestedTemplateSpecializationOrName(const Syntax *S,
                                                      Declarator *Next);

  void buildName(const Syntax *S);
  void buildTemplate(const ElemSyntax *S);
  void buildTemplateOrName(const Syntax *S);


  /// This is used to peek into a [] and verify that it is a declaration.
  /// The decision is based on if the contains a : because that's
  /// what's required for template parameter declarations.
  /// This is ONLY used for the main Name declaration.
  Declarator *mainElementTemplateOrSpecialization(const ElemSyntax *Elem,
                                                  Declarator *Next);

  // Internal processing functions
  UnknownDeclarator *handleUnknownADeclSyntax(const Syntax *S, Declarator *Next);
  void buildError(const ErrorSyntax *S);
  void buildGlobalNameSpecifier(const CallSyntax *S);
  void buildNestedNameSpecifier(const AtomSyntax *S);
  void buildIdentifier(const AtomSyntax *S);
  void buildFunction(const CallSyntax *S);
  void buildFunction(const ListSyntax *S);
  void buildType(const Syntax *S);
  void buildArray(const Syntax *S);
  void buildPointer(const Syntax *S);
  void buildTemplateParams(const ElemSyntax *S);
  void buildTemplateParams(const ListSyntax *S);
  ImplicitEmptyTemplateParamsDeclarator *
  handleImplicitTemplateParams(const ElemSyntax *Owner, Declarator *Next);

  void buildSpecialization(const ElemSyntax *SpecializationOwner);
  void buildPartialSpecialization(const ListSyntax *SpecializationOwner);
  void buildPartialSpecialization(const ElemSyntax *SpecializationOwner);
  void buildUsingDirectiveDeclarator(const MacroSyntax *S);

public:
  // A map maintaining an integer weight for each node. Allows us to discern
  // where we are visiting in the tree, relative to the root.
  struct LabelMapTy : public llvm::DenseMap<const Syntax *, unsigned> {
    // The weight we assigned to the root node of this syntax tree.
    unsigned RootLabel = 0;
  };

  using ParentMapTy = llvm::DenseMap<const Syntax *, const Syntax *>;
private:
  LabelMapTy NodeLabels;
  ParentMapTy NodeParents;

  // Assign an integer label/weight to each node in a CST.
  class NodeLabeler :
    public ConstSyntaxVisitor<NodeLabeler> {
    Sema &SemaRef;
    LabelMapTy &NodeLabels;
    ParentMapTy &NodeParents;
    unsigned Label = 0;
  public:
    NodeLabeler(Sema &SemaRef, LabelMapTy &NodeLabels, ParentMapTy &NodeParents)
      : SemaRef(SemaRef), NodeLabels(NodeLabels), NodeParents(NodeParents)
      {}

    void operator()(const Syntax *S);
    void VisitGoldCallSyntax(const CallSyntax *S);
    void VisitGoldElemSyntax(const ElemSyntax *S);
    void VisitGoldListSyntax(const ListSyntax *S);
    void VisitGoldAtomSyntax(const AtomSyntax *S);

  private:
    bool insertChild(const Syntax *S, unsigned Label);
    bool insertChild(const Syntax *C, const Syntax *P, unsigned Label);
    bool insertParent(const Syntax *S, unsigned Label);
    struct ParentRAII {
      ParentRAII(const Syntax *S, std::stack<const Syntax *> &InteriorNodes,
                 ParentMapTy &NodeParents);
      ~ParentRAII();

      std::stack<const Syntax *> &InteriorNodes;
      ParentMapTy &NodeParents;
    };

    // When labeling an interior node, it gets pushed here
    // so we can keep track of leaf nodes' parents.
    std::stack<const Syntax *> InteriorNodes;
  };

  const Syntax *getParent(const Syntax *S);

  // Members
  std::string OriginalNameStorage;
  llvm::StringRef OriginalName;
  clang::IdentifierInfo *Id = nullptr;
  const OpInfoBase *OpInfo = nullptr;

  // True when we can be sure we are creating a template specialization
  // with explicit parameters, as in `id[params][specialization_parameters]`
  bool ExplicitTemplateSpecialization = false;

  // True when we are treating a template specialization as a
  // nested name specifier.
  bool NestedTemplateName = false;
};


} // end namespace gold

#endif
