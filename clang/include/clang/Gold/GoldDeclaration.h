//=== GoldDeclaration.h ---------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This class contains a what we suspect is a declaration (it might not be).
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_DECLARATION_H
#define CLANG_GOLD_DECLARATION_H

#include "clang/Gold/GoldDeclarator.h"
namespace clang {
class TemplateParameterList;
}


namespace gold {
/// This is the expected kind of declaration base on only the given AST and any
/// atom names we find.
enum UnevaluatedDeclKind {
  UDK_None,                 // This is used to indicate if we haven't determined
                            // a possible lookup kind yet.

  UDK_File,                 // We are a file declaration

  UDK_Class,                // The declaration is a class, either
                            // a declaration or definition.

  UDK_Union,                // The declaration is a union, either
                            // a declaration or definition.

  UDK_Enum,                 // The declaration is an Enum, either
                            // a declaration or definition.

  // Until phase 2 we can't be sure this is a variable.
  // UDK_Variable,             // This is a variable that occurs within global
  //                           // namespace, a namespace, or as the static
  //                           // member of a class (with the static attribute).

  UDK_Namespace,            // Defines a nemaspace
  UDK_NamespaceAlias,       // Declares a namespace alias

  UDK_TemplateAlias,        // Suspected template alias. Currently this MUST
                            // have a type that evaluates to : type
                            // - cannot be a template specialization

  UDK_VarTemplateDecl,      // The only time we know we are a variable template
                            // is when we have specialization arguments.
                            // The we have to assume that we are a variable
                            // template specialization, because we can't have
                            // a template alias specialization.

  UDK_TypeAlias,            // The only time we can have a type alias is iff
                            // there is a : type, other then that we will
                            // never know.

  UDK_Parameter,            // A function parameter, only occurs within a
                            // SK_Parameter scope

  UDK_TemplateParam,        // Any template parameter as their type doesn't
                            // matter until they are elaborated, and used.

  UDK_TemplateTemplateParam,// A template template parameter.

  // The only time we ever know for sure that this is a field is during phase 2.
  // UDK_Field,                // Field associated with a class.

  UDK_EnumConstant,         // A field declared within an enum.

  UDK_MemberFunction,       // Only applies to member functions declared within
                            // The body of a class.
                            // In order to figure out if this is a member or just
                            // the definition of something defined within a namespace
                            // it would require additional lookup and evaluation.

  UDK_Constructor,          // A function with the name constructor declared
                            // within the body of a class

  UDK_Destructor,           // A function with the name Destructor declared within
                            // the body of a class.

  UDK_Function,             // Declares a function

  UDK_ConversionOperator,   // A conversion operator within the body of a class.

  UDK_MemberOperator,       // The declaration of a member operator overload.

  UDK_LiteralOperator,      // User defined literal operator declaration.

  UDK_OperatorOverload,     // An operator overload that's not within a class.

  UDK_PossibleConstructor,  // This is a function with a nested name specifier
                            // and the name Constructor
  UDK_PossibleDestructor,   // This is a function with a nested name specifier
                            // and the name destructor
  UDK_PossibleMemberOperator, // This is an operator overload with a
                              // nested name specifier.
  UDK_PossibleConversionOperator, // This is conversion operator overload with a
                                  // nested name specifier.

  UDK_VarTemplateOrTemplateAlias, // This requires that we must specifically
                                  // deduce the evaluated type before we could
                                  // evaluate this as either an alias or a variable.
                                  // but minimally we would know this is a template

  UDK_DeductionOnlyVariable,  // In order to deduce what type of declartion this
                              // is it requires non-speculative evaluation of it's
                              // assigned epxression.
                              // This could be one of the following:
                              //    - TemplateAlias
                              //    - NamespaceAlias
                              //    - VarTemplate
                              //    - VarDecl
                              //    - TypeAlias
                              //    - FieldDecl
                              //    - Not a declaration, This fully depends
                              //      on where it's used.

  UDK_UsingDirective,  // A using directive declaration.
  UDK_CatchVariable,   // the variable for a catch statement.
  // Things we don't have syntax for yet.
  /*
  UDK_UsingDecl,
  UDK_UsingPackDecl,
  UDK_FriendClass,
  UDK_FriendUnion,
  UDK_FriendFunction,
  UDK_FriendName, // Since we don't have syntax for this yet I'm relying on how
                  // it's specified in C++.
  UDK_UsingShadowDecl, // non-constructor. using ::foo() also works in namespace.
  UDK_UsingShadowConstructor, // using Shadowed constrcutor, does not work in
                              // namespace
  UDK_Concept // Not sure if we plan on using concepts or if there's a chance
              // that their use could be at all similar to other declarations.
  */
};

llvm::StringRef unevaluatedDeclKindToStr(UnevaluatedDeclKind UDK);

enum class Phase : std::size_t
{
  Unprocessed,
  Identification,
  Typing,
  Initialization
};

llvm::StringRef phaseToStr(Phase p);

struct NNSDeclaratorInfo {
  NestedNameSpecifierDeclarator *Name = nullptr;
  TemplateParamsDeclarator *Template = nullptr;
  SpecializationDeclarator *SpecializationArgs = nullptr;
};

enum InitKind {
  IK_None,
  IK_Exlaim,
  IK_Equals
};

/// A declaration is stores information about the declaration of an
/// identifier. It binds together the declaring operator, the declarator,
/// the definition, and the some corresponding C++ declaration.
class Declaration {
public:
  /// Use to create the initial file/global namespace.
  Declaration(const Syntax *File)
    : Cxt(nullptr), Op(nullptr), Decl(nullptr), Init(File),
    SuspectedKind(UDK_File)
  { }

  /// Creates a declaration.
  Declaration(Declaration *Cxt, const Syntax *Op, Declarator *Decl,
              const Syntax *Init,
              UnevaluatedDeclKind SuspectedDeclKind = UDK_None)
    : Cxt(Cxt), Op(Op), Decl(Decl), Init(Init),
      SuspectedKind(SuspectedDeclKind)
  { }

  ~Declaration();

  /// The enclosing declaration.
  Declaration *getOwner() const {
    return Cxt;
  }

  clang::SourceLocation getEndOfDecl() const;

  /// True if this declares a variable.
  bool declaresVariable() const;

  /// Declares variable with in body initialization.
  bool declaresInitializedVariable() const;

  /// Any template has default parameters. Either class or function.
  bool templateHasDefaultParameters() const;

  /// Checks to see if this is a forward declaration or not.
  bool declaresForwardRecordDecl() const;

  /// Checks if the type declaration is declaring a record.
  bool declaresTagDef() const;

  /// Checks to see if we declare a union or not.
  bool declaresUnion() const;

  /// Checks if the type declaration is declaring a namespace.
  bool declaresNamespace() const;

  /// Checks to see if we created a namespace with a nested name.
  bool declaresNamespaceWithNestedName() const;

  /// Checks if the declarator declares a template type or not.
  bool declaresTemplateType() const;

  /// True if this declares a function.
  bool declaresFunction() const;

  /// Check if we use = and we are a function
  bool declaresFunctionWithImplicitReturn() const;

  /// Check if we have a function who has = 0 assignment.
  bool declaresPossiblePureVirtualFunction() const;

  /// Checks to see if the function is defined as = default
  bool declaresDefaultedFunction() const;

  /// Checks to see if the function is defined as = delete
  bool declaresDeletedFunction() const;

  /// Returns true if the CXX type is a FieldDecl.
  /// This is true when declares variable is also true some times.
  bool declaresMemberVariable() const;

  /// Returns true if the declaration is inside of a class, and it's a member
  /// function.
  bool declaresMemberFunction() const;

  /// Returns true if the declaration is inside of a class, and it's a
  /// constructor
  bool declaresConstructor() const;

  /// Returns True if the given function is a destructor, and it's declared
  /// within a class.
  bool declaresDestructor() const;

  /// Checks to see if SuspectedKind == UDK_ConversionOperator
  bool declaresConversionOperator() const;

  /// Checks to see if SuspectedKind == UDK_LiteralOperator
  bool declaresUserDefinedLiteral() const;

  /// Checks to see if SuspectedKind == UDK_CatchVariable
  bool declaresCatchVariable() const { return SuspectedKind == UDK_CatchVariable; }

  /// True if this declares a template.
  bool declaresFunctionTemplate() const;

  /// This checks to see if the declaration is a possible operator overload.
  /// This COULD be wrong, all we are checking for is that we have both a
  /// fused identifier name, OpId, and there is a function declarator.
  bool declaresOperatorOverload() const;

  /// This is true iff isa<TypeAliasDecl>(Cxx)
  bool declaresTypeAlias() const;

  /// Checks if a declaration is static.
  bool declIsStatic() const;

  /// Checks available attributes to see if the constexpr attribute is within the
  /// list of attributes.
  bool declIsConstexpr() const;

  /// Checks if a decl is a declaration and it doesn't have a body.
  bool declaresFunctionDecl() const;

  /// checks if a function has a body.
  bool declaresFunctionDef() const;

  /// Checks is a declaration is a variadic function.
  bool isVariadic() const;

  /// Checks if this is a using directive declaration.
  bool declaresUsingDirective() const;

  bool hasNestedNameSpecifier() const;

  llvm::StringRef getSuspectedKindStr() const;

  clang::DeclContext *getOwningDeclContext() const { return DeclaringContext; }

  template<typename T>
  bool defines() const {
    return Cxx && clang::isa<T>(Cxx);
  }

  template<typename T>
  T* getAs() const {
    if (!Cxx)
      return nullptr;
    return clang::dyn_cast<T>(Cxx);
  }
  /// Checks if the current Cxx decl is a static member variable of a class.
  bool declaresInlineInitializedStaticVarDecl() const;

  /// This returns the kind that this most likely evaluates to.
  UnevaluatedDeclKind getKind() const { return SuspectedKind; }

  /// The identifier of the declaration, if any.
  clang::IdentifierInfo *getId() const { return Id; }

  clang::IdentifierInfo *getLiteralId() const { return Id; }

  const IdentifierDeclarator *getIdDeclarator() const;
  IdentifierDeclarator *getIdDeclarator();

  // const Declarator *getFirstDeclarator(DeclaratorKind DK) const;
  // Declarator *getFirstDeclarator(DeclaratorKind DK);

  /// The corresponding C++ declaration as a context.
  clang::DeclContext *getCxxContext() const;

  /// Set the previous declaration in the redeclaration chain.
  void setPreviousDecl(Declaration *Prev);

  /// This function checks to see if the current scope was declared within
  // the scope of another class body.
  bool isDeclaredWithinClass() const;

  /// Dump the current implementation to a stream.
  void dump(llvm::raw_ostream &os = llvm::outs()) const;

  /// The owning context.
  Declaration *Cxt = nullptr;

  /// The top-level operator that forms the declaration or definition.
  const Syntax *Op = nullptr;

  /// The declarator of the declaration.
  Declarator *Decl = nullptr;

  /// The initializer or definition.
  const Syntax *Init = nullptr;

  /// This is the deduced kind based on syntax only.
  UnevaluatedDeclKind SuspectedKind = UDK_None;

  /// The list of members associated with this declaration.
  Scope *SavedScope = nullptr;

  /// The identifier for the declaration.
  clang::IdentifierInfo *Id = nullptr;

  /// User-Defined literal suffix identifier.
  clang::IdentifierInfo *UDLSuffixId = nullptr;

  /// This name indicates if this declaration declares an operator name, and if
  /// the operator name is a known valid operator. This operator name is only
  /// used by C++ iff there is a function declarator. This is the actual name
  /// used by the clang::Decl in CXX. This is to be consistent with C++.
  const OpInfoBase* OpInfo = nullptr;


  /// The corresponding C++ declaration.
  clang::Decl* Cxx = nullptr;

  /// ====================================================================== ///
  /// Below are declarations pertaining to the redeclaration chain, which
  /// is a circularly linked list containing declarations with the same
  /// signature.

  /// The first declaration in the redeclaration chain.
  Declaration *First = this;

  /// The next decl in the redeclaration chain.
  Declaration *Next = this;

  /// ====================================================================== ///

  /// Decl phase completed. This is used to paint the declarations and
  /// avoid re-visitation during lookup/elaboration. This has a value from 0-3.
  /// 0 is unprocessed (the default value), 1 is identified, 2 is the declaration
  /// is elaborated and 3 is the definition is complete.
  Phase CurrentPhase = Phase::Unprocessed;

  /// Flag to prevent cyclic elaboration from happening.
  /// This is really only useful during phase 1 & 2
  bool IsElaborating = false;

  /// This information is to aid with early elaboration. This allows the
  /// elabrotor to restore the state in which something was declared.
  ///
  /// This is the current clang scope that the clang declaration is part of.
  clang::Scope *ClangDeclaringScope = nullptr;

  /// This is the scope that this declaration is a member of.
  /// This is also the parent scope to the SavedScope, if set.
  gold::Scope *ScopeForDecl = nullptr;

  /// This is the gold DeclContext for a declaration.
  Declaration* ParentDecl = nullptr;

  /// This is the current DeclContext when the declaration was encountered.
  clang::DeclContext *DeclaringContext = nullptr;

  /// This is used to handle late elaboration of exception specification for
  /// member functions.
  const CallSyntax *ES_Call = nullptr;
  const AtomSyntax *ES_Name = nullptr;

  /// Declarator inspecting variables.
  GlobalNameSpecifierDeclarator *GlobalNsSpecifier = nullptr;
  llvm::SmallVector<NNSDeclaratorInfo, 4> NNSInfo;
  IdentifierDeclarator *IdDcl = nullptr;

  TemplateParamsDeclarator *Template = nullptr;
  SpecializationDeclarator *SpecializationArgs = nullptr;

  FunctionDeclarator *FunctionDcl = nullptr;
  TypeDeclarator *TypeDcl = nullptr;

  llvm::SmallVector<clang::TemplateParameterList *, 4> TemplateParamStorage;

  /// ====================================================================== ///
  /// Additional identifing information about the current declaration.

  /// This implies that the current declaration is only a declaration, and
  /// not the definition for a declaration.
  bool IsDeclOnly = false;

  /// This is used to indicate if the declaration was initialized with = 0
  // bool HasEqualZero = false;

  /// This is used to indicate if a function declaration has an = default;
  // bool HasEqualDefault = false;

  /// This is used to indicate if a function has a body of = delete
  // bool HasEqualDelete = false;

  /// Identifies if the declaration created uses the equalsOperator
  InitKind InitOpUsed = IK_None;

  clang::CXXScopeSpec ScopeSpec;
  bool IsRedeclaration = false;
  bool NeedToBeElaboratedByClangBeforeUse = false;

  bool IsVariadic = false;

  /// An ellipsis for a parameter pack.
  clang::SourceLocation EllipsisLoc;
};

Phase phaseOf(Declaration *D);

} // end namespace gold

#endif
