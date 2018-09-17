//===--- SemaInject.cpp - Semantic Analysis for Injection -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic rules for the injection of declarations into
//  various declarative contexts.
//
//===----------------------------------------------------------------------===//

#include "TreeTransform.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/SemaInternal.h"

using namespace clang;


namespace clang {

enum InjectedDefType {
  InjectedDef_Field,
  InjectedDef_Method
};

/// Records information about a definition inside a fragment that must be
/// processed later. These are typically fields and methods.
struct InjectedDef {
  InjectedDef(const InjectedDefType& T, Decl *F, Decl *I) :
    Type(T), Fragment(F), Injected(I) { }

  InjectedDefType Type;

  /// The declaration within the fragment.
  Decl *Fragment;

  /// The injected declaration.
  Decl *Injected;
};

class InjectionContext;

/// \brief An injection context. This is declared to establish a set of
/// substitutions during an injection.
class InjectionContext : public TreeTransform<InjectionContext> {
   using Base = TreeTransform<InjectionContext>;
public:
  InjectionContext(Sema &SemaRef) : Base(SemaRef) { }

  ASTContext &getContext() { return getSema().Context; }

  /// Detach the context from the semantics object. Returns this object for
  /// convenience.
  InjectionContext *Detach() {
    return this;
  }

  /// Re-attach the context to the context stack.
  void Attach() {
  }

  /// \brief Adds a substitution from one declaration to another.
  void AddDeclSubstitution(const Decl *Old, Decl *New) {
    assert(TransformedLocalDecls.count(Old) == 0 && "Overwriting substitution");
    transformedLocalDecl(const_cast<Decl*>(Old), New);
  }

  /// Returns a replacement for D if a substitution has been registered or
  /// nullptr if no such replacement exists.
  Decl *GetDeclReplacement(Decl *D) {
    auto Iter = TransformedLocalDecls.find(D);
    if (Iter != TransformedLocalDecls.end())
      return Iter->second;
    else
      return nullptr;
  }

  /// Returns true if D is within an injected fragment or cloned declaration.
  bool IsInInjection(Decl *D);

  DeclarationNameInfo TransformDeclarationName(NamedDecl *ND) {
    DeclarationNameInfo DNI(ND->getDeclName(), ND->getLocation());
    return TransformDeclarationNameInfo(DNI);
  }

  bool InjectDeclarator(DeclaratorDecl *D, DeclarationNameInfo &DNI,
		   TypeSourceInfo *&TSI);
  bool InjectMemberDeclarator(DeclaratorDecl *D, DeclarationNameInfo &DNI,
                              TypeSourceInfo *&TSI, CXXRecordDecl *&Owner);

  void UpdateFunctionParms(FunctionDecl* Old, FunctionDecl* New);

  Decl *InjectTypedefNameDecl(TypedefNameDecl *D);
  Decl *InjectVarDecl(VarDecl *D);
  Decl *InjectCXXRecordDecl(CXXRecordDecl *D);
  Decl *InjectFieldDecl(FieldDecl *D);
  Decl *InjectCXXMethodDecl(CXXMethodDecl *D);
  Decl *InjectDeclImpl(Decl *D);
  Decl *InjectDecl(Decl *D);
  Decl *InjectAccessSpecDecl(AccessSpecDecl *D);
  Decl *InjectCXXMetaprogramDecl(CXXMetaprogramDecl *D);

  // Members

  /// \brief A list of declarations whose definitions have not yet been
  /// injected. These are processed when a class receiving injections is
  /// completed.
  llvm::SmallVector<InjectedDef, 8> InjectedDefinitions;
};

bool InjectionContext::IsInInjection(Decl *D) {
  return true;
}

// Inject the name and the type of a declarator declaration. Sets the
// declaration name info, type, and owner. Returns true if the declarator
// is invalid.
//
// FIXME: If the declarator has a nested names specifier, rebuild that
// also. That potentially modifies the owner of the declaration
bool InjectionContext::InjectDeclarator(DeclaratorDecl *D,
                                        DeclarationNameInfo &DNI,
                                        TypeSourceInfo *&TSI) {
  bool Invalid = false;

  // Rebuild the name.
  DNI = TransformDeclarationName(D);
  if (D->getDeclName().isEmpty() != DNI.getName().isEmpty()) {
    DNI = DeclarationNameInfo(D->getDeclName(), D->getLocation());
    Invalid = true;
  }

  // Rebuild the type.
  TSI = TransformType(D->getTypeSourceInfo());
  if (!TSI) {
    TSI = D->getTypeSourceInfo();
    Invalid = true;
  }

  return Invalid;
}

// Inject the name and the type of a declarator declaration. Sets the
// declaration name info, type, and owner. Returns true if the declarator
// is invalid.
bool InjectionContext::InjectMemberDeclarator(DeclaratorDecl *D,
                                              DeclarationNameInfo &DNI,
                                              TypeSourceInfo *&TSI,
                                              CXXRecordDecl *&Owner) {
  bool Invalid = InjectDeclarator(D, DNI, TSI);
  Owner = cast<CXXRecordDecl>(getSema().CurContext);
  return Invalid;
}

void InjectionContext::UpdateFunctionParms(FunctionDecl* Old,
                                           FunctionDecl* New) {
  // Make sure the parameters are actually bound to the function.
  TypeSourceInfo *TSI = New->getTypeSourceInfo();
  FunctionProtoTypeLoc TL = TSI->getTypeLoc().castAs<FunctionProtoTypeLoc>();
  New->setParams(TL.getParams());

  // Update the parameters their owning functions and register substitutions
  // as needed. Note that we automatically register substitutions for injected
  // parameters.
  unsigned OldIndex = 0;
  unsigned NewIndex = 0;
  auto OldParms = Old->parameters();
  auto NewParms = New->parameters();
  if (OldParms.size() > 0) {
    do {
      ParmVarDecl *OldParm = OldParms[OldIndex++];
      ParmVarDecl *NewParm = NewParms[NewIndex++];
      NewParm->setOwningFunction(New);
    } while (OldIndex < OldParms.size() && NewIndex < NewParms.size());
  } else {
    assert(NewParms.size() == 0);
  }
  assert(OldIndex == OldParms.size() && NewIndex == NewParms.size());
}

Decl* InjectionContext::InjectTypedefNameDecl(TypedefNameDecl *D) {
  bool Invalid = false;

  DeclContext *Owner = getSema().CurContext;

  // Transform the type. If this fails, just retain the original, but
  // invalidate the declaration later.
  TypeSourceInfo *TSI = TransformType(D->getTypeSourceInfo());
  if (!TSI) {
    TSI = D->getTypeSourceInfo();
    Invalid = true;
  }

  // Create the new typedef
  TypedefNameDecl *Typedef;
  if (isa<TypeAliasDecl>(D))
    Typedef = TypeAliasDecl::Create(
        getContext(), Owner, D->getBeginLoc(), D->getLocation(),
        D->getIdentifier(), TSI);
  else
    Typedef = TypedefDecl::Create(
        getContext(), Owner, D->getBeginLoc(), D->getLocation(),
        D->getIdentifier(), TSI);
  AddDeclSubstitution(D, Typedef);

  Typedef->setAccess(D->getAccess());
  Typedef->setInvalidDecl(Invalid);
  Owner->addDecl(Typedef);

  return Typedef;
}

static bool InjectVariableInitializer(InjectionContext &Cxt,
                                      VarDecl *Old,
                                      VarDecl *New) {
  if (Old->getInit()) {
    if (New->isStaticDataMember() && !Old->isOutOfLine())
      Cxt.getSema().PushExpressionEvaluationContext(
          Sema::ExpressionEvaluationContext::ConstantEvaluated, Old);
    else
      Cxt.getSema().PushExpressionEvaluationContext(
          Sema::ExpressionEvaluationContext::PotentiallyEvaluated, Old);

    // Instantiate the initializer.
    ExprResult Init;
    {
      Sema::ContextRAII SwitchContext(Cxt.getSema(), New->getDeclContext());
      bool DirectInit = (Old->getInitStyle() == VarDecl::CallInit);
      Init = Cxt.TransformInitializer(Old->getInit(), DirectInit);
    }

    if (!Init.isInvalid()) {
      Expr *InitExpr = Init.get();
      if (New->hasAttr<DLLImportAttr>() &&
          (!InitExpr ||
           !InitExpr->isConstantInitializer(Cxt.getContext(), false))) {
        // Do not dynamically initialize dllimport variables.
      } else if (InitExpr) {
        Cxt.getSema().AddInitializerToDecl(New, InitExpr, Old->isDirectInit());
      } else {
        Cxt.getSema().ActOnUninitializedDecl(New);
      }
    } else {
      New->setInvalidDecl();
    }

    Cxt.getSema().PopExpressionEvaluationContext();
  } else {
    if (New->isStaticDataMember()) {
      if (!New->isOutOfLine())
        return New;

      // If the declaration inside the class had an initializer, don't add
      // another one to the out-of-line definition.
      if (Old->getFirstDecl()->hasInit())
        return New;
    }

    // We'll add an initializer to a for-range declaration later.
    if (New->isCXXForRangeDecl())
      return New;

    Cxt.getSema().ActOnUninitializedDecl(New);
  }

  return New;
}

Decl *InjectionContext::InjectVarDecl(VarDecl *D) {
  DeclContext *Owner = getSema().CurContext;

  DeclarationNameInfo DNI;
  TypeSourceInfo *TSI;
  bool Invalid = InjectDeclarator(D, DNI, TSI);

  VarDecl *Var = VarDecl::Create(
      getContext(), Owner, D->getInnerLocStart(), DNI, TSI->getType(),
      TSI, D->getStorageClass());
  AddDeclSubstitution(D, Var);

  if (D->isNRVOVariable()) {
    QualType ReturnType = cast<FunctionDecl>(Owner)->getReturnType();
    if (getSema().isCopyElisionCandidate(ReturnType, Var, Sema::CES_Strict))
      Var->setNRVOVariable(true);
  }

  Var->setImplicit(D->isImplicit());
  Var->setInvalidDecl(Invalid);
  Owner->addDecl(Var);

  // If we are instantiating a local extern declaration, the
  // instantiation belongs lexically to the containing function.
  // If we are instantiating a static data member defined
  // out-of-line, the instantiation will have the same lexical
  // context (which will be a namespace scope) as the template.
  if (D->isLocalExternDecl()) {
    Var->setLocalExternDecl();
    Var->setLexicalDeclContext(Owner);
  } else if (D->isOutOfLine()) {
    Var->setLexicalDeclContext(D->getLexicalDeclContext());
  }
  Var->setTSCSpec(D->getTSCSpec());
  Var->setInitStyle(D->getInitStyle());
  Var->setCXXForRangeDecl(D->isCXXForRangeDecl());
  Var->setConstexpr(D->isConstexpr());
  Var->setInitCapture(D->isInitCapture());
  Var->setPreviousDeclInSameBlockScope(D->isPreviousDeclInSameBlockScope());
  Var->setAccess(D->getAccess());

  if (!D->isStaticDataMember()) {
    if (D->isUsed(false))
      Var->setIsUsed();
    Var->setReferenced(D->isReferenced());
  }

  // Forward the mangling number from the template to the instantiated decl.
  getContext().setManglingNumber(
      Var, getContext().getManglingNumber(D));
  getContext().setStaticLocalNumber(
      Var, getContext().getStaticLocalNumber(D));

  if (D->isInlineSpecified())
    Var->setInlineSpecified();
  else if (D->isInline())
    Var->setImplicitlyInline();

  InjectVariableInitializer(*this, D, Var);

  return Var;
}

/// Injects the base specifier Base into Class.
static bool InjectBaseSpecifiers(InjectionContext &Cxt, 
                                 CXXRecordDecl *OldClass,
                                 CXXRecordDecl *NewClass) {
  bool Invalid = false;
  SmallVector<CXXBaseSpecifier*, 4> Bases;
  for (const CXXBaseSpecifier &OldBase : OldClass->bases()) {
    TypeSourceInfo *TSI = Cxt.TransformType(OldBase.getTypeSourceInfo());
    if (!TSI) {
      Invalid = true;
      continue;
    }

    CXXBaseSpecifier *NewBase = Cxt.getSema().CheckBaseSpecifier(
        NewClass, OldBase.getSourceRange(), OldBase.isVirtual(), 
        OldBase.getAccessSpecifierAsWritten(), TSI, OldBase.getEllipsisLoc());
    if (!NewBase) {
      Invalid = true;
      continue;
    }

    Bases.push_back(NewBase);
  }

  if (!Invalid && Cxt.getSema().AttachBaseSpecifiers(NewClass, Bases))
    Invalid = true;

  // Invalidate the class if necessary.
  NewClass->setInvalidDecl(Invalid);

  return Invalid;
}

static bool InjectClassMembers(InjectionContext &Cxt,
                               CXXRecordDecl *OldClass,
                               CXXRecordDecl *NewClass) {
  for (Decl *OldMember : OldClass->decls()) {
    // Don't transform invalid declarations.
    if (OldMember->isInvalidDecl())
      continue;

    // Don't transform non-members appearing in a class.
    if (OldMember->getDeclContext() != OldClass)
      continue;

    Decl *NewMember = Cxt.InjectDecl(OldMember);
    if (!NewMember)
      NewClass->setInvalidDecl();
  }
  return NewClass->isInvalidDecl();
}

static bool InjectClassDefinition(InjectionContext &Cxt,
                                  CXXRecordDecl *OldClass,
                                  CXXRecordDecl *NewClass) {
  Sema::ContextRAII SwitchContext(Cxt.getSema(), NewClass);
  Cxt.getSema().StartDefinition(NewClass);
  InjectBaseSpecifiers(Cxt, OldClass, NewClass);
  InjectClassMembers(Cxt, OldClass, NewClass);
  Cxt.getSema().CompleteDefinition(NewClass);
  return NewClass->isInvalidDecl();
}

Decl *InjectionContext::InjectCXXRecordDecl(CXXRecordDecl *D) {
  bool Invalid = false;
  DeclContext *Owner = getSema().CurContext;

  CXXRecordDecl *Class;
  if (D->isInjectedClassName()) {
    DeclarationName DN = cast<CXXRecordDecl>(Owner)->getDeclName();
    Class = CXXRecordDecl::Create(
        getContext(), D->getTagKind(), Owner, D->getBeginLoc(),
        D->getLocation(), DN.getAsIdentifierInfo(), /*PrevDecl=*/nullptr);
  } else {
    DeclarationNameInfo DNI = TransformDeclarationName(D);
    if (!DNI.getName())
      Invalid = true;
    Class = CXXRecordDecl::Create(
        getContext(), D->getTagKind(), Owner, D->getBeginLoc(),
        D->getLocation(), DNI.getName().getAsIdentifierInfo(),
        /*PrevDecl=*/nullptr);
  }
  AddDeclSubstitution(D, Class);

  Class->setAccess(D->getAccess());
  Class->setImplicit(D->isImplicit());
  Class->setInvalidDecl(Invalid);
  Owner->addDecl(Class);

  if (D->hasDefinition())
    InjectClassDefinition(*this, D, Class);

  return Class;
}

Decl *InjectionContext::InjectFieldDecl(FieldDecl *D) {
  DeclarationNameInfo DNI;
  TypeSourceInfo *TSI;
  CXXRecordDecl *Owner;
  bool Invalid = InjectMemberDeclarator(D, DNI, TSI, Owner);

  Expr *BitWidth = nullptr;

  // Build and check the field.
  FieldDecl *Field = getSema().CheckFieldDecl(
      DNI.getName(), TSI->getType(), TSI, Owner, D->getLocation(),
      D->isMutable(), BitWidth, D->getInClassInitStyle(), D->getInnerLocStart(),
      D->getAccess(), nullptr);
  AddDeclSubstitution(D, Field);

  // Propagate semantic properties.
  Field->setImplicit(D->isImplicit());
  Field->setAccess(D->getAccess());

  if (!Field->isInvalidDecl())
    Field->setInvalidDecl(Invalid);

  Owner->addDecl(Field);

  // If the field has an initializer, add it to the Fragment so that we
  // can process it later.
  if (D->hasInClassInitializer())
    InjectedDefinitions.push_back(InjectedDef(InjectedDef_Field, D, Field));

  return Field;
}

Decl *InjectionContext::InjectCXXMethodDecl(CXXMethodDecl *D) {
  ASTContext &AST = getContext();
  DeclarationNameInfo DNI;
  TypeSourceInfo *TSI;
  CXXRecordDecl *Owner;
  bool Invalid = InjectMemberDeclarator(D, DNI, TSI, Owner);

  // Build the underlying method.
  //
  // FIXME: Should we propagate implicit operators?
  CXXMethodDecl *Method;
  if (CXXConstructorDecl *Ctor = dyn_cast<CXXConstructorDecl>(D)) {
    Method = CXXConstructorDecl::Create(AST, Owner, D->getBeginLoc(), DNI,
                                        TSI->getType(), TSI,
                                        Ctor->isExplicit(),
                                        Ctor->isInlineSpecified(),
                                        Ctor->isImplicit(),
                                        Ctor->isConstexpr());
    Method->setRangeEnd(D->getEndLoc());
  } else if (CXXDestructorDecl *Dtor = dyn_cast<CXXDestructorDecl>(D)) {
    Method = CXXDestructorDecl::Create(AST, Owner, D->getBeginLoc(), DNI,
                                       TSI->getType(), TSI,
                                       Dtor->isInlineSpecified(),
                                       Dtor->isImplicit());
    Method->setRangeEnd(D->getEndLoc());
  } else if (CXXConversionDecl *Conv = dyn_cast<CXXConversionDecl>(D)) {
    Method = CXXConversionDecl::Create(AST, Owner, D->getBeginLoc(), DNI,
                                       TSI->getType(), TSI,
                                       Conv->isInlineSpecified(),
                                       Conv->isExplicit(), Conv->isConstexpr(),
                                       Conv->getEndLoc());
  } else {
    Method = CXXMethodDecl::Create(AST, Owner, D->getBeginLoc(), DNI,
                                   TSI->getType(), TSI,
                                   D->isStatic() ? SC_Static : SC_None,
                                   D->isInlineSpecified(), D->isConstexpr(),
                                   D->getEndLoc());
  }
  AddDeclSubstitution(D, Method);
  UpdateFunctionParms(D, Method);

  // Propagate semantic properties.
  Method->setImplicit(D->isImplicit());
  Method->setAccess(D->getAccess());

  // Propagate virtual flags.
  Method->setVirtualAsWritten(D->isVirtualAsWritten());
  if (D->isPure())
    SemaRef.CheckPureMethod(Method, Method->getSourceRange());

  Method->setDeletedAsWritten(D->isDeletedAsWritten());
  Method->setDefaulted(D->isDefaulted());

  if (!Method->isInvalidDecl())
    Method->setInvalidDecl(Invalid);

  // Don't register the declaration if we're injecting the declaration of
  // a template-declaration. We'll add the template later.
  if (!D->getDescribedFunctionTemplate())
    Owner->addDecl(Method);

  // If the method is has a body, add it to the context so that we can
  // process it later. Note that deleted/defaulted definitions are just
  // flags processed above. Ignore the definition if we've marked this
  // as pure virtual.
  if (D->hasBody() && !Method->isPure())
    InjectedDefinitions.push_back(InjectedDef(InjectedDef_Method, D, Method));

  return Method;
}

Decl *InjectionContext::InjectDeclImpl(Decl *D) {
  // Inject the declaration.
  switch (D->getKind()) {
  case Decl::Typedef:
  case Decl::TypeAlias:
    return InjectTypedefNameDecl(cast<TypedefNameDecl>(D));
  case Decl::Var:
    return InjectVarDecl(cast<VarDecl>(D));
  case Decl::CXXRecord:
    return InjectCXXRecordDecl(cast<CXXRecordDecl>(D));
  case Decl::Field:
    return InjectFieldDecl(cast<FieldDecl>(D));
  case Decl::CXXMethod:
  case Decl::CXXConstructor:
  case Decl::CXXDestructor:
  case Decl::CXXConversion:
    return InjectCXXMethodDecl(cast<CXXMethodDecl>(D));
  case Decl::AccessSpec:
    return InjectAccessSpecDecl(cast<AccessSpecDecl>(D));
  case Decl::CXXMetaprogram:
    return InjectCXXMetaprogramDecl(cast<CXXMetaprogramDecl>(D));
  default:
    break;
  }
  D->dump();
  llvm_unreachable("unhandled declaration");
}

/// \brief Injects a new version of the declaration. Do not use this to
/// resolve references to declarations; use ResolveDecl instead.
Decl *InjectionContext::InjectDecl(Decl *D) {
  assert(!GetDeclReplacement(D) && "Declaration already injected");

  // If the declaration does not appear in the context, then it need
  // not be resolved.
  if (!IsInInjection(D))
    return D;

  Decl* R = InjectDeclImpl(D);
  if (!R)
    return nullptr;

  // If we injected a top-level declaration, notify the AST consumer,
  // so that it can be processed for code generation.
  if (isa<TranslationUnitDecl>(R->getDeclContext()))
    getSema().Consumer.HandleTopLevelDecl(DeclGroupRef(R));

  return R;
}

Decl *InjectionContext::InjectAccessSpecDecl(AccessSpecDecl *D) {
  CXXRecordDecl *Owner = cast<CXXRecordDecl>(getSema().CurContext);
  return AccessSpecDecl::Create(
      getContext(), D->getAccess(), Owner, D->getLocation(), D->getColonLoc());
}

Decl *InjectionContext::InjectCXXMetaprogramDecl(CXXMetaprogramDecl *D) {
  // We can use the ActOn* members since the initial parsing for these
  // declarations is trivial (i.e., don't have to translate declarators).
  unsigned ScopeFlags; // Unused
  Decl *New = getSema().ActOnCXXMetaprogramDecl(
    /*Scope=*/nullptr, D->getLocation(), ScopeFlags);

  getSema().ActOnStartCXXMetaprogramDecl(/*Scope=*/nullptr, New);
  StmtResult S = TransformStmt(D->getBody());
  if (!S.isInvalid())
    getSema().ActOnFinishCXXMetaprogramDecl(/*Scope=*/nullptr, New, S.get());
  else
    getSema().ActOnCXXMetaprogramDeclError(/*Scope=*/nullptr, New);

  return New;
}

} // namespace clang

/// Called at the start of a source code fragment to establish the fragment
/// declaration and placeholders.
Decl *Sema::ActOnStartCXXFragment(Scope* S, SourceLocation Loc) {
  CXXFragmentDecl *Fragment = CXXFragmentDecl::Create(Context, CurContext, Loc);

  if (S)
    PushDeclContext(S, Fragment);

  return Fragment;
}


/// Binds the content the fragment declaration. Returns the updated fragment.
/// The Fragment is nullptr if an error occurred during parsing. However,
/// we still need to pop the declaration context.
Decl *Sema::ActOnFinishCXXFragment(Scope *S, Decl *Fragment, Decl *Content) {
  CXXFragmentDecl *FD = nullptr;
  if (Fragment) {
    FD = cast<CXXFragmentDecl>(Fragment);
    FD->setContent(Content);
  }

  if (S)
    PopDeclContext();

  return FD;
}


/// Builds a new fragment expression.
ExprResult Sema::ActOnCXXFragmentExpr(SourceLocation Loc, Decl *Fragment) {
  return BuildCXXFragmentExpr(Loc, Fragment);
}

/// \brief Builds a new fragment expression.
/// Consider the following:
///
///   constexpr {
///     auto x = <<class: int a, b, c;>>;
///   }
///
/// The type of the expression is a new meta:: class defined, approximately,
/// like this:
///
///   using typename(reflexpr(<fragment>)) = refl_type;
///
///   struct __fragment_type  {
///     refl_type fragment_reflection;
///
///     __fragment_type(refl_type fragment_reflection) :
///        fragment_reflection(fragment_reflection) { }
///   };
///
ExprResult Sema::BuildCXXFragmentExpr(SourceLocation Loc, Decl *Fragment) {
  CXXFragmentDecl *FD = cast<CXXFragmentDecl>(Fragment);

  // Reflection; need to get a ReflectExpr the Fragment
  // hold as a static consptexpr std::meta::info on the generated class
  // create a default constructor so that the fragment can
  // be initialized.

  // If the fragment appears in a context that depends on template parameters,
  // then the expression is dependent.
  //
  // FIXME: This is just an approximation of the right answer. In truth, the
  // expression is dependent if the fragment depends on any template parameter
  // in this or any enclosing context.
  if (CurContext->isDependentContext()) {
    return new (Context) CXXFragmentExpr(Context, Loc, Context.DependentTy,
                                         FD, nullptr);
  }

  // Build the expression used to the reflection of fragment.
  //
  // TODO: We should be able to compute the type without generating an
  // expression. We're not actually using the expression.
  ExprResult Reflection = ActOnCXXReflectExpression(
    /*KWLoc=*/SourceLocation(), /*Kind=*/ReflectionKind::REK_declaration,
    /*Entity=*/FD->getContent(), /*LPLoc=*/SourceLocation(),
    /*RPLoc=*/SourceLocation());
  if (Reflection.isInvalid())
    return ExprError();

  // Build our new class implicit class to hold our fragment info.
  CXXRecordDecl *Class = CXXRecordDecl::Create(
					       Context, TTK_Class, CurContext, Loc, Loc,
					       /*Id=*/nullptr,
					       /*PrevDecl=*/nullptr);
  StartDefinition(Class);

  Class->setImplicit(true);
  Class->setFragment(true);

  QualType ClassTy = Context.getRecordType(Class);
  TypeSourceInfo *ClassTSI = Context.getTrivialTypeSourceInfo(ClassTy);


  // Build the class fields.
  SmallVector<FieldDecl *, 4> Fields;
  QualType ReflectionType = Reflection.get()->getType();
  IdentifierInfo *ReflectionFieldId = &Context.Idents.get(
      "fragment_reflection");
  TypeSourceInfo *ReflectionTypeInfo = Context.getTrivialTypeSourceInfo(
      ReflectionType);

  /// TODO This can be changed to a VarDecl to make it static
  /// member data
  QualType ConstReflectionType = ReflectionType.withConst();
  FieldDecl *Field = FieldDecl::Create(
                                       Context, Class, Loc, Loc, ReflectionFieldId,
                                       ConstReflectionType, ReflectionTypeInfo,
                                       nullptr, false,
                                       ICIS_NoInit);
  Field->setAccess(AS_public);
  Field->setImplicit(true);

  Fields.push_back(Field);
  Class->addDecl(Field);

  // Build a new constructor for our fragment type.
  DeclarationName Name = Context.DeclarationNames.getCXXConstructorName(
      Context.getCanonicalType(ClassTy));
  DeclarationNameInfo NameInfo(Name, Loc);
  CXXConstructorDecl *Ctor = CXXConstructorDecl::Create(
      Context, Class, Loc, NameInfo, /*Type*/QualType(), /*TInfo=*/nullptr,
      /*isExplicit=*/true, /*isInline=*/true, /*isImplicitlyDeclared=*/false,
      /*isConstexpr=*/true);
  Ctor->setAccess(AS_public);

  // Build the function type for said constructor.
  FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpec.Type = EST_Unevaluated;
  EPI.ExceptionSpec.SourceDecl = Ctor;
  EPI.ExtInfo = EPI.ExtInfo.withCallingConv(
      Context.getDefaultCallingConvention(/*IsVariadic=*/false,
                                          /*IsCXXMethod=*/true));

  SmallVector<QualType, 4> ArgTypes;
  ArgTypes.push_back(ReflectionType);

  QualType CtorTy = Context.getFunctionType(Context.VoidTy, ArgTypes, EPI);
  Ctor->setType(CtorTy);

  // Build the constructor params.
  SmallVector<ParmVarDecl *, 4> Parms;
  IdentifierInfo *ReflectionParmId = &Context.Idents.get("fragment_reflection");
  ParmVarDecl *Parm = ParmVarDecl::Create(Context, Ctor, Loc, Loc,
                                          ReflectionParmId,
                                          ReflectionType, ReflectionTypeInfo,
                                          SC_None, nullptr);
  Parm->setScopeInfo(0, 0);
  Parm->setImplicit(true);
  Parms.push_back(Parm);

  Ctor->setParams(Parms);

  // Build constructor initializers.
  std::size_t NumInits = Fields.size();
  CXXCtorInitializer **Inits = new (Context) CXXCtorInitializer *[NumInits];

  // Build member initializers.
  for (std::size_t I = 0; I < Parms.size(); ++I) {
    ParmVarDecl *Parm = Parms[I];
    FieldDecl *Field = Fields[I];
    DeclRefExpr *Ref = new (Context) DeclRefExpr(
        Parm, false, Parm->getType(), VK_LValue, Loc);
    Expr *Arg = new (Context) ParenListExpr(Context, Loc, Ref, Loc);
    Inits[I] = BuildMemberInitializer(Field, Arg, Loc).get();
  }
  Ctor->setNumCtorInitializers(NumInits);
  Ctor->setCtorInitializers(Inits);

  // Build the definition.
  Stmt *Def = CompoundStmt::Create(Context, None, Loc, Loc);
  Ctor->setBody(Def);
  Class->addDecl(Ctor);

  CompleteDefinition(Class);

  // Setup the arguments to use for initialization.
  SmallVector<Expr *, 4> CtorArgs;
  CtorArgs.push_back(Reflection.get());

  // Build an expression that that initializes the fragment object.
  CXXConstructExpr *Cast = CXXConstructExpr::Create(
      Context, ClassTy, Loc, Ctor, true, CtorArgs,
      /*HadMultipleCandidates=*/false, /*ListInitialization=*/false,
      /*StdInitListInitialization=*/false, /*ZeroInitialization=*/false,
      CXXConstructExpr::CK_Complete, SourceRange(Loc, Loc));
  Expr *Init = CXXFunctionalCastExpr::Create(
      Context, ClassTy, VK_RValue, ClassTSI, CK_NoOp, Cast,
      /*Path=*/nullptr, Loc, Loc);

  // Finally, build the fragment expression.
  return new (Context) CXXFragmentExpr(Context, Loc, ClassTy, FD, Init);
}

/// Returns an injection statement.
StmtResult Sema::ActOnCXXInjectionStmt(SourceLocation Loc, Expr *Fragment) {
  return BuildCXXInjectionStmt(Loc, Fragment);
}

/// Returns an injection statement.
StmtResult Sema::BuildCXXInjectionStmt(SourceLocation Loc, Expr *Fragment) {
  // The operand must be a reflection (if non-dependent).
  // if (!Fragment->isTypeDependent() && !Fragment->isValueDependent()) {
  if (!Fragment->getType()->getAsCXXRecordDecl()->isFragment()) {
    Diag(Fragment->getExprLoc(), diag::err_not_a_fragment);
    return StmtError();
  }
  // }

  // Perform an lvalue-to-value conversion so that we get an rvalue in
  // evaluation.
  if (Fragment->isGLValue())
    Fragment = ImplicitCastExpr::Create(Context, Fragment->getType(),
                                        CK_LValueToRValue, Fragment,
                                        nullptr, VK_RValue);

  return new (Context) CXXInjectionStmt(Loc, Fragment);
}

// Returns an integer value describing the target context of the injection.
// This correlates to the second %select in err_invalid_injection.
static int DescribeInjectionTarget(DeclContext *DC) {
  if (DC->isFunctionOrMethod())
    return 0;
  else if (DC->isRecord())
    return 1;
  else if (DC->isNamespace())
    return 2;
  else if (DC->isTranslationUnit())
    return 3;
  else
    llvm_unreachable("Invalid injection context");
}

struct TypedValue
{
  QualType Type;
  APValue Value;
};

// Generate an error injecting a declaration of kind SK into the given
// declaration context. Returns false. Note that SK correlates to the first
// %select in err_invalid_injection.
static bool InvalidInjection(Sema& S, SourceLocation POI, int SK,
                             DeclContext *DC) {
  S.Diag(POI, diag::err_invalid_injection) << SK << DescribeInjectionTarget(DC);
  return false;
}

static bool CheckInjectionContexts(Sema &SemaRef, SourceLocation POI,
                                   DeclContext *Injection,
                                   DeclContext *Injectee) {
  if (Injection->isRecord() && !Injectee->isRecord()) {
    InvalidInjection(SemaRef, POI, 1, Injectee);
    return false;
  } else if (Injection->isFileContext() && !Injectee->isFileContext()) {
    InvalidInjection(SemaRef, POI, 0, Injectee);
    return false;
  }
  return true;
}

/// Inject a fragment into the current context.
bool Sema::InjectFragment(SourceLocation POI,
                          const Decl *Injection,
                          Decl *Injectee) {
  assert(isa<CXXRecordDecl>(Injection) || isa<NamespaceDecl>(Injection));
  DeclContext *InjectionDC = Decl::castToDeclContext(Injection);
  DeclContext *InjecteeDC = Decl::castToDeclContext(Injectee);

  if (!CheckInjectionContexts(*this, POI, InjectionDC, InjecteeDC))
    return false;

  ContextRAII Switch(*this, InjecteeDC, isa<CXXRecordDecl>(Injectee));

  // Establish the injection context and register the substitutions.
  InjectionContext *Cxt = new InjectionContext(*this);
  Cxt->AddDeclSubstitution(Injection, Injectee);
  // Cxt->AddPlaceholderSubstitutions(Fragment, Class, Captures);

  // Inject each declaration in the fragment.
  for (Decl *D : InjectionDC->decls()) {
    // Never inject injected class names.
    if (CXXRecordDecl *Class = dyn_cast<CXXRecordDecl>(D))
      if (Class->isInjectedClassName())
        continue;

    Decl *R = Cxt->InjectDecl(D);
    if (!R || R->isInvalidDecl()) {
      Injectee->setInvalidDecl(true);
      continue;
    }
  }

  // If we're injecting into a class and have pending definitions, attach
  // those to the class for subsequent analysis.
  if (CXXRecordDecl *ClassInjectee = dyn_cast<CXXRecordDecl>(Injectee)) {
    if (!Injectee->isInvalidDecl() && !Cxt->InjectedDefinitions.empty()) {
      PendingClassMemberInjections.push_back(Cxt->Detach());
      return true;
    }
  }

  delete Cxt;
  return !Injectee->isInvalidDecl();
}

static const Decl *
GetDeclFromReflection(Sema &SemaRef, APValue FragmentData, SourceLocation Loc) {
  assert(FragmentData.isStruct()
	 && "expected FragmentData to be a struct value");
  Reflection Refl(FragmentData.getStructField(0));
  return Refl.getDeclaration();
}

bool Sema::ApplyInjection(SourceLocation POI, InjectionInfo &II) {
  const Decl *Injection = GetDeclFromReflection(*this, II.FragmentData, POI);

  Decl *Injectee = Decl::castFromDeclContext(CurContext);
  if (!Injectee) {
    return false;
  }

  // FIXME: We need to validate the Injection is compatible
  // with the Injectee.

  return InjectFragment(POI, Injection, Injectee);
}

static void
PrintDecl(Sema &SemaRef, const Decl *D) {
  PrintingPolicy PP = SemaRef.Context.getPrintingPolicy();
  PP.TerseOutput = false;
  D->print(llvm::errs(), PP);
  llvm::errs() << '\n';
}

static void
PrintType(Sema &SemaRef, const Type *T) {
  if (TagDecl *TD = T->getAsTagDecl())
    return PrintDecl(SemaRef, TD);
  PrintingPolicy PP = SemaRef.Context.getPrintingPolicy();
  QualType QT(T, 0);
  QT.print(llvm::errs(), PP);
  llvm::errs() << '\n';
}

static bool
ApplyDiagnostic(Sema &SemaRef, SourceLocation Loc, const APValue &Arg) {
  Reflection R(Arg);
  if (const Decl *D = R.getAsDeclaration()) {
    // D->dump();
    PrintDecl(SemaRef, D);
  }
  else if (const Type *T = R.getAsType()) {
    // if (TagDecl *TD = T->getAsTagDecl())
    //   TD->dump();
    // else
    //   T->dump();
    PrintType(SemaRef, T);
  }
  else
    llvm_unreachable("printing invalid reflection");
  return true;
}

/// Inject a sequence of source code fragments or modification requests
/// into the current AST. The point of injection (POI) is the point at
/// which the injection is applied.
///
/// \returns  true if no errors are encountered, false otherwise.
bool Sema::ApplyEffects(SourceLocation POI,
                        SmallVectorImpl<EvalEffect> &Effects) {
  bool Ok = true;
  for (EvalEffect &Effect : Effects) {
    if (Effect.Kind == EvalEffect::InjectionEffect)
      Ok &= ApplyInjection(POI, *Effect.Injection);
    else
      Ok &= ApplyDiagnostic(*this, POI, *Effect.DiagnosticArg);
  }
  return Ok;
}


/// Check if there are any pending definitions of member functions for
/// this class or any of its nested class definitions. We can simply look
/// at the most recent injection; if it's D or declared inside D, then
/// the answer is yes. Otherwise the answer is no.
///
/// We need to check for this whenever a class is completed during an
/// injection. We don't want to prematurely inject definitions.
///
/// FIXME: It's likely that this wouldn't be necessarily if we integrated
/// injection contexts into the template instantiation context; they are
/// somewhat similar.
bool Sema::HasPendingInjections(DeclContext *D) {
  bool is_empty = PendingClassMemberInjections.empty();
  if (is_empty)
    return false;
  InjectionContext *Cxt = PendingClassMemberInjections.back();
  assert(!Cxt->InjectedDefinitions.empty() && "bad injection queue");
  InjectedDef& Def = Cxt->InjectedDefinitions.front();
  DeclContext *DC = Def.Injected->getDeclContext();
  while (!DC->isFileContext()) {
    if (DC == D)
      return true;
    DC = DC->getParent();
  }
  return false;
}

static void CleanupUsedContexts(
  std::deque<InjectionContext *>& PendingClassMemberInjections) {
  while (!PendingClassMemberInjections.empty()) {
    delete PendingClassMemberInjections.back();
    PendingClassMemberInjections.pop_back();
  }
}

void Sema::InjectPendingFieldDefinitions() {
  for (auto&& Cxt : PendingClassMemberInjections) {
    InjectPendingFieldDefinitions(Cxt);
  }
}

void Sema::InjectPendingMethodDefinitions() {
  for (auto&& Cxt : PendingClassMemberInjections) {
    InjectPendingMethodDefinitions(Cxt);
  }
  CleanupUsedContexts(PendingClassMemberInjections);
}

void Sema::InjectPendingFieldDefinitions(InjectionContext *Cxt) {
  Cxt->Attach();
  for (InjectedDef& Def : Cxt->InjectedDefinitions) {
    if (Def.Type != InjectedDef_Field)
      continue;

    InjectPendingDefinition(Cxt,
  			    static_cast<FieldDecl *>(Def.Fragment),
	  		    static_cast<FieldDecl *>(Def.Injected));
  }
}

void Sema::InjectPendingMethodDefinitions(InjectionContext *Cxt) {
  Cxt->Attach();
  for (InjectedDef& Def : Cxt->InjectedDefinitions) {
    if (Def.Type != InjectedDef_Method)
      continue;

    InjectPendingDefinition(Cxt,
  			    static_cast<CXXMethodDecl *>(Def.Fragment),
	  		    static_cast<CXXMethodDecl *>(Def.Injected));
  }
}

void Sema::InjectPendingDefinition(InjectionContext *Cxt,
                                   FieldDecl *OldField,
                                   FieldDecl *NewField) {
  // Switch to the class enclosing the newly injected declaration.
  ContextRAII ClassCxt (*this, NewField->getDeclContext());

  // This is necessary to provide the correct lookup behavior
  // for any injected field with a default initializer using
  // a decl owned by the injectee
  this->CXXThisTypeOverride = Context.getPointerType(
    Context.getRecordType(NewField->getParent()));

  ExprResult Init = Cxt->TransformExpr(OldField->getInClassInitializer());
  if (Init.isInvalid())
    NewField->setInvalidDecl();
  else
    NewField->setInClassInitializer(Init.get());
}

void Sema::InjectPendingDefinition(InjectionContext *Cxt,
                                   CXXMethodDecl *OldMethod,
                                   CXXMethodDecl *NewMethod) {
  // FIXME: Everything should already be parsed
  // this should be unncessary
  PushFunctionScope();

  ContextRAII MethodCxt (*this, NewMethod);
  StmtResult Body = Cxt->TransformStmt(OldMethod->getBody());
  if (Body.isInvalid())
    NewMethod->setInvalidDecl();
  else
    NewMethod->setBody(Body.get());

  if (CXXConstructorDecl *OldConstructor = dyn_cast<CXXConstructorDecl>(OldMethod)) {
    CXXConstructorDecl *NewConstructor = cast<CXXConstructorDecl>(NewMethod);

    int NumCtorInits = OldConstructor->getNumCtorInitializers();
    CXXCtorInitializer** NewCtorInits = new CXXCtorInitializer*[NumCtorInits]();

    MutableArrayRef<CXXCtorInitializer *> NewInitArgs(NewCtorInits, NumCtorInits);

    auto OldIterator = OldConstructor->init_begin();
    auto OldIteratorEnd = OldConstructor->init_end();
    auto NewIterator = NewInitArgs.begin();
    auto NewIteratorEnd = NewInitArgs.end();

    while (OldIterator != OldIteratorEnd && NewIterator != NewIteratorEnd) {
      CXXCtorInitializer* OldInitializer = *OldIterator;

      ASTContext &AST = getASTContext();
      FieldDecl* NewField = cast<FieldDecl>(
        Cxt->GetDeclReplacement(OldInitializer->getMember()));
      ExprResult NewInit = Cxt->TransformExpr(OldInitializer->getInit());

      CXXCtorInitializer* NewInitializer = new CXXCtorInitializer(
	AST, NewField, OldInitializer->getMemberLocation(),
        OldInitializer->getLParenLoc(), NewInit.get(),
	OldInitializer->getRParenLoc());

      *NewIterator = NewInitializer;

      OldIterator++;
      NewIterator++;
    }

    SetCtorInitializers(NewConstructor, /*AnyErrors=*/false, NewInitArgs);
    // DiagnoseUninitializedFields(*this, Constructor);
  }
}
