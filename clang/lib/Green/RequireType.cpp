#include "clang/AST/DeclTemplate.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/Green/GreenTokens.h"
#include "clang/Green/RequireType.h"

using namespace clang;

namespace green {

TypeRequirer::TypeRequirer(SyntaxContext &Context, GreenSema &GSemaRef)
  : Context(Context), GSemaRef(GSemaRef), PP(GSemaRef.getPP())
{
  OperatorColonII = PP.getIdentifierInfo("operator':'");
  OperatorExclaimII = PP.getIdentifierInfo("operator'!'");
}

void
TypeRequirer::RequireType(IdentifierInfo *Name, const Syntax *S) {
  auto MapIter = TypeMapping.find(Name);
  if (MapIter != TypeMapping.end())
    return;

  if (isa<CallSyntax>(S))
    return RequireTypeForCall(Name, cast<CallSyntax>(S));
  else if (isa<ArraySyntax>(S))
    return RequireTypeForArray(Name, cast<ArraySyntax>(S));
  else if (isa<AtomSyntax>(S))
    return RequireTypeForAtom(Name, cast<AtomSyntax>(S));
}

void
TypeRequirer::RequireTypeForArray(IdentifierInfo *Name, const ArraySyntax *S) {
  for (const Syntax *Child : S->children())
    if (isa<ListSyntax>(Child)) 
      RequireTypeForList(Name, cast<ListSyntax>(Child));
}

void
TypeRequirer::RequireTypeForList(IdentifierInfo *Name, const ListSyntax *S) {
  // A list might contain an untyped identifier or an operator call.
  for (const Syntax *Child : S->children()) {
    if (isa<AtomSyntax>(Child)) {
      const AtomSyntax *Atom = cast<AtomSyntax>(Child);
      auto BuiltinIter = BuiltinTypes.find(Atom->Tok.spelling());

      // Don't type a keyword.
      if (Atom->Tok.has_kind(tok_identifier) &&
          BuiltinIter == BuiltinTypes.end())
        RequireTypeForAtom(Name, Atom);

    } else if (isa<CallSyntax>(Child)) {
      RequireTypeForCall(Name, cast<CallSyntax>(Child));
    }
  }
}

void
TypeRequirer::RequireTypeForCall(IdentifierInfo *Name, const CallSyntax *S) {
  if (isa<AtomSyntax>(S->Callee())) {
    const AtomSyntax *CalleeAtom = cast<AtomSyntax>(S->Callee());
    IdentifierInfo *Spelling = PP.getIdentifierInfo(CalleeAtom->Tok.spelling());
    if (Spelling == OperatorColonII) {
      const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
      const AtomSyntax *Typename = cast<AtomSyntax>(ArgList->Elems[1]);
      auto MapIter = BuiltinTypes.find(Typename->Tok.spelling());
      if (MapIter == BuiltinTypes.end()) {
        // error
        llvm::errs() << "Use of unknown type: " << Typename->Tok.spelling() << '\n';
        return;
      }

      TypeMapping.insert({Name, MapIter->second.getTypePtr()});
    } else if (Spelling == OperatorExclaimII) {
      // We're declaring a new function, so reset the template index.
      TemplateIndex = 0;

      const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
      return RequireTypeForCall(Name, cast<CallSyntax>(ArgList->Elems[0]));
    } else {
      RequireTypeForAtom(Name, cast<AtomSyntax>(S->Callee()));
    }
  }
}

void
TypeRequirer::RequireTypeForAtom(IdentifierInfo *Name, const AtomSyntax *S) {
  // This is an untyped variable.
  if (!S->isParam()) {
    const Type *Auto = Context.ClangContext.getAutoDeductType().getTypePtr();
    TypeMapping.insert({Name, Auto});
    return;
  }

  // Otherwise, this is an untyped function parameter, so we need to create an
  // implicit template type for it.

  // Temporarily add this decl to the translation unit, until we can add it
  // to the created function decl.
  DeclContext *TUDecl = Context.ClangContext.getTranslationUnitDecl();

  TemplateTypeParmDecl *ParamType =
    TemplateTypeParmDecl::Create(Context.ClangContext, TUDecl, SourceLocation(),
                                 SourceLocation(), /*Depth*/0, TemplateIndex++,
                                 /*Identifier*/nullptr, /*Typename*/false,
                                 /*ParameterPack*/false);
  ParamType->setImplicit();
  TypeMapping.insert({Name, ParamType->getTypeForDecl()});
}

} // namespace green
