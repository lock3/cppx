#ifndef LLVM_GREEN_SYNTAXVISITOR_H
#define LLVM_GREEN_SYNTAXVISITOR_H

#include "clang/GreenAST/Syntax.h"
#include "llvm/ADT/STLExtras.h"

#include <utility>

namespace usyntax {
namespace syntaxvisitor {

template<template <typename> class Ptr, typename ImplClass, typename RetTy=void>
class Base {
public:
#define PTR(CLASS) typename Ptr<CLASS>::type
#define DISPATCH(NAME, CLASS) \
  return static_cast<ImplClass*>(this)->Visit##NAME(static_cast<PTR(CLASS)>(S))

  // FIXME: Consider replacing this with a SyntaxNodes.inc file
  RetTy Visit(PTR(Syntax) S) {
    switch (S->getKind()) {
    case Syntax::SK_ConstInt:
      DISPATCH(SyntaxConstInt, SyntaxConstInt);
    case Syntax::SK_ConstString:
      DISPATCH(SyntaxConstString, SyntaxConstString);
    case Syntax::SK_ConstPath:
      DISPATCH(SyntaxConstPath, SyntaxConstPath);
    case Syntax::SK_Ident:
      DISPATCH(SyntaxIdent, SyntaxIdent);
    case Syntax::SK_Call:
      DISPATCH(SyntaxCall, SyntaxCall);
    case Syntax::SK_Attr:
      DISPATCH(SyntaxAttr, SyntaxAttr);
    case Syntax::SK_Macro:
      DISPATCH(SyntaxMacro, SyntaxMacro);
    case Syntax::SK_Escape:
      DISPATCH(SyntaxEscape, SyntaxEscape);
    }
  }

  // If a function is not implemented, fall back to the base.
  RetTy VisitSyntaxConstInt(PTR(SyntaxConstInt) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxConstString(PTR(SyntaxConstString) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxConstPath(PTR(SyntaxConstPath) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxIdent(PTR(SyntaxIdent) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxCall(PTR(SyntaxCall) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxAttr(PTR(SyntaxAttr) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxMacro(PTR(SyntaxMacro) S) { DISPATCH(Syntax, Syntax); }
  RetTy VisitSyntaxEscape(PTR(SyntaxEscape) S) { DISPATCH(Syntax, Syntax); }

  RetTy VisitSyntax(PTR(Syntax) S) { return RetTy(); }

  #undef PTR
  #undef DISPATCH
};
} // namespace syntaxvisitor

/// A simple visitor class that helps create syntax visitors.
///
/// This class does not preserve constness of Syntax pointers (see also
/// ConstSyntaxVisitor).
template <typename ImplClass, typename RetTy = void>
class SyntaxVisitor
    : public syntaxvisitor::Base<std::add_pointer, ImplClass, RetTy> {};

/// A simple visitor class that helps create syntax visitors.
///
/// This class preserves constness of Syntax pointers (see also SyntaxVisitor).
template <typename ImplClass, typename RetTy = void>
class ConstSyntaxVisitor
    : public syntaxvisitor::Base<llvm::make_const_ptr, ImplClass, RetTy> {};


} // namespace usyntax

#endif
