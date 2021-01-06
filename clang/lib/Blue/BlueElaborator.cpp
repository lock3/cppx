//===- BlueElaborator.cpp - Blue Language Elaborator ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Translates concrete syntax into C++ AST trees.
//
//===----------------------------------------------------------------------===//

#include "clang/Blue/BlueElaborator.h"
#include "clang/Blue/BlueSyntax.h"
#include "clang/Blue/BlueDeclarator.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/ExprCXX.h"

#include <iostream>

namespace blue {

clang::Decl *Elaborator::elaborateTop(const Syntax *S)
{
  const TopSyntax *Top = cast<TopSyntax>(S);
  SemaRef.CurContext = getCxxContext().getTranslationUnitDecl();
  Sema::ScopeRAII NamespaceScope(SemaRef, Scope::Namespace, S);

  for (const Syntax *SS : Top->children())
    elaborateDecl(SS);

  for (const Syntax *SS : Top->children())
    elaborateDefinition(SS);

  return getCxxContext().getTranslationUnitDecl();
}

clang::Decl* Elaborator::elaborateDecl(const Syntax *S)
{
  switch (S->getKind()) {
  case Syntax::Def:
    return elaborateDefDecl(static_cast<const DefSyntax *>(S));
  default:
    break;
  }
  llvm_unreachable("invalid declaration");
}

clang::Decl *Elaborator::elaborateDefDecl(const DefSyntax *S) {
  // Build the declarator.
  Declarator *Dcl = getDeclarator(S->getDeclarator());

  if (Dcl->declaresValue())
    return makeValueDecl(S, Dcl);

  if (Dcl->declaresFunction())
    return makeFunctionDecl(S, Dcl);

  if (Dcl->declaresTemplate())
    return makeTemplateDecl(S, Dcl);

  llvm_unreachable("Invalid declarator");
}

void Elaborator::elaborateParameters(const ListSyntax *S) {
  if (S->isSemicolonSeparated())
    return elaborateParameterGroup(S);
  else
    return elaborateParameterList(S);
}

void Elaborator::elaborateParameterGroup(const ListSyntax *S) {
  for (const Syntax *SS : S->children())
    elaborateParameterList(cast<ListSyntax>(SS));
}

void Elaborator::elaborateParameterList(const ListSyntax *S) {
  for (const Syntax *SS : S->children())
    elaborateParameter(SS);

  // FIXME: Examine the parameters we just created. We might be able
  // to back-propagate types to some of them. For example, if we have;
  //
  //    a, b : int
  //
  // Then this should be equivalent to a : int, b: int.
}

clang::Decl *Elaborator::elaborateParameter(const Syntax *S) {
  if (!isa<DefSyntax>(S)) {
    Error(S->getLocation(), "invalid parameter syntax");
    return nullptr;
  }

  // FIXME: Implement me.
  const auto *Def = cast<DefSyntax>(S);
  Declarator *Dcl = getDeclarator(Def->getDeclarator());
  (void)Dcl;
  return nullptr;
}


// Declarator construction

// A declarator is a sequence of unary (pointer) and binary (application)
// operators that can be linearized to describe a declaration.
//
// For applications, the left-hand side of a binary operator is always a
// parameter or type list describing a function or template (i.e., mapping)
// type. The right hand side denotes the type or kind of value produced by
// the function's evaluation or instantiation.
//
// For pointers, the operand is the type of object pointed at.
//
// The right-most leaf of the tree ultimately determines the type of the
// object accessed by the declaration.
//
// The first term in the list is always an identifier, which is established
// by the function above.
Declarator *Elaborator::getDeclarator(const Syntax *S) {
  if (!S)
    return getImplicitAutoDeclarator();

  if (const auto *U = dyn_cast<UnarySyntax>(S))
    return getUnaryDeclarator(U);
  if (const auto* B = dyn_cast<BinarySyntax>(S))
    return getBinaryDeclarator(B);
  return getLeafDeclarator(S);
}

Declarator *Elaborator::getUnaryDeclarator(const UnarySyntax *S) {
  if (S->getOperator().hasKind(tok::Caret)) {
    Declarator *Dcl = getDeclarator(S->getOperand());
    return new Declarator(Declarator::Pointer, S, Dcl);
  }

  Error(S->getLocation(), "invalid operator in declarator");
  return nullptr;
}

Declarator *Elaborator::getBinaryDeclarator(const BinarySyntax *S) {
  if (S->isApplication()) {
    Declarator *Dcl = getDeclarator(S->getRightOperand());

    // FIXME: This should be an error, not an assertion.
    //
    // TODO: Can the S be anything other than a list?
    //
    // TODO: Can we limit the way in which mappings compose? For example,
    // what is this declaring?
    //
    //    x : (a) [t : type] int;
    //
    // It appears to be a function returning a variable template. I don't
    // know if there's any way to write a metafunction that returns a
    // template.
    //
    // This should be disallowed.
    const auto *List = cast<ListSyntax>(S);

    // Elaborate function parameters.
    // enterScope(Scope::Parameter);
    elaborateParameters(List);
    // leaveScope();

    if (List->isParenList())
      return new Declarator(Declarator::Function, S, Dcl);

    // FIXME: This is only a template declarator if there are template
    // parameters in the list.
    if (List->isBracketList())
      return new Declarator(Declarator::Template, S, Dcl);

    // FIXME: Braces are possible, but invalid. This should be an error,
    // not an assertion.
    Error(List->getLocation(), "invalid list in declarator");
    return nullptr;
  }

  // TODO: We could support binary type composition (e.g., T1 * T2) as
  // an alternative spelling of product types. However, this most likely
  // needs to be wrapped in parens, so it should end up as a leaf. Maybe
  // this is a non-issue.
  Error(S->getLocation(), "invalid operator in declarator");
  return nullptr;
}

Declarator *Elaborator::getLeafDeclarator(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Literal:
  case Syntax::Identifier:
  case Syntax::List:
    return new Declarator(Declarator::Type, S);
  default:
    break;
  }
  llvm_unreachable("Invalid type expression");
}

Declarator *Elaborator::getImplicitAutoDeclarator() {
  return new Declarator(Declarator::ImplicitType, nullptr);
}

// Declaration construction

clang::Decl *Elaborator::makeValueDecl(const Syntax *S, Declarator* Dcl)
{
  // Elaborate the declarator.
  //
  // FIXME: An ill-typed declaration isn't the end of the world. Can we
  // poison the declaration and move on?
  clang::Expr *E = elaborateDeclarator(Dcl);
  if (!E)
    return nullptr;

  clang::QualType T;
  if (E->getType()->isTypeOfTypes())
    T = cast<clang::CppxTypeLiteral>(E)->getValue()->getType();

  if (T->isKindType())
    return makeTypeDecl(S, Dcl, T);
  else
    return makeObjectDecl(S, Dcl, E);
}

static inline clang::StorageClass getDefaultVariableStorageClass(Sema &SemaRef) {
  return SemaRef.getCurrentScope()->isBlockScope() ||
    SemaRef.getCurrentScope()->isControlScope()
    ? clang::SC_Auto
    : clang::SC_None;
}

clang::Decl *Elaborator::makeObjectDecl(const Syntax *S, Declarator *Dcl, clang::Expr *Ty) {
  // llvm::outs() << "OBJECT!\n";
  // FIXME: possibly invalid assertion
  assert(isa<DefSyntax>(S) && "not a definition");
  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  const DefSyntax *Def = cast<DefSyntax>(S);
  clang::IdentifierInfo *Id =
    &CxxAST.Idents.get({Def->getIdentifierSpelling()});
  clang::DeclarationName Name(Id);
  clang::SourceLocation Loc = S->getLocation();

  assert(Ty->getType()->isTypeOfTypes() && "type of decalration is not a type");
  clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(Ty)->getValue();

  clang::DeclContext *Owner = SemaRef.CurContext;
  clang::VarDecl *VD =
    clang::VarDecl::Create(CxxAST, Owner, Loc, Loc, Id, T->getType(), T,
                           getDefaultVariableStorageClass(SemaRef));
  Owner->addDecl(VD);

  return VD;
}

clang::Decl *Elaborator::makeTypeDecl(const Syntax *S, Declarator *Dcl, clang::QualType T) {
  llvm::outs() << "TYPE!\n";
  return nullptr;
}

clang::Decl *Elaborator::makeFunctionDecl(const Syntax *S, Declarator* Dcl) {
  llvm_unreachable("Not implemented");
}

clang::Decl *Elaborator::makeTemplateDecl(const Syntax *S, Declarator* Dcl) {
  llvm_unreachable("Not implemented");
}

// Type elaboration
//
// TODO: Can we combine this with the creating the declarator? It would save
// a pass over the AST. We could store the type directly in the declarator
// node (which we might do anyway), along with intermediate structures
// created to manage the declaration.

/// Return the type of entity declared by Dcl and establish any semantic
/// state needed to process the declaration and its initializer.
clang::Expr *Elaborator::elaborateDeclarator(const Declarator *Dcl) {
  switch (Dcl->getKind()) {
  case Declarator::Type:
    return elaborateTypeDeclarator(Dcl);
  case Declarator::Pointer:
    return elaboratePointerDeclarator(Dcl);
  case Declarator::Array:
    return elaborateArrayDeclarator(Dcl);
  case Declarator::Function:
    return elaborateFunctionDeclarator(Dcl);
  case Declarator::Template:
    return elaborateTemplateDeclarator(Dcl);
  case Declarator::ImplicitType:
    return elaborateImplicitTypeDeclarator(Dcl);
  }
  llvm_unreachable("Unhandled kind of declarator.");
}

/// Elaborate declarations of the form 'T' as an expression.
clang::Expr *Elaborator::elaborateTypeDeclarator(const Declarator *Dcl) {
  clang::Expr *E = elaborateExpression(Dcl->getInfo());
  if (!E)
    return nullptr;
  clang::QualType T = E->getType();
  if (!T->isKindType()) {
    Error(Dcl->getLocation(), "invalid type");
    return nullptr;
  }
  return E;
}

/// Elaborate declarations of the form '^E'.
clang::Expr *Elaborator::elaboratePointerDeclarator(const Declarator *Dcl) {
  clang::Expr *E = elaborateDeclarator(Dcl->getNext());
  if (!E)
    return nullptr;

  // FIXME: Build an address-of expression for the type...
  // clang::Expression *E = Dcl->getNext()->getExpression();

  // if (T.isNull())
  //   return {};
  // return getCxxContext().getPointerType(T);

  return E;
}

/// Elaborate declarations of the form '[E] T'.
clang::Expr *Elaborator::elaborateArrayDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Not implemented");
}

/// Elaborate declarations of the form '(parms) T'.
clang::Expr *Elaborator::elaborateFunctionDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Not implemented");
}

/// Elaborate declarations of the form '[parms] T'.
clang::Expr *Elaborator::elaborateTemplateDeclarator(const Declarator *Dcl) {
  llvm_unreachable("Not implemented");
}

clang::Expr *Elaborator::elaborateImplicitTypeDeclarator(const Declarator *Dcl) {
  clang::QualType Ty = getCxxContext().getAutoDeductType();
  return SemaRef.buildTypeExpr(Ty, clang::SourceLocation());
}


// Expression elaboration

clang::Expr *Elaborator::elaborateExpression(const Syntax *S) {
  switch (S->getKind()) {
  case Syntax::Literal:
    return elaborateLiteralExpression(cast<LiteralSyntax>(S));
  case Syntax::Identifier:
    return elaborateIdentifierExpression(cast<IdentifierSyntax>(S));
  case Syntax::List:
    return elaborateListExpression(cast<ListSyntax>(S));
  case Syntax::Seq:
    return elaborateSeqExpression(cast<SeqSyntax>(S));
  case Syntax::Unary:
    return elaborateUnaryExpression(cast<UnarySyntax>(S));
  case Syntax::Binary:
    return elaborateBinaryExpression(cast<BinarySyntax>(S));
  default:
    break;
  }
  llvm_unreachable("Unexpected syntax tree");
}

static clang::Expr *makeIntegerLiteral(Elaborator &Elab, const Token &Tok) {
  // FIXME: Implement me.
  return {};
}

// The type of a type literal is always `type` even when declaring a type.
static clang::Expr *makeTypeLiteral(Elaborator &Elab, clang::QualType T, Token const& Tok) {
  clang::ASTContext &Cxt = Elab.getCxxContext();
  clang::QualType K = Cxt.CppxKindTy;
  llvm_unreachable("Brian broke this during refactoring from QualType to "
      "TypeSourceInfo.");
  // return new (Cxt) clang::CppxTypeLiteral(K, T, Tok.getLocation());
}

clang::Expr *Elaborator::elaborateLiteralExpression(const LiteralSyntax *S) {
  const Token& Tok = S->getToken();
  switch (Tok.getKind()) {
  case tok::DecimalInteger:
    return makeIntegerLiteral(*this, S->getToken());
  case tok::DecimalFloat:
    break;
  case tok::BinaryInteger:
    break;
  case tok::HexadecimalInteger:
    break;
  case tok::HexadecimalFloat:
    break;
  case tok::Character:
    break;
  case tok::String:
    break;

  case tok::VoidKeyword:
    return makeTypeLiteral(*this, getCxxContext().VoidTy, Tok);
  case tok::BoolKeyword:
    return makeTypeLiteral(*this, getCxxContext().BoolTy, Tok);
  case tok::ByteKeyword:
    return makeTypeLiteral(*this, getCxxContext().UnsignedCharTy, Tok);
  case tok::CharacterKeyword:
    return makeTypeLiteral(*this, getCxxContext().CharTy, Tok);
  case tok::IntegerKeyword:
    // FIXME: Support arbitrary length integer types via the lexer.
    return makeTypeLiteral(*this, getCxxContext().IntTy, Tok);
  case tok::FloatKeyword:
    // FIXME: Support arbitrary length floating point types vie the lexer.
    return makeTypeLiteral(*this, getCxxContext().FloatTy, Tok);
  case tok::TypeKeyword:
    return makeTypeLiteral(*this, getCxxContext().CppxKindTy, Tok);

  default:
    break;
  }

  llvm_unreachable("Not implemented");
}

void Elaborator::elaborateDefinition(const Syntax *S) {
  
}

clang::Expr *Elaborator::elaborateIdentifierExpression(const IdentifierSyntax *S) {
  // Check for builtin types
  auto BuiltinMapIter = SemaRef.BuiltinTypes.find(S->getSpelling());
  if (BuiltinMapIter != SemaRef.BuiltinTypes.end())
    return SemaRef.buildTypeExpr(BuiltinMapIter->second, S->getLocation());

  llvm::outs() << "ELABORATING NON-TYPE EXPR\n";
  return nullptr;
}

clang::Expr *Elaborator::elaborateListExpression(const ListSyntax *S) {
  llvm_unreachable("Not implemented");
}

clang::Expr *Elaborator::elaborateSeqExpression(const SeqSyntax *S) {
  llvm_unreachable("Not implemented");
}

clang::Expr *Elaborator::elaborateUnaryExpression(const UnarySyntax *S) {
  llvm_unreachable("Not implemented");
}

clang::Expr *Elaborator::elaborateBinaryExpression(const BinarySyntax *S) {
  llvm_unreachable("Not implemented");
}


// Diagnostics

void Elaborator::Error(clang::SourceLocation Loc, llvm::StringRef Msg) {
  CxxSema.Diags.Report(Loc, clang::diag::err_blue_elaboration) << Msg;
}


} // namespace blue
