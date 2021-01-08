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

#include "clang/AST/Expr.h"
#include "clang/AST/ExprCppx.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"

#include <iostream>

namespace blue {

Declaration *Elaborator::createDeclaration(const Syntax *Def,
                                           Declarator *Dcl,
                                           const Syntax *Init) {
  Declaration *TheDecl =
    new Declaration(SemaRef.getCurrentDecl(), Def, Dcl, Init);

  if (const DefSyntax *Id = dyn_cast<DefSyntax>(Def))
    TheDecl->Id = &SemaRef.getCxxAST().Idents.get({Id->getIdentifierSpelling()});
  else if (const IdentifierSyntax *Id = dyn_cast<IdentifierSyntax>(Def))
    TheDecl->Id = &SemaRef.getCxxAST().Idents.get({Id->getSpelling()});

  TheDecl->DeclaringContext = SemaRef.getCurClangDeclContext();
  Scope *CurScope = SemaRef.getCurrentScope();
  CurScope->addDecl(TheDecl);

  return TheDecl;
}

clang::Decl *Elaborator::elaborateTop(const Syntax *S) {
  if (!S)
    return nullptr;

  clang::TranslationUnitDecl *TU = getCxxContext().getTranslationUnitDecl();
  Declaration *D = new Declaration(S);
  D->setCxx(SemaRef, TU);
  SemaRef.pushDecl(D);

  const TopSyntax *Top = cast<TopSyntax>(S);
  Sema::ScopeRAII NamespaceScope(SemaRef, Scope::Namespace, S);

  for (const Syntax *SS : Top->children()){
    if (isa<DefSyntax>(SS))
      elaborateDecl(SS);
    else {
      auto E = elaborateExpression(SS);
      if (!E)
        return nullptr;
      getCxxSema().ActOnExprStmt(E, /*discardedValue*/true);
    }
  }

  for (const Syntax *SS : Top->children()) {
    elaborateDefinition(SS);
  }

  SemaRef.getCxxSema().ActOnEndOfTranslationUnit();
  SemaRef.popDecl();

  return TU;
}

clang::Decl* Elaborator::elaborateDecl(const Syntax *S)
{
  switch (S->getKind()) {
  case Syntax::Def:
    return elaborateDefDecl(static_cast<const DefSyntax *>(S));
  default:
    break;
  }
  // TODO: Remove this eventaully, when we are not doing elaboration.
  // This will allow us to pretend to evaluate expressions inside the global
  // scope of a probram.
  return nullptr;
  // llvm_unreachable("invalid declaration");
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
  if (!isa<DefSyntax>(S) && !isa<IdentifierSyntax>(S)) {
    Error(S->getLocation(), "invalid parameter syntax");
    return nullptr;
  }

  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::SourceLocation Loc = S->getLocation();

  if (const IdentifierSyntax *Id = dyn_cast<IdentifierSyntax>(S)) {
    // FIXME: create context for auto parameters to keep track of their
    // index and depth.
    // Declaration *TheDecl = createDeclaration(S, nullptr, nullptr);

    // clang::IdentifierInfo *TypeII =
    //   CxxSema.InventAbbreviatedTemplateParameterTypeName(Name, Index);

    // // We add the template parm to the TU temporarily, until we create the
    // // template. We'll set the depth and index later.
    // TemplateTypeParmDecl *TheType =
    //   TemplateTypeParmDecl::Create(ClangContext, TUDecl, SourceLocation(),
    //                                SourceLocation(), /*Depth=*/0, /*Index=*/0,
    //                                /*Identifier=*/nullptr, /*Typename=*/false,
    //                                /*ParameterPack=*/false);
    // TheType->setImplicit();
  }

  // FIXME: There is a lot of duplication with makeObjectDecl here.
  // In Gold it's just one function.
  const auto *Def = cast<DefSyntax>(S);
  Declarator *Dcl = getDeclarator(Def->getDeclarator());
  clang::Expr *Ty = elaborateDeclarator(Dcl);
  if (!Ty)
    return nullptr;

  // Create the Blue Declaration
  Declaration *TheDecl = createDeclaration(Def, Dcl, Def->getInitializer());

  // Create the Clang Decl Node
  clang::IdentifierInfo *Id = TheDecl->Id;
  clang::DeclarationName Name(Id);

  if(!Ty->getType()->isTypeOfTypes()) {
    Error(Ty->getExprLoc(), "expected type");
    return nullptr;
  }

  clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(Ty)->getValue();
  // Create the parameters in the translation unit decl for now, we'll
  // move them into the function later.
  // FIXME: replace this with TU
  clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
  clang::ParmVarDecl *PVD =
    clang::ParmVarDecl::Create(CxxAST, Owner, Loc, Loc,
                               Name, T->getType(), T,
                               clang::SC_Auto, /*def=*/nullptr);
  TheDecl->setCxx(SemaRef, PVD);
  return PVD;
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
    return new Declarator(Declarator::Type, S);
  case Syntax::List:
    return new Declarator(Declarator::Function, S);
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
  assert(isa<DefSyntax>(S) && "not a definition");
  const DefSyntax *Def = cast<DefSyntax>(S);

  // Create the Blue Declaration
  Declaration *TheDecl = createDeclaration(Def, Dcl, Def->getInitializer());

  // Create the Clang Decl Node
  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::IdentifierInfo *Id = TheDecl->Id;
  clang::DeclarationName Name(Id);
  clang::SourceLocation Loc = S->getLocation();

  assert(Ty->getType()->isTypeOfTypes() && "type of declaration is not a type");
  clang::TypeSourceInfo *T = cast<clang::CppxTypeLiteral>(Ty)->getValue();

  clang::DeclContext *Owner = SemaRef.getCurClangDeclContext();
  clang::VarDecl *VD =
    clang::VarDecl::Create(CxxAST, Owner, Loc, Loc, Id, T->getType(), T,
                           getDefaultVariableStorageClass(SemaRef));
  Owner->addDecl(VD);
  TheDecl->setCxx(SemaRef, VD);
  TheDecl->CurrentPhase = Phase::Typing;

  return VD;
}

clang::Decl *Elaborator::makeTypeDecl(const Syntax *S, Declarator *Dcl, clang::QualType T) {
  llvm::outs() << "TYPE!\n";
  return nullptr;
}

clang::CppxTypeLiteral *Elaborator::createFunctionType(Declarator *Dcl) {
  const ListSyntax *ParamList = dyn_cast<ListSyntax>(Dcl->getInfo());
  if (!ParamList)
    return nullptr;
  clang::SourceLocation Loc = ParamList->getLocation();
  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  Sema::ScopeRAII ParamScope(SemaRef, Scope::Parameter, ParamList);

  elaborateParameters(ParamList);

  unsigned N = ParamList->getNumChildren();
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  for (unsigned I = 0; I < N; ++I) {
    const Syntax *P = ParamList->getChild(I);
    Declaration *BluePD = SemaRef.getCurrentScope()->findDecl(P);
    assert(BluePD && "associated declaration never found");
    assert(isa<clang::ParmVarDecl>(BluePD->getCxx()) &&
           "Parameter is not a ParmVarDecl");
    clang::ParmVarDecl *PVD = cast<clang::ParmVarDecl>(BluePD->getCxx());

    CxxAST.setParameterIndex(PVD, I);
    PVD->setScopeInfo(0, I);
    Types.push_back(PVD->getType());
    Params.push_back(PVD);
  }

  // FIXME: We need to configure parts of the prototype (e.g., noexcept).
  clang::FunctionProtoType::ExtProtoInfo EPI;
  clang::QualType ReturnType = CxxAST.getAutoDeductType();
  clang::QualType FnTy = CxxAST.getFunctionType(ReturnType, Types, EPI);
  return SemaRef.buildFunctionTypeExpr(FnTy, Loc, Loc, Loc,
                                       clang::SourceRange(Loc, Loc),
                                       Loc, Params);
}

clang::Decl *Elaborator::makeFunctionDecl(const Syntax *S, Declarator *Dcl) {
  assert(Dcl->declaresFunction() && "not a function declarator");
  assert(isa<DefSyntax>(S) && "not a definition");
  const DefSyntax *Def = cast<DefSyntax>(S);

  Declaration *BlueDecl = createDeclaration(Def, Dcl, Def->getInitializer());

  clang::ASTContext &CxxAST = SemaRef.getCxxAST();
  clang::QualType ReturnType = CxxAST.getAutoDeductType();
  clang::DeclarationName Name(BlueDecl->Id);

  clang::CppxTypeLiteral *FnTy = createFunctionType(Dcl);
  if (!FnTy)
    return nullptr;
  return nullptr;
}

clang::Decl *Elaborator::makeTemplateDecl(const Syntax *S, Declarator* Dcl) {
  llvm::outs() << "TEMPLATE!\n";
  return nullptr;
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
/// This returns a type expression of the form `(parms) -> T`.
clang::Expr *Elaborator::elaborateFunctionDeclarator(const Declarator *Dcl) {


  
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
  assert(S && "invalid expression");
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
  case Syntax::Error:
    Error(S->getLocation(), "failed parse");
    return nullptr;
  default:
    break;
  }
  S->dump();
  llvm_unreachable("Unexpected syntax tree");
}



static bool alwaysFitsInto64Bits(unsigned Radix, unsigned NumDigits) {
  switch (Radix) {
  case 2:
    return NumDigits <= 64;
  case 8:
    return NumDigits <= 64 / 3; // Digits are groups of 3 bits.
  case 10:
    return NumDigits <= 19; // floor(log10(2^64))
  case 16:
    return NumDigits <= 64 / 4; // Digits are groups of 4 bits.
  default:
    llvm_unreachable("impossible Radix");
  }
}

static bool checkOverflow(unsigned Radix, llvm::StringRef Literal,
                          llvm::APInt &Val) {
  const unsigned NumDigits = Literal.size();

  auto isDigitSeparator = [](char C) -> bool {
    return C == '\'';
  };

  if (alwaysFitsInto64Bits(Radix, NumDigits)) {
    uint64_t N = 0;
    for (const char *Ptr = Literal.begin(); Ptr != Literal.end(); ++Ptr)
      if (!isDigitSeparator(*Ptr))
        N = N * Radix + llvm::hexDigitValue(*Ptr);

    // This will truncate the value to Val's input width. Simply check
    // for overflow by comparing.
    Val = N;
    return Val.getZExtValue() != N;
  }

  Val = 0;
  const char *Ptr = Literal.begin();

  llvm::APInt RadixVal(Val.getBitWidth(), Radix);
  llvm::APInt CharVal(Val.getBitWidth(), 0);
  llvm::APInt OldVal = Val;

  bool OverflowOccurred = false;
  while (Ptr < Literal.end()) {
    if (isDigitSeparator(*Ptr)) {
      ++Ptr;
      continue;
    }

    unsigned C = llvm::hexDigitValue(*Ptr++);

    // If this letter is out of bound for this radix, reject it.
    assert(C < Radix && "checkOverflow called with wrong radix");

    CharVal = C;

    // Add the digit to the value in the appropriate radix.  If adding in digits
    // made the value smaller, then this overflowed.
    OldVal = Val;

    // Multiply by radix, did overflow occur on the multiply?
    Val *= RadixVal;
    OverflowOccurred |= Val.udiv(RadixVal) != OldVal;

    // Add value, did overflow occur on the value?
    //   (a + b) ult b  <=> overflow
    Val += CharVal;
    OverflowOccurred |= Val.ult(CharVal);
  }
  return OverflowOccurred;
}

static clang::IntegerLiteral *
createIntegerLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                     const LiteralSyntax *S, std::size_t Base = 10) {
  unsigned Width = S->Suffix.BitWidth;
  bool Signed = S->Suffix.IsSigned;

  // In case we didn't set either flag, this is signed by default.
  if (!Signed && !S->Suffix.IsUnsigned)
    Signed = true;

  unsigned TargetIntWidth = CxxAST.getTargetInfo().getIntWidth();
  if (!Width)
    Width = TargetIntWidth;

  clang::QualType IntTy = CxxAST.getIntTypeForBitwidth(Width, Signed);
  if (IntTy.isNull()) {
    if (Width <= TargetIntWidth)
      IntTy = Signed ? CxxAST.IntTy : CxxAST.UnsignedIntTy;
    else if (Width <= CxxAST.getTargetInfo().getLongWidth())
      IntTy = Signed ? CxxAST.LongTy : CxxAST.UnsignedLongTy;
    else
      IntTy = Signed ? CxxAST.LongLongTy : CxxAST.UnsignedLongLongTy;
  }

  if (Width != CxxAST.getIntWidth(IntTy)) {
    clang::SourceLocation Loc = S->getLocation();
    SemaRef.getCxxSema().Diags.Report(Loc,
      clang::diag::err_integer_bitwidth_mismatch)
      << IntTy << Width << CxxAST.getIntWidth(IntTy);
    return nullptr;
  }

  // skip over any [0.] prefix
  std::string Spelling = Base == 10 ? S->getSpelling().str() :
    S->getSpelling().substr(2).str();

  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

  llvm::APInt Value(Width, Spelling, Base);
  Value = Value.zextOrTrunc(Width);

  if (checkOverflow(Base, Spelling, Value)) {
    SemaRef.getCxxSema().Diags.Report(S->getLocation(),
                                     clang::diag::err_integer_literal_too_large)
      << /* Unsigned */ 1;
    return nullptr;
  }

  return clang::IntegerLiteral::Create(CxxAST, Value, IntTy, S->getLocation());
}

static clang::FloatingLiteral *
createFloatLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                   const LiteralSyntax *S) {
  // If we don't have a specified type, just create a default float.
  clang::QualType FloatType = CxxAST.FloatTy;
  if (S->Suffix.IsDouble)
    FloatType = CxxAST.DoubleTy;
  else if (S->Suffix.IsHalf)
    FloatType = CxxAST.Float16Ty;
  else if (S->Suffix.IsQuarter) {
    unsigned DiagID =
      SemaRef.getCxxSema().Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "minifloats not yet supported by Clang");
    SemaRef.getCxxSema().Diags.Report(S->getLocation(), DiagID);
    return nullptr;
  }

  const llvm::fltSemantics &Format = CxxAST.getFloatTypeSemantics(FloatType);
  using llvm::APFloat;
  APFloat Val = llvm::APFloat(Format);

  std::string Spelling = S->getSpelling().str();
  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

  auto StatusOrErr =
    Val.convertFromString(Spelling, APFloat::rmNearestTiesToEven);
  assert(StatusOrErr && "Invalid floating point representation");
  return clang::FloatingLiteral::Create(CxxAST, Val, /*Exact=*/true,
                                        FloatType, S->getLocation());
}

static clang::FloatingLiteral *
createExponentLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                      const LiteralSyntax *S, clang::SourceLocation Loc) {
  std::string Spelling = S->getSpelling().str();
  assert((Spelling.find_first_of("E") != std::string::npos ||
         Spelling.find_first_of("e") != std::string::npos) &&
         "non-exponent");
  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

  const llvm::fltSemantics &Format =
    CxxAST.getFloatTypeSemantics(CxxAST.DoubleTy);
  llvm::APFloat Val(Format);
  auto StatusOrErr =
    Val.convertFromString(Spelling, llvm::APFloat::rmNearestTiesToEven);
  assert(StatusOrErr && "invalid floating point representation");
  if (llvm::errorToBool(StatusOrErr.takeError()))
    return nullptr;

  llvm::APFloat::opStatus Result = *StatusOrErr;
  if ((Result & llvm::APFloat::opOverflow) ||
      ((Result & llvm::APFloat::opUnderflow) && Val.isZero())) {
    unsigned Diagnostic;
    llvm::SmallString<20> Buffer;
    if (Result & llvm::APFloat::opOverflow) {
      Diagnostic = clang::diag::warn_float_overflow;
      llvm::APFloat::getLargest(Format).toString(Buffer);
    } else {
      Diagnostic = clang::diag::warn_float_underflow;
      llvm::APFloat::getSmallest(Format).toString(Buffer);
    }

    SemaRef.getCxxSema().Diags.Report(Loc, Diagnostic)
      << CxxAST.DoubleTy
      << llvm::StringRef(Buffer.data(), Buffer.size());
  }

  clang::QualType FloatType = CxxAST.FloatTy;
  if (S->Suffix.IsDouble)
    FloatType = CxxAST.DoubleTy;

  bool isExact = (Result == llvm::APFloat::opOK);
  return clang::FloatingLiteral::Create(CxxAST, Val, isExact, FloatType, Loc);
}

/// This was copied from clang/lib/lex/LiteralSupport.cpp:91, and modified.
static unsigned processCharEscape(Sema &SemaRef, clang::SourceLocation Loc,
    const char *&ThisTokBuf, const char *ThisTokEnd,
    bool &HadError, unsigned CharWidth) {

  // Skip the '\' char.
  ++ThisTokBuf;

  // We know that this character can't be off the end of the buffer, because
  // that would have been \", which would not have been the end of string.
  unsigned ResultChar = *ThisTokBuf++;

  switch (ResultChar) {
  // These map to themselves.
  case '\\': case '\'': case '"': case '?': break;

    // These have fixed mappings.
  case 'a':
    ResultChar = 7;
    break;
  case 'b':
    ResultChar = 8;
    break;
  case 'e':
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << "e";
    ResultChar = 27;
    break;
  case 'E':
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << "E";
    ResultChar = 27;
    break;
  case 'f':
    ResultChar = 12;
    break;
  case 'n':
    ResultChar = 10;
    break;
  case 'r':
    ResultChar = 13;
    break;
  case 't':
    ResultChar = 9;
    break;
  case 'v':
    ResultChar = 11;
    break;
  case 'x': { // Hex escape.
    ResultChar = 0;
    if (ThisTokBuf == ThisTokEnd || !clang::isHexDigit(*ThisTokBuf)) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_hex_escape_no_digits)
          << "x";
      HadError = true;
      break;
    }

    // Hex escapes are a maximal series of hex digits.
    bool Overflow = false;
    for (; ThisTokBuf != ThisTokEnd; ++ThisTokBuf) {
      int CharVal = llvm::hexDigitValue(ThisTokBuf[0]);
      if (CharVal == -1) break;
      // About to shift out a digit?
      if (ResultChar & 0xF0000000)
        Overflow = true;
      ResultChar <<= 4;
      ResultChar |= CharVal;
    }

    // See if any bits will be truncated when evaluated as a character.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      Overflow = true;
      ResultChar &= ~0U >> (32-CharWidth);
    }

    // Check for overflow.
    if (Overflow)   // Too many digits to fit in
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_escape_too_large)
          << 0;
    break;
  }
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7': {
    // Octal escapes.
    --ThisTokBuf;
    ResultChar = 0;

    // Octal escapes are a series of octal digits with maximum length 3.
    // "\0123" is a two digit sequence equal to "\012" "3".
    unsigned NumDigits = 0;
    do {
      ResultChar <<= 3;
      ResultChar |= *ThisTokBuf++ - '0';
      ++NumDigits;
    } while (ThisTokBuf != ThisTokEnd && NumDigits < 3 &&
             ThisTokBuf[0] >= '0' && ThisTokBuf[0] <= '7');

    // Check for overflow.  Reject '\777', but not L'\777'.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::err_escape_too_large)
          << 1;
      ResultChar &= ~0U >> (32-CharWidth);
    }
    break;
  }

    // Otherwise, these are not valid escapes.
  case '(': case '{': case '[': case '%':
    // GCC accepts these as extensions.  We warn about them as such though.
    // TODO: We need to determine if we need to suppor this or not.
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << std::string(1, ResultChar);
    break;
  default:

    if (clang::isPrintable(ResultChar))
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
          << std::string(1, ResultChar);
    else
      SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
          <<  "x" + llvm::utohexstr(ResultChar);
    break;
  }

  return ResultChar;
}

/// readCharacter attempts to read the next character in a literal value
/// if there's an error true is returned, and otherwise the result is false.
///
/// The Iter will be advanced to the position of the next character in the
/// string.
///
/// This function will indicate an error when Iter == End. It's important to
/// set test that value before the next call to this function.
static bool readCharacter(Sema &SemaRef, clang::SourceLocation Loc,
                          const char *&Iter, const char *End, unsigned &Value,
                          bool &Escape) {
  assert (Iter <= End && "Invalid character");

  // Process an escape sequence if we encounter one, otherwise do a simple
  // character literal read.
  if (*Iter == '\\') {
    Escape = true;
    bool DidError = false;
    Value = processCharEscape(SemaRef, Loc, Iter, End, DidError, 8u);
    return DidError;
  } else {
    Escape = false;
    Value = *Iter;
    ++Iter;
  }

  return false;
}

static clang::CharacterLiteral *
createCharLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                  Token T, clang::SourceLocation Loc) {
  auto Spelling = T.getSpelling();
  assert(Spelling[0] == '\'' && "atom is not a character");

  Spelling = Spelling.substr(1, Spelling.size());
  Spelling = Spelling.substr(0, Spelling.find_last_of('\''));
  if (Spelling.empty()) {
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_empty_character);
    return nullptr;
  }

  llvm::SmallString<16> CharBuffer;
  CharBuffer.append(Spelling.begin(), Spelling.end());
  unsigned Character = 0;
  bool EscapeSeq;
  const char *CharBegin = CharBuffer.data();
  const char *CharEnd = CharBuffer.data() + CharBuffer.size();
  if (readCharacter(SemaRef, Loc, CharBegin, CharEnd, Character, EscapeSeq)) {
    SemaRef.getCxxSema().Diags.Report(Loc, clang::diag::ext_unknown_escape)
      << CharBuffer.data();
    return nullptr;
  }

  // A multi-character character constant is actually valid, so we'll just
  // warn and move on.
  if (!EscapeSeq && Spelling.size() > 1) {
    unsigned DiagID =
      SemaRef.getCxxSema().Diags.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                    "multi-character character constant");
    SemaRef.getCxxSema().Diags.Report(Loc, DiagID);
  }

  return new (CxxAST) clang::CharacterLiteral(Character,
                                              clang::CharacterLiteral::UTF8,
                                              SemaRef.DefaultCharTy, Loc);
}

static clang::CharacterLiteral *
createUTF8Literal(clang::ASTContext &CxxAST, Sema &SemaRef,
                  Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling().str();
  Spelling = Spelling.substr(Spelling.find_first_not_of("0c"), Spelling.size());
  unsigned Value = (unsigned)std::stoi(Spelling, 0, 16);

  // FIXME: warn on overflow?

  return new (CxxAST)
    clang::CharacterLiteral(Value, clang::CharacterLiteral::UTF8,
                            CxxAST.Char8Ty, Loc);
}

static clang::CharacterLiteral *
createUnicodeLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                     Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling().str();
  Spelling = Spelling.substr(Spelling.find_first_not_of("0u"), Spelling.size());
  unsigned Value = (unsigned)std::stoi(Spelling, 0, 16);

  // FIXME: warn on overflow?

  clang::CharacterLiteral::CharacterKind CharKind;
  clang::QualType CharType;
  if (Value <= 0xFF) {
    CharKind = clang::CharacterLiteral::UTF8;
    CharType = CxxAST.Char8Ty;
  } else if (Value <= 0xFFFF) {
    CharKind = clang::CharacterLiteral::UTF16;
    CharType = CxxAST.Char16Ty;
  } else if (Value <= 0xFFFFFFFF) {
    CharKind = clang::CharacterLiteral::UTF32;
    CharType = CxxAST.Char32Ty;
  } else {
    return nullptr;
  }

  return new (CxxAST) clang::CharacterLiteral(Value, CharKind, CharType, Loc);
}

static clang::StringLiteral *
createStringLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                    Token T, const Syntax *StrNode) {
  auto Str = T.getSpelling();
  clang::Token CTok;
  CTok.startToken();
  CTok.setKind(clang::tok::utf8_string_literal);
  CTok.setLocation(StrNode->getLocation());
  CTok.setLength(Str.size());
  llvm::SmallVector<clang::Token, 1> StrTokens;
  StrTokens.push_back(CTok);
  clang::StringLiteralParser StrParser(StrTokens, CxxAST.getSourceManager(),
                                       CxxAST.getLangOpts(),
                                       CxxAST.getTargetInfo(),
                                       &SemaRef.getCxxSema().Diags);

  clang::QualType StrTy = CxxAST.getStringLiteralArrayType(
                          SemaRef.DefaultCharTy, StrParser.GetNumStringChars());
  auto EncodingKind = clang::StringLiteral::Ascii;
  // if (SemaRef.insideAttributeExpr()) {
  //   EncodingKind = ;
  // }
  return clang::StringLiteral::Create(CxxAST, StrParser.GetString(),
                                      EncodingKind,
                                      false, StrTy, StrNode->getLocation());
}

static clang::CXXBoolLiteralExpr *
createBoolLiteral(clang::ASTContext &CxxAST, Token T,
                  clang::SourceLocation Loc) {
  return new (CxxAST) clang::CXXBoolLiteralExpr(T.hasKind(tok::TrueKeyword),
                                                CxxAST.BoolTy, Loc);
}

static clang::CXXNullPtrLiteralExpr *
createNullLiteral(clang::ASTContext &CxxAST, clang::SourceLocation Loc) {
  return new (CxxAST) clang::CXXNullPtrLiteralExpr(CxxAST.NullPtrTy, Loc);
}

clang::Expr *Elaborator::elaborateLiteralExpression(const LiteralSyntax *S) {
  // Other things we need in the future.
  // DecimalExponent
  // HexadecimalCharacter
  // UnicodeCharacter
  const Token& Tok = S->getToken();
  switch (Tok.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S);
  case tok::DecimalFloat:
    return createFloatLiteral(getCxxContext(), SemaRef, S);
  case tok::BinaryInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S, 2);
  case tok::HexadecimalInteger:
    return createIntegerLiteral(getCxxContext(), SemaRef, S, /*Base=*/16);
  case tok::HexadecimalFloat:
    llvm_unreachable("Hexadecimal float not implemented");
    break;
  case tok::Character:
    return createCharLiteral(getCxxContext(), SemaRef, Tok, S->getLocation());
  case tok::String:
    return createStringLiteral(getCxxContext(), SemaRef, Tok, S);
  case tok::TrueKeyword:
  case tok::FalseKeyword:
    return createBoolLiteral(getCxxContext(), Tok, S->getLocation());
  case tok::NullKeyword:
    return createNullLiteral(getCxxContext(), S->getLocation());
  case tok::VoidKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().VoidTy, Tok.getLocation());
  case tok::BoolKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().BoolTy, Tok.getLocation());
  case tok::ByteKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().UnsignedCharTy,
                                 Tok.getLocation());
  case tok::CharacterKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().CharTy,
                                 Tok.getLocation());
  case tok::IntegerKeyword:
    // FIXME: Support arbitrary length integer types via the lexer.
    return SemaRef.buildTypeExpr(getCxxContext().IntTy,
                                 Tok.getLocation());
  case tok::FloatKeyword:
    // FIXME: Support arbitrary length floating point types vie the lexer.
    return SemaRef.buildTypeExpr(getCxxContext().FloatTy,
                                 Tok.getLocation());
  case tok::TypeKeyword:
    return SemaRef.buildTypeExpr(getCxxContext().CppxKindTy,
                                 Tok.getLocation());

  default:
    break;
  }

  llvm_unreachable("Not implemented");
}

void Elaborator::elaborateDefinition(const Syntax *S) {
  auto Decl = SemaRef.getCurrentScope()->findDecl(S);
  if (!Decl)
    return;

  // If the current phase isn't typing then bail.
  if (phaseOf(Decl) != Phase::Typing)
    return;

  if (Decl->IsVariableDecl())
    return elaborateVarDef(Decl);

  llvm_unreachable("Elaboration for this kind of declaration isn't "
                   "implemented yet.");

}
void Elaborator::elaborateVarDef(Declaration *D) {
  D->CurrentPhase = Phase::Initialization;
  // if (!D->Cxx)
  //   return;

  // If this isn't early elaboration then we have to actually track it.
  // if (!IsEarly)
  //   ElabTracker.init(D);

  // Sema::OptionalInitScope<Sema::ResumeScopeRAII> OptResumeScope(SemaRef);
  // clang::Expr *InitExpr = nullptr;
  // clang::VarDecl *VD = nullptr;
  // Sema::DeclInitializationScope ClangInitScope(SemaRef, D);
  // bool NeedsConstEvaluation = false;
  // if (D->defines<clang::VarTemplateDecl>()) {
  //   if (SemaRef.checkForRedefinition<clang::VarTemplateDecl>(D))
  //     return;

  //   // We need to attempt to re-enter the template context for this variable.
  //   OptResumeScope.Init(D->SavedScope, D->Op);
  //   clang::VarTemplateDecl *VTD = cast<clang::VarTemplateDecl>(D->Cxx);
  //   VD = VTD->getTemplatedDecl();
  // } else {
  //   if (SemaRef.checkForRedefinition<clang::VarDecl>(D))
  //     return;
  //   VD = cast<clang::VarDecl>(D->Cxx);
  // }

  // if (VD->isConstexpr())
  //   NeedsConstEvaluation = true;
  clang::VarDecl *VD = cast<clang::VarDecl>(D->getCxx());
  const DefSyntax *Def = D->asDef();
  if (!Def)
    return;
  if (!Def->hasInitializer()) {
    // if (isa<clang::ParmVarDecl>(VD))
    //   return;
    // FIXME: We probably want to synthesize some kind of initializer here.
    // Not quite sure how we want to do this.
    //
    // FIXME: What if D has type auto? Surely this is an error. For example:
    //
    //    x : auto
    //
    // declares an undeduced-type variable with no initializer. Presumably
    // this should be an error.

    // This handles implcit initialization/constructor calls for variables
    // that don't have a = sign on first use, but have a type.
    // That includes complex types.
    getCxxSema().ActOnUninitializedDecl(VD);
    getCxxSema().FinalizeDeclaration(VD);
    return;
  }
  // if (D->defines<clang::VarTemplateDecl>()) {
  //   // I may need to revisit this in the furture becaus this might not be
  //   // the right thing to do in this case.
  //   VD->setInit(InitExpr);
  // } else {

  // if (D->isDeclaredWithinClass() && !VD->isInlineSpecified()
  //     && (!VD->getType().isConstant(Context.CxxAST) && !VD->isConstexpr())) {
  //   SemaRef.Diags.Report(D->IdDcl->getLoc(),
  //                       clang::diag::err_in_class_initializer_non_const);
  //   return;
  // }
  auto InitExpr = elaborateExpression(Def->getInitializer());
  if (!InitExpr)
    return;
  // Update the initializer.
  SemaRef.getCxxSema().AddInitializerToDecl(VD, InitExpr, /*DirectInit=*/true);

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
  auto Operand = elaborateExpression(S->getOperand());
  if (!Operand)
    return nullptr;
  clang::QualType Ty = Operand->getType();
  if (Ty->isTypeOfTypes()) {
    // TODO: Implementation for creating pointers goes here.
    llvm_unreachable("Unary operator on type expression not implemented yet.");
  }
  auto OpIter = SemaRef.UnaryOpMap.find(S->getOperatorSpelling());
  if (OpIter == SemaRef.UnaryOpMap.end()) {
    Error(S->getLocation(), "invalid unary operator");
    return nullptr;
  }
  auto Res = SemaRef.getCxxSema().BuildUnaryOp(
    /*scope*/nullptr, S->getLocation(), OpIter->second, Operand);
  return Res.get();
}

clang::Expr *Elaborator::elaborateBinaryExpression(const BinarySyntax *S) {
  auto LHS = elaborateExpression(S->getLeftOperand());
  if (!LHS)
    return nullptr;

  auto RHS = elaborateExpression(S->getRightOperand());
  if (!RHS)
    return nullptr;

  auto OpIter = SemaRef.BinOpMap.find(S->getOperatorSpelling());
  if (OpIter == SemaRef.BinOpMap.end()) {
    Error(S->getLocation(), "invalid binary operator");
    return nullptr;
  }
  clang::ExprResult Res = SemaRef.getCxxSema().BuildBinOp(/*Scope=*/nullptr,
                                                          S->getLocation(),
                                                          OpIter->second, LHS,
                                                          RHS);
  return Res.get();
}


// Diagnostics

void Elaborator::Error(clang::SourceLocation Loc, llvm::StringRef Msg) {
  CxxSema.Diags.Report(Loc, clang::diag::err_blue_elaboration) << Msg;
}


} // namespace blue
