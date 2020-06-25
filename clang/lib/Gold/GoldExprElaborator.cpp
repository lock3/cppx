//=== GoldExprElaborator.cpp - Elaboration for Gold Expressions -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ExprElaborator interface, which creates
//  clang::Expr nodes out of gold expressions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/AST/ExprCppx.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/DiagnosticSema.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/LexDiagnostic.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TypeLocBuilder.h"
#include "clang/Sema/TypeLocUtil.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Error.h"

#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldExprMarker.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldTokens.h"


#include <cstring>

namespace gold {

using TypeInfo = ExprElaborator::TypeInfo;
using Expression = ExprElaborator::Expression;

ExprElaborator::ExprElaborator(SyntaxContext &Context, Sema &SemaRef,
      clang::DeclContext *DC, gold::Scope *GoldScope)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
  CurrentLookUpContext(DC), OwningScope(GoldScope)
{ }

Expression ExprElaborator::elaborateExpr(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S), clang::QualType());
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));
  if (isa<MacroSyntax>(S))
    return elaborateMacro(cast<MacroSyntax>(S));
  if (isa<ElemSyntax>(S))
    return elaborateElementExpr(cast<ElemSyntax>(S));
  if (const ListSyntax *List = dyn_cast<ListSyntax>(S)) {
    if (List->getNumChildren() == 1)
      return elaborateExpr(List->getChild(0));
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
  }

  return nullptr;
}

static clang::IntegerLiteral *
createIntegerLiteral(clang::ASTContext &CxxAST, Token T,
                     clang::QualType IntType, clang::SourceLocation Loc,
                     std::size_t Base = 10) {
  llvm::APInt Value;
  unsigned Width = 0;

  // If we don't have a specified type, just create a default int.
  if (IntType.isNull() || IntType == CxxAST.AutoDeductTy)
    IntType = CxxAST.IntTy;

  // TODO: support all kinds of integer types.
  if (IntType == CxxAST.IntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    long int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    long long int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.ShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    short int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.UnsignedShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    unsigned short int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedIntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    unsigned int Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    unsigned long Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    unsigned long Literal = std::stoi(T.getSymbol().data(), 0, Base);
    Value = llvm::APSInt::getUnsigned(Literal);
  } else {
    assert(false && "Unsupported integer type.");
  }

  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);
  return clang::IntegerLiteral::Create(CxxAST, Value, IntType, Loc);
}

static clang::FloatingLiteral *
createFloatLiteral(clang::ASTContext &CxxAST, Token T,
                   clang::QualType FloatType, clang::SourceLocation Loc) {
  // If we don't have a specified type, just create a default float.
  if (FloatType.isNull() || FloatType == CxxAST.AutoDeductTy)
    FloatType = CxxAST.FloatTy;

  if (FloatType == CxxAST.FloatTy) {
    float Literal = (float)atof(T.getSymbol().data());
    auto Value = llvm::APFloat(Literal);
    return clang::FloatingLiteral::Create(CxxAST, Value, /*Exact=*/true,
                                          FloatType, Loc);
  } else if (FloatType == CxxAST.DoubleTy) {
    double Literal = atof(T.getSymbol().data());
    auto Value = llvm::APFloat(Literal);
    return clang::FloatingLiteral::Create(CxxAST, Value, /*Exact=*/true,
                                          FloatType, Loc);
  }

  llvm_unreachable("unsupported float type");
}

static clang::FloatingLiteral *
createExponentLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                      Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling().str();
  assert((Spelling.find_first_of("E") != std::string::npos ||
         Spelling.find_first_of("e") != std::string::npos) &&
         "non-exponent");

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

    SemaRef.Diags.Report(Loc, Diagnostic)
      << CxxAST.DoubleTy
      << llvm::StringRef(Buffer.data(), Buffer.size());
  }

  bool isExact = (Result == llvm::APFloat::opOK);
  return clang::FloatingLiteral::Create(CxxAST, Val, isExact, CxxAST.DoubleTy, Loc);
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
    SemaRef.Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << "e";
    ResultChar = 27;
    break;
  case 'E':
    SemaRef.Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
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
      SemaRef.Diags.Report(Loc, clang::diag::err_hex_escape_no_digits)
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
      SemaRef.Diags.Report(Loc, clang::diag::err_escape_too_large)
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
      SemaRef.Diags.Report(Loc, clang::diag::err_escape_too_large)
          << 1;
      ResultChar &= ~0U >> (32-CharWidth);
    }
    break;
  }

    // Otherwise, these are not valid escapes.
  case '(': case '{': case '[': case '%':
    // GCC accepts these as extensions.  We warn about them as such though.
    // TODO: We need to determine if we need to suppor this or not.
    SemaRef.Diags.Report(Loc, clang::diag::ext_nonstandard_escape)
        << std::string(1, ResultChar);
    break;
  default:

    if (clang::isPrintable(ResultChar))
      SemaRef.Diags.Report(Loc, clang::diag::ext_unknown_escape)
          << std::string(1, ResultChar);
    else
      SemaRef.Diags.Report(Loc, clang::diag::ext_unknown_escape)
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
  std::string Spelling = T.getSpelling().str();
  assert(Spelling[0] == '\'' && "atom is not a character");

  Spelling = Spelling.substr(1, Spelling.size());
  Spelling = Spelling.substr(0, Spelling.find_last_of('\''));
  if (Spelling.empty()) {
    SemaRef.Diags.Report(Loc, clang::diag::ext_empty_character);
    return nullptr;
  }

  llvm::SmallString<16> CharBuffer;
  CharBuffer.append(Spelling.begin(), Spelling.end());
  unsigned Character = 0;
  bool EscapeSeq;
  const char *CharBegin = CharBuffer.data();
  const char *CharEnd = CharBuffer.data() + CharBuffer.size();
  if (readCharacter(SemaRef, Loc, CharBegin, CharEnd, Character, EscapeSeq)) {
    SemaRef.Diags.Report(Loc, clang::diag::ext_unknown_escape)
      << CharBuffer.data();
    return nullptr;
  }

  // A multi-character character constant is actually valid, so we'll just
  // warn and move on.
  if (!EscapeSeq && Spelling.size() > 1) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                    "multi-character character constant");
    SemaRef.Diags.Report(Loc, DiagID);
  }

  return new (CxxAST) clang::CharacterLiteral(Character,
                                              clang::CharacterLiteral::Ascii,
                                              CxxAST.CharTy, Loc);
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

static clang::IntegerLiteral *
createBoolLiteral(clang::ASTContext &CxxAST, Token T,
                  clang::SourceLocation Loc) {
  llvm::APInt Value = llvm::APSInt::get(T.hasKind(tok::TrueKeyword));

  unsigned Width = CxxAST.getIntWidth(CxxAST.BoolTy);
  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);

  return clang::IntegerLiteral::Create(CxxAST, Value, CxxAST.BoolTy, Loc);
}

static clang::CXXNullPtrLiteralExpr *
createNullLiteral(clang::ASTContext &CxxAST, clang::SourceLocation Loc) {
  return new (CxxAST) clang::CXXNullPtrLiteralExpr(CxxAST.NullPtrTy, Loc);
}

static clang::TypeSourceInfo*
HandleClassTemplateSelection(ExprElaborator& Elab, Sema &SemaRef,
    SyntaxContext& Context, clang::TypeSourceInfo* IdExpr, const ElemSyntax *Elem) {
  clang::CXXScopeSpec SS;
  clang::Sema::TemplateTy Template;
  clang::UnqualifiedId TemplateName;
  clang::ParsedType ObjectType;
  // FIXME: Handling non-nested elements.
  if (!isa<AtomSyntax>(Elem->getObject()))
    llvm_unreachable("Nested/qualified name access to template syntax "
        "not implemented yet.");
  const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Elem->getObject());
  clang::IdentifierInfo &II = Context.CxxAST.Idents.get(Atom->getSpelling());
  TemplateName.setIdentifier(&II, Atom->getLoc());
  bool MemberOfUnknownSpecialization = false;
  
  if (clang::TemplateNameKind TNK = SemaRef.getCxxSema().isTemplateName(
      SemaRef.getCurClangScope(), SS, /*hasTemplateKeyword=*/false,
      TemplateName, ObjectType, /*EnteringContext*/false, Template,
      MemberOfUnknownSpecialization)) {

    llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;

    const ListSyntax *ElemArgs = cast<ListSyntax>(Elem->getArguments());
    for(const Syntax *SyntaxArg : ElemArgs->children()) {
      clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
        SemaRef.getCxxSema(),
        clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
        /*LambdaContextDecl=*/nullptr,
        /*ExprContext=*/
        clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);

      // TODO: Attempt to process this initially as a template template
      // parameter and see if it fails or not, if it fails then it's not a
      // template template parameter.
      Expression ArgExpr = Elab.elaborateExpr(SyntaxArg);
      if (ArgExpr.isNull()) 
        return nullptr;
      
      if (ArgExpr.is<clang::NamespaceDecl *>()) {
        // FIXME: Figure out the correct error message to display here.
        // Basically we need to say that a namespace is not a type.
        llvm::errs() << "Unable to a namespace name for a template argument\n";
        return nullptr;
      }

      if (ArgExpr.is<clang::TypeSourceInfo *>()) {
        // TODO: Figure out how to handle template template parameters here?
        // Because currently that's impossible.
        auto *SrcInfo = ArgExpr.get<clang::TypeSourceInfo *>();
        ParsedArguments.emplace_back(
          SemaRef.getCxxSema().ActOnTemplateTypeArgument(
            SemaRef.getCxxSema().CreateParsedType(SrcInfo->getType(), SrcInfo)));
      }
      
      if (ArgExpr.is<clang::Expr *>()) {
        // Leveraging constant expression evaluation from clang's sema class.
        clang::ExprResult ConstExpr(ArgExpr.get<clang::Expr*>());
        ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(ConstExpr);
        ParsedArguments.emplace_back(clang::ParsedTemplateArgument::NonType,
            ConstExpr.get(), SyntaxArg->getLoc());
      }
    }
    switch(TNK) {
    case clang::TemplateNameKind::TNK_Concept_template:{
      llvm_unreachable("TNK_Concept_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Dependent_template_name:{
      llvm_unreachable("TNK_Dependent_template_name has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Function_template:{
      llvm_unreachable("TNK_Function_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Non_template:{
      // TODO: It might be best to emit something here indicating that we
      // found something that wasn't a templat that is being used as a template
      // But within the function that I got this from clang doesn't emit an error
      // they simply return false instead.
      //
      // This was taken from the function Parser::ParseUnqualifiedIdTemplateId
      // in ParseExprCXX.cpp line:2281
      return nullptr;
    }
    case clang::TemplateNameKind::TNK_Type_template:{
      clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
      clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
        SemaRef.getCurClangScope(), SS,
        /*TemplateKWLoc*/ clang::SourceLocation(), Template, &II, Atom->getLoc(),
        /*LAngleLoc*/ clang::SourceLocation(), InArgs,
        /*RAngleLoc*/ clang::SourceLocation(), false, false);
      if (Result.isInvalid()) {
        // TODO: Figure out correct error message this.
        llvm::errs() << "We hae an invalid result ?!\n";
        llvm_unreachable("We have an invlaid result for ActOnTemplateIdType.");
      }
      clang::QualType Ty(Result.get().get());
      const clang::LocInfoType *TL = cast<clang::LocInfoType>(Ty.getTypePtr());
      clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST,
          TL->getType(), Atom->getLoc());
      return TInfo;
    }
    case clang::TemplateNameKind::TNK_Undeclared_template:{
      llvm_unreachable("TNK_Undeclared_template has not been implemented yet.");
      break;
    }
    case clang::TemplateNameKind::TNK_Var_template:{
      llvm_unreachable("TNK_Var_template has not been implemented yet.");
      break;
    }
    }
  }
  return nullptr;
}

static Expression handleElementExpression(ExprElaborator &Elab,
    Sema &SemaRef, SyntaxContext &Context, const ElemSyntax *Elem,
    clang::Expr *E) {

  // Attempting to correctly handle the result of an Id expression.
  clang::OverloadExpr *OverloadExpr = dyn_cast<clang::OverloadExpr>(E);
  if (!OverloadExpr) {
    // TODO: When we to add array indexing, we need to add it here.
    llvm_unreachable("Processing of array indices isn't implemented yet.");
    return nullptr;
  }

  // At this point we are an overload set which means we must be some kind of
  // template or lookup failure.
  clang::TemplateArgumentListInfo TemplateArgs(Elem->getLoc(), Elem->getLoc());
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  for (const Syntax *SS : Elem->getArguments()->children()) {
    ExprElaborator ParamElaborator(Context, SemaRef);
    Expression ParamExpression = ParamElaborator.elaborateExpr(SS);
    if (ParamExpression.isNull())
      return nullptr;

    if (ParamExpression.is<clang::TypeSourceInfo *>()) {
      auto *TypeParam = ParamExpression.get<clang::TypeSourceInfo *>();
      clang::TemplateArgument Arg(TypeParam->getType());
      TemplateArgs.addArgument({Arg, TypeParam});
      ActualArgs.emplace_back(Arg);
    } else {
      clang::TemplateArgument Arg(ParamExpression.get<clang::Expr *>(),
                                  clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ParamExpression.get<clang::Expr *>()});
      ActualArgs.emplace_back(Arg);
    }
  }
  clang::TemplateArgumentList TemplateArgList(
      clang::TemplateArgumentList::OnStack, ActualArgs);
  if (OverloadExpr->getNumDecls() == 1) {
    clang::NamedDecl *ND = *OverloadExpr->decls_begin();
    if (isa<clang::TemplateDecl>(ND)) {
      
      // We need to instantiate the template with parameters.
      if (clang::UnresolvedMemberExpr *MemAccess
              = dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
        clang::FunctionTemplateDecl *FTD
                                = dyn_cast<clang::FunctionTemplateDecl>(ND);
        clang::FunctionDecl *FD
            = SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
              &TemplateArgList, Elem->getLoc());
        if (!FD) {
          // TODO: Create error message for this.
          llvm_unreachable("Function template instantiation failure.");
        }
        SemaRef.getCxxSema().InstantiateFunctionDefinition(
            Elem->getLoc(), FD, true, true, false);
        return clang::MemberExpr::Create(Context.CxxAST,
            MemAccess->getBase(), MemAccess->isArrow(),
            MemAccess->getOperatorLoc(), MemAccess->getQualifierLoc(),
            clang::SourceLocation(), FD,
            clang::DeclAccessPair::make(FD, ND->getAccess()),
            MemAccess->getMemberNameInfo(), &TemplateArgs, E->getType(),
            MemAccess->getValueKind(), MemAccess->getObjectKind(),
            clang::NonOdrUseReason::NOUR_None);
      } else {
        llvm_unreachable("We don't have code for processing of non-member "
            "lookup expressions.");
      }
    } else if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(ND)) {
      if (FD->getTemplatedKind() == clang::FunctionDecl::TK_NonTemplate) {
        // TODO: Create error message for here.
        llvm_unreachable("Function is not a template unable to continue.");
      }
      clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate();
      if (!FTD) {
        // TODO: Create an error message for here.
        llvm_unreachable("Function doesn't have any template parameters.");
      }
      clang::FunctionDecl *InstantiatedFunc
          = SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
            &TemplateArgList, Elem->getLoc());
      SemaRef.getCxxSema().InstantiateFunctionDefinition(
          Elem->getLoc(), InstantiatedFunc, true, true, false);
      clang::LookupResult ResultTemp(SemaRef.getCxxSema(),
                                      OverloadExpr->getNameInfo(),
                                      clang::Sema::LookupAnyName);
      ResultTemp.addDecl(InstantiatedFunc);
      return clang::UnresolvedLookupExpr::Create(Context.CxxAST, 
                                                 OverloadExpr->getNamingClass(),
                                                OverloadExpr->getQualifierLoc(),
                                                 // OverloadExpr->getNameLoc(),
                                                 OverloadExpr->getNameInfo(),
                                                 /*ADL=*/true, false,
                                                 ResultTemp.begin(),
                                                 ResultTemp.end());
    }
    llvm_unreachable("Unknown unresolved lookup type located. Unable to "
        "continue.");
  } else {
    


    if (isa<clang::UnresolvedLookupExpr>(OverloadExpr)) {
      clang::LookupResult ResultTemp(SemaRef.getCxxSema(),
                                      OverloadExpr->getNameInfo(),
                                      clang::Sema::LookupAnyName);
      ResultTemp.setTemplateNameLookup(true);
      for (clang::NamedDecl *ND : OverloadExpr->decls()) {
        if(clang::FunctionDecl *FD = ND->getAsFunction())
          if(clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate()) {
            ResultTemp.addDecl(FTD);
          }
      }
      ResultTemp.resolveKind();
      if (ResultTemp.empty()) {
        // TODO: Create an error message for here. This should indicate that we
        // don't have a valid template to instantiate.
        llvm_unreachable("None of the given names were a template.");
      }
      return clang::UnresolvedLookupExpr::Create(Context.CxxAST, 
                                                 OverloadExpr->getNamingClass(),
                                                OverloadExpr->getQualifierLoc(),
                                                 OverloadExpr->getNameLoc(),
                                                 OverloadExpr->getNameInfo(),
                                                 /*ADL=*/true, &TemplateArgs,
                                                 ResultTemp.begin(),
                                                 ResultTemp.end());
      
    }
    if(clang::UnresolvedMemberExpr *UME = 
                          dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
      clang::LookupResult ResultTemp(SemaRef.getCxxSema(),
                                      OverloadExpr->getNameInfo(),
                                      clang::Sema::LookupAnyName);
      ResultTemp.setTemplateNameLookup(true);
      for (clang::NamedDecl *ND : OverloadExpr->decls()) {
        ND = SemaRef.getCxxSema().getAsTemplateNameDecl(ND, true, true);
        if(ND)
          if(clang::FunctionDecl *FD = ND->getAsFunction())
            if(clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate()) {
              // clang::FunctionDecl *InstantiatedFunc
              //     = SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
              //       &TemplateArgList, Elem->getLoc());
              // if (InstantiatedFunc) {
              //   SemaRef.getCxxSema().InstantiateFunctionDefinition(
              //       Elem->getLoc(), InstantiatedFunc, true, true, false);
              //   ResultTemp.addDecl(InstantiatedFunc);
              // }
              ResultTemp.addDecl(FTD);
            }
      }
      ResultTemp.resolveKind();
      if (ResultTemp.empty()) {
        // TODO: Create an error message for here. This should indicate that we
        // don't have a valid template to instantiate.
        llvm_unreachable("None of the given names were a template.");
      }
      return clang::UnresolvedMemberExpr::Create(Context.CxxAST,
                                                 UME->hasUnresolvedUsing(),
                                                 UME->getBase(),
                                                 UME->getBaseType(),
                                                 UME->isArrow(),
                                                 UME->getOperatorLoc(),
                                                 UME->getQualifierLoc(),
                                                 UME->getTemplateKeywordLoc(),
                                                 UME->getNameInfo(),
                                                 &TemplateArgs,
                                                 ResultTemp.begin(),
                                                 ResultTemp.end());
    }
    llvm_unreachable("Unhandled type of overload.");
  }
  llvm_unreachable("This should never occur all other paths lead to return "
      "or abort.");
}

Expression ExprElaborator::elaborateElementExpr(const ElemSyntax *Elem) {
  Expression IdExpr = elaborateExpr(Elem->getObject());
  if (IdExpr.is<clang::NamespaceDecl *>()) {
    llvm_unreachable("Nested Templated namespace don't exist.");
    return nullptr;
  }
  if (IdExpr.is<clang::TypeSourceInfo *>()) {
    return HandleClassTemplateSelection(*this, SemaRef, Context,
      IdExpr.get<clang::TypeSourceInfo*>(), Elem);
  }

  if (clang::Expr *E = IdExpr.dyn_cast<clang::Expr *>()) {
    return handleElementExpression(*this, SemaRef, Context, Elem, E);
  }

  llvm_unreachable("Unable to handle indexing into given expression within the AST.");
}

static ExprElaborator::Expression
createIdentAccess(SyntaxContext &Context, Sema &SemaRef, const AtomSyntax *S,
                  clang::QualType Ty, clang::SourceLocation Loc) {


  clang::ASTContext &CxxAST = Context.CxxAST;
  clang::IdentifierInfo &Id = CxxAST.Idents.get(S->getSpelling());
  clang::DeclarationNameInfo DNI({&Id}, Loc);
  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  R.setTemplateNameLookup(true);

  if (SemaRef.isQualifiedLookupContext())
    SemaRef.lookupQualifiedName(R);
  else
    SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope());

  if (!R.empty()) {
    R.resolveKind();
    if (!R.isSingleResult()) {
      if (R.isAmbiguous()) {
        SemaRef.Diags.Report(S->getLoc(), clang::diag::err_multiple_declarations);
        return nullptr;
      }
      if (R.isOverloadedResult()) {
        // Need to figure out if the potential overload is a member function
        // or not.
        return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
            R.getNamingClass(), clang::NestedNameSpecifierLoc(),
            R.getLookupNameInfo(), /*ADL=*/true, true,
            R.begin(), R.end());
      }

      // TODO: FIXME: Create error message for here.
      llvm_unreachable("We are not currently handling multiple declarations "
          "returned. This needs to be fixed in order to correctly create proper "
          "results that can be returned to the caller.");
      // This needs to be changed because we are literally looking up a
      // multitude of things, and this is only an error in some of the cases,
      // for example if we a set of function overloads then this isn't going to
      // work correctly and we may need to simply return access to a function
      // address rather then something else?

      return nullptr;
    }

    if(clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>()) {
      clang::QualType FoundTy = VD->getType();
      // If the user annotated the DeclRefExpr with an incorrect type.
      if (!Ty.isNull() && Ty != FoundTy) {
        SemaRef.Diags.Report(Loc, clang::diag::err_type_annotation_mismatch)
          << FoundTy << Ty;
        return nullptr;
      }

      if (isa<clang::FieldDecl>(VD)) {
        // Building this access.
        clang::FieldDecl* Field = cast<clang::FieldDecl>(VD);
        clang::RecordDecl* RD = Field->getParent();
        // FIXME: Add CV qualifiers here if needed
        clang::QualType ThisTy(RD->getTypeForDecl(), 0);
        clang::QualType ThisPtrTy = SemaRef.getContext().CxxAST.getPointerType(ThisTy);
        clang::Expr* This = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
            ThisPtrTy, true);
        // FIXME: I may need to change the access specifier look up in this case.
        // Because otherwise we are skirting access to the AccessSpecifier.
        clang::DeclAccessPair FoundDecl = clang::DeclAccessPair::make(Field,
              clang::AccessSpecifier::AS_public);
        clang::CXXScopeSpec SS;
        clang::ExprResult MemberExpr
            = SemaRef.getCxxSema().BuildFieldReferenceExpr(
              This, true, clang::SourceLocation(), SS, Field, FoundDecl, DNI
            );
        clang::Expr *Ret = MemberExpr.get();
        if (!Ret) {
          SemaRef.Diags.Report(Loc, clang::diag::err_no_member)
              << Field << ThisTy;
        }
        ExprMarker(Context.CxxAST, SemaRef).Visit(Ret);
        return Ret;
      }
      // Need to check if the result is a CXXMethodDecl because that's a
      // ValueDecl.
      if(isa<clang::CXXMethodDecl>(VD)) {
        clang::CXXScopeSpec SS;
        clang::SourceLocation Loc;
        // This may need to change into a different type of function call
        // base on given arguments, because this could be an issue.
        return SemaRef.getCxxSema().BuildPossibleImplicitMemberExpr(
            SS, Loc, R, nullptr, SemaRef.getCurClangScope()).get();
      }

      if(isa<clang::FunctionDecl>(VD)) {
        return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
            R.getNamingClass(), clang::NestedNameSpecifierLoc(),
            R.getLookupNameInfo(), /*ADL=*/true, true,
            R.begin(), R.end());
      }

      // Checking if the current declaration is a variable.
      // FIXME: discern whether this is an lvalue or rvalue properly
      clang::DeclRefExpr *DRE =
        clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                  clang::SourceLocation(), VD, /*Capture=*/false,
                                  Loc, FoundTy, clang::VK_LValue, VD);
      return DRE;
    }

    // Processing the case when the returned result is a type.
    if (const clang::TagDecl *TD = R.getAsSingle<clang::TagDecl>())
      return BuildAnyTypeLoc(CxxAST, CxxAST.getTypeDeclType(TD), Loc);

    if (const clang::NamespaceDecl *NS = R.getAsSingle<clang::NamespaceDecl>())
      return const_cast<clang::NamespaceDecl *>(NS);
    if (const auto *NS = R.getAsSingle<clang::CppxNamespaceDecl>())
      return const_cast<clang::CppxNamespaceDecl *>(NS);
  }

  // TODO: FIXME: Create error reporting here for lookup failure.
  return nullptr;
}

Expression ExprElaborator::elaborateAtom(const AtomSyntax *S,
                                         clang::QualType ExplicitType) {
  Token T = S->Tok;

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(CxxAST, T, ExplicitType,
                                S->getLoc());
  case tok::DecimalFloat:
    return createFloatLiteral(CxxAST, T, ExplicitType,
                              S->getLoc());
    break;
  case tok::BinaryInteger: {
    std::string TData = std::string(T.getSymbol().data());
    TData =  TData.substr(TData.find_first_not_of("0b"), TData.size());
    Token RawBin = Token(tok::BinaryInteger, T.getLocation(), Symbol(&TData));
    return createIntegerLiteral(CxxAST, RawBin, ExplicitType,
                                S->getLoc(), /*Base=*/2);
  }

  case tok::HexadecimalInteger:
    return createIntegerLiteral(CxxAST, T, ExplicitType,
                                S->getLoc(), /*Base=*/16);
  case tok::HexadecimalFloat:
    llvm::errs() << "elaborateAtom::HexadecimalFloat not implemented.\n";
    break;
  case tok::Identifier:
    return createIdentAccess(Context, SemaRef, S, ExplicitType, S->getLoc());
  case tok::Character:
    return createCharLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::HexadecimalCharacter:
    return createUTF8Literal(CxxAST, SemaRef, T, S->getLoc());
  case tok::UnicodeCharacter:
    return createUnicodeLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::String:
    llvm::errs() << "elaborateAtom::String not implemented.\n";
    break;
  case tok::DecimalExponent:
    return createExponentLiteral(CxxAST, SemaRef, T, S->getLoc());

  /// Keyword Literals
  case tok::TrueKeyword:
  case tok::FalseKeyword:
    return createBoolLiteral(CxxAST, T, S->getLoc());

  case tok::NullKeyword:
    return createNullLiteral(CxxAST, S->getLoc());

  case tok::VoidKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.VoidTy, S->getLoc());
  case tok::BoolKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.BoolTy, S->getLoc());
  case tok::CharKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CharTy, S->getLoc());
  case tok::Char8Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Char8Ty, S->getLoc());
  case tok::Char16Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Char16Ty, S->getLoc());
  case tok::Char32Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Char32Ty, S->getLoc());
  case tok::IntKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.IntTy, S->getLoc());
  case tok::Int8Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Int8Ty, S->getLoc());
  case tok::Int16Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Int16Ty, S->getLoc());
  case tok::Int32Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Int32Ty, S->getLoc());
  case tok::Int64Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Int64Ty, S->getLoc());
  case tok::Int128Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Int128Ty, S->getLoc());
  case tok::UintKeyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UIntTy, S->getLoc());
  case tok::Uint8Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UInt8Ty, S->getLoc());
  case tok::Uint16Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UInt16Ty, S->getLoc());
  case tok::Uint32Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UInt32Ty, S->getLoc());
  case tok::Uint64Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UInt64Ty, S->getLoc());
  case tok::Uint128Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.UInt128Ty, S->getLoc());
  case tok::Float16Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Float16Ty, S->getLoc());
  case tok::Float32Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Float32Ty, S->getLoc());
  case tok::Float64Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Float64Ty, S->getLoc());
  case tok::Float128Keyword:
    return BuildAnyTypeLoc(CxxAST, SemaRef.Float128Ty, S->getLoc());
  case tok::TypeKeyword:
    return BuildAnyTypeLoc(CxxAST, CxxAST.CppxKindTy, S->getLoc());

  default: break;
  }

  return nullptr;
}

static bool buildFunctionCallAruments(Sema &SemaRef, SyntaxContext &Context,
    const ListSyntax *ArgList, 
    llvm::SmallVector<clang::Expr *, 8> &Args) {
  for (const Syntax *A : ArgList->children()) {
    ExprElaborator Elab(Context, SemaRef);
    Expression Argument = Elab.elaborateExpr(A);

    // FIXME: What kind of expression is the unary ':typename' expression?
    if (Argument.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
      return true;
    }

    Args.push_back(Argument.get<clang::Expr *>());
  }
  return false;
}

/// This function's job is to create the correct call based upon the result
/// type of the CalleeExpr, which could be any of the types within the
/// Expression union type.
static Expression handleExpressionResultCall(Sema &SemaRef,
    const CallSyntax *S, Expression CalleeExpr,
    llvm::SmallVector<clang::Expr *, 8> &Args) {
  if (CalleeExpr.isNull()) {
    // TODO: Create error message for this.
    return nullptr;
  }
  if (clang::Expr *E = CalleeExpr.dyn_cast<clang::Expr *>()) {
    clang::ExprResult Call =
      SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                        E, S->getCalleeLoc(),
                                        Args, S->getCalleeLoc());
    return Call.get();
  }

  if (clang::TypeSourceInfo *TInfo
                             = CalleeExpr.dyn_cast<clang::TypeSourceInfo *>()) {
    // llvm_unreachable("Constructor calls have not been implemented yet.");
    clang::ExprResult ConstructorExpr =
        SemaRef.getCxxSema().BuildCXXTypeConstructExpr(TInfo, S->getLoc(),
                                                      Args, S->getLoc(), false);
    if (!ConstructorExpr.get()) {
      SemaRef.Diags.Report(S->getLoc(),
                            clang::diag::err_coroutine_invalid_func_context)
                            << TInfo->getType() << "a constructor";
      return nullptr;
    }
    return ConstructorExpr.get();
  }
  if (CalleeExpr.is<clang::NamespaceDecl *>()) {
    // TODO: Create a more appropriate error messaage for here.
    llvm_unreachable("Cannot call a namespace.");
    
  }
  llvm_unreachable("Invalid expression result type.");
}

static bool callIsCastOperator(const CallSyntax *S) {
  if (const ElemSyntax *Elem = dyn_cast<ElemSyntax>(S->getCallee())) {
    if (const AtomSyntax *Callee
        = clang::dyn_cast<AtomSyntax>(Elem->getObject())) {
      clang::StringRef Name = Callee->getSpelling();
      return Name == "static_cast" || Name == "dynamic_cast"
          || Name == "const_cast" || Name == "reinterpret_cast";
    }
  }
  return false;
}

Expression ExprElaborator::elaborateCall(const CallSyntax *S) {
  if (callIsCastOperator(S)) {
    return elaborateCastOp(S);
  }
  // Determining the type of call associated with the given syntax.
  // There are multiple kinds of atoms for multiple types of calls
  // but in the event that the callee object is not an Atom, it means
  // that we have to process the sub expression as normal.
  Expression CalleeExpr;
  if (!isa<AtomSyntax>(S->getCallee())) {
    CalleeExpr = elaborateExpr(S->getCallee());
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
    if (buildFunctionCallAruments(SemaRef, Context, ArgList, Args)) {
      // TODO: Determine the correct message to output here.
      return nullptr;
    }
    return handleExpressionResultCall(SemaRef, S, CalleeExpr, Args);
  }

  // In the event that we do have an atom for the call name we need to do
  // something slightly different before we can fully elaborate the entire call.
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  FusedOpKind Op = getFusedOpKind(SemaRef, Callee->getSpelling());

  switch (Op) {
  case FOK_MemberAccess: {
    const ListSyntax *Args = cast<ListSyntax>(S->getArguments());

    if (Args->getNumChildren() == 1)
      return elaborateGlobalNNS(S, Args->getChild(0));
    return elaborateMemberAccess(Args->getChild(0), S, Args->getChild(1));
  }

  case FOK_DotDot:
    return handleOperatorDotDot(S);

  case FOK_Const:
    return handleOperatorConst(S);
  case FOK_Ref:
    return handleRefType(S);
  case FOK_RRef:
    return handleRRefType(S);
  case FOK_Arrow:
    return handleFunctionType(S);
  default:
    break;
  }

  llvm::StringRef Spelling = Callee->getSpelling();
  if (Spelling.startswith("operator'")) {
    if (S->getNumArguments() == 1) {
      clang::UnaryOperatorKind UnaryOpKind;
      if (!SemaRef.GetUnaryOperatorKind(Spelling, UnaryOpKind))
        return elaborateUnaryOp(S, UnaryOpKind);
    }

    if (S->getNumArguments() == 2) {
      // Check if this is a binary operator.
      clang::BinaryOperatorKind BinaryOpKind;
      if (!SemaRef.GetBinaryOperatorKind(Spelling, BinaryOpKind))
        return elaborateBinOp(S, BinaryOpKind);
    }
  }

  // Elaborating callee name expression.
  CalleeExpr = elaborateExpr(S->getCallee());
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  if (buildFunctionCallAruments(SemaRef, Context, ArgList, Args)) {
    // TODO: Determine the correct message to output here.
    return nullptr;
  }
  return handleExpressionResultCall(SemaRef, S, CalleeExpr, Args);
}


Expression ExprElaborator::elaborateCastOp(const CallSyntax *CastOp) {
  if (CastOp->getNumArguments() != 1) {
    SemaRef.Diags.Report(CastOp->getLoc(),
        clang::diag::err_invalid_cast_arg_count);
    return nullptr;
  }
  // Verify that the syntax is correct here, meaning that if we don't have 
  // a single type argument and single call argument then we are not valid.
  const ElemSyntax *Elem = dyn_cast<ElemSyntax>(CastOp->getCallee());
  const AtomSyntax *Callee = clang::dyn_cast<AtomSyntax>(Elem->getObject());
  llvm::SmallVector<const Syntax *, 1> TypeArgs;
  const Syntax *Args = Elem->getArguments();
  const VectorNode<Syntax> *TypeArgumentList = nullptr;
  
  if (const ListSyntax *LS = dyn_cast<ListSyntax>(Args)) {
    TypeArgumentList = LS;
  } else if (const ArraySyntax *AS = dyn_cast<ArraySyntax>(Args)) {
      TypeArgumentList = AS;
  } else {
    CastOp->dump();
    llvm_unreachable("Improperly formatted AST.");
  }
  if (TypeArgumentList->getNumChildren() != 1) {
    SemaRef.Diags.Report(CastOp->getLoc(),
        clang::diag::err_invalid_cast_type_arg_count);
    return nullptr;
  }

  clang::StringRef Name = Callee->getSpelling();
  clang::tok::TokenKind CastKind;
  if (Name == "static_cast") {
    CastKind = clang::tok::TokenKind::kw_static_cast;
  } else if (Name == "dynamic_cast") {
    CastKind = clang::tok::TokenKind::kw_dynamic_cast;
  } else if (Name == "reinterpret_cast") {
    CastKind = clang::tok::TokenKind::kw_reinterpret_cast;
  } else if (Name == "const_cast") {
    CastKind = clang::tok::TokenKind::kw_const_cast;
  } else {
    llvm_unreachable("Invalid cast name.");
  }

  Expression DestinationType = elaborateExpr(Elem->getArgument(0));
  if (DestinationType.isNull()) {
    SemaRef.Diags.Report(Elem->getArgument(0)->getLoc(),
                         clang::diag::err_invalid_cast_destination_type);
    return nullptr;
  }
  if (!DestinationType.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(Elem->getArgument(0)->getLoc(),
                         clang::diag::err_invalid_cast_destination_type);
    return nullptr;
  }

  Expression Source = elaborateExpr(CastOp->getArgument(0));
  if (Source.isNull()) {
    SemaRef.Diags.Report(CastOp->getArgument(0)->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }
  if (!Source.is<clang::Expr *>()) {
    SemaRef.Diags.Report(CastOp->getArgument(0)->getLoc(),
                         clang::diag::err_invalid_cast_source);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = DestinationType.get<clang::TypeSourceInfo*>();
  auto CastExpr = SemaRef.getCxxSema().BuildCXXNamedCast(
    Callee->getLoc(), CastKind, TInfo, Source.get<clang::Expr*>(),
    CastOp->getArgument(0)->getLoc(), CastOp->getArgument(0)->getLoc()
  );
  return CastExpr.get();
}



Expression ExprElaborator::elaborateMemberAccess(const Syntax *LHS,
    const CallSyntax *Op, const Syntax *RHS) {
  Expression ElaboratedLHS = elaborateExpr(LHS);

  if (ElaboratedLHS.isNull()) {
    SemaRef.Diags.Report(LHS->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  if (ElaboratedLHS.is<clang::Expr *>()) {
    if (isa<AtomSyntax>(RHS)) {
      // TODO: I need an example where this isn't an atom but I'm not sure
      // one exists yet.
      const AtomSyntax *RHSAtom = cast<AtomSyntax>(RHS);
      // TODO: figure out how to make the pointer work correctly?

      clang::UnqualifiedId Id;
      clang::IdentifierInfo *IdInfo = &Context.CxxAST.Idents.get(
        RHSAtom->getSpelling());
      Id.setIdentifier(IdInfo, RHSAtom->getLoc());
      clang::CXXScopeSpec SS;
      clang::SourceLocation Loc;
      clang::ExprResult RHSExpr = SemaRef.getCxxSema().ActOnMemberAccessExpr(
        SemaRef.getCurClangScope(), ElaboratedLHS.get<clang::Expr*>(), Op->getLoc(),
        clang::tok::TokenKind::period, SS, Loc, Id, nullptr);
      if (RHSExpr.get()) {
        ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr.get());
      } else {
        // TODO: Need to create error message for here.
        llvm_unreachable("We were not able to elaborate the member access "
            "expression.\n");
      }
      return RHSExpr.get();
    }

    llvm_unreachable("Member access from non-variables unimplemented.");
  } else if (ElaboratedLHS.is<clang::TypeSourceInfo *>()) {
    return elaborateNestedLookUpAccess(ElaboratedLHS, Op, RHS);
  } else if (ElaboratedLHS.is<clang::CppxNamespaceDecl *>()) {
    auto *NS = ElaboratedLHS.get<clang::CppxNamespaceDecl *>();
    return elaborateNNS(NS, Op, RHS);
  }

  llvm_unreachable("Unimplemented access expression");
}

Expression ExprElaborator::elaborateNNS(clang::CppxNamespaceDecl *NS,
                                        const CallSyntax *Op,
                                        const Syntax *RHS) {
  const clang::CppxNamespaceType *NSTy =
    NS->getTypeForDecl()->getAs<clang::CppxNamespaceType>();
  clang::NamespaceDecl *CxxNS = NSTy->getDecl();

  // FIXME: create the correct ObjectType (last param) that is used when this
  // NNS appears as after an operator'.' of an object.
  clang::Sema::NestedNameSpecInfo IdInfo(CxxNS->getIdentifier(),
                                         CxxNS->getBeginLoc(),
                                         Op->getLoc(), clang::QualType());

  // Look this up as an NNS.
  bool EnteringContext = SemaRef.isQualifiedLookupContext();
  bool Failure = SemaRef.getCxxSema().
    ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                EnteringContext, SemaRef.CurNNSContext,
                                /*RecoveryLookup=*/false,
                                /*IsCorrected=*/nullptr,
                                /*OnlyNamespace=*/false);
  if (Failure) {
    llvm::outs() << "failed to create nns\n";
    return nullptr;
  }

  // FIXME: handle templated types

  Sema::QualifiedLookupRAII Qual(SemaRef, SemaRef.QualifiedLookupContext, &NS);
  Expression RHSExpr = ExprElaborator(Context, SemaRef).elaborateExpr(RHS);

  if (RHSExpr.isNull())
    return nullptr;

  if (RHSExpr.is<clang::Expr *>()) {
    SemaRef.CurNNSContext.clear();
    clang::Expr *Ret = RHSExpr.get<clang::Expr *>();
    ExprMarker(Context.CxxAST, SemaRef).Visit(Ret);
    return Ret;
  }

  if (RHSExpr.is<clang::CppxNamespaceDecl *>())
    return RHSExpr.get<clang::CppxNamespaceDecl *>(); 

  SemaRef.Diags.Report(RHS->getLoc(), clang::diag::err_failed_to_translate_expr);
  return nullptr;
}

Expression ExprElaborator::elaborateGlobalNNS(const CallSyntax *Op,
                                              const Syntax *RHS) {
  bool Failure = SemaRef.getCxxSema().
    ActOnCXXGlobalScopeSpecifier(Op->getLoc(), SemaRef.CurNNSContext);

  if (Failure) {
    llvm::outs() << "failed to create nns\n";
    return nullptr;
  }

  gold::Scope *GlobalScope = SemaRef.getCurrentScope();
  while (GlobalScope->getParent())
    GlobalScope = GlobalScope->getParent();
  auto *NS = clang::CppxNamespaceDecl::Create(
    Context.CxxAST, Context.CxxAST.getTranslationUnitDecl(),
    clang::SourceLocation(), nullptr, nullptr, GlobalScope);

  Sema::QualifiedLookupRAII Qual(SemaRef, SemaRef.QualifiedLookupContext, &NS);
  Expression RHSExpr = ExprElaborator(Context, SemaRef).elaborateExpr(RHS);

  if (RHSExpr.is<clang::Expr *>()) {
    SemaRef.CurNNSContext.clear();
    clang::Expr *Ret = RHSExpr.get<clang::Expr *>();
    ExprMarker(Context.CxxAST, SemaRef).Visit(Ret);
    return Ret;
  }

  if (RHSExpr.is<clang::CppxNamespaceDecl *>())
    return RHSExpr.get<clang::CppxNamespaceDecl *>(); 

  SemaRef.Diags.Report(RHS->getLoc(), clang::diag::err_failed_to_translate_expr);
  return nullptr;
}

static ExprElaborator::Expression handleLookUpInsideType(Sema &SemaRef,
    clang::ASTContext &CxxAST, Expression Previous,
    const CallSyntax *Op, const Syntax *RHS) {
  clang::TypeSourceInfo *TInfo = Previous.get<clang::TypeSourceInfo*>();
  clang::QualType QT = TInfo->getType();
  const clang::Type *T = QT.getTypePtrOrNull();
  clang::TagDecl *TD = T->getAsTagDecl();
  if (!TD) {
    // TODO: Figure out the appropriate diagnostic message to output here.
    // SemaRef.Diags.Report(LHS->getLoc(), clang::diag::err_no_member)
    //     << TD << Op.Loc;
    llvm::errs() << "Type " << TD->getNameAsString()
                 << " doesn't have any members.";
    return nullptr;
  }

  // Processing if is a single name.
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(RHS)) {
    // clang::DeclarationName 
    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())},
      Atom->getLoc());
    auto R = TD->lookup(DNI.getName());
    if (R.size() != 1u) {
      SemaRef.Diags.Report(RHS->getLoc(), clang::diag::err_no_member)
        << Atom->getSpelling() << TD;
      return nullptr;
    }
    // R.front();
    clang::NamedDecl *ND = R.front();
    if (clang::TypeDecl *TD = dyn_cast<clang::TypeDecl>(ND)) {
      TD->setIsUsed();
      clang::QualType Ty = CxxAST.getTypeDeclType(TD);
      return BuildAnyTypeLoc(CxxAST, Ty, RHS->getLoc());
    }
    if (clang::NamespaceDecl *NsDecl = dyn_cast<clang::NamespaceDecl>(ND)) {
      return NsDecl;
    }
    if (clang::VarDecl *VDecl = dyn_cast<clang::VarDecl>(ND)) {
      // This is how we access static member variables.
      return clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                        clang::SourceLocation(),VDecl,
                                        /*Capture=*/false, RHS->getLoc(),
                                        VDecl->getType(), clang::VK_LValue);
    }

    // FIXME: This needs to support referencing base members by qualified name.
    llvm_unreachable("Direct referencing of member variables it not "
        "permitted yet.");
  }

  llvm_unreachable("Unknown syntax encountered during nested member lookup.");
}

Expression ExprElaborator::elaborateNestedLookUpAccess(Expression Previous,
                                                       const CallSyntax *Op,
                                                       const Syntax *RHS) {
  assert(!Previous.isNull() && "Expression scoping.");
  if (Previous.is<clang::TypeSourceInfo*>()) {
    return handleLookUpInsideType(SemaRef, Context.CxxAST, Previous, Op, RHS);
  }

  if (Previous.is<clang::NamespaceDecl*>()) {
    llvm_unreachable("Nested namespace declarations not implemented");
  }

  if (Previous.is<clang::Expr *>()) {
    // assert(!"Nested access to static variables it no implemented yet.");
    llvm_unreachable("Nested access to static variables not implemented");
  }

  llvm_unreachable("Expression type not an expression, type, or namespace");
}

Expression ExprElaborator::elaborateUnaryOp(const CallSyntax *S,
                                            clang::UnaryOperatorKind Op) {
  const Syntax *Operand = S->getArgument(0);
  Expression OperandResult = elaborateExpr(Operand);
  if (OperandResult.isNull() || OperandResult.is<clang::NamespaceDecl *>()) {
    SemaRef.Diags.Report(Operand->getLoc(),
      clang::diag::err_expected_expression);
    return nullptr;
  }

  // This is used to construct a pointer type because the carrot has two
  // meanings. Dereference and pointer declaration.
  if (Op == clang::UO_Deref) {
    if (OperandResult.is<clang::TypeSourceInfo *>()) {
      clang::QualType RetType = Context.CxxAST.getPointerType(
                        OperandResult.get<clang::TypeSourceInfo*>()->getType());
      return BuildAnyTypeLoc(Context.CxxAST, RetType, S->getLoc());
    } 
  } else if (OperandResult.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(Operand->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }
  clang::ExprResult UnaryOpRes = SemaRef.getCxxSema().BuildUnaryOp(
                                                              /*scope*/nullptr,
                                                              S->getCalleeLoc(),
                                                              Op,
                                             OperandResult.get<clang::Expr*>());

  ExprMarker(Context.CxxAST, SemaRef).Visit(OperandResult.get<clang::Expr *>());
  return UnaryOpRes.get();
}

Expression ExprElaborator::elaborateBinOp(const CallSyntax *S,
                                          clang::BinaryOperatorKind Op) {
  const Syntax *LHSSyntax = S->getArgument(0);
  const Syntax *RHSSyntax = S->getArgument(1);

  Expression LHS = elaborateExpr(LHSSyntax);
  if (LHS.is<clang::TypeSourceInfo *>() || LHS.isNull()) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(),
        clang::diag::err_expected_expression);
    return nullptr;
  }

  Expression RHS = elaborateExpr(RHSSyntax);
  if (RHS.is<clang::TypeSourceInfo *>() || RHS.isNull()) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(),
        clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  // FIXME: Replace with ActOnBinOp so precedence issues get warnings.
  clang::ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr,
                                               S->getLoc(), Op,
                                               LHS.get<clang::Expr *>(),
                                               RHS.get<clang::Expr *>());
  if (Res.isInvalid()) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  ExprMarker(Context.CxxAST, SemaRef).Visit(LHS.get<clang::Expr *>());
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHS.get<clang::Expr *>());

  return Res.get();
}

/// Create an expression for a block condition. Ex:
///
/// \code
/// if:
///   expr_1
///   expr_2
///   ...
///   expr_n
/// \endcode
/// We just create a logical and expression with n terms: one for each
/// sub expression.
Expression
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  Expression LHS, RHS;

  {
    ExprElaborator ExEl(Context, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));

    if (LHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(1));

    if (RHS.is<clang::TypeSourceInfo *>()) {
      SemaRef.Diags.Report(Conditions->getChild(1)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }

  clang::ExprResult BinOp =
    SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                    clang::tok::ampamp,
                                    LHS.get<clang::Expr *>(),
                                    RHS.get<clang::Expr *>());
  if (BinOp.isInvalid()) {
    SemaRef.Diags.Report(Conditions->getLoc(),
                         clang::diag::err_invalid_block_condition);
    return nullptr;
  }

  // For all remaining terms, append them to the back of the && expression.
  // Ex., if we had `1 && 2`, we would append `3` to get `1 && 2 && 3`.
  for (unsigned I = 2; I < Conditions->getNumChildren(); ++I) {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(I));

    BinOp =
      SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                      clang::tok::ampamp, BinOp.get(),
                                      RHS.get<clang::Expr *>());
    if (BinOp.isInvalid()) {
      SemaRef.Diags.Report(Conditions->getLoc(),
                           clang::diag::err_invalid_block_condition);
      return nullptr;
    }
  }

  return BinOp.get();
}

static clang::Expr *handleArrayMacro(SyntaxContext &Context, Sema &SemaRef,
                                     const MacroSyntax *S) {
  const ArraySyntax *ArrayInit = cast<ArraySyntax>(S->getBlock());
  const ListSyntax *Init = cast<ListSyntax>(ArrayInit->getChild(0));

  llvm::SmallVector<clang::Expr *, 8> Elements;
  for (const Syntax *SI :  Init->children()) {
    Expression Element = ExprElaborator(Context, SemaRef).elaborateExpr(SI);

    if (Element.is<clang::TypeSourceInfo *>() || Element.isNull())
      return nullptr;

    Elements.push_back(Element.get<clang::Expr *>());
  }

  clang::ExprResult InitList =
    SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Elements,
                                       S->getLoc());
  if (InitList.isInvalid())
    return nullptr;

  return InitList.get();
}

Expression ExprElaborator::elaborateMacro(const MacroSyntax *S) {
  assert (isa<AtomSyntax>(S->getCall()) && "Unexpected macro call");

  const AtomSyntax *Call = cast<AtomSyntax>(S->getCall());

  if (Call->getSpelling() == "if")
    assert(false && "If expression processing not implemented yet.");
  else if (Call->getSpelling() == "while")
    assert(false && "while loop processing not implemented yet.");
  else if(Call->getSpelling() == "for")
    assert(false && "For loop processing not implemented yet.");
  else if (Call->getSpelling() == "array")
    return handleArrayMacro(Context, SemaRef, S);
  else{
    // FIXME: Need to handle any other conditions here.
    assert(false && "Unsupported macro");
  }
}




//===----------------------------------------------------------------------===//
//                        Type Expression Elaboration                         //
//===----------------------------------------------------------------------===//
// Get a vector of declarators.
static void getDeclarators(Declarator *D,
                           llvm::SmallVectorImpl<Declarator *> &Decls) {
  while (D) {
    Decls.push_back(D);
    D = D->Next;
  }
}

Expression ExprElaborator::elaborateTypeExpr(Declarator *D) {
  // The type of a declarator is constructed back-to-front.
  llvm::SmallVector<Declarator *, 4> Decls;
  getDeclarators(D, Decls);

  // The type is computed from back to front. Start by assuming the type
  // is auto. This will be replaced if an explicit type specifier is given.
  clang::QualType AutoType = CxxAST.getAutoDeductType();
  TypeInfo *TInfo = BuildAnyTypeLoc(CxxAST, AutoType, D->getLoc());
  for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
    D = *Iter;
    switch (D->Kind) {
    case DK_Identifier:
      // The identifier is not part of the type.
      break;
      
    case DK_Error:
      // If we find an error we exit because we can't continue.
      return nullptr;

    case DK_Pointer: {
      Expression TypeExpr = elaboratePointerType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Const: {
      Expression TypeExpr = elaborateConstType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Ref:{
      Expression TypeExpr = elaborateRefType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_RRef:{
      Expression TypeExpr = elaborateRRefType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Array: {
      Expression TypeExpr = elaborateArrayType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Function: {
      Expression TypeExpr = elaborateFunctionType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }

    case DK_Type: {
      Expression TypeExpr = elaborateExplicitType(D, TInfo);
      if (TypeExpr.isNull())
        return nullptr;

      TInfo = TypeExpr.get<TypeInfo *>();
      break;
    }
    
    case DK_TemplateType:{
      llvm_unreachable("Template types are not directly part of declarator.");
      break;
    }
    default:
      llvm_unreachable("Invalid declarator");
    }
  }
  return TInfo;
}

Expression ExprElaborator::elaboratePointerType(Declarator *D, TypeInfo *Ty) {
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (BaseTypeExpr.is<clang::Expr *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType =
    BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::QualType PtrType = CxxAST.getPointerType(BaseType);

  return BuildAnyTypeLoc(CxxAST, PtrType, D->getType()->getLoc());
}

Expression ExprElaborator::elaborateConstType(Declarator *D, TypeInfo *Ty) {
  const CallSyntax *ConstCall = cast<CallSyntax>(D->Call);
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);
  return makeConstType(BaseTypeExpr, ConstCall);
}

Expression ExprElaborator::elaborateRefType(Declarator *D, TypeInfo *Ty) {
  const CallSyntax *RefCall = cast<CallSyntax>(D->Call);
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);
  return makeRefType(BaseTypeExpr, RefCall);
}

Expression ExprElaborator::elaborateRRefType(Declarator *D, TypeInfo *Ty) {
  const CallSyntax *RRefCall = cast<CallSyntax>(D->Call);
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);
  return makeRRefType(BaseTypeExpr, RRefCall);
}

Expression ExprElaborator::elaborateArrayType(Declarator *D, TypeInfo *Ty) {
  Expression BaseTypeExpr = elaborateTypeExpr(D->Next);

  if (!BaseTypeExpr.is<clang::TypeSourceInfo *>() || BaseTypeExpr.isNull()) {
    SemaRef.Diags.Report(D->getType()->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  Expression IndexExpr =
    ExprElaborator(Context, SemaRef).elaborateExpr(D->Data.Index);

  // FIXME: what do we do for an empty array index, such as []int = {...}
  if (IndexExpr.is<clang::TypeSourceInfo *>() || IndexExpr.isNull()) {
    SemaRef.Diags.Report(D->Data.Index->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType =
    BaseTypeExpr.get<clang::TypeSourceInfo *>()->getType();
  clang::Expr *Index = IndexExpr.get<clang::Expr *>();

  clang::Expr::EvalResult IdxResult;
  clang::Expr::EvalContext
    EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

  if (!Index->EvaluateAsConstantExpr(IdxResult, clang::Expr::EvaluateForCodeGen,
                                     EvalCtx))
    return nullptr;

  clang::QualType ArrayType =
    Context.CxxAST.getConstantArrayType(BaseType, IdxResult.Val.getInt(), Index,
                                        clang::ArrayType::Normal, 0);
  return BuildAnyTypeLoc(CxxAST, ArrayType, D->getType()->getLoc());
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
Expression ExprElaborator::elaborateFunctionType(Declarator *D, TypeInfo *Ty) {
  const auto *Call = cast<CallSyntax>(D->Call);

  // FIXME: Handle array-based arguments.
  assert(isa<ListSyntax>(D->Data.ParamInfo.Params)
         && "Array parameters not supported");
  const ListSyntax *Args = cast<ListSyntax>(D->Data.ParamInfo.Params);

  bool IsVariadic = D->Data.ParamInfo.VariadicParam;
  // Elaborate the parameter declarations in order to get their types, and save
  // the resulting scope with the declarator.
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  SemaRef.enterScope(SK_Parameter, Call);
  for (unsigned I = 0; I < Args->getNumChildren(); ++I) {
    // There isn't really anything to translate here.
    if (IsVariadic && I == Args->getNumChildren() - 1)
      break;
    const Syntax *P = Args->getChild(I);

    Elaborator Elab(Context, SemaRef);
    clang::ValueDecl *VD =
      cast_or_null<clang::ValueDecl>(Elab.elaborateDeclSyntax(P));
    if (!VD) {
      SemaRef.leaveScope(Call);
      return nullptr;
    }

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");
    assert(isa<clang::ParmVarDecl>(VD) && "Parameter is not a ParmVarDecl");

    Types.push_back(VD->getType());
    Params.push_back(cast<clang::ParmVarDecl>(VD));
  }
  D->Data.ParamInfo.ConstructedScope = SemaRef.saveScope(Call);


  // FIXME: We need to configure parts of the prototype (e.g., noexcept).
  clang::FunctionProtoType::ExtProtoInfo EPI;
  if (IsVariadic) {
    EPI.ExtInfo = Context.CxxAST.getDefaultCallingConvention(true, false);
    EPI.Variadic = true;
  }


  using clang::SourceLocation;
  using clang::SourceRange;

  clang::QualType FnTy = CxxAST.getFunctionType(Ty->getType(), Types, EPI);
  return BuildFunctionTypeLoc(CxxAST, FnTy,
    SourceLocation(), SourceLocation(), SourceLocation(),
    SourceRange(), SourceLocation(), Params);
}



Expression ExprElaborator::elaborateExplicitType(Declarator *D, TypeInfo *Ty) {
  assert(D->Kind == DK_Type);
  if (const auto *Atom = dyn_cast<AtomSyntax>(D->Data.Type)) {
    clang::SourceLocation Loc = Atom->getLoc();
    clang::IdentifierInfo *II = &CxxAST.Idents.get(Atom->getSpelling());
    clang::DeclarationNameInfo DNI(II, Loc);
    clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
    if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
      return nullptr;
    }
    if (R.empty()) {
      auto BuiltinMapIter = SemaRef.BuiltinTypes.find(Atom->getSpelling());
      if (BuiltinMapIter == SemaRef.BuiltinTypes.end()) {
        return nullptr;
      }

      return BuildAnyTypeLoc(CxxAST, BuiltinMapIter->second, Loc);
    }

    clang::TypeDecl *TD = R.getAsSingle<clang::TypeDecl>();
    TD->setIsUsed();
    return BuildAnyTypeLoc(CxxAST, Context.CxxAST.getTypeDeclType(TD), Loc);
  }

  return elaborateExpr(D->Data.Type);
}

void dumpExpression(ExprElaborator::Expression Expr, llvm::raw_ostream& Out) {
  if (Expr.isNull()) {
    Out << "[Null Expr]\n";
    return;
  }

  if (Expr.is<clang::Expr *>()) {
    Out << "Type = clang::Expr *\n";
    Expr.get<clang::Expr *>()->dump(Out);
    Out << "\n";
    return;
  }

  if (Expr.is<clang::TypeSourceInfo *>()) {
    Out << "Type = clang::TypeSourceInfo *\n";
    Expr.get<clang::TypeSourceInfo *>()->getType().dump(Out);
    Out << "\n";
    return;
  }

  if (Expr.is<clang::NamespaceDecl *>()) {
    Out << "Type = clang::NamespaceDecl *\n";
    Expr.get<clang::NamespaceDecl *>()->dump(Out);
    Out << "\n";
    return;
  }
  Out << "[Unknown Expression type]\n";
}

clang::Expr *ExprElaborator::handleOperatorDotDot(const CallSyntax *S) {
  assert(isa<AtomSyntax>(S->getCallee()));
  assert(cast<AtomSyntax>(S->getCallee())->getSpelling() == "operator'..'"
         && "invalid .. call");

  llvm_unreachable("unimplemented");
}

clang::TypeSourceInfo *
ExprElaborator::handleOperatorConst(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "const operator");
  Expression innerTypeExpr = elaborateExpr(S->getArgument(0));
  return makeConstType(innerTypeExpr, S);
}

clang::TypeSourceInfo *ExprElaborator::handleRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "ref operator");
  Expression innerTypeExpr = elaborateExpr(S->getArgument(0));
  return makeRefType(innerTypeExpr, S);
}

clang::TypeSourceInfo *ExprElaborator::handleRRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "rref operator");
  Expression innerTypeExpr = elaborateExpr(S->getArgument(0));
  return makeRRefType(innerTypeExpr, S);
}

static bool isVarArgs(Sema &SemaRef, const Syntax *S) {
  const CallSyntax *ColonOp = dyn_cast<CallSyntax>(S);
  if (!ColonOp)
    return false;
  FusedOpKind OpKind = getFusedOpKind(SemaRef, ColonOp);
  if (OpKind != FOK_Colon)
    return false;

  // We might have something like `varargs : args` or just `:args`
  std::size_t N = ColonOp->getNumArguments() - 1;
  if (N > 1)
    return false;

  if (const AtomSyntax *Ty = dyn_cast<AtomSyntax>(ColonOp->getArgument(N)))
    return Ty->Tok.hasKind(tok::ArgsKeyword);

  return false;
}

// Handle a function type in the form of `() -> type`
clang::TypeSourceInfo *ExprElaborator::handleFunctionType(const CallSyntax *S) {
  assert(S->getNumArguments() == 2 && "invalid operator'->' call");

  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  llvm::SmallVector<clang::QualType, 4> Types;
  bool IsVariadic = false;

  const Syntax *ParamBegin = S->getArgument(0);
  clang::SourceLocation EndLoc;
  if (const ListSyntax *ParamSyntaxes = dyn_cast<ListSyntax>(ParamBegin)) {
    Sema::ScopeRAII ParamScopeRAII(SemaRef, SK_Parameter, ParamBegin);

    for (unsigned I = 0; I < ParamSyntaxes->getNumChildren(); ++I) {
      const Syntax *ParamSyntax = ParamSyntaxes->getChild(I);

      if (I == ParamSyntaxes->getNumChildren() - 1) {
        EndLoc = ParamSyntax->getLoc();

        IsVariadic = isVarArgs(SemaRef, ParamSyntax);
        if (IsVariadic)
          break;
      }

      Elaborator ParamElaborator(Context, SemaRef);
      clang::Decl *Param = ParamElaborator.elaborateDeclSyntax(ParamSyntax);

      if (!Param || !isa<clang::ParmVarDecl>(Param))
        continue;

      Types.push_back(cast<clang::ParmVarDecl>(Param)->getType());
    }
  } else {
    SemaRef.Diags.Report(ParamBegin->getLoc(),
                         clang::diag::err_invalid_param_list);
    return nullptr;
  }

  if (!EndLoc.isValid())
    EndLoc = S->getLoc();

  const Syntax *ReturnSyntax = S->getArgument(1);
  ExprElaborator ReturnElaborator(Context, SemaRef);
  Expression Return = ReturnElaborator.elaborateExpr(ReturnSyntax);

  if (Return.isNull() || !Return.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(ReturnSyntax->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::TypeSourceInfo *ReturnType = Return.get<clang::TypeSourceInfo *>();

  // Create the clang type
  clang::FunctionProtoType::ExtProtoInfo EPI;
  if (IsVariadic) {
    EPI.ExtInfo = Context.CxxAST.getDefaultCallingConvention(true, false);
    EPI.Variadic = true;
  }

  clang::QualType FnTy =
    CxxAST.getFunctionType(ReturnType->getType(), Types, EPI);
  clang::QualType FnPtrTy = CxxAST.getPointerType(FnTy);
  FnPtrTy.addConst();

  clang::TypeLocBuilder TLB;
  clang::TypeSourceInfo *FnTSI = BuildFunctionTypeLoc(Context.CxxAST, TLB, FnTy,
    ParamBegin->getLoc(), ParamBegin->getLoc(), EndLoc,
    clang::SourceRange(), ReturnSyntax->getLoc(), Params);
  return BuildFunctionPtrTypeLoc(CxxAST, TLB, FnTSI, S->getLoc());
}

clang::TypeSourceInfo* ExprElaborator::makeConstType(Expression InnerType,
    const CallSyntax* ConstOpNode) {
  if (InnerType.isNull() || !InnerType.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(ConstOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::QualType EvaluatedTy =
    InnerType.get<clang::TypeSourceInfo *>()->getType();
  if (EvaluatedTy.isConstQualified()) {
    SemaRef.Diags.Report(ConstOpNode->getLoc(),
                         clang::diag::err_conflicting_specifier)
      << "const";
    return nullptr;
  }
  EvaluatedTy.addConst();
  return BuildAnyTypeLoc(CxxAST, EvaluatedTy, ConstOpNode->getLoc());
}
clang::TypeSourceInfo* ExprElaborator::makeRefType(Expression InnerTy,
    const CallSyntax* RefOpNode) {
  if (InnerTy.isNull() || !InnerTy.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(RefOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType Inner = InnerTy.get<clang::TypeSourceInfo *>()->getType();
  if (Inner.getTypePtr()->isReferenceType()) {
    SemaRef.Diags.Report(RefOpNode->getLoc(),
                         clang::diag::err_conflicting_specifier)
      << "ref";
    return nullptr;
  }
  return BuildAnyTypeLoc(CxxAST,
    CxxAST.getLValueReferenceType(Inner),
    RefOpNode->getLoc());
}

clang::TypeSourceInfo* ExprElaborator::makeRRefType(Expression InnerTy,
    const CallSyntax* RRefOpNode) {
  if (InnerTy.isNull() || !InnerTy.is<clang::TypeSourceInfo *>()) {
    SemaRef.Diags.Report(RRefOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType Inner = InnerTy.get<clang::TypeSourceInfo *>()->getType();
  if (Inner.getTypePtr()->isReferenceType()) {
    SemaRef.Diags.Report(RRefOpNode->getLoc(),
                         clang::diag::err_conflicting_specifier)
      << "rref";
    return nullptr;
  }  
  return BuildAnyTypeLoc(CxxAST,
      CxxAST.getRValueReferenceType(Inner),
      RRefOpNode->getLoc());
}
} // namespace gold
