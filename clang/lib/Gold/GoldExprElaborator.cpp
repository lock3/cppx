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
#include "clang/AST/ExprCppx.h"
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

ExprElaborator::ExprElaborator(SyntaxContext &Context, Sema &SemaRef,
      clang::DeclContext *DC, gold::Scope *GoldScope)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
  CurrentLookUpContext(DC), OwningScope(GoldScope)
{ }

clang::Expr *ExprElaborator::elaborateExpr(const Syntax *S) {
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

clang::Expr *ExprElaborator::elaborateExpectedConstantExpr(const Syntax* S) {
  clang::EnterExpressionEvaluationContext ConstantEvaluated(
                                                           SemaRef.getCxxSema(),
                   clang::Sema::ExpressionEvaluationContext::ConstantEvaluated);
  clang::Expr *Res = elaborateExpr(S);
  if (!Res)
    return Res;
  auto ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(Res);
  if (ConstExpr.isInvalid())
    return nullptr;
  return ConstExpr.get();
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
    clang::SourceLocation Loc = S->getLoc();
    SemaRef.Diags.Report(Loc, clang::diag::err_integer_bitwidth_mismatch)
      << IntTy << Width << CxxAST.getIntWidth(IntTy);
    return nullptr;
  }

  // skip over any [0.] prefix
  llvm::StringRef Spelling = Base == 10 ? S->getSpelling() :
    std::string(S->getSpelling().begin() + 2, S->getSpelling().end());

  llvm::APInt Value(Width, Spelling, Base);
  Value = Value.zextOrTrunc(Width);

  if (checkOverflow(Base, Spelling, Value)) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_integer_literal_too_large)
      << /* Unsigned */ 1;
    return nullptr;
  }

  return clang::IntegerLiteral::Create(CxxAST, Value, IntTy, S->getLoc());
}

static clang::FloatingLiteral *
createFloatLiteral(clang::ASTContext &CxxAST, const LiteralSyntax *S) {
  // If we don't have a specified type, just create a default float.
  clang::QualType FloatType = CxxAST.FloatTy;
  if (S->Suffix.IsDouble)
    FloatType = CxxAST.DoubleTy;

  const llvm::fltSemantics &Format = CxxAST.getFloatTypeSemantics(FloatType);
  using llvm::APFloat;
  APFloat Val = llvm::APFloat(Format);

  // if (FloatType == CxxAST.FloatTy) {
  auto StatusOrErr =
    Val.convertFromString(S->getSpelling(), APFloat::rmNearestTiesToEven);
  assert(StatusOrErr && "Invalid floating point representation");
  return clang::FloatingLiteral::Create(CxxAST, Val, /*Exact=*/true,
                                        FloatType, S->getLoc());
  // } else if (FloatType == CxxAST.DoubleTy) {
    // double Literal = atof(T.getSymbol().data());
    // auto Value = llvm::APFloat(Literal);
    // return clang::FloatingLiteral::Create(CxxAST, Value, /*Exact=*/true,
    //                                       FloatType, S->getLoc());
  // }

  // llvm_unreachable("unsupported float type");
}

static clang::FloatingLiteral *
createExponentLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                      const LiteralSyntax *S, clang::SourceLocation Loc) {
  std::string Spelling = S->getSpelling().str();
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

static clang::ParsedTemplateArgument
convertExprToTemplateArg(Sema &SemaRef, clang::Expr *E) {
  // Type parameters start here.
  if (E->getType()->isTypeOfTypes()) {
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                          E, E->getExprLoc());
    if (!TInfo)
      return clang::ParsedTemplateArgument();

    return SemaRef.getCxxSema().ActOnTemplateTypeArgument(
               SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo));
  }

  if (E->getType()->isTemplateType()) {
    // This is a template template parameter?
    llvm_unreachable("Template template params not implemented yet.");
  }

  // Anything else is a constant expression?
  clang::ExprResult ConstExpr(E);
  ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(ConstExpr);
  return clang::ParsedTemplateArgument(clang::ParsedTemplateArgument::NonType,
      ConstExpr.get(), E->getExprLoc());
}


static clang::Expr*
handleClassTemplateSelection(ExprElaborator& Elab, Sema &SemaRef,
    SyntaxContext& Context, clang::Expr* IdExpr, const ElemSyntax *Elem) {
  clang::TemplateArgumentListInfo TemplateArgs(Elem->getLoc(), Elem->getLoc());
  llvm::SmallVector<clang::ParsedTemplateArgument, 16> ParsedArguments;
  const ListSyntax *ElemArgs = cast<ListSyntax>(Elem->getArguments());

  for(const Syntax *SyntaxArg : ElemArgs->children()) {
    clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
                                                          SemaRef.getCxxSema(),
                  clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
                                                /*LambdaContextDecl=*/nullptr,
                                                              /*ExprContext=*/
          clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);

    clang::Expr *ArgExpr = Elab.elaborateExpr(SyntaxArg);
    if (!ArgExpr) {
      SemaRef.Diags.Report(SyntaxArg->getLoc(),
                           clang::diag::err_failed_to_translate_expr);
      continue;
    }

    auto TemplateArg = convertExprToTemplateArg(SemaRef, ArgExpr);
    if (TemplateArg.isInvalid())
      // TODO: Figure out if this needs an error message or not.
      // I assume that the errore message should be delivered prior to this.
      return nullptr;

    ParsedArguments.emplace_back(TemplateArg);

    // Also building template Argument Info.
    if (ArgExpr->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ArgTInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                       ArgExpr, Elem->getLoc());
      if (!ArgTInfo)
        return nullptr;
      clang::TemplateArgument Arg(ArgTInfo->getType());
      TemplateArgs.addArgument({Arg, ArgTInfo});
    } else {
      clang::TemplateArgument Arg(ArgExpr, clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ArgExpr});
    }
  }

  clang::Decl *Decl = SemaRef.getDeclFromExpr(IdExpr,
                                              Elem->getObject()->getLoc());
  if (!Decl)
    return nullptr;

  clang::TemplateDecl *CTD = dyn_cast<clang::TemplateDecl>(Decl);
  assert(CTD && "Invalid CppxDeclRefExpr");

  clang::CXXScopeSpec SS;
  clang::TemplateName TName(CTD);
  clang::Sema::TemplateTy TemplateTyName = clang::Sema::TemplateTy::make(TName);
  clang::IdentifierInfo *II = CTD->getIdentifier();
  clang::ASTTemplateArgsPtr InArgs(ParsedArguments);
  if (clang::VarTemplateDecl *VTD = dyn_cast<clang::VarTemplateDecl>(CTD)) {
    clang::DeclarationNameInfo DNI(VTD->getDeclName(), Elem->getLoc());
    clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                          clang::Sema::LookupAnyName);
    R.addDecl(VTD);
    clang::ExprResult ER = SemaRef.getCxxSema().BuildTemplateIdExpr(
          SS, clang::SourceLocation(), R, false, &TemplateArgs);
    if (ER.isInvalid())
      return nullptr;
    return ER.get();
  } else {


    clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
      SemaRef.getCurClangScope(), SS, /*TemplateKWLoc*/ clang::SourceLocation(),
      TemplateTyName, II, Elem->getObject()->getLoc(),
      /*LAngleLoc*/clang::SourceLocation(),
      InArgs, /*RAngleLoc*/ clang::SourceLocation(), false, false);

    if (Result.isInvalid()) {
      SemaRef.Diags.Report(Elem->getObject()->getLoc(),
                          clang::diag::err_failed_to_translate_expr);
      return nullptr;
    }

    clang::QualType Ty(Result.get().get());
    const clang::LocInfoType *TL = cast<clang::LocInfoType>(Ty.getTypePtr());
    return SemaRef.buildTypeExpr(TL->getType(), Elem->getLoc());
  }
}

static clang::Expr *
handleElementExpression(ExprElaborator &Elab, Sema &SemaRef,
                        SyntaxContext &Context, const ElemSyntax *Elem,
                        clang::Expr *E) {
  // Attempting to correctly handle the result of an Id expression.
  clang::OverloadExpr *OverloadExpr = dyn_cast<clang::OverloadExpr>(E);
  if (!OverloadExpr) {
    llvm::SmallVector<clang::Expr *, 5> ArgExprs;
    for (const Syntax *SS : Elem->getArguments()->children()) {
      clang::Expr *Res = Elab.elaborateExpr(SS);
      if (!Res)
        return nullptr;
      ArgExprs.push_back(Res);
    }

    if (ArgExprs.size() == 0) {
      SemaRef.Diags.Report(Elem->getArguments()->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
    if (ArgExprs.size() != 1) {
      // TODO: Implement multiple argument indexing here.
      llvm_unreachable("Multi-index expressions not implemented yet.");
    }
    auto SubScriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
                                                     SemaRef.getCurClangScope(),
                                                     E, clang::SourceLocation(),
                                                     ArgExprs[0],
                                                     clang::SourceLocation());
    return SubScriptExpr.get();
  }
  // At this point we are an overload set which means we must be some kind of
  // templated function, or overloaded function.
  clang::TemplateArgumentListInfo TemplateArgs(Elem->getLoc(), Elem->getLoc());
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  for (const Syntax *SS : Elem->getArguments()->children()) {
    ExprElaborator ParamElaborator(Context, SemaRef);
    clang::Expr *ParamExpression = ParamElaborator.elaborateExpr(SS);
    if (!ParamExpression)
      return nullptr;

    if (ParamExpression->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ParamTInfo = SemaRef.getTypeSourceInfoFromExpr(
                                            ParamExpression, Elem->getLoc());
      if (!ParamTInfo)
        return nullptr;
      clang::TemplateArgument Arg(ParamTInfo->getType());
      TemplateArgs.addArgument({Arg, ParamTInfo});
      ActualArgs.emplace_back(Arg);
    } else {
      clang::TemplateArgument Arg(ParamExpression,
                                           clang::TemplateArgument::Expression);
      TemplateArgs.addArgument({Arg, ParamExpression});
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
          llvm_unreachable("Function template instantiation failure Need to "
                           "do SFINAE here.");
        }
        SemaRef.getCxxSema().InstantiateFunctionDefinition(Elem->getLoc(), FD,
                                                           true, true, false);
        return clang::MemberExpr::Create(Context.CxxAST, MemAccess->getBase(),
                                         MemAccess->isArrow(),
                                         MemAccess->getOperatorLoc(),
                                         MemAccess->getQualifierLoc(),
                                         clang::SourceLocation(), FD,
                               clang::DeclAccessPair::make(FD, ND->getAccess()),
                                         MemAccess->getMemberNameInfo(),
                                         &TemplateArgs, E->getType(),
                                         MemAccess->getValueKind(),
                                         MemAccess->getObjectKind(),
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
                                                               &TemplateArgList,
                                                                Elem->getLoc());
      SemaRef.getCxxSema().InstantiateFunctionDefinition(Elem->getLoc(),
                                                         InstantiatedFunc,
                                                         true, true, false);
      clang::LookupResult ResultTemp(SemaRef.getCxxSema(),
                                     OverloadExpr->getNameInfo(),
                                     clang::Sema::LookupAnyName);
      ResultTemp.addDecl(InstantiatedFunc);
      return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
                                                 OverloadExpr->getNamingClass(),
                                                OverloadExpr->getQualifierLoc(),
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
            if(clang::FunctionTemplateDecl *FTD
                                         = FD->getDescribedFunctionTemplate()) {
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

clang::Expr *ExprElaborator::elaborateElementExpr(const ElemSyntax *Elem) {
  clang::Expr *IdExpr = elaborateExpr(Elem->getObject());
  if (!IdExpr)
    return nullptr;

  // If we got a template specialization from elaboration, this is probably
  // a nested-name-specifier, there's nothing left to do.
  if (IdExpr->getType()->isTypeOfTypes()) {
    clang::CppxTypeLiteral *Lit = cast<clang::CppxTypeLiteral>(IdExpr);
    clang::CXXRecordDecl *RD = Lit->getValue()->getType()->getAsCXXRecordDecl();
    if (isa<clang::ClassTemplateSpecializationDecl>(RD))
      return IdExpr;
  }

  if (IdExpr->getType()->isTypeOfTypes()
      || IdExpr->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(Elem->getObject()->getLoc(),
                         clang::diag::err_non_template_in_template_id)
                         << IdExpr;
    return nullptr;
  }

  if (IdExpr->getType()->isTemplateType())
    return handleClassTemplateSelection(*this, SemaRef, Context, IdExpr, Elem);
  return handleElementExpression(*this, SemaRef, Context, Elem, IdExpr);
}

static clang::Expr *
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
                                                   R.getNamingClass(),
                                                clang::NestedNameSpecifierLoc(),
                                                   R.getLookupNameInfo(),
                                                   /*ADL=*/true, true,
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

    if (clang::ValueDecl *VD = R.getAsSingle<clang::ValueDecl>()) {
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
        clang::QualType ThisTy(RD->getTypeForDecl(), 0);
        clang::QualType ThisPtrTy = SemaRef.getContext().CxxAST.getPointerType(
                                                                        ThisTy);
        clang::Expr* This = SemaRef.getCxxSema().BuildCXXThisExpr(Loc,
                                                                  ThisPtrTy,
                                                                  true);

        clang::DeclAccessPair FoundDecl = clang::DeclAccessPair::make(Field,
                                                            Field->getAccess());
        clang::CXXScopeSpec SS;
        clang::ExprResult MemberExpr
            = SemaRef.getCxxSema().BuildFieldReferenceExpr(This, true,
                                                        clang::SourceLocation(),
                                                           SS, Field, FoundDecl,
                                                           DNI);
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
        return SemaRef.getCxxSema().BuildPossibleImplicitMemberExpr(SS, Loc, R,
                                                                    nullptr,
                                              SemaRef.getCurClangScope()).get();
      }

      if(isa<clang::FunctionDecl>(VD)) {
        return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
                                                   R.getNamingClass(),
                                                clang::NestedNameSpecifierLoc(),
                                                   R.getLookupNameInfo(),
                                                   /*ADL=*/true, true,
                                                   R.begin(), R.end());
      }

      clang::QualType ResultType = VD->getType();
      if (ResultType.getTypePtr()->isReferenceType()) {
        ResultType = ResultType.getTypePtr()->getPointeeType();
      }

      clang::ExprValueKind ValueKind = SemaRef.getCxxSema()
                     .getValueKindForDeclReference(ResultType, VD, S->getLoc());

      clang::DeclRefExpr *DRE =
        SemaRef.getCxxSema().BuildDeclRefExpr(VD, ResultType, ValueKind, DNI,
                                              clang::NestedNameSpecifierLoc(),
                                              VD, clang::SourceLocation(),
                                              nullptr);
      return DRE;
    }

    // Processing the case when the returned result is a type.
    if (const clang::TagDecl *TD = R.getAsSingle<clang::TagDecl>())
      return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);

    if (clang::ClassTemplateDecl *CTD
                                  = R.getAsSingle<clang::ClassTemplateDecl>())
      return SemaRef.buildTemplateType(CTD, Loc);

    if (auto *NS = R.getAsSingle<clang::CppxNamespaceDecl>())
      return SemaRef.buildNSDeclRef(NS, Loc);

    if (auto *TD = R.getAsSingle<clang::TypeDecl>())
      return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);

    if (auto *TD = R.getAsSingle<clang::TemplateDecl>())
      return SemaRef.buildTemplateType(TD, Loc);
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_identifier_not_declared_in_scope)
                       << S->getSpelling();
  return nullptr;
}
static clang::Expr *createThisExpr(Sema &SemaRef, const AtomSyntax *S) {
  return SemaRef.getCxxSema().ActOnCXXThis(S->getLoc()).get();
}

clang::Expr *ExprElaborator::elaborateAtom(const AtomSyntax *S,
                                         clang::QualType ExplicitType) {
  Token T = S->Tok;

  // Check if we have a floating literal that looks like an int.
  if (const LiteralSyntax *L = dyn_cast<LiteralSyntax>(S))
    if (L->Suffix.IsFloat || L->Suffix.IsDouble)
      return createFloatLiteral(CxxAST, L);

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(CxxAST, SemaRef, cast<LiteralSyntax>(S));
  case tok::DecimalFloat:
    return createFloatLiteral(CxxAST, cast<LiteralSyntax>(S));
  case tok::BinaryInteger: {
    std::string TData = std::string(T.getSymbol().data());
    TData =  TData.substr(TData.find_first_not_of("0b"), TData.size());
    Token RawBin = Token(tok::BinaryInteger, T.getLocation(), Symbol(&TData));
    return createIntegerLiteral(CxxAST, RawBin, ExplicitType,
                                S->getLoc(), /*Base=*/2);
  }

  case tok::HexadecimalInteger:
    return createIntegerLiteral(CxxAST, SemaRef,
                                cast<LiteralSyntax>(S), /*Base=*/16);
  case tok::HexadecimalFloat:
    llvm_unreachable("Hexadecimal float not implemented.");
  case tok::Identifier:
    return createIdentAccess(Context, SemaRef, S, ExplicitType, S->getLoc());
  case tok::Character:
    return createCharLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::HexadecimalCharacter:
    return createUTF8Literal(CxxAST, SemaRef, T, S->getLoc());
  case tok::UnicodeCharacter:
    return createUnicodeLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::String:
    llvm_unreachable("String not implemented.");
  case tok::DecimalExponent:
    return createExponentLiteral(CxxAST, SemaRef,
                                 cast<LiteralSyntax>(S), S->getLoc());

  /// Keyword Literals
  case tok::TrueKeyword:
  case tok::FalseKeyword:
    return createBoolLiteral(CxxAST, T, S->getLoc());

  case tok::NullKeyword:
    return createNullLiteral(CxxAST, S->getLoc());
  case tok::ThisKeyword:
    return createThisExpr(SemaRef, S);
  default:
    break;
  }

  auto BuiltinMapIter = SemaRef.BuiltinTypes.find(S->getSpelling());
  if (BuiltinMapIter != SemaRef.BuiltinTypes.end())
    return SemaRef.buildTypeExpr(BuiltinMapIter->second, S->getLoc());

  SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_identifier_type)
                       << S->getSpelling();

  return nullptr;
}

static bool buildFunctionCallAruments(Sema &SemaRef, SyntaxContext &Context,
    const ListSyntax *ArgList,
    llvm::SmallVector<clang::Expr *, 8> &Args) {
  for (const Syntax *A : ArgList->children()) {
    ExprElaborator Elab(Context, SemaRef);
    clang::Expr *Argument = Elab.elaborateExpr(A);

    // FIXME: What kind of expression is the unary ':typename' expression?
    if (!Argument) {
      SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
      return true;
    }

    Args.push_back(Argument);
  }
  return false;
}

/// This function's job is to create the correct call based upon the result
/// type of the CalleeExpr, which could be any of the types within the
/// Expression union type.
static clang::Expr *handleExpressionResultCall(Sema &SemaRef,
                                               const CallSyntax *S,
                                               clang::Expr *CalleeExpr,
                                    llvm::SmallVector<clang::Expr *, 8> &Args) {
  if (!CalleeExpr) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  if (CalleeExpr->getType()->isTypeOfTypes()) {
    // This means constructor call possibly, unless it's some how a function
    // call returning a type.
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                 CalleeExpr, S->getLoc());
    if (!TInfo) {
      // If this isn't a CppxTypeLiteral then we can't get the type yet and it
      // might be a meta function.kind
      llvm_unreachable("Meta function calls not implemented yet?");
    }
    clang::ExprResult ConstructorExpr =
        SemaRef.getCxxSema().BuildCXXTypeConstructExpr(TInfo, S->getLoc(),
                                                      Args, S->getLoc(), false);

    if (!ConstructorExpr.get()) {
      SemaRef.Diags.Report(S->getLoc(),
                            clang::diag::err_coroutine_invalid_func_context)
                            << 0 << TInfo->getType();
      return nullptr;
    }
    return ConstructorExpr.get();

  }
  // TODO: create a test for this were we call a namespace just to see what kind
  // of error actually occurs.

  // For any thing else we simply try and make the call and see what happens.
  clang::ExprResult Call = SemaRef.getCxxSema().ActOnCallExpr(
                                            SemaRef.getCxxSema().getCurScope(),
                                            CalleeExpr, S->getCalleeLoc(), Args,
                                            S->getCalleeLoc());
  // if (Args.size() == 1)
  if (Call.isInvalid())
    return nullptr;
  return Call.get();
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



clang::Expr *ExprElaborator::elaborateCall(const CallSyntax *S) {
  if (clang::Expr *elaboratedCall = elaborateBuiltinOperator(S))
    return elaboratedCall;

  if (callIsCastOperator(S))
    return elaborateCastOp(S);

  // Determining the type of call associated with the given syntax.
  // There are multiple kinds of atoms for multiple types of calls
  // but in the event that the callee object is not an Atom, it means
  // that we have to process the sub expression as normal.
  clang::Expr *CalleeExpr;
  if (!isa<AtomSyntax>(S->getCallee())) {
    CalleeExpr = elaborateExpr(S->getCallee());
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());

    if (buildFunctionCallAruments(SemaRef, Context, ArgList, Args))
      return nullptr;

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
    llvm_unreachable("FOK_DotDot is not supported in this context.");

  case FOK_Const:
    return handleOperatorConst(S);
  case FOK_Ref:
    return handleRefType(S);
  case FOK_RRef:
    return handleRRefType(S);
  case FOK_Arrow:
    return handleFunctionType(S);
  case FOK_Brackets:
    return handleArrayType(S);
  case FOK_Parens:
    return handleRawBaseSpecifier(S);
  default:
    break;
  }

  llvm::StringRef Spelling = Callee->getSpelling();
  if (Spelling.startswith("operator'")) {
    if (S->getNumArguments() == 1) {
      clang::UnaryOperatorKind UnaryOpKind;
      if (!SemaRef.OpInfo.getUnaryOperatorUseKind(Spelling, UnaryOpKind))
        return elaborateUnaryOp(S, UnaryOpKind);
    }

    if (S->getNumArguments() == 2) {
      // Check if this is a binary operator.
      clang::BinaryOperatorKind BinaryOpKind;
      if (!SemaRef.OpInfo.getBinaryOperatorUseKind(Spelling, BinaryOpKind))
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

/// This returns false if the keyword is a builtin function.
static bool isBuitinOperator(const CallSyntax *S) {
  if (const auto *Atom = dyn_cast<AtomSyntax>(S->getCallee())) {
    switch (Atom->Tok.getKind()) {
      case tok::AlignOfKeyword:
      case tok::SizeOfKeyword:
      case tok::NoExceptKeyword:
      case tok::DeclTypeKeyword:
        return false;
      default:
        break;
    }
  }
  return true;
}

clang::Expr *ExprElaborator::elaborateBuiltinOperator(const CallSyntax *S) {
  if (isBuitinOperator(S)) {
    return nullptr;
  }
  const AtomSyntax *Atom = cast<AtomSyntax>(S->getCallee());
  switch (Atom->Tok.getKind()) {
  case tok::NoExceptKeyword:
    return elaborateNoExceptOp(Atom, S);

  case tok::DeclTypeKeyword:
    return elaborateDeclTypeOp(Atom, S);

  case tok::AlignOfKeyword:
    return elaborateTypeTraitsOp(Atom, S, clang::UETT_AlignOf);

  case tok::SizeOfKeyword:
    return elaborateTypeTraitsOp(Atom, S, clang::UETT_SizeOf);
  default:
    llvm_unreachable("Invalid buildin function elaboration.");
  }

}

clang::Expr *
ExprElaborator::elaborateTypeTraitsOp(const AtomSyntax *Name, const CallSyntax *S,
                                      clang::UnaryExprOrTypeTrait Trait) {
  if (S->getNumArguments() != 1) {
    SemaRef.Diags.Report(Name->getLoc(),
                      clang::diag::err_incorrect_number_of_arguments)
                      << Name->getSpelling();
    return nullptr;
  }
  clang::EnterExpressionEvaluationContext Unevaluated(
    SemaRef.getCxxSema(),
    clang::Sema::ExpressionEvaluationContext::Unevaluated,
    clang::Sema::ReuseLambdaContextDecl);

  // Attempting to elaborate the given argument
  clang::Expr *ResultExpr = elaborateExpr(S->getArgument(0));
  if (!ResultExpr)
    return nullptr;

  if (ResultExpr->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_cannot_apply_operator_to_a_namespace)
                         << Name->getSpelling();
    return nullptr;
  }

  if (ResultExpr->getType()->isTemplateType()) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_cannot_apply_operator_to_template)
                         << Name->getSpelling();
    return nullptr;
  }

  bool IsType = false;
  void *ExprOrTySourceInfo = ResultExpr;
  if (ResultExpr->getType()->isTypeOfTypes()) {
    clang::ParsedType ParsedTy = SemaRef.getParsedTypeFromExpr(ResultExpr,
                                                  S->getArgument(0)->getLoc());
    if (!ParsedTy.getAsOpaquePtr())
      return nullptr;
    ExprOrTySourceInfo = ParsedTy.getAsOpaquePtr();
    IsType = true;
  }
  clang::SourceLocation ArgLoc = S->getArgument(0)->getLoc();
  auto Result = SemaRef.getCxxSema().ActOnUnaryExprOrTypeTraitExpr(
                                        S->getCallee()->getLoc(), Trait, IsType,
                                        ExprOrTySourceInfo,
                          clang::SourceRange(S->getCallee()->getLoc(), ArgLoc));
  return Result.get();
}

clang::Expr *
ExprElaborator::elaborateDeclTypeOp(const AtomSyntax *Name,
                                    const CallSyntax *S) {
  assert(Name->Tok.getKind() == tok::DeclTypeKeyword
         && "Invalid elaboration of decltype operator.");
  if (S->getNumArguments() != 1) {
    SemaRef.Diags.Report(Name->getLoc(),
                      clang::diag::err_incorrect_number_of_arguments)
                      << Name->getSpelling();
    return nullptr;
  }
  // Entering decltype context for evaluation of subexpression.
  clang::EnterExpressionEvaluationContext Unevaluated(SemaRef.getCxxSema(),
                 clang::Sema::ExpressionEvaluationContext::Unevaluated, nullptr,
                 clang::Sema::ExpressionEvaluationContextRecord::EK_Decltype);
  clang::Expr *ArgEval = elaborateExpr(S->getArgument(0));
  if (!ArgEval)
    // TODO: Might an error message here. Although the error message
    /// SHOULD be coming from elaborateExpr.
    return nullptr;

  // In order to do this correctly I need to check for a few things,
  // I need to make sure that if the expression's result is actually a namespace
  // or template that I handle things correctly because I can't pass them through
  // the SemaRef.getCxxSema().ActOnDecltypeExpression because they wouldn't make
  // any sense, it may be benifical to do the same for the kind type.
  if (ArgEval->getType()->isTypeOfTypes()) {
    // TODO: We may need to check for auto here. although decltype(auto) doesn't
    // have syntax yet.
    return SemaRef.buildTypeExpr(Context.CxxAST.CppxKindTy, S->getLoc());
  }

  if (ArgEval->getType()->isNamespaceType()) {
    llvm_unreachable("Decltype of a namespace not implemented yet\n");
  }

  if (ArgEval->getType()->isTemplateType()) {
    llvm_unreachable("Template expression decltype not implemented yet.");
  }

  // This does some semantic checking on the given expression.
  auto ExprResult = SemaRef.getCxxSema().ActOnDecltypeExpression(ArgEval);
  if (ExprResult.isInvalid())
    return nullptr;

  clang::QualType Ty = SemaRef.getCxxSema().BuildDecltypeType(ExprResult.get(),
                                                   S->getArgument(0)->getLoc());

  return SemaRef.buildTypeExpr(Ty, S->getArgument(0)->getLoc());
}

clang::Expr *
ExprElaborator::elaborateNoExceptOp(const AtomSyntax *Name,
                                    const CallSyntax *S) {
  assert(Name->Tok.getKind() == tok::NoExceptKeyword
         && "Invalid elaboration of noexcept operator.");
  clang::EnterExpressionEvaluationContext Unevaluated(SemaRef.getCxxSema(),
    clang::Sema::ExpressionEvaluationContext::Unevaluated);

  clang::Expr *ArgEval = elaborateExpr(S->getArgument(0));
  if (!ArgEval)
    return nullptr;

  if (ArgEval->getType()->isTypeOfTypes()) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_cannot_appy_operator_to_type)
                         << Name->getSpelling();
    return nullptr;
  }

  if (ArgEval->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_cannot_apply_operator_to_a_namespace)
                         << Name->getSpelling();
    return nullptr;
  }

  if (ArgEval->getType()->isTemplateType()) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_cannot_apply_operator_to_template)
                         << Name->getSpelling();
    return nullptr;
  }

  auto Result = SemaRef.getCxxSema().ActOnNoexceptExpr(Name->getLoc(),
                                                       clang::SourceLocation(),
                                                       ArgEval,
                                                       clang::SourceLocation());
  if (Result.isInvalid())
    return nullptr;

  return Result.get();
}

clang::Expr *ExprElaborator::elaborateCastOp(const CallSyntax *CastOp) {
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
    // TODO: Create error message for here?!
    // CastOp->dump();
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

  clang::Expr *DestinationTy = elaborateExpr(Elem->getArgument(0));
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                 DestinationTy, Elem->getArgument(0)->getLoc());
  if (!TInfo){
    SemaRef.Diags.Report(Elem->getArgument(0)->getLoc(),
                      clang::diag::err_invalid_cast_destination_type);
    return nullptr;
  }

  clang::Expr *Source = elaborateExpr(CastOp->getArgument(0));
  if (!Source) {
    SemaRef.Diags.Report(CastOp->getArgument(0)->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }
  auto CastExpr = SemaRef.getCxxSema().BuildCXXNamedCast(Callee->getLoc(),
                                                         CastKind, TInfo,
                                                         Source,
                                              CastOp->getArgument(0)->getLoc(),
                                              CastOp->getArgument(0)->getLoc());
  return CastExpr.get();
}

static clang::Expr *handleLookupInsideType(Sema &SemaRef,
                                           clang::ASTContext &CxxAST,
                                           const clang::Expr *Previous,
                                           const Syntax *RHS);

/// Elaborate a base specifier that is not part of a member access expression.
/// For example:
///
/// \code
///   some_method() : void!
///     (some_base)base_member
/// \endcode
clang::Expr *ExprElaborator::handleRawBaseSpecifier(const CallSyntax *Op) {
  // Fabricate a `this` to be an LHS of a member access and save ourselves
  // some work.
  Token ThisTok(tok::ThisKeyword, Op->getLoc(), getSymbol("this"));
  AtomSyntax *This = new (Context) AtomSyntax(ThisTok);

  Token DotTok(tok::ThisKeyword, Op->getLoc(), getSymbol("operator'.'"));
  AtomSyntax *Dot = new (Context) AtomSyntax(DotTok);
  Syntax *Arg = const_cast<CallSyntax *>(Op);
  ListSyntax *Args = new (Context) ListSyntax(&Arg, 1);
  CallSyntax *DotCall = new (Context) CallSyntax(Dot, Args);

  return elaborateMemberAccess(This, DotCall, Op);
}

/// This function handles explicit operator calls, to member functions that are
/// have an operator name, if one cannot be located then nullptr is returned.
static clang::Expr *doDerefAndXOrLookUp(SyntaxContext &Context,
                                     Sema &SemaRef,
                                     clang::OverloadedOperatorKind UnaryOO,
                                     clang::OverloadedOperatorKind BinaryOO,
                                     clang::CXXScopeSpec &SS,
                                     clang::Expr *BaseExpr,
                                     const Syntax *OpSyntax,
                                     clang::SourceLocation RhsLoc,
                                     OpInfoBase const *Op) {
  assert(BaseExpr && "Invalid base expression.");
  assert(Op && "Invalid operator info.");
  clang::SourceRange BaseRange = BaseExpr ? BaseExpr->getSourceRange() :
                                 clang::SourceRange();
  const clang::RecordType *RTy = BaseExpr->getType()->getAsStructureType();
  if (!RTy) {
    // TODO: Create a error message.
    SemaRef.Diags.Report(BaseExpr->getExprLoc(),
                      clang::diag::err_typecheck_member_reference_struct_union)
                        << BaseExpr->getType();

    // llvm_unreachable("This only happens when we don't have a valid structured "
    //                  "type to invoke a member function upon.");
  }
  // Getting record decl to search.
  clang::RecordDecl *RDecl = RTy->getDecl();
  // I'm not sure what's happening here, or why this is necessary, yet.
  // There must be some kind of precondition for SOMETHING.

  // Checking if this is outside of a member function body
  if (!SemaRef.getCxxSema().isThisOutsideMemberFunctionBody(
                                                    clang::QualType(RTy, 0)) &&
      SemaRef.getCxxSema().RequireCompleteType(OpSyntax->getLoc(),
                                               clang::QualType(RTy, 0),
                                    clang::diag::err_typecheck_incomplete_tag,
                                               BaseRange))
    return nullptr;

  clang::DeclContext *LookupCtxt = RDecl;
  if (SS.isSet()) {
    // If the member name was a qualified-id, look into the
    // nested-name-specifier.
    LookupCtxt = SemaRef.getCxxSema().computeDeclContext(SS, false);

    // The error in this case might not be necessary because we can guarantee
    // that any base type is already elaborated.
    if (SemaRef.getCxxSema().RequireCompleteDeclContext(SS, LookupCtxt)) {
      SemaRef.getCxxSema().Diag(SS.getRange().getEnd(),
                                clang::diag::err_typecheck_incomplete_tag)
                                << SS.getRange() << LookupCtxt;
      return nullptr;
    }

    // Make sure that if we do infact have a valid ScopeSpec that it's a type
    // if it's not a type we have need to report and return an error.
    if (!isa<clang::TypeDecl>(LookupCtxt)) {
      SemaRef.getCxxSema().Diag(RhsLoc,
                                clang::diag::err_qualified_member_nonclass)
                                << LookupCtxt << SS.getRange();
      return nullptr;
    }
  }

  assert(LookupCtxt && "Cannot handle non-computable dependent contexts "
         "in lookup");
  clang::DeclarationName DN = Context.CxxAST.DeclarationNames
                                                   .getCXXOperatorName(UnaryOO);
  auto UnaryOps = LookupCtxt->lookup(DN);
  clang::DeclarationName DN2 = Context.CxxAST.DeclarationNames
                                                  .getCXXOperatorName(BinaryOO);
  auto BinaryOps = LookupCtxt->lookup(DN2);
  clang::DeclarationNameInfo DNI({Op->getClangName()}, RhsLoc);
  DNI.setName(DN);
  clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                        clang::Sema::LookupMemberName,
                        clang::Sema::NotForRedeclaration);
  for (clang::NamedDecl *ND : UnaryOps) {
    if (!ND->isCXXClassMember())
      continue;

    if (clang::CXXMethodDecl *MD
                = dyn_cast_or_null<clang::CXXMethodDecl>(ND->getAsFunction())) {
      if (!MD->isOverloadedOperator())
        continue;
      if (MD->getOverloadedOperator() == UnaryOO)
        R.addDecl(ND, ND->getAccess());
    }

  }
  for (clang::NamedDecl *ND : BinaryOps) {
    if (!ND->isCXXClassMember())
      continue;

    if (clang::CXXMethodDecl *MD
                = dyn_cast_or_null<clang::CXXMethodDecl>(ND->getAsFunction())) {
      if (!MD->isOverloadedOperator())
        continue;
      if (MD->getOverloadedOperator() == BinaryOO)
        R.addDecl(ND, ND->getAccess());
    }
  }
  clang::TemplateArgumentListInfo TemplateArgs;
  auto *UME = clang::UnresolvedMemberExpr::Create(Context.CxxAST,
                                                  false,
                                                  BaseExpr,
                                                  BaseExpr->getType(),
                                           BaseExpr->getType()->isPointerType(),
                                                  RhsLoc,
                                                clang::NestedNameSpecifierLoc(),
                                                  clang::SourceLocation(),
                                                  clang::DeclarationNameInfo(),
                                                  &TemplateArgs,
                                                  R.begin(),
                                                  R.end());
  return UME;
}

// possible expression results, used for diagnostics.
#define VALUE 0
#define NAMESPACE 1
#define TYPE 2
#define TEMPLATE 3

static unsigned getExprResultFromType(clang::QualType const &T) {
  if (T->isCppxNamespaceType())
    return NAMESPACE;
  if (T->isTemplateType())
    return TEMPLATE;
  if (T->isTypeOfTypes())
    return TYPE;

  return VALUE;
}

clang::Expr *ExprElaborator::elaborateMemberAccess(const Syntax *LHS,
                                                   const CallSyntax *Op,
                                                   const Syntax *RHS) {
  clang::Expr *ElaboratedLHS = elaborateExpr(LHS);

  if (!ElaboratedLHS) {
    SemaRef.Diags.Report(LHS->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  if (ElaboratedLHS->getType()->isTypeOfTypes())
    return elaborateNestedLookupAccess(ElaboratedLHS, RHS);

  if (ElaboratedLHS->getType()->isNamespaceType()) {
    if (clang::CppxNamespaceDecl *NSRef = dyn_cast<clang::CppxNamespaceDecl>(
                                        SemaRef.getDeclFromExpr(ElaboratedLHS,
                                        LHS->getLoc()))) {
      return elaborateNNS(NSRef, Op, RHS);
    }
    llvm_unreachable("Invalid namespace type returned.");
  }

  if (isa<AtomSyntax>(RHS)) {
    const AtomSyntax *RHSAtom = cast<AtomSyntax>(RHS);
    clang::UnqualifiedId Id;
    clang::IdentifierInfo *IdInfo =
      &Context.CxxAST.Idents.get(RHSAtom->getSpelling());
    OpInfoBase const *OpInfo = SemaRef.OpInfo.getOpInfo(IdInfo);
    if (OpInfo) {
      clang::OverloadedOperatorKind UnaryOO
                                               = OpInfo->getUnaryOverloadKind();
      clang::OverloadedOperatorKind BinaryOO
                                              = OpInfo->getBinaryOverloadKind();
      if (UnaryOO != BinaryOO) {
        clang::CXXScopeSpec TempSS;
        clang::Expr *LookedUpCandidates = doDerefAndXOrLookUp(Context, SemaRef,
                                                              UnaryOO, BinaryOO,
                                                              TempSS,
                                                              ElaboratedLHS,
                                                              Op, RHS->getLoc(),
                                                              OpInfo);
        return LookedUpCandidates;
      }
      clang::SourceLocation RHSLoc = RHSAtom->getLoc();
      clang::SourceLocation SymbolLocations[3] = {RHSLoc, RHSLoc, RHSLoc};
      Id.setOperatorFunctionId(RHSLoc, OpInfo->getUnaryOverloadKind(),
                               SymbolLocations);
    } else {
      Id.setIdentifier(IdInfo, RHSAtom->getLoc());
    }
    clang::CXXScopeSpec SS;
    clang::SourceLocation Loc;
    clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
    if (ElaboratedLHS->getType()->isPointerType())
      AccessTokenKind = clang::tok::TokenKind::arrow;

    clang::ExprResult RHSExpr =
      SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                                 ElaboratedLHS, Op->getLoc(),
                                                 AccessTokenKind, SS, Loc, Id,
                                                 nullptr);
    if (RHSExpr.isInvalid())
      return nullptr;

    ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr.get());
    return RHSExpr.get();
  }

  // A disambiguator of the form (a)b
  if (const CallSyntax *Disambig = dyn_cast<CallSyntax>(RHS)) {
    clang::Expr *LHS =
      ExprElaborator(Context, SemaRef).elaborateExpr(Disambig->getArgument(0));

    if (!LHS)
      return nullptr;
    if (!LHS->getType()->isTypeOfTypes()) {
      SemaRef.Diags.Report(LHS->getExprLoc(),
                           clang::diag::err_unexpected_expression_result)
        << TYPE << getExprResultFromType(LHS->getType());
      return nullptr;
    }

    clang::QualType Base =
      cast<clang::CppxTypeLiteral>(LHS)->getValue()->getType();

    clang::Expr *Res = elaborateNestedLookupAccess(LHS, Disambig->getArgument(1));
    if (!Res)
      return nullptr;
    if (!isa<clang::DeclRefExpr>(Res)) {
      SemaRef.Diags.Report(LHS->getExprLoc(),
                           clang::diag::err_failed_to_translate_expr);
      return nullptr;
    }

    clang::DeclRefExpr *FieldRef = cast<clang::DeclRefExpr>(Res);
    // TODO: need a diagnostic here, not sure what it should say
    if (!isa<clang::FieldDecl>(FieldRef->getDecl()))
      return nullptr;
    clang::FieldDecl *Field = cast<clang::FieldDecl>(FieldRef->getDecl());

    // FIXME: needs to handle templates and prefixes
    clang::CXXScopeSpec SS;
    clang::TypeLoc BaseTL =
      BuildAnyTypeLoc(CxxAST, Base, LHS->getExprLoc())->getTypeLoc();
    SS.Extend(CxxAST, clang::SourceLocation(), BaseTL, Disambig->getLoc());
    clang::UnqualifiedId Id;
    clang::IdentifierInfo *IdInfo =
      &Context.CxxAST.Idents.get(Field->getName());
    Id.setIdentifier(IdInfo, Field->getLocation());
    clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::period;
    if (ElaboratedLHS->getType()->isPointerType())
      AccessTokenKind = clang::tok::TokenKind::arrow;
    clang::SourceLocation Loc;
    clang::ExprResult Access =
      SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                                 ElaboratedLHS, Op->getLoc(),
                                                 AccessTokenKind, SS, Loc,
                                                 Id, nullptr);
    if (Access.isInvalid())
      return nullptr;

    return Access.get();
  }

  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "member access from non-object");
  SemaRef.Diags.Report(LHS->getLoc(), DiagID);
  return nullptr;
}

clang::Expr *ExprElaborator::elaborateNNS(clang::CppxNamespaceDecl *NS,
                                          const CallSyntax *Op,
                                          const Syntax *RHS) {
  clang::NamespaceDecl *CxxNS = NS->getNamespace();

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
  if (Failure)
    return nullptr;

  Sema::QualifiedLookupRAII Qual(SemaRef, SemaRef.QualifiedLookupContext, &NS);
  clang::Expr *RHSExpr = ExprElaborator(Context, SemaRef).elaborateExpr(RHS);
  if (!RHSExpr)
    return nullptr;

  if (RHSExpr->getType()->isNamespaceType())
    return RHSExpr;

  // This means that we've reached the end of a lookup and we can now clear the
  // namespace specifier context.
  SemaRef.CurNNSContext.clear();
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr);
  return RHSExpr;
}

clang::Expr *ExprElaborator::elaborateGlobalNNS(const CallSyntax *Op,
                                                const Syntax *RHS) {
  bool Failure = SemaRef.getCxxSema().ActOnCXXGlobalScopeSpecifier(Op->getLoc(),
                                                         SemaRef.CurNNSContext);

  if (Failure) {
    // FIXME: Create error message for this.
    llvm::outs() << "failed to create nns\n";
    return nullptr;
  }

  gold::Scope *GlobalScope = SemaRef.getCurrentScope();
  while (GlobalScope->getParent())
    GlobalScope = GlobalScope->getParent();

  auto *NS = clang::CppxNamespaceDecl::Create(Context.CxxAST,
                                        Context.CxxAST.getTranslationUnitDecl(),
                                              clang::SourceLocation(),
                                              nullptr, nullptr, GlobalScope);
  // TODO:: Create a reference to the newly created namespace?
  Sema::QualifiedLookupRAII Qual(SemaRef, SemaRef.QualifiedLookupContext, &NS);
  clang::Expr *RHSExpr = ExprElaborator(Context, SemaRef).elaborateExpr(RHS);

  if (!RHSExpr)
    return nullptr;

  if (RHSExpr->getType()->isNamespaceType())
    return RHSExpr;

  // TODO: should we account for an NNS that is returned from a function?

  SemaRef.CurNNSContext.clear();
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr);
  return RHSExpr;
}

clang::Expr *handleLookupInsideType(Sema &SemaRef, clang::ASTContext &CxxAST,
                                    const clang::Expr *Previous, const Syntax *RHS) {
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                              Previous, Previous->getExprLoc());
  if (!TInfo)
    return nullptr;

  clang::QualType QT = TInfo->getType();
  const clang::Type *T = QT.getTypePtr();
  if (!(T->isStructureOrClassType() || T->isUnionType()
      || T->isEnumeralType())) {
    SemaRef.Diags.Report(Previous->getExprLoc(),
                         clang::diag::err_invalid_type_for_name_spec)
                         << QT;
    return nullptr;
  }
  clang::TagDecl *TD = T->getAsTagDecl();

  // Processing if is a single name.
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(RHS)) {
    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())},
      Atom->getLoc());
    auto R = TD->lookup(DNI.getName());

    clang::NamedDecl *ND = nullptr;
    if (R.size() != 1u) {
      // This wasn't the name of a member, check if it is the name of a base.
      if (clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
        for (const auto &Base : RD->bases()) {
          clang::CXXRecordDecl *BaseRD = Base.getType()->getAsCXXRecordDecl();
          if (BaseRD->getIdentifier() == DNI.getName().getAsIdentifierInfo())
            ND = BaseRD;
        }
      }

      if (!ND) {
        SemaRef.Diags.Report(RHS->getLoc(), clang::diag::err_no_member)
          << Atom->getSpelling() << TD;
        return nullptr;
      }
    }

    if (!ND)
      ND = R.front();
    if (clang::TypeDecl *TD = dyn_cast<clang::TypeDecl>(ND)) {
      TD->setIsUsed();
      clang::QualType Ty = CxxAST.getTypeDeclType(TD);
      return SemaRef.buildTypeExpr(Ty, RHS->getLoc());
    }

    // This is how we access static member variables.
    if (clang::VarDecl *VDecl = dyn_cast<clang::VarDecl>(ND))
      return clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                        clang::SourceLocation(),VDecl,
                                        /*Capture=*/false, RHS->getLoc(),
                                        VDecl->getType(), clang::VK_LValue);

    // access a record from an NNS
    if (isa<clang::CXXRecordDecl>(ND))
      return SemaRef.buildTypeExprFromTypeDecl(TD, RHS->getLoc());

    // otherwise, we have a FieldDecl from a nested name specifier lookup.
    if (clang::ValueDecl *VD = dyn_cast<clang::ValueDecl>(ND))
      return clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                        clang::SourceLocation(), VD,
                                        /*Capture=*/false, RHS->getLoc(),
                                        VD->getType(), clang::VK_LValue);
  }

  llvm_unreachable("Unknown syntax encountered during nested member lookup.");
}

clang::Expr *ExprElaborator::elaborateNestedLookupAccess(const clang::Expr *Previous,
                                                         const Syntax *RHS) {
  assert(Previous && "Expression scoping.");
  if (Previous->getType()->isTypeOfTypes())
    return handleLookupInsideType(SemaRef, Context.CxxAST, Previous, RHS);

  if (Previous->getType()->isNamespaceType())
    llvm_unreachable("Nested namepsace not implemented");


  llvm_unreachable("Nested access to static variables not implemented");

}

clang::Expr *ExprElaborator::elaborateUnaryOp(const CallSyntax *S,
                                              clang::UnaryOperatorKind Op) {
  const Syntax *Operand = S->getArgument(0);
  clang::Expr *OperandResult = elaborateExpr(Operand);

  if (!OperandResult || OperandResult->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(Operand->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  // This is used to construct a pointer type because the carot has two
  // meanings. Dereference and pointer declaration.
  if (Op == clang::UO_Deref) {
    if (OperandResult->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                    OperandResult, S->getLoc());
      if (!TInfo)
        return nullptr;
      clang::QualType RetType = Context.CxxAST.getPointerType(TInfo->getType());
      return SemaRef.buildTypeExpr(RetType, S->getLoc());
    }
  }
  clang::ExprResult UnaryOpRes = SemaRef.getCxxSema().BuildUnaryOp(
                                                              /*scope*/nullptr,
                                                              S->getCalleeLoc(),
                                                              Op,
                                                              OperandResult);

  ExprMarker(Context.CxxAST, SemaRef).Visit(OperandResult);
  return UnaryOpRes.get();
}

clang::Expr *ExprElaborator::elaborateBinOp(const CallSyntax *S,
                                            clang::BinaryOperatorKind Op) {
  const Syntax *LHSSyntax = S->getArgument(0);
  const Syntax *RHSSyntax = S->getArgument(1);

  clang::Expr *LHS = elaborateExpr(LHSSyntax);
  if (!LHS) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Expr *RHS = elaborateExpr(RHSSyntax);
  if (!RHS) {
    SemaRef.Diags.Report(RHSSyntax->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  // FIXME: Replace with ActOnBinOp so precedence issues get warnings.
  clang::ExprResult Res = SemaRef.getCxxSema().BuildBinOp(/*Scope=*/nullptr,
                                                          S->getLoc(), Op, LHS,
                                                          RHS);
  if (Res.isInvalid()) {
    SemaRef.Diags.Report(S->getLoc(), clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }

  ExprMarker(Context.CxxAST, SemaRef).Visit(LHS);
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHS);

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
clang::Expr *
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  clang::Expr *LHS = nullptr;
  clang::Expr *RHS = nullptr;

  {
    ExprElaborator ExEl(Context, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));

    if (!LHS) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(Context, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(1));

    if (!RHS) {
      SemaRef.Diags.Report(Conditions->getChild(1)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }

  clang::ExprResult BinOp =
    SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, clang::SourceLocation(),
                                    clang::tok::ampamp, LHS, RHS);
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
                                      RHS);
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
    clang::Expr *Element = ExprElaborator(Context, SemaRef).elaborateExpr(SI);

    if (!Element)
      return nullptr;

    Elements.push_back(Element);
  }

  clang::ExprResult InitList =
    SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Elements, S->getLoc());
  if (InitList.isInvalid())
    return nullptr;

  return InitList.get();
}

clang::Expr *ExprElaborator::elaborateMacro(const MacroSyntax *S) {
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

  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "unknown macro");
  SemaRef.Diags.Report(S->getLoc(), DiagID);
  return nullptr;
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

clang::Expr *ExprElaborator::elaborateTypeExpr(Declarator *D) {
  // The type of a declarator is constructed back-to-front.
  llvm::SmallVector<Declarator *, 4> Decls;
  getDeclarators(D, Decls);

  // The type is computed from back to front. Start by assuming the type
  // is auto. This will be replaced if an explicit type specifier is given.
  clang::QualType AutoType = CxxAST.getAutoDeductType();
  clang::Expr *TyExpr = SemaRef.buildTypeExpr(AutoType, D->getLoc());
  for (auto Iter = Decls.rbegin(); Iter != Decls.rend(); ++Iter) {
    D = *Iter;
    switch (D->Kind) {
    case DK_Identifier:
      break;

    case DK_Error:
      // If we find an error we exit because we can't continue.
      return nullptr;

    case DK_Function: {
      clang::Expr *TypeExpr = elaborateFunctionType(D, TyExpr);
      if (!TypeExpr)
        return nullptr;

      TyExpr = TypeExpr;
      break;
    }

    case DK_Type: {
      clang::Expr *TypeExpr = elaborateExplicitType(D, TyExpr);
      if (!TypeExpr)
        return nullptr;

      TyExpr = TypeExpr;
      break;
    }

    case DK_TemplateType:{
      // Expression
      llvm_unreachable("Template parameters within the declarator "
          "not implemented yet.");
      break;
    }
    default:
      llvm_unreachable("Invalid declarator");
    }
  }
  return TyExpr;
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
clang::Expr *
ExprElaborator::elaborateFunctionType(Declarator *D, clang::Expr *Ty) {
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
      cast_or_null<clang::ValueDecl>(Elab.elaborateParmDeclSyntax(P));
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

  clang::QualType RetTy = SemaRef.getQualTypeFromTypeExpr(Ty);
  if (RetTy.isNull())
    return nullptr;
  clang::QualType FnTy = CxxAST.getFunctionType(RetTy, Types, EPI);
  return SemaRef.buildFunctionTypeExpr(FnTy, SourceLocation(), SourceLocation(),
                                       SourceLocation(), SourceRange(),
                                       SourceLocation(), Params);
}



clang::Expr *ExprElaborator::elaborateExplicitType(Declarator *D, clang::Expr *Ty) {
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
      return SemaRef.buildTypeExpr(BuiltinMapIter->second, Loc);
    }

    clang::TypeDecl *TD = R.getAsSingle<clang::TypeDecl>();
    TD->setIsUsed();
    return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);
  }
  return elaborateExpr(D->Data.Type);
}


clang::Expr *
ExprElaborator::handleOperatorConst(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "const operator");
  clang::Expr *innerTypeExpr = elaborateExpr(S->getArgument(0));
  return makeConstType(innerTypeExpr, S);
}

clang::Expr *ExprElaborator::handleRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "ref operator");
  clang::Expr *innerTypeExpr = elaborateExpr(S->getArgument(0));
  return makeRefType(innerTypeExpr, S);
}

clang::Expr *ExprElaborator::handleRRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "rref operator");
  clang::Expr *innerTypeExpr = elaborateExpr(S->getArgument(0));
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
clang::Expr *ExprElaborator::handleFunctionType(const CallSyntax *S) {
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

      ExprElaborator ParamElaborator(Context, SemaRef);
      clang::Expr *Param = ParamElaborator.elaborateExpr(ParamSyntax);

      if (!Param || !Param->getType()->isTypeOfTypes())
        continue;

      clang::SourceLocation Loc = ParamSyntax->getLoc();
      auto *ParmTInfo = SemaRef.getTypeSourceInfoFromExpr(Param, Loc);
      Types.push_back(ParmTInfo->getType());

      clang::IdentifierInfo *II = nullptr;
      II = SemaRef.getCxxSema().InventAbbreviatedTemplateParameterTypeName(II, I);
      clang::DeclarationName Name(II);
      clang::DeclContext *DC = SemaRef.getCurrentCxxDeclContext();
      clang::ParmVarDecl *PVD = clang::ParmVarDecl::Create(CxxAST, DC, Loc, Loc,
        Name, ParmTInfo->getType(), ParmTInfo, clang::SC_Auto, /*def=*/nullptr);
      Params.push_back(PVD);
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
  clang::Expr *Return = ReturnElaborator.elaborateExpr(ReturnSyntax);

  if (!Return->getType()->isTypeOfTypes()) {
    SemaRef.Diags.Report(ReturnSyntax->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::TypeSourceInfo *ReturnType =
    SemaRef.getTypeSourceInfoFromExpr(Return, S->getLoc());

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
  clang::TypeSourceInfo *FnPtrTSI =
    BuildFunctionPtrTypeLoc(CxxAST, TLB, FnTSI, S->getLoc());

  return SemaRef.buildTypeExpr(FnPtrTSI);
}

clang::Expr *ExprElaborator::handleArrayType(const CallSyntax *S) {
  if (S->getNumArguments() == 0
     || S->getNumArguments() == 1
     || S->getNumArguments() > 2) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::Expr *IdExpr = elaborateExpr(S->getArgument(1));
  if (!IdExpr) {
    return IdExpr;
  }
  // Attempt to translate into type location.
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(IdExpr,
                                                   S->getArgument(1)->getLoc());
  if (!TInfo)
    return nullptr;


  clang::Expr *IndexExpr = elaborateExpr(S->getArgument(0));

  // FIXME: what do we do for an empty array index, such as []int = {...}
  if (!IndexExpr) {
    SemaRef.Diags.Report(S->getArgument(0)->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::QualType BaseType = TInfo->getType();
  clang::Expr::EvalResult IdxResult;
  clang::Expr::EvalContext
    EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

  if (!IndexExpr->EvaluateAsConstantExpr(IdxResult,
                                         clang::Expr::EvaluateForCodeGen,
                                         EvalCtx))
    return nullptr;

  clang::QualType ArrayType =
    Context.CxxAST.getConstantArrayType(BaseType, IdxResult.Val.getInt(),
                                        IndexExpr, clang::ArrayType::Normal, 0);
  return SemaRef.buildTypeExpr(ArrayType, S->getLoc());
}

clang::Expr *ExprElaborator::makeConstType(clang::Expr *InnerTy,
                                           const CallSyntax* ConstOpNode) {
  if (!InnerTy) {
    SemaRef.Diags.Report(ConstOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InnerTy,
                                                        ConstOpNode->getLoc());
  if (!TInfo)
    return nullptr;
  clang::QualType EvaluatedTy = TInfo->getType();
  if (EvaluatedTy.isConstQualified()) {
    SemaRef.Diags.Report(ConstOpNode->getLoc(),
                         clang::diag::ext_warn_duplicate_declspec)
      << "const";
    return SemaRef.buildTypeExpr(EvaluatedTy, ConstOpNode->getLoc());
  }

  return SemaRef.buildTypeExpr(Context.CxxAST.getConstType(EvaluatedTy),
                               ConstOpNode->getLoc());
}

clang::Expr *ExprElaborator::makeRefType(clang::Expr *InnerTy,
                                         const CallSyntax* RefOpNode) {
  if (!InnerTy) {
    SemaRef.Diags.Report(RefOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InnerTy,
                                                           RefOpNode->getLoc());
  if (!TInfo)
    return nullptr;

  clang::QualType Inner = TInfo->getType();
  if (Inner.getTypePtr()->isReferenceType()) {
    SemaRef.Diags.Report(RefOpNode->getLoc(),
                         clang::diag::err_conflicting_specifier)
      << "ref";
    return nullptr;
  }
  return SemaRef.buildTypeExpr(CxxAST.getLValueReferenceType(Inner),
                               RefOpNode->getLoc());
}

clang::Expr *ExprElaborator::makeRRefType(clang::Expr *InnerTy,
                                          const CallSyntax* RRefOpNode) {
  if (!InnerTy) {
    SemaRef.Diags.Report(RRefOpNode->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(InnerTy,
                                                          RRefOpNode->getLoc());
  if (!TInfo)
    return nullptr;

  clang::QualType Inner = TInfo->getType();
  if (Inner.getTypePtr()->isReferenceType()) {
    SemaRef.Diags.Report(RRefOpNode->getLoc(),
                         clang::diag::err_conflicting_specifier)
      << "rref";
    return nullptr;
  }
  return SemaRef.buildTypeExpr(CxxAST.getRValueReferenceType(Inner),
                               RRefOpNode->getLoc());
}

#undef VALUE
#undef NAMESPACE
#undef TYPE
#undef TEMPLATE

} // namespace gold

