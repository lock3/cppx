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

#include "clang/Gold/ClangToGoldDeclBuilder.h"
#include "clang/Gold/GoldElaborator.h"
#include "clang/Gold/GoldExprElaborator.h"
#include "clang/Gold/GoldExprMarker.h"
#include "clang/Gold/GoldScope.h"
#include "clang/Gold/GoldSema.h"
#include "clang/Gold/GoldStmtElaborator.h"
#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldTokens.h"

 // This is needed for handling string literal trigraphs and escape characters.
#include "clang/Lex/Lexer.h"

#include <cstring>

namespace gold {

// possible expression results, used for diagnostics.
#define VALUE 0
#define NAMESPACE 1
#define TYPE 2
#define TEMPLATE 3
static unsigned getExprResultFromType(clang::QualType const &T);

static const llvm::StringMap<unsigned> BuiltinFnMap = {
#define BUILTIN(ID, TYPE, ATTRS) \
  {#ID, clang::Builtin::BI ## ID},
#include "clang/Basic/Builtins.def"
#undef BUILTIN
};

using TypeInfo = ExprElaborator::TypeInfo;

ExprElaborator::ExprElaborator(SyntaxContext &Context, Sema &SemaRef,
      clang::DeclContext *DC, gold::Scope *GoldScope)
  : Context(Context), CxxAST(Context.CxxAST), SemaRef(SemaRef),
  ParenToListHelper(SemaRef, false), CurrentLookUpContext(DC),
  OwningScope(GoldScope)
{ }

clang::Expr *ExprElaborator::elaborateExpr(const Syntax *S) {
  Sema::AttrElabRAII NonAttrContext(SemaRef, false);
  return doElaborateExpr(S);
}

clang::Expr *ExprElaborator::doElaborateExpr(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return completePartialExpr(
      elaborateAtom(cast<AtomSyntax>(S), clang::QualType())
    );
  if (isa<CallSyntax>(S))
    return completePartialExpr(
      elaborateCall(cast<CallSyntax>(S))
    );
  if (isa<MacroSyntax>(S))
    return completePartialExpr(
      elaborateMacro(cast<MacroSyntax>(S))
    );
  if (isa<ElemSyntax>(S))
    return completePartialExpr(
      elaborateElementExpr(cast<ElemSyntax>(S))
    );
  if (const ListSyntax *List = dyn_cast<ListSyntax>(S)) {
    if (List->getNumChildren() == 1) {
      clang::Expr *E = doElaborateExpr(List->getChild(0));
      if (SemaRef.getListIsParenExpr()) {
        auto Ret = SemaRef.getCxxSema().ActOnParenExpr(
                      List->getChild(0)->getLoc(), List->getChild(0)->getLoc(),
                      E);
        return Ret.get();
      }
      return E;
    }
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
  }
  return nullptr;
}

clang::Expr *ExprElaborator::elaborateConstexprAttrExpr(const Syntax *S) {
  Sema::AttrElabRAII AttrContext(SemaRef, true);
  clang::EnterExpressionEvaluationContext ConstantEvaluated(
                                                           SemaRef.getCxxSema(),
                   clang::Sema::ExpressionEvaluationContext::ConstantEvaluated);
  clang::Expr *Res = doElaborateExpr(S);
  if (!Res)
    return Res;
  auto ConstExpr = SemaRef.getCxxSema().ActOnConstantExpression(Res);
  if (ConstExpr.isInvalid())
    return nullptr;
  return ConstExpr.get();
}

clang::Expr *ExprElaborator::elaborateAttrExpr(const Syntax *S) {
  Sema::AttrElabRAII AttrContext(SemaRef, true);
  return doElaborateExpr(S);
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
  std::string Spelling = Base == 10 ? S->getSpelling() :
    S->getSpelling().substr(2);

  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

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
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "minifloats not yet supported by Clang");
    SemaRef.Diags.Report(S->getLoc(), DiagID);
    return nullptr;
  }

  const llvm::fltSemantics &Format = CxxAST.getFloatTypeSemantics(FloatType);
  using llvm::APFloat;
  APFloat Val = llvm::APFloat(Format);

  std::string Spelling = S->getSpelling();
  auto It = std::find(Spelling.begin(), Spelling.end(), '\'');
  while(It != std::end(Spelling)) {
    Spelling.erase(It);
    It = std::find(Spelling.begin(), Spelling.end(), '\'');
  }

  auto StatusOrErr =
    Val.convertFromString(Spelling, APFloat::rmNearestTiesToEven);
  assert(StatusOrErr && "Invalid floating point representation");
  return clang::FloatingLiteral::Create(CxxAST, Val, /*Exact=*/true,
                                        FloatType, S->getLoc());
}

static clang::FloatingLiteral *
createExponentLiteral(clang::ASTContext &CxxAST, Sema &SemaRef,
                      const LiteralSyntax *S, clang::SourceLocation Loc) {
  std::string Spelling = S->getSpelling();
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
  std::string Spelling = T.getSpelling();
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
                                              clang::CharacterLiteral::UTF8,
                                              SemaRef.DefaultCharTy, Loc);
}

static clang::CharacterLiteral *
createUTF8Literal(clang::ASTContext &CxxAST, Sema &SemaRef,
                  Token T, clang::SourceLocation Loc) {
  std::string Spelling = T.getSpelling();
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
  std::string Spelling = T.getSpelling();
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

  std::string Str = T.getSpelling();
  clang::Token CTok;
  CTok.startToken();
  CTok.setKind(clang::tok::utf8_string_literal);
  CTok.setLocation(StrNode->getLoc());
  CTok.setLength(Str.size());
  llvm::SmallVector<clang::Token, 1> StrTokens;
  StrTokens.push_back(CTok);
  clang::StringLiteralParser StrParser(StrTokens, CxxAST.getSourceManager(),
                                       CxxAST.getLangOpts(),
                                       CxxAST.getTargetInfo(), &SemaRef.Diags);

  clang::QualType StrTy = CxxAST.getStringLiteralArrayType(
                          SemaRef.DefaultCharTy, StrParser.GetNumStringChars());
  auto EncodingKind = clang::StringLiteral::UTF8;
  if (SemaRef.insideAttributeExpr()) {
    EncodingKind = clang::StringLiteral::Ascii;
  }
  return clang::StringLiteral::Create(CxxAST, StrParser.GetString(),
                                      EncodingKind,
                                      false, StrTy, StrNode->getLoc());

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
    clang::TemplateDecl *TD =
      E->getType()->getAs<clang::CppxTemplateType>()->getTemplateDecl();

    return clang::ParsedTemplateArgument(clang::ParsedTemplateArgument::Template,
                                         (void *)TD, E->getExprLoc());
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

  if (Elab.elaborateTemplateArugments(ElemArgs, TemplateArgs, ParsedArguments))
    return nullptr;

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
  clang::SourceLocation Loc = Elem->getLoc();
  if (clang::VarTemplateDecl *VTD = dyn_cast<clang::VarTemplateDecl>(CTD)) {
    clang::DeclarationNameInfo DNI(VTD->getDeclName(), Loc);
    clang::LookupResult R(SemaRef.getCxxSema(), DNI,
                          clang::Sema::LookupAnyName);
    R.addDecl(VTD);
    clang::ExprResult ER = SemaRef.getCxxSema().BuildTemplateIdExpr(
          SS, Loc, R, false, &TemplateArgs);
    if (ER.isInvalid())
      return nullptr;
    return ER.get();
  } else {
    clang::TypeResult Result = SemaRef.getCxxSema().ActOnTemplateIdType(
      SemaRef.getCurClangScope(), SS, /*TemplateKWLoc*/ Loc,
      TemplateTyName, II, Elem->getObject()->getLoc(),
      /*LAngleLoc*/Loc, InArgs, /*RAngleLoc*/ Loc, false, false);

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

static void constructTemplateArgList(SyntaxContext &Context, Sema &SemaRef,
                                     const ElemSyntax *Elem,
                                  clang::TemplateArgumentListInfo &TemplateArgs,
                   llvm::SmallVectorImpl<clang::TemplateArgument> &ActualArgs) {
  for (const Syntax *SS : Elem->getArguments()->children()) {
    ExprElaborator ParamElaborator(Context, SemaRef);
    clang::Expr *ParamExpression = ParamElaborator.doElaborateExpr(SS);
    if (!ParamExpression)
      continue;

    if (ParamExpression->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ParamTInfo
        = SemaRef.getTypeSourceInfoFromExpr(ParamExpression, Elem->getLoc());
      if (!ParamTInfo)
        continue;
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
}

static clang::Expr *
handleElementExpression(ExprElaborator &Elab, Sema &SemaRef,
                        SyntaxContext &Context, const ElemSyntax *Elem,
                        clang::Expr *E) {
  // This could be an attemped lambda instantiation.
  if (clang::DeclRefExpr *DRE = dyn_cast<clang::DeclRefExpr>(E)) {
    if (clang::VarDecl *VD = dyn_cast<clang::VarDecl>(DRE->getDecl())) {
      if (VD->getInit() && isa<clang::LambdaExpr>(VD->getInit())) {
        unsigned DiagID =
          SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                        "cannot explicitly instantiate lambda");
        SemaRef.Diags.Report(Elem->getLoc(), DiagID);
        return nullptr;
      }
    }
  }

  clang::OverloadExpr *OverloadExpr = dyn_cast<clang::OverloadExpr>(E);

  // Create a normal array access.
  if (!OverloadExpr) {
    llvm::SmallVector<clang::Expr *, 4> ArgExprs;
    for (const Syntax *SS : Elem->getArguments()->children()) {
      clang::Expr *Res = Elab.doElaborateExpr(SS);
      if (!Res)
        return nullptr;
      ArgExprs.push_back(Res);
    }

    if (ArgExprs.size() == 0) {
      SemaRef.Diags.Report(Elem->getArguments()->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }

    // Create the first subscript out of the base expression.
    auto SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
      SemaRef.getCurClangScope(), E, ArgExprs[0]->getExprLoc(),
      ArgExprs[0], ArgExprs[0]->getExprLoc());
    if (SubscriptExpr.isInvalid())
      return nullptr;

    // Then use the previous subscripts as bases, recursively.
    for (unsigned I = 1; I < ArgExprs.size(); ++I) {
      SubscriptExpr = SemaRef.getCxxSema().ActOnArraySubscriptExpr(
        SemaRef.getCurClangScope(), SubscriptExpr.get(),
        ArgExprs[I]->getExprLoc(), ArgExprs[I], ArgExprs[I]->getExprLoc());

      // We don't know what will happen if we try to recover, so just quit.
      if (SubscriptExpr.isInvalid())
        return nullptr;
    }

    return SubscriptExpr.get();
  }

  // We have an overload set, meaning this must be some kind of
  // overloaded function or function template.
  clang::TemplateArgumentListInfo TemplateArgs(Elem->getLoc(), Elem->getLoc());
  llvm::SmallVector<clang::TemplateArgument, 16> ActualArgs;
  constructTemplateArgList(Context, SemaRef, Elem, TemplateArgs, ActualArgs);
  clang::TemplateArgumentList
    TemplateArgList(clang::TemplateArgumentList::OnStack, ActualArgs);

  if (OverloadExpr->getNumDecls() == 1) {
    clang::NamedDecl *ND = *OverloadExpr->decls_begin();
    if (isa<clang::TemplateDecl>(ND)) {
      // We need to instantiate the template with parameters.
      if (clang::UnresolvedMemberExpr *MemAccess
              = dyn_cast<clang::UnresolvedMemberExpr>(OverloadExpr)) {
        auto *FTD = dyn_cast<clang::FunctionTemplateDecl>(ND);
        clang::FunctionDecl *FD =
          SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
                                                              &TemplateArgList,
                                                              Elem->getLoc());
        if (!FD) {
          unsigned DiagID =
            SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                          "function template instantiation failure");
          SemaRef.Diags.Report(Elem->getLoc(), DiagID);
          return nullptr;
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
      }
    } else if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(ND)) {
      if (FD->getTemplatedKind() == clang::FunctionDecl::TK_NonTemplate) {
        unsigned DiagID =
          SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                        "function is not a template");
        SemaRef.Diags.Report(Elem->getLoc(), DiagID);
        return nullptr;
      }

      clang::FunctionTemplateDecl *FTD = FD->getDescribedFunctionTemplate();
      if (!FTD) {
        unsigned DiagID =
          SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                        "function template does not have "
                                        "template parameters");
        SemaRef.Diags.Report(Elem->getLoc(), DiagID);
        return nullptr;
      }

      clang::FunctionDecl *InstantiatedFunc =
        SemaRef.getCxxSema().InstantiateFunctionDeclaration(FTD,
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
  }


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
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "unresolved template resolves to "
                                      "non-template");
      SemaRef.Diags.Report(Elem->getLoc(), DiagID);
      return nullptr;
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

  if (clang::UnresolvedMemberExpr *UME =
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
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "unresolved template resolves to "
                                      "non-template");
      SemaRef.Diags.Report(Elem->getLoc(), DiagID);
      return nullptr;
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

  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "cannot compile this type of overload yet");
  SemaRef.Diags.Report(Elem->getLoc(), DiagID);
  return nullptr;
}

clang::Expr *ExprElaborator::elaborateElementExpr(const ElemSyntax *Elem) {
  clang::Expr *IdExpr = doElaborateExpr(Elem->getObject());
  if (!IdExpr)
    return nullptr;
  if (isa<clang::CppxPartialEvalExpr>(IdExpr))
    return elaboratePartialElementExpr(IdExpr, Elem);

  // If we got a template specialization from elaboration, this is probably
  // a nested-name-specifier, there's nothing left to do.
  if (IdExpr->getType()->isTypeOfTypes()) {
    clang::CppxTypeLiteral *Lit = cast<clang::CppxTypeLiteral>(IdExpr);
    clang::CXXRecordDecl *RD = Lit->getValue()->getType()->getAsCXXRecordDecl();

    if (!RD)
      return nullptr;
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

bool ExprElaborator::elaborateTemplateArugments(const ListSyntax *Args,
                                    clang::TemplateArgumentListInfo &ArgInfo,
            llvm::SmallVectorImpl<clang::ParsedTemplateArgument> &ParsedArgs) {

  for(const Syntax *SyntaxArg : Args->children()) {
    clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
                                                          SemaRef.getCxxSema(),
                  clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
                                                /*LambdaContextDecl=*/nullptr,
                                                              /*ExprContext=*/
          clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);

    clang::Expr *ArgExpr = doElaborateExpr(SyntaxArg);
    if (!ArgExpr) {
      SemaRef.Diags.Report(SyntaxArg->getLoc(),
                           clang::diag::err_failed_to_translate_expr);
      continue;
    }

    auto TemplateArg = convertExprToTemplateArg(SemaRef, ArgExpr);
    if (TemplateArg.isInvalid())
      // TODO: Figure out if this needs an error message or not.
      // I assume that the errore message should be delivered prior to this.
      return true;

    ParsedArgs.emplace_back(TemplateArg);

    // Also building template Argument Info.
    if (ArgExpr->getType()->isTypeOfTypes()) {
      clang::TypeSourceInfo *ArgTInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                  ArgExpr, SyntaxArg->getLoc());
      if (!ArgTInfo)
        return true;
      clang::TemplateArgument Arg(ArgTInfo->getType());
      ArgInfo.addArgument({Arg, ArgTInfo});
    } else {
      clang::TemplateArgument Arg(ArgExpr, clang::TemplateArgument::Expression);
      ArgInfo.addArgument({Arg, ArgExpr});
    }
  }
  return false;
}

static clang::Expr *
createIdentAccess(SyntaxContext &Context, Sema &SemaRef, const AtomSyntax *S,
                  clang::QualType Ty, clang::SourceLocation Loc) {
  clang::ASTContext &CxxAST = Context.CxxAST;
  clang::IdentifierInfo *Id = &CxxAST.Idents.get(S->getSpelling());
  clang::DeclarationNameInfo DNI({Id}, Loc);

  if (S->getSpelling() == SemaRef.OpInfo.GoldDecl_OpNew->getName()
      || S->getSpelling() == SemaRef.OpInfo.GoldDecl_OpDelete->getName()
      || S->getSpelling() == SemaRef.OpInfo.GoldDecl_OpArray_New->getName()
      || S->getSpelling() == SemaRef.OpInfo.GoldDecl_OpArray_Delete->getName()) {
    SemaRef.createBuiltinOperatorNewDeleteDecls();
  }

  clang::LookupResult R(SemaRef.getCxxSema(), DNI, clang::Sema::LookupAnyName);
  R.setTemplateNameLookup(true);

  if (SemaRef.isQualifiedLookupContext()) {

    SemaRef.lookupQualifiedName(R);
  } else {
    if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
      SemaRef.Diags.Report(S->getLoc(),
                          clang::diag::err_identifier_not_declared_in_scope)
                          << S->getSpelling();
      return nullptr;
    }
  }

  if (!R.empty()) {
    // This has to be done this way specifically because the loop up will resolve
    // the NamespaceAliasDecl to it's associated namespace before it's used.
    // So this is the only solution that I could find that doesn't
    // rely on getUnderlyingType() to obtain the final declaration.
    const auto &DeclSet = R.asUnresolvedSet();
    if (DeclSet.size() == 1) {
      if (auto *NS = dyn_cast<clang::NamespaceAliasDecl>(*DeclSet.begin())) {
        return SemaRef.buildNSDeclRef(NS, Loc);
      }
    }
    R.resolveKind();
    if (!R.isSingleResult()) {
      if (R.isAmbiguous()) {
        SemaRef.Diags.Report(S->getLoc(), clang::diag::err_multiple_declarations)
                            << S->getSpelling();
        return nullptr;
      }
      if (R.isOverloadedResult()) {
        // This is necessary because of an error when converting between DNI's
        // with different DeclarationNameLoc types.
        DNI = SemaRef.rebuildDeclarationNameInfo(DNI);
        // Need to figure out if the potential overload is a member function
        // or not.
        return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
                                                   R.getNamingClass(),
                                                clang::NestedNameSpecifierLoc(),
                                                   DNI,
                                                   /*ADL=*/true, true,
                                                   R.begin(), R.end());
      }

      // TODO: FIXME: Create error message for here.
      llvm_unreachable("We are not currently handling multiple declarations "
          "returned. This needs to be fixed in order to correctly create proper "
          "results that can be returned to the caller");
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
        // Correctly rebuilding the declaration name info.
        DNI = SemaRef.rebuildDeclarationNameInfo(DNI);
        return clang::UnresolvedLookupExpr::Create(Context.CxxAST,
                                                   R.getNamingClass(),
                                                clang::NestedNameSpecifierLoc(),
                                                   DNI,
                                                   /*ADL=*/true, true,
                                                   R.begin(), R.end());
      }

      clang::QualType ResultType = VD->getType();
      if (ResultType.getTypePtr()->isReferenceType()) {
        ResultType = ResultType.getTypePtr()->getPointeeType();
      }

      clang::ExprValueKind ValueKind =
        SemaRef.getCxxSema().getValueKindForDeclReference(ResultType,
                                                          VD, S->getLoc());
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

    if (auto *NS = R.getAsSingle<clang::CppxNamespaceDecl>()) {

      return SemaRef.buildNSDeclRef(NS, Loc);
    }


    if (auto *TD = R.getAsSingle<clang::TypeDecl>())
      return SemaRef.buildTypeExprFromTypeDecl(TD, Loc);

    if (auto *TD = R.getAsSingle<clang::TemplateDecl>())
      return SemaRef.buildTemplateType(TD, Loc);
  } else {
    // Check for builtin types
    auto BuiltinMapIter = SemaRef.BuiltinTypes.find(S->getSpelling());
    if (BuiltinMapIter != SemaRef.BuiltinTypes.end())
      return SemaRef.buildTypeExpr(BuiltinMapIter->second, S->getLoc());
  }
  SemaRef.Diags.Report(S->getLoc(),
                       clang::diag::err_identifier_not_declared_in_scope)
                       << S->getSpelling();
  return nullptr;
}

static clang::Expr *createThisExpr(Sema &SemaRef, const AtomSyntax *S) {
  return SemaRef.getCxxSema().ActOnCXXThis(S->getLoc()).get();
}

static clang::CXXThisExpr *createThisCapture(clang::ASTContext &CxxAST,
                                             Sema &SemaRef,
                                             const AtomSyntax *S) {
  clang::Sema &CxxSema = SemaRef.getCxxSema();
  clang::SourceLocation Loc = S->getLoc();

  clang::sema::LambdaScopeInfo *LSI = CxxSema.getCurLambda();
  // C++11 [expr.prim.lambda]p8:
  //   An identifier or this shall not appear more than once in a
  //   lambda-capture.
  if (LSI->isCXXThisCaptured()) {
    SemaRef.Diags.Report(Loc, clang::diag::err_capture_more_than_once)
      << "'this'" << clang::SourceRange(LSI->getCXXThisCapture().getLocation())
      << clang::FixItHint::CreateRemoval(clang::SourceRange(Loc, Loc));
    return nullptr;
  }

  clang::QualType ThisTy = CxxSema.getCurrentThisType();
  if (ThisTy.isNull()) {
    SemaRef.Diags.Report(Loc, clang::diag::err_invalid_this_use);
    return nullptr;
  }

  auto *This = new (CxxAST) clang::CXXThisExpr(Loc, ThisTy, /*Implicit=*/false);
  CxxSema.CheckCXXThisCapture(Loc, /*Explicit=*/true, /*Diagnose=*/true,
                              /*StopAt=*/nullptr, /*ByCopy=*/true);
  return This;
}

static inline bool hasFloatingTypeSuffix(const LiteralSyntax *S) {
  return S->Suffix.IsFloat || S->Suffix.IsDouble
    || S->Suffix.IsHalf || S->Suffix.IsQuarter;
}

clang::Expr *ExprElaborator::elaborateAtom(const AtomSyntax *S,
                                           clang::QualType ExplicitType) {
  Token T = S->Tok;

  // Check if we have a floating literal that looks like an int.
  if (const LiteralSyntax *L = dyn_cast<LiteralSyntax>(S))
    if (hasFloatingTypeSuffix(L))
      return createFloatLiteral(CxxAST, SemaRef, L);

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(CxxAST, SemaRef, cast<LiteralSyntax>(S));
  case tok::DecimalFloat:
    return createFloatLiteral(CxxAST, SemaRef, cast<LiteralSyntax>(S));
  case tok::BinaryInteger: 
    return createIntegerLiteral(CxxAST, SemaRef, cast<LiteralSyntax>(S), 2);
  case tok::HexadecimalInteger:
    return createIntegerLiteral(CxxAST, SemaRef,
                                cast<LiteralSyntax>(S), /*Base=*/16);
  case tok::HexadecimalFloat:
    llvm_unreachable("Hexadecimal float not implemented");
  case tok::Identifier:
    return createIdentAccess(Context, SemaRef, S, ExplicitType, S->getLoc());
  case tok::Character:
    return createCharLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::HexadecimalCharacter:
    return createUTF8Literal(CxxAST, SemaRef, T, S->getLoc());
  case tok::UnicodeCharacter:
    return createUnicodeLiteral(CxxAST, SemaRef, T, S->getLoc());
  case tok::String:
    return createStringLiteral(CxxAST, SemaRef, T, S);
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
    return SemaRef.getCurrentScope()->isLambdaCaptureScope() ?
      createThisCapture(CxxAST, SemaRef, S) : createThisExpr(SemaRef, S);
  default:
    break;
  }

  auto BuiltinMapIter = SemaRef.BuiltinTypes.find(S->getSpelling());
  if (BuiltinMapIter != SemaRef.BuiltinTypes.end()) {
    if (BuiltinMapIter->second.isNull()) {
      SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_builtin_type)
        << S->getSpelling();
      return nullptr;
    }

    return SemaRef.buildTypeExpr(BuiltinMapIter->second, S->getLoc());
  }

  SemaRef.Diags.Report(S->getLoc(), clang::diag::err_invalid_identifier_type)
                       << S->getSpelling();

  return nullptr;
}

static bool buildAnyFunctionCallArguments(Sema &SemaRef, SyntaxContext &Context,
    Syntax::child_range Children, llvm::SmallVector<clang::Expr *, 8> &Args) {
  for (const Syntax *A : Children) {
    ExprElaborator Elab(Context, SemaRef);
    clang::Expr *Argument = Elab.doElaborateExpr(A);
    if (!Argument) {
      SemaRef.Diags.Report(A->getLoc(), clang::diag::err_expected_expression);
      return true;
    }
    Args.push_back(Argument);
  }
  return false;
}

static bool buildFunctionCallArguments(Sema &SemaRef, SyntaxContext &Context,
    const ListSyntax *ArgList,
    llvm::SmallVector<clang::Expr *, 8> &Args) {
  return buildAnyFunctionCallArguments(SemaRef, Context, ArgList->children(), Args);
}

static bool buildFunctionCallArguments(Sema &SemaRef, SyntaxContext &Context,
    const ArraySyntax *ArgList,
    llvm::SmallVector<clang::Expr *, 8> &Args) {

  if (ArgList->getNumChildren() > 0)
    if (auto LS = dyn_cast<ListSyntax>(ArgList->getChild(0)))
      return buildAnyFunctionCallArguments(SemaRef, Context,
                                          LS->children(), Args);

  return buildAnyFunctionCallArguments(SemaRef, Context,
                                       ArgList->children(), Args);
}


/// This function's job is to create the correct call based upon the result
/// type of the CalleeExpr, which could be any of the types within the
/// Expression union type.
static clang::Expr *handleExpressionResultCall(Sema &SemaRef,
                                               ExprElaborator &Elab,
                                               const CallSyntax *S,
                                               clang::Expr *CalleeExpr,
                                    llvm::SmallVector<clang::Expr *, 8> &Args) {
  if (!CalleeExpr) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_expr);
    return nullptr;
  }
  if (isa<clang::CppxPartialEvalExpr>(CalleeExpr))
    return Elab.elaboratePartialCallExpr(CalleeExpr, S, Args);

  if (CalleeExpr->getType()->isTypeOfTypes()) {
    // This means constructor call possibly, unless it's some how a function
    // call returning a type.
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                                 CalleeExpr, S->getLoc());
    if (!TInfo)
      // If this isn't a CppxTypeLiteral then we can't get the type yet and it
      // might be a meta function.kind
      return nullptr;
    clang::ExprResult ConstructorExpr =
        SemaRef.getCxxSema().BuildCXXTypeConstructExpr(TInfo, S->getLoc(),
                                                      Args, S->getLoc(),
                                                   /*ListInitialization*/false);

    if (!ConstructorExpr.get()) {
      SemaRef.Diags.Report(S->getLoc(),
                            clang::diag::err_coroutine_invalid_func_context)
                            << 0 << TInfo->getType();
      return nullptr;
    }
    return ConstructorExpr.get();
  }

  // A call to a specialization without arguments has to be handled differently
  // than other call expressions, so figure out if this could be one.
  // FIXME: What is this? It doesn't look like it actually does anything?!
  if (auto *ULE = dyn_cast<clang::UnresolvedLookupExpr>(CalleeExpr))
    if (!ULE->hasExplicitTemplateArgs())
      for (auto D : ULE->decls())
        if (auto *FD = dyn_cast<clang::FunctionDecl>(D))
          if (FD->getTemplatedKind() ==
              clang::FunctionDecl::TK_FunctionTemplateSpecialization) {
            ;
          }

  // TODO: create a test for this were we call a namespace just to see what kind
  // of error actually occurs.

  // For any thing else we simply try and make the call and see what happens.
  clang::ExprResult Call = SemaRef.getCxxSema().ActOnCallExpr(
    SemaRef.getCxxSema().getCurScope(), CalleeExpr, S->getCalleeLoc(),
    Args, S->getCalleeLoc());

  if (Call.isInvalid())
    return nullptr;

  return Call.get();
}

static bool callIsCastOperator(const CallSyntax *S) {
  if (const ElemSyntax *Elem = dyn_cast<ElemSyntax>(S->getCallee())) {
    if (const AtomSyntax *Callee
        = clang::dyn_cast<AtomSyntax>(Elem->getObject())) {
      std::string Name = Callee->getSpelling();
      return Name == "static_cast" || Name == "dynamic_cast"
          || Name == "const_cast" || Name == "reinterpret_cast";
    }
  }
  return false;
}

clang::Expr *ExprElaborator::elaborateCall(const CallSyntax *S) {
  if (callIsCastOperator(S))
    return elaborateCastOp(S);

  if (clang::Expr *elaboratedCall = elaborateBuiltinOperator(S))
    return elaboratedCall;


  // Determining the type of call associated with the given syntax.
  // There are multiple kinds of atoms for multiple types of calls
  // but in the event that the callee object is not an Atom, it means
  // that we have to process the sub expression as normal.
  clang::Expr *CalleeExpr;
  if (!isa<AtomSyntax>(S->getCallee())) {
    CalleeExpr = doElaborateExpr(S->getCallee());
    llvm::SmallVector<clang::Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
    // TODO: Add partial expr management here?
    if (buildFunctionCallArguments(SemaRef, Context, ArgList, Args))
      return nullptr;

    return handleExpressionResultCall(SemaRef, *this, S, CalleeExpr, Args);
  }


  // In the event that we do have an atom for the call name we need to do
  // something slightly different before we can fully elaborate the entire call.
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  Token Tok = Callee->getToken();
  if (Tok.getKind() == tok::ArrayDelete)
    return elaborateDeleteExpr(true, S);

  if (Tok.getKind() == tok::DeleteKeyword)
    return elaborateDeleteExpr(false, S);

  FusedOpKind Op = getFusedOpKind(SemaRef, Callee->getSpelling());

  switch (Op) {
  case FOK_MemberAccess: {
    const ListSyntax *Args = cast<ListSyntax>(S->getArguments());

    if (Args->getNumChildren() == 1)
      return elaborateGlobalNNS(S, Args->getChild(0));
    return elaborateMemberAccess(Args->getChild(0), S, Args->getChild(1));
  }

  case FOK_DotDot:
    llvm_unreachable("FOK_DotDot is not supported in this context");

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
  case FOK_Throw:
    return elaborateThrowExpr(S);

  default:
    break;
  }

  // Handling special case for type
  if (Callee->hasToken(tok::Ellipsis))
    return handleOpPackExpansion(S);

  std::string Temp = Callee->getSpelling();
  llvm::StringRef Spelling = Temp;
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

  auto BuiltinFnIt = BuiltinFnMap.find(Callee->getSpelling());
  if (BuiltinFnIt != BuiltinFnMap.end())
    return handleBuiltinCall(S, BuiltinFnIt->second);
  if (Callee->hasToken(tok::VaArgKeyword))
    return handleVaArg(S);

  // Elaborating callee name expression.
  CalleeExpr = doElaborateExpr(S->getCallee());
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  if (buildFunctionCallArguments(SemaRef, Context, ArgList, Args)) {
    // TODO: Determine the correct message to output here.
    return nullptr;
  }
  return handleExpressionResultCall(SemaRef, *this, S, CalleeExpr, Args);
}

/// This returns true if the keyword is a builtin function.
static bool isBuiltinOperator(const CallSyntax *S) {
  if (const auto *Atom = dyn_cast<AtomSyntax>(S->getCallee())) {
    switch (Atom->Tok.getKind()) {
      case tok::AlignOfKeyword:
      case tok::SizeOfKeyword:
      case tok::SizeOfPack:
      case tok::NoExceptKeyword:
      case tok::DeclTypeKeyword:
      case tok::UnaryRightFold:
      case tok::UnaryLeftFold:
      case tok::BinaryFold:
      case tok::TypeidKeyword:
        return true;
      default:
        break;
    }
  }
  return false;
}

clang::Expr *ExprElaborator::elaborateBuiltinOperator(const CallSyntax *S) {
  if (!isBuiltinOperator(S))
    return nullptr;

  const AtomSyntax *Atom = cast<AtomSyntax>(S->getCallee());
  switch (Atom->Tok.getKind()) {
  case tok::NoExceptKeyword:
    return elaborateNoExceptOp(Atom, S);

  case tok::DeclTypeKeyword:
    return elaborateDeclTypeOp(Atom, S);

  case tok::SizeOfPack:
    return elaborateSizeOfPack(Atom, S);

  case tok::AlignOfKeyword:
    return elaborateTypeTraitsOp(Atom, S, clang::UETT_AlignOf);

  case tok::SizeOfKeyword:
    return elaborateTypeTraitsOp(Atom, S, clang::UETT_SizeOf);
  case tok::UnaryRightFold:
    return elaborateRightFoldExpr(Atom, S);
  case tok::UnaryLeftFold:
    return elaborateLeftFoldExpr(Atom, S);
  case tok::BinaryFold:
    return elaborateBinaryFoldExpr(Atom, S);
  case tok::TypeidKeyword:
    return elaborateTypeidOp(Atom, S);
  default:
    llvm_unreachable("Invalid buildin function elaboration");
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
  clang::Expr *ResultExpr = doElaborateExpr(S->getArgument(0));
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

namespace {

// Callback to only accept typo corrections that refer to parameter packs.
class ParameterPackValidatorCCC final : public clang::CorrectionCandidateCallback {
 public:
  bool ValidateCandidate(const clang::TypoCorrection &candidate) override {
    clang::NamedDecl *ND = candidate.getCorrectionDecl();
    return ND && ND->isParameterPack();
  }

  std::unique_ptr<CorrectionCandidateCallback> clone() override {
    return std::make_unique<ParameterPackValidatorCCC>(*this);
  }
};

}

clang::Expr *
ExprElaborator::elaborateSizeOfPack(const AtomSyntax *Name,
                                    const CallSyntax *S) {
  if (S->getNumArguments() != 1) {
    SemaRef.Diags.Report(Name->getLoc(),
                         clang::diag::err_invalid_call_to_sizeof_pack);
    return nullptr;
  }

  auto PackName = dyn_cast_or_null<AtomSyntax>(S->getArgument(0));
  if (!PackName) {
    SemaRef.Diags.Report(Name->getLoc(),
                         clang::diag::err_invalid_sizeof_complex_arg);
    return nullptr;
  }
  clang::IdentifierInfo &Id = Context.CxxAST.Idents.get(PackName->getSpelling());
  // C++0x [expr.sizeof]p5:
  //   The identifier in a sizeof... expression shall name a parameter pack.
  clang::LookupResult R(SemaRef.getCxxSema(), &Id, Name->getLoc(),
                        clang::Sema::LookupOrdinaryName);
  if (!SemaRef.lookupUnqualifiedName(R, SemaRef.getCurrentScope())) {
    SemaRef.Diags.Report(S->getLoc(),
                        clang::diag::err_identifier_not_declared_in_scope)
                        << Name->getSpelling();
    return nullptr;
  }

  clang::NamedDecl *ParameterPack = nullptr;
  switch (R.getResultKind()) {
  case clang::LookupResult::Found:
    ParameterPack = R.getFoundDecl();
    break;

  case clang::LookupResult::NotFound:
  case clang::LookupResult::NotFoundInCurrentInstantiation: {
    ParameterPackValidatorCCC CCC{};
    if (clang::TypoCorrection Corrected =
            SemaRef.getCxxSema().CorrectTypo(R.getLookupNameInfo(),
                                             R.getLookupKind(),
                                             SemaRef.getCurClangScope(),
                                             nullptr,
                                             CCC,
                                             clang::Sema::CTK_ErrorRecovery)) {
      SemaRef.getCxxSema().diagnoseTypo(Corrected,
        SemaRef.getCxxSema().PDiag(
          clang::diag::err_sizeof_pack_no_pack_name_suggest) << &Id,
        SemaRef.getCxxSema().PDiag(clang::diag::note_parameter_pack_here));
      ParameterPack = Corrected.getCorrectionDecl();
    }
    break;
  }
  case clang::LookupResult::FoundOverloaded:
  case clang::LookupResult::FoundUnresolvedValue:
    break;

  case clang::LookupResult::Ambiguous:
    SemaRef.getCxxSema().DiagnoseAmbiguousLookup(R);
    // return ExprError();
    return nullptr;
  }

  if (!ParameterPack || !ParameterPack->isParameterPack()) {
    SemaRef.getCxxSema().Diag(Name->getLoc(),
                              clang::diag::err_sizeof_pack_no_pack_name)
                              << &Id;
    return nullptr;
  }

  SemaRef.getCxxSema().MarkAnyDeclReferenced(Name->getLoc(), ParameterPack, true);

  return clang::SizeOfPackExpr::Create(Context.CxxAST,
                                       Name->getLoc(), ParameterPack,
                                       Name->getLoc(),
                                       PackName->getLoc());
}

clang::Expr *
ExprElaborator::elaborateDeclTypeOp(const AtomSyntax *Name,
                                    const CallSyntax *S) {
  assert(Name->Tok.getKind() == tok::DeclTypeKeyword
         && "Invalid elaboration of decltype operator");
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
  clang::Expr *ArgEval = doElaborateExpr(S->getArgument(0));
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
    clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(ArgEval,
                                                   S->getArgument(0)->getLoc());
    if (TInfo->getType() == Context.CxxAST.getAutoDeductType()) {
      clang::QualType Ty = Context.CxxAST.getAutoType(clang::QualType(),
                                           clang::AutoTypeKeyword::DecltypeAuto,
                                                      /*IsPack-*/false);
      return SemaRef.buildTypeExpr(Ty, S->getLoc());
    }

    return SemaRef.buildTypeExpr(Context.CxxAST.CppxKindTy, S->getLoc());
  }

  if (ArgEval->getType()->isNamespaceType()) {
    llvm_unreachable("Decltype of a namespace not implemented yet");
  }

  if (ArgEval->getType()->isTemplateType()) {
    llvm_unreachable("Template expression decltype not implemented yet");
  }

  // This does some semantic checking on the given expression.
  auto ExprResult = SemaRef.getCxxSema().ActOnDecltypeExpression(ArgEval);
  if (ExprResult.isInvalid())
    return nullptr;

  clang::QualType Ty = SemaRef.getCxxSema().BuildDecltypeType(ExprResult.get(),
                                                   S->getArgument(0)->getLoc());

  return SemaRef.buildTypeExpr(Ty, S->getArgument(0)->getLoc());
}

clang::Expr *ExprElaborator::elaborateTypeidOp(const AtomSyntax *Name,
                                               const CallSyntax *S) {
  assert(Name->Tok.hasKind(tok::TypeidKeyword) && "invalid typeid syntax");
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
  const Syntax *ArgSyntax = S->getArgument(0);
  clang::Expr *ArgEval = doElaborateExpr(ArgSyntax);
  if (!ArgEval)
    return nullptr;

  if (ArgEval->getType()->isNamespaceType() ||
      ArgEval->getType()->isTemplateType()  ||
      isa<clang::CppxDeclRefExpr>(ArgEval)) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "operand to 'typeid' must have type of "
                                    "types or value type");
    SemaRef.Diags.Report(S->getLoc(), DiagID);
    return nullptr;
  }

  clang::ExprResult Res =
    SemaRef.getCxxSema().ActOnCXXTypeid(S->getLoc(), ArgSyntax->getLoc(),
                                        ArgEval->getType()->isTypeOfTypes(),
                                        ArgEval, ArgSyntax->getLoc());
  if (Res.isInvalid())
    return nullptr;
  return Res.get();
}

clang::Expr *
ExprElaborator::elaborateNoExceptOp(const AtomSyntax *Name,
                                    const CallSyntax *S) {
  assert(Name->Tok.getKind() == tok::NoExceptKeyword
         && "Invalid elaboration of noexcept operator");
  clang::EnterExpressionEvaluationContext Unevaluated(SemaRef.getCxxSema(),
    clang::Sema::ExpressionEvaluationContext::Unevaluated);

  clang::Expr *ArgEval = doElaborateExpr(S->getArgument(0));
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

clang::Expr *ExprElaborator::elaborateRightFoldExpr(const AtomSyntax *Name,
                                                    const CallSyntax *S) {
  Sema::ParenExprRAII ParenExpr(SemaRef, true);
  assert(Name && "Missing name");
  assert(S && "Missing call syntax");
  assert(Name->getToken().getKind() == tok::UnaryRightFold
         && "Incorrect fold operator");
  assert(S->getNumArguments() == 1 && "Incorrectly formatted AST");
  const Syntax *OperandSyntax = S->getArgument(0);
  clang::Expr *OperandExpr = elaborateExpr(OperandSyntax);
  if (!OperandExpr)
    return nullptr;
  clang::Expr *Ret = SemaRef.actOnCxxFoldExpr(Name->getLoc(), OperandExpr,
                                              Name->getToken(),
                                              Name->getLoc(), nullptr,
                                              OperandSyntax->getLoc());
  return Ret;
}

clang::Expr *ExprElaborator::elaborateLeftFoldExpr(const AtomSyntax *Name,
                                                   const CallSyntax *S) {
  Sema::ParenExprRAII ParenExpr(SemaRef, true);
  assert(Name && "Missing name");
  assert(S && "Missing call syntax");
  assert(Name->getToken().getKind() == tok::UnaryLeftFold
         && "Incorrect fold operator");
  assert(S->getNumArguments() == 1 && "Incorrectly formatted AST");
  const Syntax *OperandSyntax = S->getArgument(0);
  clang::Expr *OperandExpr = elaborateExpr(OperandSyntax);
  if (!OperandExpr)
    return nullptr;
  clang::Expr *Ret = SemaRef.actOnCxxFoldExpr(Name->getLoc(), nullptr,
                                              Name->getToken(),
                                              Name->getLoc(), OperandExpr,
                                              OperandSyntax->getLoc());
  return Ret;
}

clang::Expr *ExprElaborator::elaborateBinaryFoldExpr(const AtomSyntax *Name,
                                                     const CallSyntax *S) {
  Sema::ParenExprRAII ParenExpr(SemaRef, true);
  assert(Name && "Missing name");
  assert(S && "Missing call syntax");
  assert(Name->getToken().getKind() == tok::BinaryFold
         && "Incorrect fold operator");
  assert(S->getNumArguments() == 2 && "Incorrectly formatted AST");
  const Syntax *LHSSyntax = S->getArgument(0);
  clang::Expr *LHSExpr = elaborateExpr(LHSSyntax);
  if (!LHSExpr)
    return nullptr;
  const Syntax *RHSSyntax = S->getArgument(1);
  clang::Expr *RHSExpr = elaborateExpr(RHSSyntax);
  if (!RHSExpr)
    return nullptr;
  clang::Expr *Ret = SemaRef.actOnCxxFoldExpr(LHSSyntax->getLoc(), LHSExpr,
                                              Name->getToken(),
                                              Name->getLoc(), RHSExpr,
                                              RHSSyntax->getLoc());
  return Ret;
}

clang::Expr *ExprElaborator::elaborateThrowExpr(const CallSyntax *S) {
  assert(S && "Invalid statement");
  clang::Expr *ThrowExpr = nullptr;
  if (S->getNumArguments() == 1) {
    ThrowExpr = ExprElaborator(Context, SemaRef).elaborateExpr(S->getArgument(0));
  }
  auto ExprRes = SemaRef.getCxxSema().ActOnCXXThrow(SemaRef.getCurClangScope(),
    S->getCallee()->getLoc(), ThrowExpr);

  return ExprRes.get();
}

clang::Expr *
ExprElaborator::elaborateDeleteExpr(bool IsArrayDelete, const CallSyntax *Call) {
  auto DelArgList = dyn_cast_or_null<ListSyntax>(Call->getArguments());
  if (DelArgList->getNumChildren() != 1) {
    SemaRef.Diags.Report(Call->getCallee()->getLoc(),
                         clang::diag::warn_call_wrong_number_of_arguments)
                         << (DelArgList->getNumChildren() >= 1)
                         << (IsArrayDelete ? "delete[]" : "delete");
    return nullptr;
  }
  clang::Expr *E = elaborateExpr(DelArgList->getChild(0));
  if (!E)
    return nullptr;
  clang::QualType Ty = E->getType();

  // Enfocing someone trying to delete a type, or namespace.
  if (Ty->isTypeOfTypes() || Ty->isTemplateType() || Ty->isNamespaceType()) {
    SemaRef.Diags.Report(E->getExprLoc(),
                         clang::diag::err_unexpected_expression_result)
                         << /*value*/0 << getExprResultFromType(Ty);
    return nullptr;
  }
  auto DelExpr = SemaRef.getCxxSema().ActOnCXXDelete(Call->getLoc(),
                                                     /*UseGlobal*/false,
                                                     IsArrayDelete, E);
  return DelExpr.get();
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
    llvm_unreachable("Improperly formatted AST");
  }
  if (TypeArgumentList->getNumChildren() != 1) {
    SemaRef.Diags.Report(CastOp->getLoc(),
                         clang::diag::err_invalid_cast_type_arg_count);
    return nullptr;
  }

  std::string Name = Callee->getSpelling();
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
    llvm_unreachable("Invalid cast name");
  }

  clang::Expr *DestinationTy = doElaborateExpr(Elem->getArgument(0));
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(
                                 DestinationTy, Elem->getArgument(0)->getLoc());
  if (!TInfo){
    SemaRef.Diags.Report(Elem->getArgument(0)->getLoc(),
                      clang::diag::err_invalid_cast_destination_type);
    return nullptr;
  }

  clang::Expr *Source = doElaborateExpr(CastOp->getArgument(0));
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
                                           const CallSyntax *Op,
                                           clang::Expr *Previous,
                                           const Syntax *RHS,
                                           bool AddressOf);

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
        R.addDecl(MD, MD->getAccess());
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
        R.addDecl(MD, MD->getAccess());
    }
  }

  if (R.empty()) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "expected operator but did not find it");
    SemaRef.Diags.Report(OpSyntax->getLoc(), DiagID);
    return nullptr;
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
  clang::Expr *ElaboratedLHS = doElaborateExpr(LHS);

  if (!ElaboratedLHS) {
    SemaRef.Diags.Report(LHS->getLoc(), clang::diag::err_expected_expression);
    return nullptr;
  }

  if (ElaboratedLHS->getType()->isTypeOfTypes())
    return elaborateNestedLookupAccess(ElaboratedLHS, Op, RHS);

  if (ElaboratedLHS->getType()->isNamespaceType()) {
    if (clang::CppxNamespaceDecl *NSRef = dyn_cast<clang::CppxNamespaceDecl>(
                                        SemaRef.getDeclFromExpr(ElaboratedLHS,
                                        LHS->getLoc()))) {
      return elaborateNNS(NSRef, Op, RHS);
    }
    if (clang::NamespaceAliasDecl *NSAliasRef = dyn_cast<clang::NamespaceAliasDecl>(
                                        SemaRef.getDeclFromExpr(ElaboratedLHS,
                                        LHS->getLoc()))) {
      return elaborateNNS(NSAliasRef, Op, RHS);
    }
    llvm_unreachable("Invalid namespace type returned.");
  }

  // Handling member elaboration.
  return elaborateMemberAccessRHS(ElaboratedLHS, LHS, Op, RHS);
}

clang::Expr *ExprElaborator::elaborateMemberAccessRHS(clang::Expr *ElaboratedLHS,
                                                      const Syntax *LHS,
                                                      const CallSyntax *Op,
                                                      const Syntax *RHS) {
  if (auto RHSAtom = dyn_cast<AtomSyntax>(RHS))
    return elaborateMemberAccessRHSAtom(ElaboratedLHS, LHS, Op, RHSAtom);

  // A disambiguator of the form (a)b
  if (const CallSyntax *Disambig = dyn_cast<CallSyntax>(RHS))
    return elaborateDisambuationSyntax(ElaboratedLHS, LHS, Op, Disambig);

  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "member access from non-object");
  SemaRef.Diags.Report(LHS->getLoc(), DiagID);
  return nullptr;
}

clang::Expr *
ExprElaborator::elaborateConstructDestructExpr(clang::Expr *ElaboratedLHS,
                                               const Syntax *LHS,
                                               const CallSyntax *Op,
                                               const AtomSyntax *RHS) {
  if (RHS->getSpelling() == "construct")
    return elaborateInPlaceNewCall(ElaboratedLHS, Op, RHS);
  if (RHS->getSpelling() == "destruct")
    return elaborateDestructCall(ElaboratedLHS, Op, RHS);
  return nullptr;
}

clang::Expr *
ExprElaborator::elaborateMemberAccessRHSAtom(clang::Expr *ElaboratedLHS,
                                             const Syntax *LHS,
                                             const CallSyntax *Op,
                                             const AtomSyntax *RHS) {
  // Handling special calls to Construct/Destruct
  if (clang::Expr *Ret = elaborateConstructDestructExpr(ElaboratedLHS, LHS,
                                                        Op, RHS))
    return Ret;
  clang::UnqualifiedId Id;
  clang::IdentifierInfo *IdInfo =
    &Context.CxxAST.Idents.get(RHS->getSpelling());
  OpInfoBase const *OpInfo = SemaRef.OpInfo.getOpInfo(IdInfo);
  if (OpInfo) {
    clang::OverloadedOperatorKind UnaryOO = OpInfo->getUnaryOverloadKind();
    clang::OverloadedOperatorKind BinaryOO = OpInfo->getBinaryOverloadKind();
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
    clang::SourceLocation RHSLoc = RHS->getLoc();
    clang::SourceLocation SymbolLocations[3] = {RHSLoc, RHSLoc, RHSLoc};
    Id.setOperatorFunctionId(RHSLoc, OpInfo->getUnaryOverloadKind(),
                              SymbolLocations);
  } else {
    Id.setIdentifier(IdInfo, RHS->getLoc());
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

clang::Expr *ExprElaborator::elaborateDisambuationSyntax(clang::Expr *ElaboratedLHS,
                                                         const Syntax *LHSParam,
                                                         const CallSyntax *Op,
                                                         const CallSyntax *Disambig) {
  clang::Expr *LHS = doElaborateExpr(Disambig->getArgument(0));
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

  clang::Expr *Res =
    elaborateNestedLookupAccess(LHS, Op, Disambig->getArgument(1));
  if (!Res)
    return nullptr;
  if (ElaboratedLHS->getType()->isTypeOfTypes()) {
    return Res;
  }

  if (clang::DeclRefExpr *FieldRef = dyn_cast<clang::DeclRefExpr>(Res)) {
    if (!isa<clang::FieldDecl>(FieldRef->getDecl())) {
      // FIXME: This is fine because this could be a reference to a static variable
      // but then again we'd also need to make sure that the LHS is an object
      // of some kind.
      return nullptr;
    }
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

  // Handling member function rebuild.
  if (auto *ULE = dyn_cast<clang::UnresolvedLookupExpr>(Res)) {
    // FIXME: I need to figure out if this will work for static functions.
    clang::CXXScopeSpec SS;
    bool IsArrow = false;
    clang::QualType BaseType = ElaboratedLHS->getType();
    if (ElaboratedLHS->getType()->isPointerType())
      IsArrow = true;
    auto BaseExpr
        = SemaRef.getCxxSema().PerformMemberExprBaseConversion(ElaboratedLHS,
                                                               IsArrow);
    if (BaseExpr.isInvalid())
      return nullptr;
    clang::TemplateArgumentListInfo TemplateArgs;
    return clang::UnresolvedMemberExpr::Create(Context.CxxAST, false,
                                               BaseExpr.get(), BaseType,
                                               IsArrow, Op->getLoc(),
                                               SS.getWithLocInContext(Context.CxxAST),
                                               clang::SourceLocation(),
                                               ULE->getNameInfo(),
                                               &TemplateArgs,
                                               ULE->decls_begin(), ULE->decls_end());
  }
  return Res;
}

clang::Expr *ExprElaborator::elaborateInPlaceNewCall(clang::Expr *LHSPtr,
                                                     const CallSyntax *Op,
                                                     const Syntax *RHS) {
  return SemaRef.buildPartialInPlaceNewExpr(RHS, LHSPtr, Op->getLoc());
}

clang::Expr *ExprElaborator::elaborateDestructCall(clang::Expr *LHSPtr,
                                                   const CallSyntax *Op,
                                                   const Syntax *RHS) {
  clang::QualType ExprTy = LHSPtr->getType();
  if (!ExprTy->isPointerType()) {
    // FIXME: I need to figure out how to ensure that we have the correct
    // error message for our destructor.
    SemaRef.Diags.Report(RHS->getLoc(),
                         clang::diag::err_pseudo_dtor_base_not_scalar)
                         << ExprTy;
    return nullptr;
  } else {
    ExprTy = ExprTy->getPointeeType();
  }
  clang::CXXScopeSpec SS;
  clang::SourceLocation Loc;
  clang::tok::TokenKind AccessTokenKind = clang::tok::TokenKind::arrow;

  clang::UnqualifiedId Id;
  clang::TypeSourceInfo *TInfo = BuildAnyTypeLoc(Context.CxxAST, ExprTy,
                                                 RHS->getLoc());
  auto PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
  Id.setDestructorName(RHS->getLoc(), PT, RHS->getLoc());
  auto Ret =
    SemaRef.getCxxSema().ActOnMemberAccessExpr(SemaRef.getCurClangScope(),
                                               LHSPtr, Op->getLoc(),
                                               AccessTokenKind, SS, Loc,
                                               Id, nullptr);
  if (Ret.isInvalid())
    return nullptr;
  return Ret.get();
}

clang::Expr *ExprElaborator::elaborateNNS(clang::NamedDecl *NS,
                                          const CallSyntax *Op,
                                          const Syntax *RHS) {
  // The object type cannot coexist with a set scope-specifier.
  clang::Sema::NestedNameSpecInfo IdInfo(NS->getIdentifier(),
                                         NS->getBeginLoc(),
                                         Op->getLoc(),
                                         /*ObjectType=*/clang::QualType());

  // Look this up as an NNS.
  bool EnteringContext = SemaRef.isQualifiedLookupContext();
  bool Failure = SemaRef.getCxxSema().
    ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                EnteringContext, SemaRef.CurNNSContext,
                                /*RecoveryLookup=*/false,
                                /*IsCorrected=*/nullptr,
                                /*OnlyNamespace=*/false);
  if (Failure) {
    SemaRef.CurNNSContext.clear();
    return nullptr;
  }

  Sema::OptionalInitScope<Sema::QualifiedLookupRAII> Qual(SemaRef);
  if (auto *CppxNs = dyn_cast<clang::CppxNamespaceDecl>(NS)) {
    Qual.Init(SemaRef.QualifiedLookupContext, CppxNs);
  } else if (auto *Alias = dyn_cast<clang::NamespaceAliasDecl>(NS)) {
    Qual.Init(SemaRef.QualifiedLookupContext, Alias);
  } else {
    NS->dump();
    llvm_unreachable("We hvae a new type of namespace specifier that we've "
                     "never seen before.");
  }
  clang::Expr *RHSExpr = doElaborateExpr(RHS);
  if (!RHSExpr) {
    SemaRef.CurNNSContext.clear();
    return nullptr;
  }

  if (RHSExpr->getType()->isNamespaceType())
    return RHSExpr;

  // We've finished lookup and can clear the NNS context.
  if (!SemaRef.isExtendedQualifiedLookupContext())
    SemaRef.CurNNSContext.clear();
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr);
  return RHSExpr;
}

clang::Expr *ExprElaborator::elaborateGlobalNNS(const CallSyntax *Op,
                                                const Syntax *RHS) {
  bool Failure = SemaRef.getCxxSema().ActOnCXXGlobalScopeSpecifier(Op->getLoc(),
                                                         SemaRef.CurNNSContext);

  if (Failure)
    return nullptr;

  gold::Scope *GlobalScope = SemaRef.getCurrentScope();
  while (GlobalScope->getParent())
    GlobalScope = GlobalScope->getParent();

  Sema::QualifiedLookupRAII Qual(SemaRef, SemaRef.QualifiedLookupContext,
                                 GlobalScope,
                                 Context.CxxAST.getTranslationUnitDecl());
  clang::Expr *RHSExpr = doElaborateExpr(RHS);
  if (!RHSExpr)
    return nullptr;

  if (RHSExpr->getType()->isNamespaceType())
    return RHSExpr;

  // TODO: should we account for an NNS that is returned from a function?
  if (!SemaRef.isExtendedQualifiedLookupContext())
    SemaRef.CurNNSContext.clear();
  ExprMarker(Context.CxxAST, SemaRef).Visit(RHSExpr);
  return RHSExpr;
}

// True if the provided operator is a dot at the right-most level of the tree.
// e.g., `b.(A)i` would return false, but `b.a.i` would return true.
static bool isOpDot(Sema &SemaRef, const CallSyntax *Op) {
  if (Op->getNumArguments() < 2)
    return false;

  const CallSyntax *S = Op;
  while (isa<CallSyntax>(S->getArgument(1))) {
    S = cast<CallSyntax>(S->getArgument(1));
    if (S->getNumArguments() < 2)
      return false;
  }

  return getFusedOpKind(SemaRef, S) == FOK_MemberAccess;
}

// True when we have an overload set while creating a Using Declaration
// inside of a class.
static bool usingClassLookupIsUnresolved(clang::DeclContextLookupResult const &R,
                                         unsigned NumShadows) {
  if (R.empty())
    return false;

  if (NumShadows > 1)
    return true;

  auto hasMethod = [](clang::NamedDecl const *D) -> bool {
    return isa<clang::CXXMethodDecl>(D);
  };
  return std::find_if_not(std::begin(R), std::end(R), hasMethod) == std::end(R);
}

clang::Expr *handleLookupInsideType(Sema &SemaRef, clang::ASTContext &CxxAST,
                                    const CallSyntax *Op, clang::Expr *Prev,
                                    const Syntax *RHS, bool AddressOf) {
  clang::TypeSourceInfo *TInfo =
                    SemaRef.getTypeSourceInfoFromExpr(Prev, Prev->getExprLoc());

  if (!TInfo)
    return nullptr;

  clang::QualType QT = TInfo->getType();
  const clang::Type *T = QT.getTypePtr();
  const auto *TST = T->getAs<clang::TemplateSpecializationType>();

  // FIXME: perform some check on TST here?
  if (!(T->isStructureOrClassType() || T->isUnionType()
        || T->isEnumeralType()) && !TST) {
    SemaRef.Diags.Report(Prev->getExprLoc(),
                         clang::diag::err_invalid_type_for_name_spec)
                         << QT;
    return nullptr;
  }

  clang::TagDecl *TD = T->getAsTagDecl();
  if (SemaRef.elaboratingUsingInClassScope() && TST) {
    TD = cast<clang::TagDecl>(TST->getTemplateName().getAsTemplateDecl()
                              ->getTemplatedDecl());
  }

  // Fetching declaration to ensure that we actually have the current scope
  // for lookup.
  // Attempthing to fetch the declaration now and popss
  Declaration *DeclForTy = SemaRef.getDeclaration(TD);
  assert(DeclForTy);

  ClangToGoldDeclRebuilder Rebuilder(SemaRef.getContext(), SemaRef);
  clang::SourceRange Range = clang::SourceRange(Op->getArgument(0)->getLoc(),
                                                RHS->getLoc());
  if (Rebuilder.finishDecl(DeclForTy, Range))
    return nullptr;

  // Processing if we have a single name.
  if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(RHS)) {
    clang::DeclarationNameInfo DNI({&CxxAST.Idents.get(Atom->getSpelling())},
                                  Atom->getLoc());
    if (Atom->getSpelling() == "destruct") {
      clang::DeclarationNameInfo DNI2({
        CxxAST.DeclarationNames.getCXXDestructorName(
          CxxAST.getCanonicalType(TInfo->getType())
        )}, Atom->getLoc());
      if (clang::CXXRecordDecl* RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
        clang::CXXDestructorDecl *Dtor = RD->getDestructor();
        clang::TemplateArgumentListInfo TemplateArgs;
        clang::UnresolvedSet<4> USet;
        USet.addDecl(Dtor, Dtor->getAccess());
        return clang::UnresolvedLookupExpr::Create(CxxAST,
                                            RD, clang::NestedNameSpecifierLoc(),
                                                   DNI2, /*ADL=*/true,
                                                   /*Overloaded*/false,
                                                   USet.begin(),
                                                   USet.end());
      } else {
        SemaRef.Diags.Report(Prev->getExprLoc(),
                            clang::diag::err_invalid_destructor_call)
                            << TInfo->getType();
        return nullptr;
      }
    }
    auto R = TD->lookup(DNI.getName());
    clang::NamedDecl *ND = nullptr;
    if (R.size() != 1u) {

      // This could be a template specialization of a member.
      if (!R.empty()) {
        clang::LookupResult Redecls(SemaRef.getCxxSema(), DNI,
                                    clang::Sema::LookupOrdinaryName,
                                    clang::Sema::ForVisibleRedeclaration);
        for (clang::NamedDecl *ND : R)
          Redecls.addDecl(ND);
        Redecls.resolveKind();
        if (Redecls.getResultKind() == clang::LookupResult::FoundOverloaded) {
          clang::TemplateArgumentListInfo TemplateArgs;
          clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(TD);
          assert (RD && "should have avoided this situation");

          clang::CXXScopeSpec SS;
          SS.Extend(CxxAST, TD->getIdentifier(), Prev->getExprLoc(),
                    Op->getLoc());
          return clang::UnresolvedLookupExpr::Create(
            CxxAST, RD, SS.getWithLocInContext(CxxAST), DNI, /*ADL=*/true,
            /*Overloaded*/true, Redecls.asUnresolvedSet().begin(),
            Redecls.asUnresolvedSet().end());
        }

        ND = Redecls.getAcceptableDecl(R.front());
        if (ND && isa<clang::ValueDecl>(ND)) {
          clang::ValueDecl *VD = cast<clang::ValueDecl>(ND);
          clang::NestedNameSpecifierLoc NNS(SemaRef.CurNNSContext.getScopeRep(),
                                            SemaRef.CurNNSContext.location_data());
          bool UseNNS = SemaRef.CurNNSContext.isSet();
          return clang::DeclRefExpr::Create(
            CxxAST, UseNNS ? NNS : clang::NestedNameSpecifierLoc(),
            clang::SourceLocation(), VD, /*Capture=*/false, RHS->getLoc(),
            VD->getType(), AddressOf ? clang::VK_RValue : clang::VK_LValue);
        }
      }

      // This wasn't the name of a member, check if it is the name of a base.
      if (clang::CXXRecordDecl *RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
        for (const auto &Base : RD->bases()) {
          clang::CXXRecordDecl *BaseRD = Base.getType()->getAsCXXRecordDecl();
          if (BaseRD->getIdentifier() == DNI.getName().getAsIdentifierInfo())
            ND = BaseRD;
        }
      }

      auto hasUsing = [](clang::NamedDecl const *D) -> bool {
        return isa<clang::UsingDecl>(D);
      };
      unsigned Shadows = 0;
      clang::UnresolvedSet<4> USet;

      // Check if we have any shadows single declarations.
      if (std::find_if(std::begin(R), std::end(R), hasUsing) != std::end(R)) {
        clang::UsingShadowDecl *S = nullptr;
        for (clang::NamedDecl *D : R) {
          if (auto *SD = dyn_cast<clang::UsingShadowDecl>(D)) {
            S = SD;
            ++Shadows;
          }

          USet.addDecl(D, D->getAccess());
        }

        if (Shadows == 1u) {
          ND = S->getTargetDecl();
        }
      }

      // Check for a shadowed overload set.
      if (usingClassLookupIsUnresolved(R, Shadows)) {
        // If we're not creating a UsingDecl, these need to be static.
        if (!SemaRef.elaboratingUsingInClassScope() && !AddressOf) {
          SemaRef.Diags.Report(Prev->getExprLoc(),
                               clang::diag::err_ref_non_value) << Prev;
          return nullptr;
        }

        if (!Shadows)
          for (clang::NamedDecl *D : R)
            USet.addDecl(D, D->getAccess());
        clang::Expr *Base = const_cast<clang::Expr *>(Prev);
        clang::TemplateArgumentListInfo TemplateArgs;
        auto *UME =
          clang::UnresolvedMemberExpr::Create(CxxAST,
                                              /*UnresolvedUsing=*/true,
                                              Base,
                                              Base->getType(),
                                              Base->getType()->isPointerType(),
                                              RHS->getLoc(),
                                              clang::NestedNameSpecifierLoc(),
                                              clang::SourceLocation(),
                                              DNI,
                                              &TemplateArgs,
                                              USet.begin(),
                                              USet.end());
        return UME;
      }

      // This was neither a type nor a shadowed declaration.
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

    // This is how we access static member variables, and strangely also fields.
    if (clang::VarDecl *VDecl = dyn_cast<clang::VarDecl>(ND))
      return clang::DeclRefExpr::Create(CxxAST, clang::NestedNameSpecifierLoc(),
                                        clang::SourceLocation(),VDecl,
                                        /*Capture=*/false, RHS->getLoc(),
                                        VDecl->getType(), clang::VK_LValue);

    // access a record from an NNS
    if (isa<clang::CXXRecordDecl>(ND))
      return SemaRef.buildTypeExprFromTypeDecl(TD, RHS->getLoc());

    // FIXME: static methods should be handled here

    // otherwise, we have a FieldDecl from a nested name specifier lookup.
    // In which case, the rhs should be static, called via operator'()',
    // inside a using macro if the lhs was a record type, or as the operand
    // of operator'&'.
    if (Prev->getType()->isTypeOfTypes() && isOpDot(SemaRef, Op)) {
      clang::QualType Ty =
        cast<clang::CppxTypeLiteral>(Prev)->getValue()->getType();
      if (!SemaRef.elaboratingUsingInClassScope() && !Ty->isEnumeralType()
          && !AddressOf) {
        SemaRef.Diags.Report(Prev->getExprLoc(),
                             clang::diag::err_ref_non_value) << Prev;
        return nullptr;
      }
    }
    if (clang::FunctionDecl *FD = dyn_cast<clang::FunctionDecl>(ND)) {
      if (clang::CXXRecordDecl* RD = dyn_cast<clang::CXXRecordDecl>(TD)) {
        clang::CXXScopeSpec SS;
        SS.Extend(CxxAST, TD->getIdentifier(), Prev->getExprLoc(),
                  Op->getLoc());
        clang::TemplateArgumentListInfo TemplateArgs;
        clang::UnresolvedSet<4> USet;
        USet.addDecl(FD, FD->getAccess());
        return clang::UnresolvedLookupExpr::Create(CxxAST,
                                                   RD,
                                                   SS.getWithLocInContext(CxxAST),
                                                   DNI, /*ADL=*/true,
                                                   /*Overloaded*/true,
                                                   USet.begin(),
                                                   USet.end());
      } else {
        llvm_unreachable("Incorrect tag type.");
      }
    }
    if (clang::ValueDecl *VD = dyn_cast<clang::ValueDecl>(ND)) {
      clang::NestedNameSpecifierLoc NNS(SemaRef.CurNNSContext.getScopeRep(),
                                        SemaRef.CurNNSContext.location_data());
      bool UseNNS = SemaRef.CurNNSContext.isSet();
      return clang::DeclRefExpr::Create(
        CxxAST, UseNNS ? NNS : clang::NestedNameSpecifierLoc(),
        clang::SourceLocation(), VD, /*Capture=*/false, RHS->getLoc(),
        VD->getType(), AddressOf ? clang::VK_RValue : clang::VK_LValue);
    }
  }

  llvm_unreachable("Unknown syntax encountered during nested member lookup.");
}

clang::Expr *ExprElaborator::elaborateNestedLookupAccess(
               clang::Expr *Previous, const CallSyntax *Op, const Syntax *RHS) {
  assert(Previous && isa<clang::CppxTypeLiteral>(Previous)
         && "Expression scoping.");

  // Build up an NNS to ensure we get an instantiation of any specializations.
  clang::CppxTypeLiteral *Literal = cast<clang::CppxTypeLiteral>(Previous);
  clang::TypeSourceInfo *TInfo = Literal->getValue();
  clang::TypeLocBuilder TLB;
  TInfo = BuildAnyTypeLoc(Context.CxxAST, TLB, TInfo->getType(), Op->getLoc());
  clang::TypeLoc TL = TLB.getTypeLocInContext(Context.CxxAST, TInfo->getType());
  clang::QualType RecordType = TInfo->getType();
  clang::CXXRecordDecl *RD = RecordType->getAsCXXRecordDecl();

  auto *TST = RecordType->getAs<clang::TemplateSpecializationType>();
  if (SemaRef.elaboratingUsingInClassScope() && TST) {
    auto *CTD = dyn_cast_or_null<clang::ClassTemplateDecl>(
      TST->getTemplateName().getAsTemplateDecl());
    clang::QualType ContextType =
      Context.CxxAST.getCanonicalType(clang::QualType(TST, 0));
    clang::QualType Injected = CTD->getInjectedClassNameSpecialization();
    if (Context.CxxAST.hasSameType(Injected, ContextType))
      RD = CTD->getTemplatedDecl();
    else
      llvm_unreachable("partials not implemented");
  }

  if (RD) {
    clang::Sema::NestedNameSpecInfo IdInfo(RD->getIdentifier(),
                                           RD->getBeginLoc(),
                                           Op->getLoc(),
                                           clang::QualType());
    clang::CXXScopeSpec SS;
    SS.Extend(
      Context.CxxAST, clang::SourceLocation(), TL, Op->getLoc());
    bool Failure = SemaRef.getCxxSema().
      ActOnCXXNestedNameSpecifier(SemaRef.getCurClangScope(), IdInfo,
                                  false, SS,
                                  /*RecoveryLookup=*/false,
                                  /*IsCorrected=*/nullptr,
                                  /*OnlyNamespace=*/false);
    if (Failure) {
      SemaRef.CurNNSContext.clear();
      return nullptr;
    }

    SemaRef.CurNNSContext = SS;
  }

  clang::Expr *Ret =
    handleLookupInsideType(SemaRef, Context.CxxAST, Op, Previous,
                           RHS, ElaboratingAddressOfOp);
  if (!SemaRef.isExtendedQualifiedLookupContext())
    SemaRef.CurNNSContext.clear();
  return Ret;
}

clang::Expr *ExprElaborator::elaborateUnaryOp(const CallSyntax *S,
                                              clang::UnaryOperatorKind Op) {
  BooleanRAII AddressOfRAII(ElaboratingAddressOfOp, Op == clang::UO_AddrOf);

  const Syntax *Operand = S->getArgument(0);
  clang::Expr *OperandResult = nullptr;
  if (ElaboratingAddressOfOp) {
    Sema::ExtendQualifiedLookupRAII ExQual(SemaRef);
    OperandResult = doElaborateExpr(Operand);
  } else {
    OperandResult = doElaborateExpr(Operand);
  }

  if (!OperandResult || OperandResult->getType()->isNamespaceType()) {
    SemaRef.Diags.Report(Operand->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  // This is used to construct a pointer type because the caret has two
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
    /*scope*/nullptr, S->getCalleeLoc(), Op, OperandResult);

  ExprMarker(Context.CxxAST, SemaRef).Visit(OperandResult);
  return UnaryOpRes.get();
}

clang::Expr *ExprElaborator::elaborateBinOp(const CallSyntax *S,
                                            clang::BinaryOperatorKind Op) {
  const Syntax *LHSSyntax = S->getArgument(0);
  const Syntax *RHSSyntax = S->getArgument(1);

  clang::Expr *LHS = doElaborateExpr(LHSSyntax);
  if (!LHS) {
    SemaRef.Diags.Report(LHSSyntax->getLoc(),
                         clang::diag::err_expected_expression);
    return nullptr;
  }

  clang::Expr *RHS = doElaborateExpr(RHSSyntax);
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
ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions,
                                        bool IsConstExpr) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1){
    if (IsConstExpr)
      return elaborateExpectedConstantExpr(Conditions->getChild(0));
    else
      return doElaborateExpr(Conditions->getChild(0));
  }
  clang::Expr *LHS = nullptr;
  clang::Expr *RHS = nullptr;

  {
    ExprElaborator ExEl(Context, SemaRef);
    if (IsConstExpr)
      LHS = ExEl.elaborateExpectedConstantExpr(Conditions->getChild(0));
    else
      LHS = ExEl.doElaborateExpr(Conditions->getChild(0));

    if (!LHS) {
      SemaRef.Diags.Report(Conditions->getChild(0)->getLoc(),
                           clang::diag::err_expected_expression);
      return nullptr;
    }
  }
  {
    ExprElaborator ExEl(Context, SemaRef);
    if (IsConstExpr)
      RHS = ExEl.elaborateExpectedConstantExpr(Conditions->getChild(1));
    else
      RHS = ExEl.doElaborateExpr(Conditions->getChild(1));

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
    if (IsConstExpr)
      RHS = ExEl.elaborateExpectedConstantExpr(Conditions->getChild(I));
    else
      RHS = ExEl.doElaborateExpr(Conditions->getChild(I));

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
  assert(isa<ArraySyntax>(S->getBlock()) && "invalid array macro");

  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "only array macros may appear in "
                                  "array initializer");
  if (const AtomSyntax *Call = dyn_cast<AtomSyntax>(S->getCall())) {
    if (Call->getSpelling() != "array") {
      SemaRef.Diags.Report(Call->getLoc(), DiagID);
      return nullptr;
    }
  } else {
    SemaRef.Diags.Report(S->getLoc(), DiagID);
    return nullptr;
  }

  const ArraySyntax *Block = cast<ArraySyntax>(S->getBlock());
  llvm::SmallVector<clang::Expr *, 2> Elements;
  for (const Syntax *SS : Block->children()) {
    if (const ListSyntax *LInit = dyn_cast<ListSyntax>(SS)) {
      llvm::SmallVector<clang::Expr *, 8> Subelements;
      for (const Syntax *SI :  LInit->children()) {
        clang::Expr *Element =
          ExprElaborator(Context, SemaRef).doElaborateExpr(SI);

        if (!Element)
          continue;

        Subelements.push_back(Element);
      }

      clang::ExprResult InitList =
        SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Subelements, S->getLoc());
      Elements.push_back(InitList.get());
    }

    else if (const MacroSyntax *MInit = dyn_cast<MacroSyntax>(SS)) {
      clang::Expr *Sublist = handleArrayMacro(Context, SemaRef, MInit);
      Elements.push_back(Sublist);
    }

    else if (isa<ArraySyntax>(SS)) {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "cannot use array in array macro");
      SemaRef.Diags.Report(SS->getLoc(), DiagID);
      continue;
    }

    else {
      clang::Expr *Element =
        ExprElaborator(Context, SemaRef).doElaborateExpr(SS);

      if (!Element)
        continue;

      Elements.push_back(Element);
    }
  }

  if (Elements.size() == 1 && isa<clang::InitListExpr>(Elements[0]))
    return Elements[0];

  clang::ExprResult InitList =
    SemaRef.getCxxSema().ActOnInitList(S->getLoc(), Elements, S->getLoc());
  return InitList.get();
}

static clang::Expr *handleOfMacro(SyntaxContext &Context, Sema &SemaRef,
                                  const MacroSyntax *S) {
  const ArraySyntax *Block = cast<ArraySyntax>(S->getBlock());

  llvm::SmallVector<clang::Expr *, 4> Args;
  for (const Syntax *SS : Block->children()) {
    clang::Expr *E = ExprElaborator(Context, SemaRef).elaborateExpr(SS);
    Args.push_back(E);
  }

  clang::ExprResult Res =
    SemaRef.getCxxSema().ActOnParenListExpr(S->getLoc(), S->getLoc(), Args);
  if (Res.isInvalid())
    return nullptr;
  return Res.get();
}

static inline bool isLocalLambdaScope(const Scope *S) {
  if (!S || !S->Parent)
    return true;

  do {
    if (S->getKind() == SK_Class || S->getKind() == SK_Function)
      return true;

    S = S->getParent();
  } while (S);

  return false;
}

static void buildLambdaCaptureInternal(SyntaxContext &Context, Sema &SemaRef,
                                       clang::LambdaIntroducer &Intro,
                                       const Syntax *Capture,
                                       clang::SourceRange const &Range) {
  clang::Expr *Ref = nullptr;
  clang::Expr *Init = nullptr;
  clang::IdentifierInfo *II = nullptr;
  // True when the current syntax is a this pointer, or operation on one.
  bool This = false;

  if (const CallSyntax *Call = dyn_cast<CallSyntax>(Capture)) {
    FusedOpKind FOK = getFusedOpKind(SemaRef, Call);
    if (FOK != FOK_Equals && FOK != FOK_Caret) {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "expected ',', '}', or dedent in "
                                      "lambda capture list");
      SemaRef.Diags.Report(Capture->getLoc(), DiagID);
      return;
    }

    auto *Elab = ExprElaborator(Context, SemaRef).elaborateExpr(Call);
    if (!Elab)
      return;

    if (auto *Bin = dyn_cast<clang::BinaryOperator>(Elab)) {
      // This might be more complex than a declaration.
      Ref = Bin->getLHS();
      Init = Bin->getRHS();
    } else if (auto *Un = dyn_cast<clang::UnaryOperator>(Elab)) {
      Ref = Un;
      This = isa<clang::CXXThisExpr>(Un->getSubExpr());
    }
  } else if (const AtomSyntax *Atom = dyn_cast<AtomSyntax>(Capture)) {
    Ref = ExprElaborator(Context, SemaRef).elaborateExpr(Atom);
    This = Atom->getSpelling() == "this";
  } else if (const ListSyntax *List = dyn_cast<ListSyntax>(Capture)) {
    // Recurse if this parameter is a list.
    for (const Syntax *Item : List->children())
      buildLambdaCaptureInternal(Context, SemaRef, Intro, Item, Range);
    return;
  } else {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "expected declaration name in "
                                    "lambda capture list");
    SemaRef.Diags.Report(Capture->getLoc(), DiagID);
    return;
  }

  // Handle this or ^this
  if (This) {
    clang::LambdaCaptureKind Kind = clang::LCK_StarThis;
    clang::LambdaCaptureInitKind InitKind =
      clang::LambdaCaptureInitKind::NoInit;
    clang::ParsedType InitCaptureType;

    Intro.addCapture(Kind, Capture->getLoc(), II,
                     /*EllipsisLoc=*/clang::SourceLocation(),
                     InitKind, Init, InitCaptureType, Range);
    return;
  }

  // FIXME: what about a dereferenced pointer?
  if (!isa<clang::DeclRefExpr>(Ref)) {
    SemaRef.Diags.Report(Capture->getLoc(),
                         clang::diag::err_capture_does_not_name_variable)
      << Ref;
    return;
  }

  clang::DeclRefExpr *DRE = cast<clang::DeclRefExpr>(Ref);
  if (!isa<clang::VarDecl>(DRE->getDecl())) {
    SemaRef.Diags.Report(Capture->getLoc(),
                         clang::diag::err_capture_does_not_name_variable)
      << DRE->getDecl();
    return;
  }

  clang::VarDecl *VD = cast<clang::VarDecl>(DRE->getDecl());
  II = VD->getIdentifier();

  // FIXME: consider this or starthis
  clang::LambdaCaptureKind Kind = clang::LCK_ByCopy;
  clang::LambdaCaptureInitKind InitKind = clang::LambdaCaptureInitKind::NoInit;
  clang::ParsedType InitCaptureType;
  if (Init) {
    // FIXME: could be direct or list init
    InitKind = clang::LambdaCaptureInitKind::CopyInit;
    clang::Sema &CxxSema = SemaRef.getCxxSema();
    InitCaptureType = CxxSema.actOnLambdaInitCaptureInitialization(
      Capture->getLoc(), false, /*EllipsisLoc=*/clang::SourceLocation(),
      II, InitKind, Init);
  }

  Intro.addCapture(Kind, Capture->getLoc(), II,
                   /*EllipsisLoc=*/clang::SourceLocation(),
                   InitKind, Init, InitCaptureType, Range);
}

// Build and attach the captures for a lambda macro
static inline void buildLambdaCaptures(SyntaxContext &Context, Sema &SemaRef,
                                       const MacroSyntax *S,
                                       clang::LambdaIntroducer &Intro) {
  const Syntax *CaptureScope = S->getNext();
  clang::SourceLocation BeginLoc = CaptureScope->getLoc();
  clang::SourceLocation EndLoc = S->getBlock()->getLoc();
  clang::SourceRange Range(BeginLoc, EndLoc);
  for (const Syntax *SS : CaptureScope->children())
    buildLambdaCaptureInternal(Context, SemaRef, Intro, SS, Range);
}

static inline bool isMutable(Sema &SemaRef, Attribute *A) {
  std::string Name;
  switch (checkAttrFormatAndName(A->getArg(), Name)) {
  case AF_Name:
  case AF_Call:
    return Name == "mutable";
  default:
    return false;
  }
}

static clang::Expr *handleLambdaMacro(SyntaxContext &Context, Sema &SemaRef,
                                      const MacroSyntax *S) {
  assert(isa<CallSyntax>(S->getCall()) && "invalid lambda");
  clang::Sema &CxxSema = SemaRef.getCxxSema();
  bool LocalLambda = isLocalLambdaScope(SemaRef.getCurrentScope());
  const CallSyntax *Call = cast<CallSyntax>(S->getCall());

  Syntax::AttrVec Attributes = Call->getAttributes();
  bool Mutable = false;
  for (Attribute *A : Attributes) {
    if (isMutable(SemaRef, A)) {
      Mutable = true;
    } else {
      unsigned DiagID =
        SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                      "lambdas may only have the "
                                      "'mutable' attribute");
      SemaRef.Diags.Report(A->getArg()->getLoc(), DiagID);
    }
  }

  // Elaborate any explicit template parameters first.
  llvm::SmallVector<clang::NamedDecl *, 4> TemplateParams;
  const ElemSyntax *Templ = dyn_cast<ElemSyntax>(Call->getCallee());
  Sema::ScopeRAII TemplateScope(SemaRef, SK_Template, Templ);
  bool GenericLambda = false;
  if (Templ) {
    Elaborator El(Context, SemaRef);
    El.buildTemplateParams(Templ->getArguments(), TemplateParams);
    GenericLambda = true;
  }

  CxxSema.PushLambdaScope();
  std::copy(TemplateParams.begin(), TemplateParams.end(),
            std::back_inserter(CxxSema.getCurLambda()->TemplateParams));
  Sema::ScopeRAII ParamScope(SemaRef, SK_Parameter, Call);
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  for (const Syntax *Arg : Call->getArguments()->children()) {
    clang::Decl *D = Elaborator(Context, SemaRef).elaborateDeclSyntax(Arg);
    if (!D)
      continue;

    clang::ParmVarDecl *PVD = cast<clang::ParmVarDecl>(D);
    if (PVD->getType()->isUndeducedAutoType()) {
      GenericLambda = true;
      CxxSema.RecordParsingTemplateParameterDepth(SemaRef.LambdaTemplateDepth);
      auto Invented = CxxSema.InventTemplateParameter(
        SemaRef.getDeclaration(PVD), PVD->getType(), nullptr,
        PVD->getType()->getAs<clang::AutoType>(), *CxxSema.getCurLambda());
      clang::TypeSourceInfo *TSI =
        BuildAnyTypeLoc(Context.CxxAST, Invented.first, PVD->getBeginLoc());
      PVD->setType(TSI->getType());
      PVD->setTypeSourceInfo(TSI);
      const clang::TemplateTypeParmType *TempTy =
        PVD->getType()->getAs<clang::TemplateTypeParmType>();
      PVD->setScopeInfo(TempTy->getDepth(), TempTy->getIndex());
    }

    Params.push_back(PVD);
  }

  if (GenericLambda)
    ++SemaRef.LambdaTemplateDepth;

  Sema::ScopeRAII BlockScope(SemaRef, SK_Block, S->getBlock());
  SemaRef.getCurrentScope()->Lambda = true;
  unsigned ScopeFlags = clang::Scope::BlockScope |
    clang::Scope::FnScope | clang::Scope::DeclScope |
    clang::Scope::CompoundStmtScope;
  SemaRef.enterClangScope(ScopeFlags);

  // Set up the captures and capture default.
  clang::LambdaIntroducer Intro;
  clang::SourceLocation NextLoc =
    S->getNext() ? S->getNext()->getLoc() : S->getLoc();
  Intro.Range =
    clang::SourceRange(S->getCall()->getLoc(), NextLoc);
  Intro.DefaultLoc = NextLoc;

  // The lambda default will always be by-value, but local lambdas have no
  // default as per [expr.prim.lambda]
  Intro.Default = LocalLambda ? clang::LCD_None : clang::LCD_ByCopy;
  Sema::ScopeRAII LambdaCaptureScope(SemaRef, SK_Block, S->getNext());
  SemaRef.getCurrentScope()->LambdaCaptureScope = true;
  buildLambdaCaptures(Context, SemaRef, S, Intro);

  // Build the lambda
  CxxSema.ActOnStartOfGoldLambdaDefinition(SemaRef, Intro, Params,
                                           SemaRef.getCurClangScope(),
                                           Mutable);
  clang::Stmt *Block =
    StmtElaborator(Context, SemaRef).elaborateBlock(S->getBlock());
  clang::ExprResult Lam =
    CxxSema.ActOnLambdaExpr(S->getLoc(), Block, SemaRef.getCurClangScope());
  SemaRef.leaveClangScope(S->getLoc());
  SemaRef.LambdaTemplateDepth = 0;
  return Lam.get();
}

static bool isOpNewCall(const MacroSyntax *S) {
  auto OpNew = dyn_cast<CallSyntax>(S->getCall());
  if (OpNew)
    if (auto Name = dyn_cast<AtomSyntax>(OpNew->getCallee()))
      return Name->hasToken(tok::NewKeyword);
  return false;
}

using SyntaxHandler =
  clang::Expr *(*)(SyntaxContext &, Sema &, const MacroSyntax *);
static const llvm::StringMap<SyntaxHandler> SyntaxHandlers = {
  {"array",  &handleArrayMacro},
  {"of",     &handleOfMacro},
  {"lambda", &handleLambdaMacro},
};

clang::Expr *ExprElaborator::elaborateMacro(const MacroSyntax *S) {
  unsigned DiagID =
    SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                  "unknown macro");

  // Checking if we are handling operator new parsing.
  if (isOpNewCall(S))
    return elaborateNewExpr(S);

  const AtomSyntax *Call = dyn_cast<AtomSyntax>(S->getCall());
  if (!Call && isa<CallSyntax>(S->getCall())) {
    const CallSyntax *C = cast<CallSyntax>(S->getCall());
    // In all cases but one, this should be an atom.
    if (isa<AtomSyntax>(C->getCallee()))
      Call = cast<AtomSyntax>(C->getCallee());
    // The other case mentioned above; generic lambdas may have an element call.
    else if (isa<ElemSyntax>(C->getCallee()))
      return handleLambdaMacro(Context, SemaRef, S);
    // Not a lambda
    else
      return elaborateInitListCall(S);
  }

  if (!Call) {
    SemaRef.Diags.Report(S->getLoc(), DiagID);
    return nullptr;
  }

  // Handle any builtin macros
  auto HandlerIt = SyntaxHandlers.find(Call->getSpelling());
  if (HandlerIt != SyntaxHandlers.end())
    return (HandlerIt->second)(Context, SemaRef, S);

  return elaborateInitListCall(S);
}

clang::Expr *
ExprElaborator::elaborateInitListCall(const MacroSyntax *Macro) {
  clang::Expr *IdExpr = elaborateExpr(Macro->getCall());
  if (!IdExpr) {
    unsigned DiagID =
      SemaRef.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "unknown macro");
    SemaRef.Diags.Report(Macro->getLoc(), DiagID);
    return nullptr;
  }
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(IdExpr,
                                                    Macro->getCall()->getLoc());
  if (!TInfo)
    return nullptr;

  llvm::SmallVector<clang::Expr *, 8> Args;
  auto Arr = cast<ArraySyntax>(Macro->getBlock());
  if (buildFunctionCallArguments(SemaRef, Context, Arr, Args))
    return nullptr;
  auto InitList = SemaRef.getCxxSema().ActOnInitList(Macro->getLoc(),
                                                     Args, Macro->getLoc());
  llvm::SmallVector<clang::Expr *, 1> InitListArg { InitList.get() };
  clang::ExprResult ConstructorExpr =
        SemaRef.getCxxSema().BuildCXXTypeConstructExpr(TInfo, Macro->getLoc(),
                                                       InitListArg,
                                                       Macro->getLoc(),
                                                    /*ListInitialization*/true);
  return ConstructorExpr.get();
}

clang::Expr *ExprElaborator::elaborateNewExpr(const MacroSyntax *Macro) {
  auto OpNew = dyn_cast<CallSyntax>(Macro->getCall());
  assert(OpNew && "Invalid AST for new expression.");

  // I need to make sure that I force the loading of the global operator new
  // functions just in case they don't already exist.
  // This is done here because when using BuildCxxNew that will also
  // load the new functions but gold wont know about it and there by skipping
  // the creating of gold declarations.
  SemaRef.createBuiltinOperatorNewDeleteDecls();
  auto InplaceArgsNode = dyn_cast_or_null<ListSyntax>(OpNew->getArguments());

  // All of these need to be filled out as we build our expression.
  // clang::SourceRange ExprRange;
  clang::SourceLocation ExprStartLoc = OpNew->getCallee()->getLoc();
  clang::SourceLocation ExprEndLoc = ExprStartLoc;
  llvm::SmallVector<clang::Expr *, 8> PlacementArgs;
  clang::SourceLocation PlacementLParen;
  clang::SourceLocation PlacementRParen;
  if (InplaceArgsNode) {
    if (buildFunctionCallArguments(SemaRef, Context, InplaceArgsNode,
                                   PlacementArgs))
      return nullptr;
  }

  // This is used for the constructor expression.
  llvm::Optional<clang::Expr *> ArraySizeExpr;

  // Initialization style is determined off of this argument.
  clang::SourceRange DirectInitRange;
  clang::TypeSourceInfo *TInfo = nullptr;
  clang::Expr *InitializationExpr = nullptr;

  // This is for if the type expression is inside ()
  clang::SourceRange TypeIdParens;

  auto TypeOrCtorExpr = Macro->getBlock();
  assert(TypeOrCtorExpr && "Invalid AST structure for operator new");
  const Syntax *TypeNode = nullptr;
  llvm::SmallVector<clang::Expr *, 8> CtorArgs;
  bool BuildCtorExpr = false;
  bool BuildInitListExpr = false;
  auto CtorCall = dyn_cast<CallSyntax>(TypeOrCtorExpr);
  if (CtorCall) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, CtorCall);
    if (OpKind != FOK_Unknown) {
      TypeNode = CtorCall;
    } else {
      TypeNode = CtorCall->getCallee();
      if (CtorCall->getArguments()) {
        auto ArgList = cast<ListSyntax>(CtorCall->getArguments());
        if (ArgList->getNumChildren() == 0) {
          DirectInitRange = clang::SourceRange(CtorCall->getLoc(),
                                              CtorCall->getLoc());
          ExprEndLoc = CtorCall->getLoc();
        } else {
          DirectInitRange = clang::SourceRange(ArgList->getChild(0)->getLoc(),
                        ArgList->getChild(ArgList->getNumChildren()-1)->getLoc());
          ExprEndLoc = ArgList->getChild(ArgList->getNumChildren()-1)->getLoc();
        }

        if (buildFunctionCallArguments(SemaRef, Context, ArgList, CtorArgs))
          return nullptr;
        BuildCtorExpr = true;

      } else {
        Macro->dump();
        llvm_unreachable("Invalid AST structure");
      }
    }
  } else if (auto MacroTypeNode = dyn_cast<MacroSyntax>(TypeOrCtorExpr)) {
    ExprEndLoc = TypeOrCtorExpr->getLoc();
    TypeNode = MacroTypeNode->getCall();
    // if (auto TypeCallNode = dyn_cast<CallSyntax>(MacroTypeNode->getCall())) {
    auto ArgList = cast<ArraySyntax>(MacroTypeNode->getBlock());
    if (ArgList->getNumChildren() == 0)
      ExprEndLoc = CtorCall->getLoc();
    else
      ExprEndLoc = ArgList->getChild(ArgList->getNumChildren()-1)->getLoc();
    if (buildFunctionCallArguments(SemaRef, Context, ArgList, CtorArgs))
      return nullptr;
    BuildInitListExpr = true;
  } else {
    ExprEndLoc = TypeOrCtorExpr->getLoc();
    TypeNode = TypeOrCtorExpr;
  }

  // Attempting to get the type expression.
  clang::Expr *TyExpr = elaborateNewExpr_TypeNode(TypeNode, ArraySizeExpr);
  // TODO: I need to figure out if this need an additional error message.
  if (!TyExpr)
    return nullptr;
  // if (!isa<clang::InitListExpr>(TyExpr)) {
  TInfo = SemaRef.getTypeSourceInfoFromExpr(TyExpr, TypeNode->getLoc());
  if (!TInfo)
    return nullptr;
  clang::QualType TyToNew = TInfo->getType();
  if (TyToNew->isTypeOfTypes()
      || TyToNew->isTemplateType()
      || TyToNew->isNamespaceType()) {
    // llvm_unreachable("Cannot create an instance of this type.");
    SemaRef.Diags.Report(TypeNode->getLoc(),
                        clang::diag::err_cannot_allocate_type)
                        << TyToNew;
    return nullptr;
  }

  if (BuildCtorExpr) {
    if (TInfo->getType()->isUndeducedAutoType()) {
      if (CtorArgs.size() != 1) {
        SemaRef.Diags.Report(TypeNode->getLoc(),
                            clang::diag::err_auto_new_ctor_multiple_expressions)
                            << TInfo->getType();
        return nullptr;
      }
      InitializationExpr = CtorArgs.front();
    } else {
      auto PT = SemaRef.getCxxSema().CreateParsedType(TInfo->getType(), TInfo);
      auto CtorExpr = SemaRef.getCxxSema().ActOnCXXTypeConstructExpr(PT,
                  DirectInitRange.getBegin(), CtorArgs, DirectInitRange.getEnd(),
                                                    /*ListInitialization=*/false);
      if (CtorExpr.isInvalid())
        return nullptr;
      InitializationExpr = CtorExpr.get();
    }
  }
  if (BuildInitListExpr) {
    clang::ExprResult InitList =
      SemaRef.getCxxSema().ActOnInitList(TypeOrCtorExpr->getLoc(), CtorArgs,
                                         TypeOrCtorExpr->getLoc());
    InitializationExpr = InitList.get();
  }
  // } else {
  //   InitializationExpr = TyExpr;
  // }
  clang::ExprResult Res = SemaRef.getCxxSema().BuildCXXNew(
      clang::SourceRange(ExprStartLoc, ExprEndLoc), /*UseGlobal*/false,
      PlacementLParen, PlacementArgs, PlacementRParen, TypeIdParens,
      TInfo->getType(), TInfo, ArraySizeExpr, DirectInitRange,
      InitializationExpr);
  if (Res.isInvalid())
    return nullptr;
  return Res.get();
}

clang::Expr *
ExprElaborator::elaborateNewExpr_ArrayCall(const CallSyntax *S,
                                  llvm::Optional<clang::Expr *> &DynArrayExpr) {
  if (S->getNumArguments() != 2) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }
  clang::Expr *IdExpr = nullptr;
  bool IsInnerMostArray = true;
  auto InnerTy = S->getArgument(1);
  if (auto InnerTyCall = dyn_cast<CallSyntax>(InnerTy)) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, InnerTyCall);
    if (OpKind == FOK_Brackets) {
      IsInnerMostArray = false;
      IdExpr = elaborateNewExpr_ArrayCall(InnerTyCall, DynArrayExpr);
    } else {
      IdExpr = doElaborateExpr(InnerTy);
    }
  } else {
    IdExpr = doElaborateExpr(InnerTy);
  }

  if (!IdExpr)
    return nullptr;

  // Attempt to translate into type location.
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(IdExpr,
                                                   S->getArgument(1)->getLoc());
  if (!TInfo)
    return nullptr;

  llvm::SmallVector<clang::Expr *, 4> IndexExprs;
  const ListSyntax *IndexList = dyn_cast<ListSyntax>(S->getArgument(0));
  if (IndexList) {
    for (const Syntax *SS : IndexList->children())
      IndexExprs.push_back(doElaborateExpr(SS));
  } else {
    IndexExprs.push_back(doElaborateExpr(S->getArgument(0)));
  }

  // FIXME: what do we do for an empty array index, such as []int = {...}
  unsigned I = 0;
  for (clang::Expr *IndexExpr : IndexExprs) {
    if (!IndexExpr) {
      SemaRef.Diags.Report(S->getArgument(I)->getLoc(),
                           clang::diag::err_failed_to_translate_type);
      return nullptr;
    }

    ++I;
  }

  clang::QualType ArrayType = TInfo->getType();
  for (auto It = IndexExprs.rbegin(); It != IndexExprs.rend(); ++It) {
    clang::Expr *IndexExpr = *It;

    clang::Expr::EvalResult IdxResult;
    clang::Expr::EvalContext
      EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

    if (IndexExpr->EvaluateAsConstantExpr(IdxResult, EvalCtx)) {
      clang::SourceRange Range(IndexExpr->getExprLoc(), IndexExpr->getExprLoc());
      ArrayType = SemaRef.getCxxSema().BuildArrayType(
        ArrayType, clang::ArrayType::Normal, IndexExpr, 0,
        Range, clang::DeclarationName());
    } else {
      if (!IsInnerMostArray || It != IndexExprs.rbegin()) {
        SemaRef.Diags.Report(IndexExpr->getExprLoc(),
                             clang::diag::err_expr_not_cce)
                             << /*array size*/3;
      } else {
        DynArrayExpr = IndexExpr;
      }
    }
  }
  return SemaRef.buildTypeExpr(ArrayType, S->getLoc());
}

clang::Expr *
ExprElaborator::elaborateNewExpr_TypeNode(const Syntax *S,
                                  llvm::Optional<clang::Expr *> &DynArrayExpr) {
  if (auto Call = dyn_cast<CallSyntax>(S)) {
    FusedOpKind OpKind = getFusedOpKind(SemaRef, Call);
    if (OpKind == FOK_Brackets) {
      return elaborateNewExpr_ArrayCall(Call, DynArrayExpr);
    }
  }

  // Normal type.
  return doElaborateExpr(S);
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
    switch (D->getKind()) {
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

    case DK_TemplateParams:
      break;

    default:
      llvm_unreachable("unhandled declarator.");
    }
  }

  return TyExpr;
}

// Elaborate the parameters and incorporate their types into  the one
// we're building. Note that T is the return type (if any).
clang::Expr *
ExprElaborator::elaborateFunctionType(Declarator *D, clang::Expr *Ty) {
  FunctionDeclarator *FuncDcl = D->getAsFunction();

  // FIXME: Handle array-based arguments.
  const ListSyntax *Args = FuncDcl->getParams();

  // bool IsVariadic = D->Data.ParamInfo.VariadicParam;
  // Elaborate the parameter declarations in order to get their types, and save
  // the resulting scope with the declarator.
  llvm::SmallVector<clang::QualType, 4> Types;
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  SemaRef.enterScope(SK_Parameter, FuncDcl->getParams());
  for (unsigned I = 0; I < Args->getNumChildren(); ++I) {
    // There isn't really anything to translate here.
    if (FuncDcl->isVariadic() && I == Args->getNumChildren() - 1)
      break;
    const Syntax *P = Args->getChild(I);

    Elaborator Elab(Context, SemaRef);
    clang::ValueDecl *VD =
      cast_or_null<clang::ValueDecl>(Elab.elaborateParmDeclSyntax(P));
    if (!VD)
      continue;
    if (VD->getType()->isVariadicType()) {
      if (I < Args->getNumChildren() - 1) {
        SemaRef.Diags.Report(Args->getChild(I)->getLoc(),
                             clang::diag::err_expected) << clang::tok::r_paren;
        continue;
      }

      FuncDcl->setIsVariadic();
      continue;
    }

    Declaration *D = SemaRef.getCurrentScope()->findDecl(P);
    assert(D && "Didn't find associated declaration");
    assert(isa<clang::ParmVarDecl>(VD) && "Parameter is not a ParmVarDecl");
    clang::ParmVarDecl *PVD = cast<clang::ParmVarDecl>(VD);

    Context.CxxAST.setParameterIndex(PVD, I);
    PVD->setScopeInfo(0, I);
    Types.push_back(VD->getType());
    Params.push_back(PVD);
  }
  FuncDcl->setScope(SemaRef.saveScope(FuncDcl->getParams()));


  // FIXME: We need to configure parts of the prototype (e.g., noexcept).
  clang::FunctionProtoType::ExtProtoInfo EPI;
  if (FuncDcl->isVariadic()) {
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
  assert(D->isType());
  TypeDeclarator *TyDcl = D->getAsType();
  return doElaborateExpr(TyDcl->getTyExpr());
}


clang::Expr *
ExprElaborator::handleOperatorConst(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "const operator");
  clang::Expr *innerTypeExpr = doElaborateExpr(S->getArgument(0));
  return makeConstType(innerTypeExpr, S);
}

clang::Expr *ExprElaborator::handleRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "ref operator");
  clang::Expr *innerTypeExpr = doElaborateExpr(S->getArgument(0));
  return makeRefType(innerTypeExpr, S);
}

clang::Expr *ExprElaborator::handleRRefType(const CallSyntax *S) {
  assert(S->getNumArguments() == 1 && "Invalid number of arguments for "
      "rref operator");
  clang::Expr *innerTypeExpr = doElaborateExpr(S->getArgument(0));
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

// Handle a function type of the form `() -> type`
clang::Expr *ExprElaborator::handleFunctionType(const CallSyntax *S) {
  assert(S->getNumArguments() == 2 && "invalid operator'->' call");

  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  llvm::SmallVector<clang::QualType, 4> Types;
  bool IsVariadic = false;

  const Syntax *ParamBegin = S->getArgument(0);

  // This might be a member function type.
  clang::TypeSourceInfo *ClassType = nullptr;
  if (const CallSyntax *MemPtr = dyn_cast<CallSyntax>(ParamBegin)) {
    if (getFusedOpKind(SemaRef, MemPtr) != FOK_MemberAccess) {
      SemaRef.Diags.Report(ParamBegin->getLoc(),
                           clang::diag::err_invalid_param_list);
      return nullptr;
    }

    clang::Expr *ClassTypeExpr =
      ExprElaborator(Context, SemaRef).elaborateExpr(MemPtr->getArgument(0));
    if (!ClassTypeExpr->getType()->isTypeOfTypes()) {
      SemaRef.Diags.Report(ClassTypeExpr->getExprLoc(),
                           clang::diag::err_invalid_type_for_name_spec)
        << ClassTypeExpr->getType();
      return nullptr;
    }

    ClassType = cast<clang::CppxTypeLiteral>(ClassTypeExpr)->getValue();
    ParamBegin = MemPtr->getArgument(1);
  }

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
      clang::Expr *Param = ParamElaborator.doElaborateExpr(ParamSyntax);

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
  clang::Expr *Return = ReturnElaborator.doElaborateExpr(ReturnSyntax);

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

  // Manually elaborate any attributes here.
  for (const Attribute *Attr : ParamBegin->getAttributes()) {
    std::string AttrName;
    if (checkAttrFormatAndName(Attr->getArg(), AttrName) == AF_Name) {
      if (AttrName == "const") {
        if (!ClassType) {
          SemaRef.Diags.Report(Attr->getArg()->getLoc(),
                               clang::diag::err_invalid_attribute_for_decl)
            << "const" << "member function";
        }

        EPI.TypeQuals.addConst();
      }
    }
  }

  clang::QualType FnTy =
    CxxAST.getFunctionType(ReturnType->getType(), Types, EPI);
  clang::QualType FnPtrTy = CxxAST.getPointerType(FnTy);
  FnPtrTy.addConst();

  if (ClassType) {
    clang::DeclarationName Name;
    clang::QualType MemberTy =
      SemaRef.getCxxSema().BuildMemberPointerType(FnTy, ClassType->getType(),
                                                  S->getLoc(), Name);
    clang::TypeSourceInfo *Ret =
      BuildMemberPtrTypeLoc(CxxAST, MemberTy, Params, S->getLoc());
    return SemaRef.buildTypeExpr(Ret);
  }

  clang::TypeLocBuilder TLB;
  clang::TypeSourceInfo *FnTSI = BuildFunctionTypeLoc(Context.CxxAST, TLB, FnTy,
    ParamBegin->getLoc(), ParamBegin->getLoc(), EndLoc,
    clang::SourceRange(), ReturnSyntax->getLoc(), Params);
  clang::TypeSourceInfo *FnPtrTSI =
    BuildFunctionPtrTypeLoc(CxxAST, TLB, FnTSI, S->getLoc());

  return SemaRef.buildTypeExpr(FnPtrTSI);
}

clang::Expr *ExprElaborator::handleArrayType(const CallSyntax *S) {
  if (S->getNumArguments() != 2) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_failed_to_translate_type);
    return nullptr;
  }

  clang::Expr *IdExpr = doElaborateExpr(S->getArgument(1));
  if (!IdExpr)
    return nullptr;

  // Attempt to translate into type location.
  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(IdExpr,
                                                   S->getArgument(1)->getLoc());
  if (!TInfo)
    return nullptr;

  llvm::SmallVector<clang::Expr *, 4> IndexExprs;
  const ListSyntax *IndexList = dyn_cast<ListSyntax>(S->getArgument(0));
  if (IndexList) {
    for (const Syntax *SS : IndexList->children())
      IndexExprs.push_back(doElaborateExpr(SS));
  } else {
    IndexExprs.push_back(doElaborateExpr(S->getArgument(0)));
  }

  // FIXME: what do we do for an empty array index, such as []int = {...}
  unsigned I = 0;
  for (clang::Expr *IndexExpr : IndexExprs) {
    if (!IndexExpr) {
      SemaRef.Diags.Report(S->getArgument(I)->getLoc(),
                           clang::diag::err_failed_to_translate_type);
      return nullptr;
    }

    ++I;
  }

  clang::QualType ArrayType = TInfo->getType();
  bool Invalid = false;
  for (auto It = IndexExprs.rbegin(); It != IndexExprs.rend(); ++It) {
    clang::Expr *IndexExpr = *It;

    clang::Expr::EvalResult IdxResult;
    clang::Expr::EvalContext
      EvalCtx(Context.CxxAST, SemaRef.getCxxSema().GetReflectionCallbackObj());

    if (!IndexExpr->EvaluateAsConstantExpr(IdxResult, EvalCtx)) {
      Invalid = true;
      continue;
    }

    clang::SourceRange Range(IndexExpr->getExprLoc(), IndexExpr->getExprLoc());
    ArrayType = SemaRef.getCxxSema().BuildArrayType(
      ArrayType, clang::ArrayType::Normal, IndexExpr, 0,
      Range, clang::DeclarationName());
  }

  if (Invalid)
    return nullptr;

  return SemaRef.buildTypeExpr(ArrayType, S->getLoc());
}

clang::Expr *ExprElaborator::handleOpPackExpansion(const CallSyntax *S) {
  // we do this in two ways first we elaborate the expression, then we see
  // if it's some kind of type otherwise we treat it as an expansion expression
  // and not as a parameter pack
  if (S->getNumArguments() != 1) {
    llvm_unreachable("This shouldn't be possible");
  }
  // Elaborating argument to possible expression.
  clang::Expr *Result = elaborateExpr(S->getArgument(0));
  if (!Result)
    // TODO: I may need to create an error message for this.
    return nullptr;

  clang::QualType ResultTy = Result->getType();
  if (ResultTy->isTypeOfTypes()) {
    return makeOpPackExpansionType(Result, S);
  } else if (ResultTy->isNamespaceType() || ResultTy->isTemplateType()) {
    SemaRef.Diags.Report(S->getCallee()->getLoc(),
                         clang::diag::err_invalid_unpack_expr);
    return Result;
  } else {
    auto ExprRes =
        SemaRef.getCxxSema().CheckPackExpansion(Result, S->getCallee()->getLoc(),
                                                llvm::None);
    // TODO: check if we need an error message here or not.
    return ExprRes.get();
  }
}

clang::Expr *ExprElaborator::handleBuiltinCall(const CallSyntax *S,
                                               unsigned ID) {
  assert(isa<AtomSyntax>(S->getCallee()) && "invalid builtin");
  const AtomSyntax *Atom = cast<AtomSyntax>(S->getCallee());
  clang::SourceLocation Loc = Atom->getLoc();

  clang::ASTContext::GetBuiltinTypeError Error;
  clang::QualType BuiltinType = CxxAST.GetBuiltinType(ID, Error);

  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  if (buildFunctionCallArguments(SemaRef, Context, ArgList, Args))
    return nullptr;

  clang::IdentifierInfo *II = &Context.CxxAST.Idents.get(Atom->getSpelling());
  clang::LookupResult R(SemaRef.getCxxSema(), {{II}, Loc},
                        clang::Sema::LookupOrdinaryName);
  if (!SemaRef.getCxxSema().LookupName(R, SemaRef.getCurClangScope(), true)) {
    llvm::StringRef Msg = llvm::StringRef(II->getNameStart());
    SemaRef.Diags.Report(Loc, clang::diag::err_undeclared_var_use) << Msg;
    return nullptr;
  }

  clang::NamedDecl *ND = R.getFoundDecl();
  assert(isa<clang::ValueDecl>(ND));
  clang::ValueDecl *VD = cast<clang::ValueDecl>(ND);
  clang::DeclRefExpr *Fn = clang::DeclRefExpr::Create(
    CxxAST, clang::NestedNameSpecifierLoc(),
    clang::SourceLocation(), VD, /*Capture=*/false, Loc,
    BuiltinType, clang::VK_RValue);

  clang::ExprResult Call = SemaRef.getCxxSema().ActOnCallExpr(
    SemaRef.getCxxSema().getCurScope(), Fn, S->getLoc(), Args, Loc);
  if (Call.isInvalid())
    return nullptr;

  return Call.get();
}

clang::Expr *ExprElaborator::handleVaArg(const CallSyntax *S) {
  clang::SourceLocation Loc = S->getCallee()->getLoc();
  llvm::SmallVector<clang::Expr *, 8> Args;
  const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
  if (buildFunctionCallArguments(SemaRef, Context, ArgList, Args))
    return nullptr;

  if (Args.size() > 2) {
    SemaRef.Diags.Report(Args[2]->getExprLoc(),
                         clang::diag::err_expected) << clang::tok::r_paren;
    return nullptr;
  }

  if (!Args[0] || getExprResultFromType(Args[0]->getType()) != VALUE) {
    clang::SourceLocation Loc = Args[0] ? Args[0]->getExprLoc() : S->getLoc();
    SemaRef.Diags.Report(Loc, clang::diag::err_expected_expression);
  }

  if (!Args[1] || getExprResultFromType(Args[1]->getType()) != TYPE) {
    clang::SourceLocation Loc = Args[1] ? Args[1]->getExprLoc() : S->getLoc();
    SemaRef.Diags.Report(Loc, clang::diag::err_expected_type);
  }

  clang::TypeSourceInfo *Ty = cast<clang::CppxTypeLiteral>(Args[1])->getValue();
  clang::ParsedType ParsedTy =
    SemaRef.getCxxSema().CreateParsedType(Ty->getType(), Ty);
  clang::ExprResult Res =
    SemaRef.getCxxSema().ActOnVAArg(Loc, Args[0], ParsedTy, Loc);
  if (Res.isInvalid())
    return nullptr;
  return Res.get();
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

clang::Expr *ExprElaborator::makeOpPackExpansionType(clang::Expr *Result,
                                                     const CallSyntax *S) {
  assert(Result && "Invalid result expression");
  assert(S && "Invalid syntax");

  clang::TypeSourceInfo *TInfo = SemaRef.getTypeSourceInfoFromExpr(Result,
                                                                   S->getLoc());
  if (!TInfo)
    return nullptr;
  clang::TypeSourceInfo *PackInfo =
        SemaRef.getCxxSema().CheckPackExpansion(TInfo, S->getCallee()->getLoc(),
                                                llvm::None);
  return SemaRef.buildTypeExpr(PackInfo);
}


clang::Expr *ExprElaborator::elaboratePartialElementExpr(clang::Expr *E,
                                                       const ElemSyntax *Elem) {
  auto *PartialExpr = dyn_cast<clang::CppxPartialEvalExpr>(E);
  assert(PartialExpr && "Invalid partial expression.");
  // The assumption for this is that we are template because there are no other
  // partial expressions other then the one that's used for calls to inplace
  // new.
  llvm::SmallVector<clang::Expr *, 32> TemplateArgExprs;
  const auto *ArgListSyntax = cast<ListSyntax>(Elem->getArguments());
  for(const auto *ArgSyntax : ArgListSyntax->children()) {
    clang::EnterExpressionEvaluationContext EnterConstantEvaluated(
                                                          SemaRef.getCxxSema(),
                  clang::Sema::ExpressionEvaluationContext::ConstantEvaluated,
                                                /*LambdaContextDecl=*/nullptr,
                                                              /*ExprContext=*/
          clang::Sema::ExpressionEvaluationContextRecord::EK_TemplateArgument);

    clang::Expr *ArgExpr = doElaborateExpr(ArgSyntax);
    if (!ArgExpr) {
      SemaRef.Diags.Report(ArgSyntax->getLoc(),
                           clang::diag::err_failed_to_translate_expr);
      continue;
    }
    TemplateArgExprs.emplace_back(ArgExpr);
  }
  // We just emit the error and still attempt to complete the remainder of the
  // partial expression.
  if (!PartialExpr->canAcceptElementArgs(TemplateArgExprs)) {
    SemaRef.Diags.Report(Elem->getObject()->getLoc(),
                         clang::diag::err_invalid_partial_expr_element_args);
  } else {
    PartialExpr->applyElementArgs(TemplateArgExprs);
  }
  return PartialExpr;
}

clang::Expr *ExprElaborator::elaboratePartialCallExpr(clang::Expr *E,
                                                      const CallSyntax *S,
                                    llvm::SmallVector<clang::Expr *, 8> &Args) {
  auto *PartialExpr = dyn_cast<clang::CppxPartialEvalExpr>(E);
  assert(PartialExpr && "Invalid partial expression.");

  if (!PartialExpr->canAcceptFunctionArgs(Args)) {
    SemaRef.Diags.Report(S->getLoc(),
                         clang::diag::err_invalid_partial_expr_call_args);
  } else {
    PartialExpr->applyFunctionArgs(Args);
  }
  return E;
}

clang::Expr *ExprElaborator::completePartialExpr(clang::Expr *E) {
  if (!E) {
    return nullptr;
  }
  if (auto *PartialExpr = dyn_cast<clang::CppxPartialEvalExpr>(E)) {
    if (PartialExpr->isCompletable()) {
      return PartialExpr->forceCompleteExpr();
    }
    // else {
    //   SemaRef.Diags.Report(E->getExprLoc(),
    //                        clang::diag::err_invalid_partial_expr_malformed);
    // }
  }
  return E;
}

#undef VALUE
#undef NAMESPACE
#undef TYPE
#undef TEMPLATE

} // namespace gold

