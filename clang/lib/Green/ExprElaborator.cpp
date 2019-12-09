#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/APSInt.h"

#include "clang/Green/ExprElaborator.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/Tokens.h"

#include <cstring>

namespace green {

using namespace clang;

ExprElaborator::ExprElaborator(ASTContext &ClangContext, GreenSema &SemaRef)
  : ClangContext(ClangContext), SemaRef(SemaRef)
{}

static IntegerLiteral *
createIntegerLiteral(ASTContext &ClangContext, Token T, QualType IntType,
                     SourceLocation Loc) {
  llvm::APInt Value;
  unsigned Width = 0;

  // TODO: support all kinds of integer types.
  if (IntType.isNull() || IntType == ClangContext.AutoDeductTy) {
    long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
    IntType = ClangContext.getIntTypeForBitwidth(64, /*Signed=*/true);
  } else if (IntType == ClangContext.IntTy) {
    Width = ClangContext.getTargetInfo().getIntWidth();

    int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == ClangContext.LongTy) {
    Width = ClangContext.getTargetInfo().getLongWidth();

    long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == ClangContext.LongLongTy) {
    Width = ClangContext.getTargetInfo().getLongLongWidth();

    long long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == ClangContext.ShortTy) {
    Width = ClangContext.getTargetInfo().getShortWidth();

    short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == ClangContext.UnsignedShortTy) {
    Width = ClangContext.getTargetInfo().getShortWidth();

    unsigned short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == ClangContext.UnsignedIntTy) {
    Width = ClangContext.getTargetInfo().getIntWidth();

    unsigned int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == ClangContext.UnsignedLongTy) {
    Width = ClangContext.getTargetInfo().getLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == ClangContext.UnsignedLongLongTy) {
    Width = ClangContext.getTargetInfo().getLongLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else {
    assert(false && "Unsupported integer type.");
  }

  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);

  return IntegerLiteral::Create(ClangContext, Value, IntType, Loc);
}

static DeclRefExpr *
createDeclRefExpr(ASTContext &ClangContext, GreenSema &SemaRef, Preprocessor &PP,
                  Token T, QualType Ty, SourceLocation Loc) {
  DeclarationNameInfo DNI({PP.getIdentifierInfo(T.getSpelling())}, Loc);
  LookupResult R(SemaRef.getClangSema(), DNI, Sema::LookupAnyName);

  SemaRef.LookupName(R, SemaRef.getCurScope());
  if (!R.empty()) {
    if (!R.isSingleResult()) {
      llvm::errs() << "Multiple declarations of \"" << T.getSpelling() << "\" found.\n";
      return nullptr;
    }

    DeclRefExpr *DRE =
      DeclRefExpr::Create(ClangContext, NestedNameSpecifierLoc(),
                          SourceLocation(), R.getAsSingle<ValueDecl>(),
                          /*Capture=*/false, Loc, Ty, VK_RValue);
    return DRE;
  }

  llvm::errs() << "Name not found.\n";
  return nullptr;
}

Expr *
ExprElaborator::elaborateExpr(const AtomSyntax *S, QualType ExplicitType) {
  Token T = S->Tok;

  switch (T.getKind()) {
  case tok::DecimalInteger:
    return createIntegerLiteral(ClangContext, T, ExplicitType, S->Loc);
  case tok::DecimalFloat:
    break;
  case tok::BinaryInteger:
    break;
  case tok::HexadecimalInteger:
    break;
  case tok::HexadecimalFloat:
    break;
  case tok::Identifier:
    return createDeclRefExpr(ClangContext, SemaRef, SemaRef.getPP(),
                             T, ExplicitType, S->Loc);
    break;
  case tok::Character:
    break;
  case tok::String:
    break;
  default: break;
  }

  return nullptr;
}

} // namespace green
