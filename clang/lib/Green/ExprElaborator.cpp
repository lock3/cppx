#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringMap.h"

#include "clang/Green/Elaborator.h"
#include "clang/Green/ExprElaborator.h"
#include "clang/Green/GreenSema.h"
#include "clang/Green/Tokens.h"

#include <cstring>

namespace green {

using namespace clang;

ExprElaborator::ExprElaborator(ASTContext &ClangContext, GreenSema &SemaRef)
  : ClangContext(ClangContext), SemaRef(SemaRef)
{}

Expr *
ExprElaborator::elaborateExpr(const Syntax *S) {
  if (isa<AtomSyntax>(S))
    return elaborateAtom(cast<AtomSyntax>(S), QualType());
  if (isa<CallSyntax>(S))
    return elaborateCall(cast<CallSyntax>(S));

  assert(false && "Unsupported expression.");
}

static IntegerLiteral *
createIntegerLiteral(ASTContext &ClangContext, Token T, QualType IntType,
                     SourceLocation Loc) {
  llvm::APInt Value;
  unsigned Width = 0;

  // If we don't have a specified type, just create a default int.
  if (IntType.isNull() || IntType == ClangContext.AutoDeductTy)
    IntType = ClangContext.IntTy;

  // TODO: support all kinds of integer types.
  if (IntType == ClangContext.IntTy) {
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

    ValueDecl *VD = R.getAsSingle<ValueDecl>();
    QualType FoundTy = VD->getType();

    // If the user annotated the DeclRefExpr with an incorrect type.
    if (!Ty.isNull() && Ty != FoundTy) {
      llvm::errs() << "Annotated type does not match expression type.\n";
      return nullptr;
    }

    DeclRefExpr *DRE =
      DeclRefExpr::Create(ClangContext, NestedNameSpecifierLoc(),
                          SourceLocation(), VD, /*Capture=*/false,
                          Loc, FoundTy, VK_RValue);
    return DRE;
  }

  llvm::errs() << "Name not found.\n";
  return nullptr;
}

Expr *
ExprElaborator::elaborateAtom(const AtomSyntax *S, QualType ExplicitType) {
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

// Mapping of Green's fused operator strings to clang Opcodes.
// TODO: Assignment will probably be handled differently than other bin ops?
const llvm::StringMap<BinaryOperatorKind> BinaryOperators = {
  {"operator'+'" , BO_Add},
  {"operator'-'" , BO_Sub},
  {"operator'*'" , BO_Mul},
  {"operator'/'" , BO_Div},
  {"operator'%'" , BO_Rem},
  {"operator'&'" , BO_And},
  {"operator'|'" , BO_Or},
  {"operator'^'" , BO_Xor},
  {"operator'&&'" , BO_LAnd},
  {"operator'||'" , BO_LOr},
  {"operator'=='" , BO_EQ},
  {"operator'<>'", BO_NE},
  {"operator'<'", BO_LT},
  {"operator'>'", BO_GT},
  {"operator'<='", BO_LE},
  {"operator'>='", BO_GE},
};

const llvm::StringMap<BinaryOperatorKind> CompoundAssignOperators = {
  {"operator'+='" , BO_AddAssign},
  {"operator'-='" , BO_SubAssign},
  {"operator'*='" , BO_MulAssign},
  {"operator'/='" , BO_DivAssign},
  {"operator'%='" , BO_RemAssign},
  {"operator'&='" , BO_AndAssign},
  {"operator'|='" , BO_OrAssign},
  {"operator'^='" , BO_XorAssign},
};

Expr *
ExprElaborator::elaborateCall(const CallSyntax *S) {
  const AtomSyntax *Callee = cast<AtomSyntax>(S->Callee());
  std::string Spelling = Callee->Tok.getSpelling();

  Preprocessor &PP = SemaRef.getPP();
  if (PP.getIdentifierInfo(Spelling) == SemaRef.OperatorColonII) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->Args());

    Elaborator Elab(SemaRef.getContext(), SemaRef);
    QualType T = Elab.getOperatorColonType(S);

    return elaborateAtom(cast<AtomSyntax>(ArgList->Elems[0]), T);
  }

  // Check if this is a standard binary operator (one that doesn't assign).
  auto BinOpMapIter = BinaryOperators.find(Spelling);
  if (BinOpMapIter != BinaryOperators.end()) {
    return elaborateBinOp(S, BinOpMapIter->second);
  }

  // Check if this is a compound assignment operator like operator'+='.
  auto CmpAssnMapIter = CompoundAssignOperators.find(Spelling);
  if (CmpAssnMapIter != CompoundAssignOperators.end()) {
    return elaborateCmpAssignOp(S, CmpAssnMapIter->second);
  }

  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

Expr *
ExprElaborator::elaborateBinOp(const CallSyntax *S, BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  Expr *RHSExpr = elaborateExpr(LHSSyntax);
  Expr *LHSExpr = elaborateExpr(RHSSyntax);

  clang::Sema &ClangSema = SemaRef.getClangSema();

  ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr, SourceLocation(), Op,
                                        LHSExpr, RHSExpr);
  if (Res.isInvalid())
    return nullptr;

  return Res.get();
}

Expr *
ExprElaborator::elaborateCmpAssignOp(const CallSyntax *S,
                                     BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->Args());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  Expr *RHSExpr = elaborateExpr(LHSSyntax);
  Expr *LHSExpr = elaborateExpr(RHSSyntax);

  clang::Sema &ClangSema = SemaRef.getClangSema();

  ExprResult Res =
    ClangSema.CreateBuiltinBinOp(SourceLocation(), Op, LHSExpr, RHSExpr);
  if (Res.isInvalid())
    return nullptr;

  return Res.get();
}

} // namespace green
