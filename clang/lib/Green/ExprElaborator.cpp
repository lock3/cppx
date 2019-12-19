#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/OperationKinds.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
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

ExprElaborator::ExprElaborator(ASTContext &CxxAST, GreenSema &SemaRef)
  : CxxAST(CxxAST), SemaRef(SemaRef)
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
createIntegerLiteral(ASTContext &CxxAST, Token T, QualType IntType,
                     SourceLocation Loc) {
  llvm::APInt Value;
  unsigned Width = 0;

  // If we don't have a specified type, just create a default int.
  if (IntType.isNull() || IntType == CxxAST.AutoDeductTy)
    IntType = CxxAST.IntTy;

  // TODO: support all kinds of integer types.
  if (IntType == CxxAST.IntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.LongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    long long int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.ShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::get(Literal);
  } else if (IntType == CxxAST.UnsignedShortTy) {
    Width = CxxAST.getTargetInfo().getShortWidth();

    unsigned short int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedIntTy) {
    Width = CxxAST.getTargetInfo().getIntWidth();

    unsigned int Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongTy) {
    Width = CxxAST.getTargetInfo().getLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else if (IntType == CxxAST.UnsignedLongLongTy) {
    Width = CxxAST.getTargetInfo().getLongLongWidth();

    unsigned long Literal = atoi(T.getSymbol().data());
    Value = llvm::APSInt::getUnsigned(Literal);
  } else {
    assert(false && "Unsupported integer type.");
  }

  if (Value.getBitWidth() != Width)
    Value = Value.trunc(Width);

  return IntegerLiteral::Create(CxxAST, Value, IntType, Loc);
}

static DeclRefExpr *
createDeclRefExpr(ASTContext &CxxAST, GreenSema &SemaRef,
                  Token T, QualType Ty, SourceLocation Loc) {
  DeclarationNameInfo DNI({&CxxAST.Idents.get(T.getSpelling())}, Loc);
  LookupResult R(SemaRef.getCxxSema(), DNI, Sema::LookupAnyName);

  SemaRef.LookupName(R, SemaRef.getCurrentScope());
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
      DeclRefExpr::Create(CxxAST, NestedNameSpecifierLoc(),
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
    return createIntegerLiteral(CxxAST, T, ExplicitType, S->Loc);
  case tok::DecimalFloat:
    break;
  case tok::BinaryInteger:
    break;
  case tok::HexadecimalInteger:
    break;
  case tok::HexadecimalFloat:
    break;
  case tok::Identifier:
    return createDeclRefExpr(CxxAST, SemaRef, T, ExplicitType, S->Loc);
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
  const AtomSyntax *Callee = cast<AtomSyntax>(S->getCallee());
  std::string Spelling = Callee->Tok.getSpelling();

  if (&CxxAST.Idents.get(Spelling) == SemaRef.OperatorColonII) {
    const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());

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

  // Try to construct a normal function-call expression.

  // First do unqualified lookup.
  DeclarationNameInfo DNI({&CxxAST.Idents.get(Spelling)}, S->Loc);
  LookupResult R(SemaRef.getCxxSema(), DNI, Sema::LookupAnyName);
  SemaRef.LookupName(R, SemaRef.getCurrentScope());

  // If we found something, see if it is viable.
  if (!R.empty()) {
    Expr *Fn = nullptr;

    R.resolveKind();
    if (R.isOverloadedResult()) {
      Fn =
        UnresolvedLookupExpr::Create(CxxAST, R.getNamingClass(),
                                     NestedNameSpecifierLoc(),
                                     R.getLookupNameInfo(), /*ADL=*/true,
                                     /*Overloaded=*/true, R.begin(),
                                     R.end());
    } else if (R.isSingleResult()) {
      ValueDecl *VD = R.getAsSingle<ValueDecl>();

      // This had better be a reference to a function.
      FunctionDecl *FD = dyn_cast<FunctionDecl>(VD);
      if (!FD) return nullptr;

      Fn =
        DeclRefExpr::Create(CxxAST, NestedNameSpecifierLoc(),
                            SourceLocation(), VD, /*Capture=*/false,
                            S->Loc, VD->getType(), VK_RValue);
    }

    if (!Fn)
      return nullptr;

    // Get the passed arguments.
    llvm::SmallVector<Expr *, 8> Args;
    const ListSyntax *ArgList = dyn_cast<ListSyntax>(S->getArguments());
    assert(ArgList && "Unexpected argument format.");
    for (const Syntax *A : ArgList->children()) {
      ExprElaborator Elab(CxxAST, SemaRef);
      Expr *AExpr = Elab.elaborateExpr(A);

      if (AExpr)
        Args.push_back(AExpr);
    }

    // Create the call.
    MultiExprArg MultiArgs(Args);
    ExprResult Call =
      SemaRef.getCxxSema().ActOnCallExpr(SemaRef.getCxxSema().getCurScope(),
                                         Fn, S->Loc, MultiArgs, S->Loc);
    if (Call.isInvalid())
      return nullptr;
    return Call.get();
  }


  llvm::errs() << "Unsupported call.\n";
  return nullptr;
}

Expr *
ExprElaborator::elaborateBinOp(const CallSyntax *S, BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  Expr *RHSExpr = elaborateExpr(LHSSyntax);
  Expr *LHSExpr = elaborateExpr(RHSSyntax);

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  ExprResult Res = ClangSema.BuildBinOp(/*Scope=*/nullptr, SourceLocation(), Op,
                                        LHSExpr, RHSExpr);
  if (Res.isInvalid())
    return nullptr;

  return Res.get();
}

Expr *
ExprElaborator::elaborateCmpAssignOp(const CallSyntax *S,
                                     BinaryOperatorKind Op) {
  const ListSyntax *ArgList = cast<ListSyntax>(S->getArguments());
  const Syntax *LHSSyntax = ArgList->Elems[0];
  const Syntax *RHSSyntax = ArgList->Elems[1];

  Expr *RHSExpr = elaborateExpr(LHSSyntax);
  Expr *LHSExpr = elaborateExpr(RHSSyntax);

  clang::Sema &ClangSema = SemaRef.getCxxSema();

  ExprResult Res =
    ClangSema.CreateBuiltinBinOp(SourceLocation(), Op, LHSExpr, RHSExpr);
  if (Res.isInvalid())
    return nullptr;

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
Expr *ExprElaborator::elaborateBlockCondition(const ArraySyntax *Conditions) {
  // If there's only one term, we don't need to do anything else.
  if (Conditions->getNumChildren() == 1)
    return elaborateExpr(Conditions->getChild(0));

  Expr *LHS, *RHS;

  {
    ExprElaborator ExEl(CxxAST, SemaRef);
    LHS = ExEl.elaborateExpr(Conditions->getChild(0));
  }
  {
    ExprElaborator ExEl(CxxAST, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(1));
  }

  ExprResult BinOp =
    SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, SourceLocation(),
                                    clang::tok::ampamp, LHS, RHS);
  if (BinOp.isInvalid())
    return nullptr;

  // For all remaining terms, append them to the back of the && expression.
  // Ex., if we had `1 && 2`, we would append `3` to get `1 && 2 && 3`.
  for (unsigned I = 2; I < Conditions->getNumChildren(); ++I) {
    ExprElaborator ExEl(CxxAST, SemaRef);
    RHS = ExEl.elaborateExpr(Conditions->getChild(I));

    BinOp =
      SemaRef.getCxxSema().ActOnBinOp(/*Scope=*/nullptr, SourceLocation(),
                                      clang::tok::ampamp, BinOp.get(), RHS);
    if (BinOp.isInvalid())
      return nullptr;
  }

  return BinOp.get();
}

  // 1, 2, 3

  // 1 && 2 && 3

  // 2 && 3
  // 1 && 2 && 3

} // namespace green
