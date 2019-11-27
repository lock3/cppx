#ifndef CLANG_GREEN_REQUIRETYPE_H
#define CLANG_GREEN_REQUIRETYPE_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Type.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/DenseMap.h"

#include "clang/Green/GreenSema.h"
#include "clang/Green/Syntax.h"
#include "clang/Green/SyntaxContext.h"

#include <unordered_map>

namespace green {

using namespace clang;

class TypeRequirer {
  friend class GreenSema; 
  // Map of names that we have already found a type for.
  llvm::DenseMap<IdentifierInfo *, const Type *> TypeMapping;

  SyntaxContext &Context;

  GreenSema &GSemaRef;

  Preprocessor &PP;

public:
  TypeRequirer(SyntaxContext &Context, GreenSema &GSemaRef);

  void RequireType(IdentifierInfo *Name, const Syntax *S);
  void RequireTypeForArray(IdentifierInfo *Name, const ArraySyntax *S);
  void RequireTypeForList(IdentifierInfo *Name, const ListSyntax *S);
  void RequireTypeForCall(IdentifierInfo *Name, const CallSyntax *S);
  void RequireTypeForAtom(IdentifierInfo *Name, const AtomSyntax *S);

private:
  // Counter of implicit template parameter types created in a single
  // function.
  unsigned TemplateIndex = 0;

  // Dictionary of built in types.
  const std::unordered_map<std::string, QualType> BuiltinTypes = {
    {"void", Context.ClangContext.VoidTy},
    {"bool", Context.ClangContext.BoolTy},
    {"char", Context.ClangContext.CharTy},
    {"wchar_t", Context.ClangContext.WideCharTy},
    {"wint_t", Context.ClangContext.WIntTy},
    {"char8_t", Context.ClangContext.Char8Ty},
    {"char16_t", Context.ClangContext.Char16Ty},
    {"char32_t", Context.ClangContext.Char32Ty},
    {"signed char", Context.ClangContext.SignedCharTy},
    {"short", Context.ClangContext.ShortTy},
    {"short int", Context.ClangContext.ShortTy},
    {"int", Context.ClangContext.IntTy},
    {"long", Context.ClangContext.LongTy},
    {"long int", Context.ClangContext.LongTy},
    {"long long", Context.ClangContext.LongLongTy},
    {"long long int", Context.ClangContext.LongLongTy},
    {"int128_t", Context.ClangContext.Int128Ty},
    {"unsigned char", Context.ClangContext.UnsignedCharTy},
    {"unsigned short", Context.ClangContext.UnsignedShortTy},
    {"unsigned short int", Context.ClangContext.UnsignedShortTy},
    {"unsigned", Context.ClangContext.UnsignedIntTy},
    {"unsigned int", Context.ClangContext.UnsignedIntTy},
    {"unsigned long", Context.ClangContext.UnsignedLongTy},
    {"unsigned long int", Context.ClangContext.UnsignedLongTy},
    {"unsigned long long", Context.ClangContext.UnsignedLongLongTy},
    {"unsigned long long int", Context.ClangContext.UnsignedLongLongTy},
    {"uint128_t", Context.ClangContext.UnsignedInt128Ty},
    {"float", Context.ClangContext.FloatTy},
    {"double", Context.ClangContext.DoubleTy},
    {"long double", Context.ClangContext.LongDoubleTy},
    {"float128_t", Context.ClangContext.Float128Ty}
  };

  // Tokenizations of commonly used strings:
  IdentifierInfo *OperatorColonII, *OperatorExclaimII;
};

} // namespace green

#endif
