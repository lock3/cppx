#include "clang/Gold/GoldOperatorInfo.h"


namespace gold {
OperatorNameConstants::OperatorNameConstants(clang::ASTContext &Context)
  :
#define init_names(Name, OpStr, ClangOpStr)                     \
  CppOp_##Name(&Context.Idents.get("operator" ClangOpStr)),     \
  GoldDecl_##Name(&Context.Idents.get("operator\"" OpStr "\"")),\
  GoldUse_##Name(&Context.Idents.get("operator'" OpStr "'")),

#define init_second_names(Name, OpStr, SecondName)                          \
  init_names(Name,OpStr,OpStr)                                              \
  GoldDecl_##Name##Text(&Context.Idents.get("operator\"" SecondName "\"")), \
  GoldUse_##Name##Text(&Context.Idents.get("operator'" SecondName "'")),

#define def_bin_op_missmatch(Name,OpStr,ClangOpStr,ClangBinOpName,    \
                             MemberOnly,OverloadStyle,OpOverloadName) \
  init_names(Name, OpStr, ClangOpStr)

#define def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, \
                   OverloadStyle,OpOverloadName)            \
  init_names(Name, OpStr, OpStr)

#define def_bin_op_second_name(Name, OpStr, SecondOpStr, ClangBinOpName,  \
                               MemberOnly, OverloadStyle, OpOverloadName) \
  init_second_names(Name, OpStr, SecondOpStr)

#define def_unary_and_binary_op(Name, OpStr,  ClangUnaryOpName,           \
                                ClangBinOpName, MemberOnly, OverloadStyle,\
                                OpOverloadName)                           \
  init_names(Name, OpStr, OpStr)

#define def_unary_and_binary_op_mismatch(Name, OpStr, ClangStr,           \
                                        ClangUnaryOpName, ClangBinOpName, \
                                        MemberOnly, OverloadStyle,        \
                                        UnaryOpOverloadName,              \
                                        BinaryOpOverloadName)             \
  init_names(Name, OpStr, ClangStr)

#define def_unary_op_second_name(Name, OpStr, SecondName, ClangUnaryOpName, \
                                 MemberOnly, OverloadStyle, OpOverloadName) \
  init_second_names(Name, OpStr, SecondName)

#define def_unary_cpp_only_op(Name,OpStr,MemberOnly,OverloadStyle,     \
                             OpOverloadName)                          \
  CppOp_##Name(&Context.Idents.get("operator" OpStr)),                \
  GoldDecl_##Name(&Context.Idents.get("operator\"" OpStr "\"")),

#define def_nary_op(Name,OpStr,MemberOnly,OverloadStyle,OpOverloadName) \
  CppOp_##Name(&Context.Idents.get("operator" OpStr)),                  \
  GoldDecl_##Name(&Context.Idents.get("operator\"" OpStr "\"")),

#include "clang/Gold/GoldOperatorInfo.def"
#undef init_names
#undef init_second_names
  Last()
{ }

OperatorDataConstants::OperatorDataConstants(
    const OperatorNameConstants & OpNames)
  :

#define def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,  \
                   OpOverloadName)                                          \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,                 \
             OpNames.GoldUse_##Name, clang::BO_##ClangBinOpName, MemberOnly,\
             AOU_##OverloadStyle, clang::OO_##OpOverloadName),

#define def_bin_op_second_name(Name, OpStr, SecondOpStr,ClangBinOpName,   \
                               MemberOnly, OverloadStyle, OpOverloadName) \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,               \
             OpNames.GoldUse_##Name, OpNames.GoldDecl_##Name##Text,       \
             OpNames.GoldUse_##Name##Text, clang::BO_##ClangBinOpName,    \
             MemberOnly, AOU_##OverloadStyle, clang::OO_##OpOverloadName),

#define def_bin_op_missmatch(Name, OpStr, ClangOpStr, ClangBinOpName,   \
                             MemberOnly, OverloadStyle, OpOverloadName) \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly,                   \
             OverloadStyle, OpOverloadName)

#define def_unary_and_binary_op(Name, OpStr, ClangUnaryOpName,      \
                                ClangBinOpName, MemberOnly,         \
                                OverloadStyle, OpOverloadName)      \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,         \
             OpNames.GoldUse_##Name, clang::UO_##ClangUnaryOpName,  \
             clang::BO_##ClangBinOpName, MemberOnly,                \
             AOU_##OverloadStyle, clang::OO_##OpOverloadName),

#define def_unary_and_binary_op_mismatch(Name,OpStr,ClangStr,         \
                                         ClangUnaryOpName,            \
                                         ClangBinOpName,MemberOnly,   \
                                         OverloadStyle,               \
                                         UnaryOpOverloadName,         \
                                         BinaryOpOverloadName)        \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,           \
            OpNames.GoldUse_##Name, clang::UO_##ClangUnaryOpName,     \
            clang::BO_##ClangBinOpName, MemberOnly,                   \
            AOU_##OverloadStyle, clang::OO_##UnaryOpOverloadName,     \
            clang::OO_##BinaryOpOverloadName),

#define def_unary_op_second_name(Name, OpStr, SecondName, ClangUnaryOpName, \
                                 MemberOnly, OverloadStyle, OpOverloadName) \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,                 \
             OpNames.GoldUse_##Name,  OpNames.GoldDecl_##Name##Text,        \
             OpNames.GoldUse_##Name##Text, clang::UO_##ClangUnaryOpName,    \
             MemberOnly, AOU_##OverloadStyle, clang::OO_##OpOverloadName),

#define def_nary_op(Name, OpStr, MemberOnly, OverloadStyle, OpOverloadName) \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name, MemberOnly,     \
             AOU_##OverloadStyle, clang::OO_##OpOverloadName),

#define def_unary_cpp_only_op(Name, OpStr, MemberOnly, OverloadStyle,   \
                              OpOverloadName)                           \
  Name##Info(OpNames.CppOp_##Name, OpNames.GoldDecl_##Name,             \
             MemberOnly, AOU_##OverloadStyle,                           \
             clang::OO_##OpOverloadName),

#include "clang/Gold/GoldOperatorInfo.def"
  Last()
{ }


// ===--- OpInfoBase ---==================================================== ///
OpInfoBase::OpInfoBase(int OpKinds, clang::IdentifierInfo *ClangOpName,
                       clang::IdentifierInfo *GoldOpDeclName, bool IsMemberOnly,
                       AllowedOverloadUse AllowedOverloadStyle,
                       clang::OverloadedOperatorKind OpKind)
  :Kinds(OpKinds),
  ClangName(ClangOpName),
  GoldDeclName(GoldOpDeclName),
  IsMemberOnlyOp(IsMemberOnly),
  Overloadability(AllowedOverloadStyle),
  OOKind(OpKind)
{ }

bool OpInfoBase::isUnary() const {
  return Kinds & OI_UnaryOp;
}

bool OpInfoBase::isBinary() const {
  return Kinds & OI_BinOp;
}

bool OpInfoBase::isUnaryAndBinary() const {
  constexpr int BothOptions = OI_BinOp | OI_UnaryOp;
  return (Kinds & BothOptions) == BothOptions;
}

bool OpInfoBase::isNAry() const {
  return Kinds & OI_NAryOp;
}

bool OpInfoBase::isNotPartOfGold() const {
  return Kinds & OI_NotPartOfGoldLanguage;
}

bool OpInfoBase::isMemberOnly() const {
  return IsMemberOnlyOp;
}

AllowedOverloadUse OpInfoBase::getAllowedOverloadUse() const {
  return Overloadability;
}

clang::OverloadedOperatorKind OpInfoBase::getUnaryOverloadKind() const {
    return OOKind;
}

clang::OverloadedOperatorKind OpInfoBase::getBinaryOverloadKind() const {
    return OOKind;
}

clang::IdentifierInfo *OpInfoBase::getGoldDeclName() const {
  return GoldDeclName;
}

clang::IdentifierInfo *OpInfoBase::getClangName() const {
  return ClangName;
}

bool OpInfoBase::HasGoldUseName() const {
  return false;
}

clang::IdentifierInfo *OpInfoBase::getGoldUseName() const {
  llvm_unreachable("Invalid gold use name.");
}

bool OpInfoBase::HasSecondName() const {
  return false;
}

clang::IdentifierInfo *OpInfoBase::getGoldDeclSecondName() const {
  llvm_unreachable("Invalid 2nd name.");
}

clang::IdentifierInfo *OpInfoBase::getGoldUseSecondName() const {
  llvm_unreachable("Invalid 2nd name.");
}

clang::UnaryOperatorKind OpInfoBase::getUnaryOperatorKind() const {
  llvm_unreachable("Current operator is not a unary operator");
}

clang::BinaryOperatorKind OpInfoBase::getBinaryOperatorKind() const {
  llvm_unreachable("Current operator is not a binary operator");
}

void OpInfoBase::registerLookup(OperatorLookup &Lookup) const {
  Lookup.insert({getGoldDeclName(), this});
  if (HasGoldUseName())
    Lookup.insert({getGoldUseName(), this});

  if (HasSecondName()) {
    Lookup.insert({getGoldDeclSecondName(), this});
    Lookup.insert({getGoldUseSecondName(), this});
  }
}


// ===--- UnaryOpInfo ---=================================================== ///
bool UnaryOpInfo::HasGoldUseName() const {
  return true;
}

clang::IdentifierInfo *UnaryOpInfo::getGoldUseName() const {
  return GoldUseName;
}

clang::UnaryOperatorKind UnaryOpInfo::getUnaryOperatorKind() const {
  return UnaryOpKind;
}

// ===--- UnaryOpWith2ndName ---============================================ ///
bool UnaryOpWith2ndName::HasSecondName() const {
  return true;
}

clang::IdentifierInfo *UnaryOpWith2ndName::getGoldDeclSecondName() const {
  return GoldDecl2ndName;
}

clang::IdentifierInfo *UnaryOpWith2ndName::getGoldUseSecondName() const {
  return GoldUse2ndName;
}

// ===--- BinOpInfo ---===================================================== ///
bool BinOpInfo::HasGoldUseName() const {
  return true;
}

clang::IdentifierInfo *BinOpInfo::getGoldUseName() const {
  return GoldUseName;
}

clang::BinaryOperatorKind BinOpInfo::getBinaryOperatorKind() const {
  return BinOpKind;
}

// ===--- BinOpInfoWith2ndName ---========================================== ///
bool BinOpInfoWith2ndName::HasSecondName() const {
  return true;
}

clang::IdentifierInfo * BinOpInfoWith2ndName::getGoldDeclSecondName() const {
  return GoldDecl2ndName;
}

clang::IdentifierInfo * BinOpInfoWith2ndName::getGoldUseSecondName() const {
  return GoldUse2ndName;
}

// ===--- UnaryAndBinOpInfo ---============================================= ///
bool UnaryAndBinOpInfo::HasGoldUseName() const {
  return true;
}

clang::IdentifierInfo *UnaryAndBinOpInfo::getGoldUseName() const {
  return GoldUseName;
}

clang::UnaryOperatorKind UnaryAndBinOpInfo::getUnaryOperatorKind() const {
  return UnaryOpKind;
}

clang::BinaryOperatorKind UnaryAndBinOpInfo::getBinaryOperatorKind() const {
  return BinOpKind;
}

// ===--- DerefAndXOrComboOp ---============================================ ///
bool DerefAndXOrComboOp::HasGoldUseName() const {
  return true;
}

clang::IdentifierInfo *DerefAndXOrComboOp::getGoldUseName() const {
  return GoldUseName;
}

clang::UnaryOperatorKind DerefAndXOrComboOp::getUnaryOperatorKind() const {
  return UnaryOpKind;
}

clang::BinaryOperatorKind DerefAndXOrComboOp::getBinaryOperatorKind() const {
  return BinOpKind;
}

clang::OverloadedOperatorKind
DerefAndXOrComboOp::getBinaryOverloadKind() const {
  return BinOOKind;
}

namespace {
  using OperatorNameConstantsPtr = OperatorNameConstants *;
}

static OperatorLookup buildLookupTable(OperatorDataConstants const& Info) {
  OperatorLookup Lookup;
#define def_op_name(Name)\
  Info.Name##Info.registerLookup(Lookup);
#include "clang/Gold/GoldOperatorInfo.def"
  return Lookup;
}
OperatorInfo::OperatorInfo(clang::ASTContext &Context)
  :OperatorNameConstants(Context),
  CxxAST(Context),
  Info(*OperatorNameConstantsPtr(this)),
  OpLookupTable(buildLookupTable(Info))
{ }

bool OperatorInfo::getBinaryOperatorUseKind(llvm::StringRef OpName,
                                        clang::BinaryOperatorKind &Kind) const {
  return getBinaryOperatorUseKind(&CxxAST.Idents.get({OpName}), Kind);
}

bool OperatorInfo::getBinaryOperatorUseKind(clang::IdentifierInfo *Id,
                                        clang::BinaryOperatorKind &Kind) const {
  auto It = OpLookupTable.find(Id);
  if (It == OpLookupTable.end())
    return true;

  if (!It->second->isBinary())
    return true;
  Kind = It->second->getBinaryOperatorKind();
  return false;
}

bool OperatorInfo::isBinaryOperator(llvm::StringRef Name) const {
  return isBinaryOperator(&CxxAST.Idents.get({Name}));
}

bool OperatorInfo::isBinaryOperator(clang::IdentifierInfo *Id) const {
  auto It = OpLookupTable.find(Id);
  if (It == OpLookupTable.end())
    return false;
  return It->second->isBinary();
}

bool OperatorInfo::getUnaryOperatorUseKind(llvm::StringRef OpName,
                                         clang::UnaryOperatorKind &Kind) const {
  return getUnaryOperatorUseKind(&CxxAST.Idents.get({OpName}), Kind);
}

bool OperatorInfo::getUnaryOperatorUseKind(clang::IdentifierInfo *Id,
                                        clang::UnaryOperatorKind &Kind) const {
  auto It = OpLookupTable.find(Id);
  if (It == OpLookupTable.end())
    return true;

  if (!It->second->isUnary())
    return true;
  Kind = It->second->getUnaryOperatorKind();
  return false;
}

bool OperatorInfo::isUnaryOperator(llvm::StringRef Name) const {
  return isUnaryOperator(&CxxAST.Idents.get({Name}));
}

bool OperatorInfo::isUnaryOperator(clang::IdentifierInfo *Id) const {
  auto It = OpLookupTable.find(Id);
  if (It == OpLookupTable.end())
    return false;
  return It->second->isUnary();
}

const OpInfoBase *OperatorInfo::getOpInfo(llvm::StringRef Name) const {
  return getOpInfo(&CxxAST.Idents.get({Name}));
}

const OpInfoBase *OperatorInfo::getOpInfo(clang::IdentifierInfo *Id) const {
  auto It = OpLookupTable.find(Id);
  if (It == OpLookupTable.end())
    return nullptr;
  return It->second;
}

clang::IdentifierInfo *
OperatorInfo::getClangName(llvm::StringRef GoldDeclOrUseName) const{
  return getClangName(&CxxAST.Idents.get({GoldDeclOrUseName}));
}

clang::IdentifierInfo *
OperatorInfo::getClangName(clang::IdentifierInfo *GoldDeclOrUseName) const {
  auto It = OpLookupTable.find(GoldDeclOrUseName);
  if (It == OpLookupTable.end())
    return nullptr;
  return It->second->getClangName();
}

bool OperatorInfo::isValidDeclName(llvm::StringRef GodlDeclName) const {
  return isValidDeclName(&CxxAST.Idents.get({GodlDeclName}));
}

bool OperatorInfo::isValidDeclName(clang::IdentifierInfo *GoldDeclName) const {
  auto It = OpLookupTable.find(GoldDeclName);
  if (It == OpLookupTable.end())
    return false;
  return It->second->getGoldDeclName() == GoldDeclName;
}

void OperatorInfo::dump() const {
  llvm::outs() << "Number of registered operators! = " << OpLookupTable.size()
               << "\n";
  for (const auto & P : OpLookupTable) {
    llvm::outs() << P.first->getName() << "\n";
  }
}

} // Namespace gold