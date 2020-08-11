//===- GoldOperatorInfo.h - Operator information --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Operator overload information stroage and building information.
//  This file is largely responsible for providing a type to contain all
//  necessary and overertly annoying operator overloadings.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_OPERATOR_INFO_H
#define CLANG_GOLD_OPERATOR_INFO_H

#include "clang/AST/ASTContext.h"

#include "clang/Gold/GoldSyntaxContext.h"
#include "clang/Gold/GoldSyntaxContext.h"

namespace gold {
struct LastMember { };
/// OperatorNameConstants
/// This contains all possible names for operators
/// there are three for some reason.
///   1. From the clang AST.
///   2. From a declaration name by the user.
///   3. From our AST when an operator is actually used.
///
/// These are all infact different
///   1. "operator" + actual-operator
///       ex. operator+
///       ex. operator()
///
///   2. "operator\"" + actual-operator +"\""
///       ex. operator"+"
///       ex. operator"()"
///
///   3. "operator'" + actual-operator + "'"
///       ex. operator'+'
///       ex. operator'()'
///
struct OperatorNameConstants {
  OperatorNameConstants(clang::ASTContext &Context);


#define def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, \
                   OverloadStyle, OpOverloadName)           \
  clang::IdentifierInfo * const CppOp_##Name;               \
  clang::IdentifierInfo * const GoldDecl_##Name;            \
  clang::IdentifierInfo * const GoldUse_##Name;

#define def_bin_op_second_name(Name, OpStr, SecondOpStr, ClangBinOpName,  \
                               MemberOnly, OverloadStyle, OpOverloadName) \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,      \
             OpOverloadName)                                              \
  clang::IdentifierInfo * const GoldDecl_##Name##Text;                    \
  clang::IdentifierInfo * const GoldUse_##Name##Text;

#define def_bin_op_missmatch(Name, OpStr, ClangOpStr, ClangBinOpName,   \
                             MemberOnly, OverloadStyle, OpOverloadName) \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,    \
             OpOverloadName)

#define def_unary_and_binary_op(Name, OpStr, ClangUnaryOpName,              \
                                ClangBinOpName, MemberOnly, OverloadStyle,  \
                                OpOverloadName)                             \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,        \
             OpOverloadName)

#define def_unary_and_binary_op_mismatch(Name, OpStr, ClangStr,       \
                                        ClangUnaryOpName,             \
                                        ClangBinOpName,               \
                                        MemberOnly, OverloadStyle,    \
                                        UnaryOverloadName,            \
                                        BinaryOverloadName)           \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,  \
             UnaryOverloadName)

#define def_unary_op_second_name(Name, OpStr, SecondName,             \
                                ClangUnaryOpName, MemberOnly,         \
                                OverloadStyle, OpOverloadName)        \
  def_bin_op_second_name(Name, OpStr, SecondName, ClangUnaryOpName,   \
                         MemberOnly, OverloadStyle, OpOverloadName)

#define def_unary_cpp_only_op(Name, OpStr, MemberOnly, OverloadStyle,  \
                             OpOverloadName)                            \
  clang::IdentifierInfo * const CppOp_##Name;                           \
  clang::IdentifierInfo * const GoldDecl_##Name;

#define def_nary_op(Name, OpStr, MemberOnly, OverloadStyle, OpOverloadName) \
  clang::IdentifierInfo * const CppOp_##Name;                               \
  clang::IdentifierInfo * const GoldDecl_##Name;

#include "clang/Gold/GoldOperatorInfo.def"
  LastMember Last;
};

enum OpInfoKind {
  OI_UnaryOp  = 0b0001,
  OI_BinOp    = 0b0010,
  OI_NAryOp   = 0b0100,
  OI_NotPartOfGoldLanguage = 0b1000
};

enum AllowedOverloadUse {
  AOU_NotOverloadable,
  AOU_CppOnly,
  AOU_GoldAndCpp
};
class OpInfoBase;
using OperatorLookup = llvm::DenseMap<clang::IdentifierInfo *,
                                    const OpInfoBase *>;

class OpInfoBase {
  int Kinds;
  clang::IdentifierInfo *ClangName;
  clang::IdentifierInfo *GoldDeclName;
  bool IsMemberOnlyOp;
  AllowedOverloadUse Overloadability;
  clang::OverloadedOperatorKind OOKind;
public:
  OpInfoBase(int OpKinds, clang::IdentifierInfo *ClangOpName,
             clang::IdentifierInfo *GoldOpDeclName, bool IsMemberOnly,
             AllowedOverloadUse AllowedOverloadStyle,
             clang::OverloadedOperatorKind OpKind);
  virtual ~OpInfoBase() = default;

  bool isUnary() const;
  bool isBinary() const;
  bool isUnaryAndBinary() const;
  bool isNAry() const;
  bool isNotPartOfGold() const;
  bool isMemberOnly() const;
  int getKind() const { return Kinds; }
  AllowedOverloadUse getAllowedOverloadUse() const;

  virtual clang::OverloadedOperatorKind getUnaryOverloadKind() const;
  virtual clang::OverloadedOperatorKind getBinaryOverloadKind() const;

  clang::IdentifierInfo *getGoldDeclName() const;
  clang::IdentifierInfo *getClangName() const;
  virtual bool HasGoldUseName() const;
  virtual clang::IdentifierInfo *getGoldUseName() const;
  virtual bool HasSecondName() const;
  virtual clang::IdentifierInfo *getGoldDeclSecondName() const;
  virtual clang::IdentifierInfo *getGoldUseSecondName() const;

  virtual clang::UnaryOperatorKind getUnaryOperatorKind() const;
  virtual clang::BinaryOperatorKind getBinaryOperatorKind() const;
  void registerLookup(OperatorLookup &Lookup) const;
};

class UnaryOpInfo : public OpInfoBase {
  clang::IdentifierInfo *GoldUseName;
  clang::UnaryOperatorKind UnaryOpKind;
public:
  UnaryOpInfo(clang::IdentifierInfo *CName, clang::IdentifierInfo *GDName,
              clang::IdentifierInfo *GUseName, clang::UnaryOperatorKind Kind,
              bool MemberOnly, AllowedOverloadUse AllowedOverloadStyle,
              clang::OverloadedOperatorKind OpKind)
    :OpInfoBase(OI_UnaryOp, CName, GDName, MemberOnly, AllowedOverloadStyle,
                OpKind),
    GoldUseName(GUseName),
    UnaryOpKind(Kind)
  { }

  virtual bool HasGoldUseName() const;
  virtual clang::IdentifierInfo *getGoldUseName() const;
  virtual clang::UnaryOperatorKind getUnaryOperatorKind() const;
};

class UnaryOpWith2ndName : public UnaryOpInfo {
  clang::IdentifierInfo *GoldDecl2ndName;
  clang::IdentifierInfo *GoldUse2ndName;
public:
  UnaryOpWith2ndName(clang::IdentifierInfo *CName,
                     clang::IdentifierInfo *GDName,
                     clang::IdentifierInfo *GUseName,
                     clang::IdentifierInfo *GDName2,
                     clang::IdentifierInfo *GUseName2,
                     clang::UnaryOperatorKind Kind,
                     bool MemberOnly,
                     AllowedOverloadUse AllowedOverloadStyle,
                     clang::OverloadedOperatorKind OpKind)
    :UnaryOpInfo(CName, GDName, GUseName, Kind, MemberOnly,
                AllowedOverloadStyle, OpKind),
    GoldDecl2ndName(GUseName2),
    GoldUse2ndName(GDName2)
  { }
  virtual bool HasSecondName() const;
  virtual clang::IdentifierInfo *getGoldDeclSecondName() const;
  virtual clang::IdentifierInfo *getGoldUseSecondName() const;
};

class BinOpInfo : public OpInfoBase {
  clang::IdentifierInfo *GoldUseName;
  clang::BinaryOperatorKind BinOpKind;
public:
  BinOpInfo(clang::IdentifierInfo *CName, clang::IdentifierInfo *GDName,
            clang::IdentifierInfo *GUseName, clang::BinaryOperatorKind Kind,
            bool isMemberOnly, AllowedOverloadUse AllowedOverloadStyle,
            clang::OverloadedOperatorKind OpKind)
    :OpInfoBase(OI_BinOp, CName, GDName, isMemberOnly, AllowedOverloadStyle,
                OpKind),
    GoldUseName(GUseName),
    BinOpKind(Kind)
  { }

  virtual bool HasGoldUseName() const;
  virtual clang::IdentifierInfo *getGoldUseName() const;
  virtual clang::BinaryOperatorKind getBinaryOperatorKind() const;
};

class BinOpInfoWith2ndName : public BinOpInfo {
  clang::IdentifierInfo *GoldDecl2ndName;
  clang::IdentifierInfo *GoldUse2ndName;
public:
  BinOpInfoWith2ndName(clang::IdentifierInfo *CName, clang::IdentifierInfo *GDName,
            clang::IdentifierInfo *GUseName, clang::IdentifierInfo *GName2,
            clang::IdentifierInfo *GUseName2, clang::BinaryOperatorKind Kind,
            bool MemberOnly, AllowedOverloadUse AllowedOverloadStyle,
            clang::OverloadedOperatorKind OpKind)
    :BinOpInfo(CName, GDName, GUseName, Kind, MemberOnly, AllowedOverloadStyle,
               OpKind),
    GoldDecl2ndName(GName2),
    GoldUse2ndName(GUseName2)
  { }

  virtual bool HasSecondName() const;
  virtual clang::IdentifierInfo *getGoldDeclSecondName() const;
  virtual clang::IdentifierInfo *getGoldUseSecondName() const;
};

class NAryOpInfo :public OpInfoBase {
public:
  NAryOpInfo(clang::IdentifierInfo *CName, clang::IdentifierInfo *GName,
             bool IsMemberOnly, AllowedOverloadUse AllowedOverloadStyle,
             clang::OverloadedOperatorKind OpKind)
    :OpInfoBase(OI_NAryOp, CName, GName, IsMemberOnly, AllowedOverloadStyle,
                OpKind)
  { }
};

class OtherOpInfo :public OpInfoBase {
public:
  OtherOpInfo(clang::IdentifierInfo *CName, clang::IdentifierInfo *GName,
              bool IsMemberOnly, AllowedOverloadUse AllowedOverloadStyle,
              clang::OverloadedOperatorKind OpKind)
    :OpInfoBase(OI_NotPartOfGoldLanguage, CName, GName, IsMemberOnly,
                AllowedOverloadStyle, OpKind)
  { }
};

class UnaryAndBinOpInfo : public OpInfoBase {
  clang::IdentifierInfo *GoldUseName;
  clang::UnaryOperatorKind UnaryOpKind;
  clang::BinaryOperatorKind BinOpKind;
public:
  UnaryAndBinOpInfo(clang::IdentifierInfo *CName,
                    clang::IdentifierInfo *GDName,
                    clang::IdentifierInfo *GUseName,
                    clang::UnaryOperatorKind UnaryKind,
                    clang::BinaryOperatorKind BinaryKind,
                    bool isMemberOnly, AllowedOverloadUse AllowedOverloadStyle,
                    clang::OverloadedOperatorKind OpKind)
    :OpInfoBase(OI_UnaryOp | OI_BinOp, CName, GDName, isMemberOnly,
                AllowedOverloadStyle, OpKind),
    GoldUseName(GUseName),
    UnaryOpKind(UnaryKind),
    BinOpKind(BinaryKind)
  { }

  virtual bool HasGoldUseName() const;
  virtual clang::IdentifierInfo *getGoldUseName() const;
  virtual clang::UnaryOperatorKind getUnaryOperatorKind() const;
  virtual clang::BinaryOperatorKind getBinaryOperatorKind() const;
};

class DerefAndXOrComboOp : public OpInfoBase {
  clang::IdentifierInfo *GoldUseName;
  clang::UnaryOperatorKind UnaryOpKind;
  clang::BinaryOperatorKind BinOpKind;
  clang::OverloadedOperatorKind BinOOKind;
public:
  DerefAndXOrComboOp(clang::IdentifierInfo *CName,
                     clang::IdentifierInfo *GDName,
                     clang::IdentifierInfo *GUseName,
                     clang::UnaryOperatorKind UnaryKind,
                     clang::BinaryOperatorKind BinaryKind,
                     bool isMemberOnly, AllowedOverloadUse AllowedOverloadStyle,
                     clang::OverloadedOperatorKind UnaryOOKind,
                     clang::OverloadedOperatorKind BinOOKind)
    :OpInfoBase(OI_UnaryOp | OI_BinOp, CName, GDName, isMemberOnly,
                AllowedOverloadStyle, UnaryOOKind),
    GoldUseName(GUseName),
    UnaryOpKind(UnaryKind),
    BinOpKind(BinaryKind),
    BinOOKind(BinOOKind)
  { }

  virtual bool HasGoldUseName() const;
  virtual clang::IdentifierInfo *getGoldUseName() const;
  virtual clang::UnaryOperatorKind getUnaryOperatorKind() const;
  virtual clang::BinaryOperatorKind getBinaryOperatorKind() const;
  virtual clang::OverloadedOperatorKind getBinaryOverloadKind() const;
};

///
struct OperatorDataConstants {
  OperatorDataConstants(const OperatorNameConstants & OpNames);

#define def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, \
                   OverloadStyle, OpOverloadName)           \
  BinOpInfo const Name##Info;

#define def_bin_op_second_name(Name, OpStr, SecondOpStr, ClangBinOpName,  \
                               MemberOnly, OverloadStyle, OpOverloadName) \
  BinOpInfoWith2ndName const Name##Info;

#define def_bin_op_missmatch(Name, OpStr, ClangOpStr, ClangBinOpName,   \
                             MemberOnly, OverloadStyle, OpOverloadName) \
  BinOpInfo const Name##Info;

#define def_unary_and_binary_op(Name, OpStr, ClangUnaryOpName,    \
                                ClangBinOpName, MemberOnly,       \
                                OverloadStyle, OpOverloadName)    \
  UnaryAndBinOpInfo const Name##Info;

#define def_unary_and_binary_op_mismatch(Name, OpStr, ClangStr,           \
                                        ClangUnaryOpName, ClangBinOpName, \
                                        MemberOnly, OverloadStyle,        \
                                        UnaryOpOverloadName,              \
                                        BinaryOpOverloadName)             \
  DerefAndXOrComboOp const Name##Info;

#define def_unary_op_second_name(Name, OpStr, SecondName, ClangUnaryOpName, \
                                 MemberOnly, OverloadStyle, OpOverloadName) \
  UnaryOpWith2ndName const Name##Info;

#define def_unary_cpp_only_op(Name, OpStr, MemberOnly, OverloadStyle,  \
                             OpOverloadName)                          \
  OtherOpInfo const Name##Info;

#define def_nary_op(Name, OpStr, MemberOnly, OverloadStyle, OpOverloadName) \
  NAryOpInfo const Name##Info;

#include "clang/Gold/GoldOperatorInfo.def"
private:
  LastMember Last;
};

/// OperatorInfo
/// This class contains all of the information about individual operators.
class OperatorInfo
  : public OperatorNameConstants {
  clang::ASTContext &CxxAST;
  OperatorDataConstants Info;
  const OperatorLookup OpLookupTable;
public:
  OperatorInfo(clang::ASTContext &Context);

  /// Accessor methods.
  const OperatorDataConstants& getOpInfo() const { return Info; }
  const OperatorLookup& getOpLookup() const { return OpLookupTable; }


  /// getBinaryOperatorKind
  /// Attempts to search for and return the binary operator associated with
  /// a given operator name.
  /// @returns false if the operator was found and true if it wasn't.
  bool getBinaryOperatorUseKind(llvm::StringRef OpName,
                                clang::BinaryOperatorKind &Kind) const;
  bool getBinaryOperatorUseKind(clang::IdentifierInfo *Id,
                                clang::BinaryOperatorKind &Kind) const;

  bool isBinaryOperator(llvm::StringRef Name) const;
  bool isBinaryOperator(clang::IdentifierInfo *Id) const;

  /// getUnaryOperatorUseKind
  /// @returns false if the operator was found and true if it wasn't.
  bool getUnaryOperatorUseKind(llvm::StringRef OpName,
                               clang::UnaryOperatorKind &Kind) const;
  bool getUnaryOperatorUseKind(clang::IdentifierInfo *Id,
                               clang::UnaryOperatorKind &Kind) const;

  bool isUnaryOperator(llvm::StringRef Name) const;
  bool isUnaryOperator(clang::IdentifierInfo *Id) const;

  const OpInfoBase *getOpInfo(llvm::StringRef Name) const;
  const OpInfoBase *getOpInfo(clang::IdentifierInfo *Id) const;

  clang::IdentifierInfo *getClangName(llvm::StringRef GoldDeclOrUseName) const;
  clang::IdentifierInfo *getClangName(
                                clang::IdentifierInfo *GoldDeclOrUseName) const;
  bool isValidDeclName(llvm::StringRef GodlDeclName) const;
  bool isValidDeclName(clang::IdentifierInfo *GodlDeclName) const;

  void dump() const;
};

} // namespace gold.

#endif
