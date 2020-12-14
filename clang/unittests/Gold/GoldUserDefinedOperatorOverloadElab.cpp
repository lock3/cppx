//=== GoldUserDefinedOperatorOverloadElab.cpp ------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Test's for different types of casting implementations.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldUserDefinedOp, Member_BinOp_Plus) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"+"(other:ref const OpTest):ref OpTest!
    return ^this

)Gold";
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(hasAnyOverloadedOperatorName("+")))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Member_BinOp_ToManyArgs) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"+"(other:ref const OpTest, i:int):ref OpTest!
    return ^this

)Gold";
  GoldFailureTest(Code);
}

TEST(GoldUserDefinedOp, Member_BinOp_ToFewArgs) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"and"():ref OpTest!
    return ^this

)Gold";
  GoldFailureTest(Code);
}

TEST(GoldUserDefinedOp, MemberOnlyFunction_AsFree) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"="(other:const ref OpTest):int!
  return 4

)Gold";
  GoldFailureTest(Code);
}


TEST(GoldUserDefinedOp, InvalidOperatorName) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"something"():ref OpTest!
    return ^this

)Gold";
  GoldFailureTest(Code);
}
static std::string MemberFunctionFirstPart = R"Gold(OpTest : type = class:
  operator")Gold";
static std::string FreeFunctionFirstPart = R"Gold(OpTest : type = class:
  ;

operator")Gold";
std::string BinaryMemberSecondPart = R"Gold("(other:ref const OpTest):ref OpTest!
    return ^this

)Gold";


std::string BinaryFreeSecondPart = 
R"Gold("(rhs:const ref OpTest, lhs:ref const OpTest):int!
  return 3

)Gold";

std::string UnaryMemberSecondPart = R"Gold("():ref OpTest!
    return ^this

)Gold";

std::string UnaryFreeSecondPart = R"Gold("(i:ref OpTest):ref OpTest!
  return i
)Gold";
static void TestMemberOpFunc(const std::string &GoldOpStr,
                                                 const std::string &SecondPart,
                                                const std::string &ExpectedOp) {
  std::string Code = MemberFunctionFirstPart + GoldOpStr + SecondPart;
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(hasAnyOverloadedOperatorName(ExpectedOp)))
  );
  ASSERT_TRUE(matches(Code, opMatches))
      << "Failed to declare a valid operator overload"; 
}

static void TestFreeOpFunc(const std::string &GoldOpStr,
                                               const std::string &SecondPart,
                                               const std::string &ExpectedOp) {
  std::string Code = FreeFunctionFirstPart + GoldOpStr + SecondPart;
  DeclarationMatcher opMatches =
    functionDecl(hasAnyOverloadedOperatorName(ExpectedOp));
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

static void TestBinMemberFunc(const std::string &OpStr,
                                             const std::string &ExpectedOpStr) {
  return TestMemberOpFunc(OpStr, BinaryMemberSecondPart, ExpectedOpStr);
}

static void TestBinFreeFunc(const std::string &OpStr,
                                             const std::string &ExpectedOpStr) {
  return TestFreeOpFunc(OpStr, BinaryFreeSecondPart, ExpectedOpStr);
}

static void TestUnaryMemberFunc(const std::string &OpStr,
                                             const std::string &ExpectedOpStr) {
  return TestMemberOpFunc(OpStr, UnaryMemberSecondPart, ExpectedOpStr);
}

static void TestUnaryFreeFunc(const std::string &OpStr,
                                             const std::string &ExpectedOpStr) {
  return TestFreeOpFunc(OpStr, UnaryFreeSecondPart, ExpectedOpStr);
}

// This disables the the assigment operator
#define def_bin_op_member_only(Name,OpStr,ClangBinOpName,MemberOnly,        \
                               OverloadStyle,OpOverloadName)                \
TEST(GoldUserDefinedOp, Member_BinOp_##Name) {                              \
  TestBinMemberFunc(OpStr,OpStr);                                           \
}
// Make this do nothing
#define def_non_onverloadable_binary(Name, OpStr, ClangBinOpName, MemberOnly,\
                                    OverloadStyle, OpOverloadName)

#define def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,  \
                   OpOverloadName)                                          \
def_bin_op_member_only(Name, OpStr, ClangBinOpName, MemberOnly,             \
                       OverloadStyle, OpOverloadName)                       \
TEST(GoldUserDefinedOp, Free_BinOp_##Name) {                                \
  TestBinFreeFunc(OpStr, OpStr);                                            \
}

#define def_bin_op_second_name(Name, OpStr, SecondOpStr,ClangBinOpName,     \
                               MemberOnly, OverloadStyle, OpOverloadName)   \
  def_bin_op(Name, OpStr, ClangBinOpName,                                   \
             MemberOnly, OverloadStyle, OpOverloadName)                     \
TEST(GoldUserDefinedOp, Member_BinOp_##Name##_SecondName) {                 \
  TestBinMemberFunc(SecondOpStr, OpStr);                                    \
}                                                                           \
TEST(GoldUserDefinedOp, Free_BinOp_##Name##_SecondName) {                   \
  TestBinFreeFunc(SecondOpStr, OpStr);                                      \
}

#define def_bin_op_missmatch(Name, OpStr, ClangOpStr, ClangBinOpName,       \
                             MemberOnly, OverloadStyle, OpOverloadName)     \
TEST(GoldUserDefinedOp, Member_BinOp_##Name) {                              \
  TestBinMemberFunc(OpStr, ClangOpStr);                                     \
}                                                                           \
TEST(GoldUserDefinedOp, Free_BinOp_##Name) {                                \
  TestBinFreeFunc(OpStr, ClangOpStr);                                       \
}


#define def_unary_op(Name, OpStr, ClangOpName, MemberOnly, OverloadStyle,   \
                     OpOverloadName)                                        \
TEST(GoldUserDefinedOp, Member_Unary_##Name) {                              \
  TestUnaryMemberFunc(OpStr, OpStr);                                        \
}                                                                           \
TEST(GoldUserDefinedOp, Free_Unary_##Name) {                                \
  TestUnaryFreeFunc(OpStr, OpStr);                                          \
}

#define def_unary_and_binary_op(Name, OpStr, ClangUnaryOpName,              \
                                ClangBinOpName, MemberOnly,                 \
                                OverloadStyle, OpOverloadName)              \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,        \
             OpOverloadName)                                                \
  def_unary_op(Name, OpStr, ClangUnaryOpName, MemberOnly, OverloadStyle,    \
               OpOverloadName)

// This is ONLY to test one thing the dereference operator
#define def_unary_and_binary_op_mismatch(Name,OpStr,ClangStr,               \
                                         ClangUnaryOpName,                  \
                                         ClangBinOpName,MemberOnly,         \
                                         OverloadStyle,                     \
                                         UnaryOpOverloadName,               \
                                         BinaryOpOverloadName)              \
  def_bin_op(Name, OpStr, ClangBinOpName, MemberOnly, OverloadStyle,        \
             OpOverloadName)                                                \
TEST(GoldUserDefinedOp, Member_Unary_##Name) {                              \
  TestUnaryMemberFunc(OpStr, ClangStr);                                     \
}                                                                           \
TEST(GoldUserDefinedOp, Free_Unary_##Name) {                                \
  TestUnaryFreeFunc(OpStr, ClangStr);                                       \
}

#define def_unary_op_second_name(Name, OpStr, SecondName, ClangUnaryOpName, \
                                 MemberOnly, OverloadStyle, OpOverloadName) \
  def_unary_op(Name, OpStr, ClangUnaryOpName, MemberOnly, OverloadStyle,    \
               OpOverloadName)                                              \
TEST(GoldUserDefinedOp, Member_Unary_##Name##_SecondName) {                 \
  TestUnaryMemberFunc(SecondName, OpStr);                                   \
}                                                                           \
TEST(GoldUserDefinedOp, Free_Unary_##Name##_SecondName) {                   \
  TestUnaryFreeFunc(SecondName, OpStr);                                     \
}

// #define def_nary_op(Name, OpStr, MemberOnly, OverloadStyle, OpOverloadName) \
// TEST(GoldUserDefinedOp, Member_NAry_##Name) {                               \
//   TestBinMemberFunc(OpStr, OpStr);                                          \
// }


#define def_unary_cpp_only_op(Name, OpStr, MemberOnly, OverloadStyle,      \
                               OpOverloadName)                             \
TEST(GoldUserDefinedOp, Member_Unary_CPPOnly_##Name) {                     \
  TestUnaryMemberFunc(OpStr, OpStr);                                       \
}

#define def_bin_cpp_only_op(Name, OpStr, MemberOnly, OverloadStyle,       \
                            OpOverloadName)                               \
TEST(GoldUserDefinedOp, Member_Binary_CPPOnly_##Name) {                   \
  TestBinMemberFunc(OpStr, OpStr);                                        \
}


#include "clang/Gold/GoldOperatorInfo.def"

TEST(GoldUserDefinedOp, Member_CppOnly_PostInc) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"++"(i:int):ref OpTest!
    return ^this

)Gold";
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(hasAnyOverloadedOperatorName("++")))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Free_CppOnly_PostInc) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"++"(x:ref OpTest, i:int):ref OpTest!
  return x

)Gold";
  DeclarationMatcher opMatches =
    functionDecl(hasAnyOverloadedOperatorName("++"));
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}



TEST(GoldUserDefinedOp, Member_CppOnly_PostDec) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"--"(i:int):ref OpTest!
    return ^this

)Gold";
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(hasAnyOverloadedOperatorName("--")))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Free_CppOnly_PostDec) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"--"(x:ref OpTest, i:int):ref OpTest!
  return x

)Gold";
  DeclarationMatcher opMatches =
    functionDecl(hasAnyOverloadedOperatorName("--"));
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Free_CppOnly_PostDec_IncorrectSecondParam) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"--"(x:ref OpTest, i:float64):ref OpTest!
  return x

)Gold";
  GoldFailureTest(Code);
}


TEST(GoldUserDefinedOp, ExplicitMemberOperatorCall) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"--"(i:int):ref OpTest!
    return ^this
  

foo(X:ref OpTest):void!
  X.operator"--"(1)
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    has(recordDecl(
      hasName("OpTest"),
      has(cxxMethodDecl(hasAnyOverloadedOperatorName("--")))
    )),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(memberExpr(member(hasName("operator--"))))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, ExplicitMemberOperatorDeref) {
  std::string Code = R"Gold(
Other : type = class:
  ;

OpTest : type = class:
  X:^ Other = null
  operator"^"():ref Other!
    return ^X

foo(X:ref OpTest):void!
  X.operator"^"()
)Gold";
  auto opMatches = translationUnitDecl(
    has(recordDecl(
      hasName("OpTest"),
      has(cxxMethodDecl(hasAnyOverloadedOperatorName("*")))
    )),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(memberExpr(member(hasName("operator*"))))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, ExplicitMemberOperatorXOr) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"^"(RHS:ref OpTest):bool!
    return false


foo(X:ref OpTest, Y:ref OpTest):void!
  X.operator"^"(Y)
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    has(recordDecl(
      hasName("OpTest"),
      has(cxxMethodDecl(hasAnyOverloadedOperatorName("^")))
    )),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(memberExpr(member(hasName("operator^"))))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, ExplicitFreeOperatorCall) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"--"(x:ref OpTest, i:int):ref OpTest!
  return x

foo (X:ref OpTest) : void!
  operator"--"(X, 1)
)Gold";
  DeclarationMatcher opMatches = functionDecl(
      hasName("foo"),
      hasDescendant(
        callExpr(callee(functionDecl(hasName("operator--"))))
      )
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Assignment_Copy_DeclVerification) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"="(Other:ref OpTest):ref OpTest!
    return ^this
  
)Gold";
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(isCopyAssignmentOperator()))
  );
  ASSERT_TRUE(matches(Code, opMatches))
      << "Failed to declare a valid operator overload"; 
}

TEST(GoldUserDefinedOp, Assignment_Move_DeclVerification) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"="(Other:rref OpTest):ref OpTest!
    return ^this
  
)Gold";
  DeclarationMatcher opMatches = recordDecl(
    hasName("OpTest"),
    has(cxxMethodDecl(isMoveAssignmentOperator()))
  );
  ASSERT_TRUE(matches(Code, opMatches))
      << "Failed to declare a valid operator overload"; 
}
