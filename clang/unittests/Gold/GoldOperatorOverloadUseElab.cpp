//=== GoldOperatorOverloadUserElab.cpp - testing operator overloads --========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
// We are testing operator overload verification.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldUserDefinedOp, Use_PlusSign) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"+"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X + Y

MemberBinTy : type = class:
  operator"+"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X + X

UnaryOpTest1 : type = class:
  ;

operator"+"(X:ref UnaryOpTest1):UnaryOpTest1!
  return UnaryOpTest1()

FreeUnaryOp(X:ref UnaryOpTest1): void!
  Z : UnaryOpTest1 = +X

UnaryOpTest2 : type = class:
  operator"+"():UnaryOpTest2!
    return UnaryOpTest2()
  

MemberUnaryOp(X:ref UnaryOpTest2): void!
  Z : UnaryOpTest2 = +X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))),
    hasDescendant(
      functionDecl( hasName("FreeUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("+"))))),
    hasDescendant(
      functionDecl( hasName("MemberUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("+")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_MinusSign) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"-"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X - Y

MemberBinTy : type = class:
  operator"-"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X - X

UnaryOpTest1 : type = class:
  ;

operator"-"(X:ref UnaryOpTest1):UnaryOpTest1!
  return UnaryOpTest1()

FreeUnaryOp(X:ref UnaryOpTest1): void!
  Z : UnaryOpTest1 = -X

UnaryOpTest2 : type = class:
  operator"-"():UnaryOpTest2!
    return UnaryOpTest2()
  

MemberUnaryOp(X:ref UnaryOpTest2): void!
  Z : UnaryOpTest2 = -X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("-"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("-"))))),
    hasDescendant(
      functionDecl( hasName("FreeUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("-"))))),
    hasDescendant(
      functionDecl( hasName("MemberUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("-")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}


TEST(GoldUserDefinedOp, Use_Multiply) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"*"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X * Y

MemberBinTy : type = class:
  operator"*"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X * X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("*"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("*")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}


TEST(GoldUserDefinedOp, Use_Divide) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"/"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X / Y

MemberBinTy : type = class:
  operator"/"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X / X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("/"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("/")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_Modulus) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"%"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X % Y

MemberBinTy : type = class:
  operator"%"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X % X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("%"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("%")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_BitwiseOr) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"|"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X | Y

MemberBinTy : type = class:
  operator"|"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X | X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("|"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("|")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_BitwiseXOr) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"^"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X ^ Y

MemberBinTy : type = class:
  operator"^"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X ^ X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("^"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("^")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_BitwiseAnd) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"&"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X & Y

MemberBinTy : type = class:
  operator"&"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X & X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("&"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("&")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}


TEST(GoldUserDefinedOp, Use_LogicalOr) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"or"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X || Y

MemberBinTy : type = class:
  operator"||"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X or X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("||"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("||")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_LogicalAnd) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"and"(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :OpTest = X and Y

MemberBinTy : type = class:
  operator"&&"(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = X and X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("&&"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("&&")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_LogicalNot) {
  std::string Code = R"Gold(
UnaryOpTest1 : type = class:
  ;

operator"!"(X:ref UnaryOpTest1):UnaryOpTest1!
  return UnaryOpTest1()

FreeUnaryOp(X:ref UnaryOpTest1): void!
  Z : UnaryOpTest1 = not X

UnaryOpTest2 : type = class:
  operator"not"():UnaryOpTest2!
    return UnaryOpTest2()
  

MemberUnaryOp(X:ref UnaryOpTest2): void!
  Z : UnaryOpTest2 = !X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("FreeUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("!"))))),
    hasDescendant(
      functionDecl( hasName("MemberUnaryOp"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("!")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_Dereference) {
  std::string Code = R"Gold(
OpTest : type = class:
  ;

operator"^"(x:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest) : void!
  Z :OpTest = ^X 

MemberBinTy : type = class:
  operator"^"(): MemberBinTy!
    return MemberBinTy()
  

MemberOperator (X:ref MemberBinTy):void!
  Y : MemberBinTy = ^X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("*"))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("*")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}




static const std::string CmpPart_1 = R"Gold(
OpTest : type = class:
  ;
operator")Gold";

static const std::string CmpPart_2 = R"Gold("(x:ref OpTest, Y:ref OpTest):bool!
  return false

foo (X:ref OpTest, Y:ref OpTest) : void!
  Z :bool = X )Gold";
static const std::string CmpPart_3 = R"Gold( Y
MemberBinTy : type = class:
  operator")Gold";

static const std::string CmpPart_4 = R"Gold("(X:MemberBinTy): bool!
    return false
  

MemberOperator (X:ref MemberBinTy):void!
  Y = X )Gold";

static const std::string CmpPart_5 = R"Gold( X
)Gold";

void EvalCMPOperator(const std::string &CmpOpGoldName,
                     const std::string &ClangCmpName) {
  std::string Code = CmpPart_1 + CmpOpGoldName
                    + CmpPart_2 + CmpOpGoldName
                    + CmpPart_3 + CmpOpGoldName
                    + CmpPart_4 + CmpOpGoldName
                    + CmpPart_5;
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(
        hasOverloadedOperatorName(ClangCmpName))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(
            hasOverloadedOperatorName(ClangCmpName)))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}
void EvalCMPOperator(const std::string &Op) {
  EvalCMPOperator(Op, Op);
}
TEST(GoldUserDefinedOp, Use_Less) {
  EvalCMPOperator("<");
}
TEST(GoldUserDefinedOp, Use_Greater) {
  EvalCMPOperator(">");
}
TEST(GoldUserDefinedOp, Use_LessEqual) {
  EvalCMPOperator("<=");
}
TEST(GoldUserDefinedOp, Use_GreaterEqual) {
  EvalCMPOperator(">=");
}
TEST(GoldUserDefinedOp, Use_Equality) {
  EvalCMPOperator("==");
}
TEST(GoldUserDefinedOp, Use_Inequality) {
  EvalCMPOperator("<>", "!=");
}


static const std::string AssignPart_1 = R"Gold(
OpTest : type = class:
  ;
operator")Gold";

static const std::string AssignPart_2 =
R"Gold("(x:ref OpTest, Y:ref OpTest):OpTest!
  return OpTest()

foo (X:ref OpTest, Y:ref OpTest) : void!
  X )Gold";
static const std::string AssignPart_3 = R"Gold( Y
MemberBinTy : type = class:
  operator")Gold";

static const std::string AssignPart_4 = R"Gold("(X:MemberBinTy): MemberBinTy!
    return ^this
  

MemberOperator (X:ref MemberBinTy, Y:ref MemberBinTy):void!
  X )Gold";

static const std::string AssignPart_5 = R"Gold( Y
)Gold";
void EvalSpecialAssign(const std::string &CmpOpGoldName) {
  std::string Code = CmpPart_1 + CmpOpGoldName
                    + CmpPart_2 + CmpOpGoldName
                    + CmpPart_3 + CmpOpGoldName
                    + CmpPart_4 + CmpOpGoldName
                    + CmpPart_5;
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant( cxxOperatorCallExpr(
        hasOverloadedOperatorName(CmpOpGoldName))))),
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(
            hasOverloadedOperatorName(CmpOpGoldName)))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}
TEST(GoldUserDefinedOp, Use_AddAssign) {
  EvalSpecialAssign("+=");
}
TEST(GoldUserDefinedOp, Use_SubAssign) {
  EvalSpecialAssign("-=");
}
TEST(GoldUserDefinedOp, Use_MulAssign) {
  EvalSpecialAssign("*=");
}
TEST(GoldUserDefinedOp, Use_DivAssign) {
  EvalSpecialAssign("/=");
}
TEST(GoldUserDefinedOp, Use_RemAssign) {
  EvalSpecialAssign("%=");
}
TEST(GoldUserDefinedOp, Use_BitWiseXOrAssign) {
  EvalSpecialAssign("^=");
}
TEST(GoldUserDefinedOp, Use_BitWiseOrAssign) {
  EvalSpecialAssign("|=");
}
TEST(GoldUserDefinedOp, Use_BitWiseAndAssign) {
  EvalSpecialAssign("&=");
}

TEST(GoldUserDefinedOp, Use_Assignment) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"="(Other:OpTest):ref OpTest!
    return ^this
  

foo (Z:ref OpTest, X:ref OpTest) : void!
  Z = X 
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("="),
                                        isAssignmentOperator()))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}


TEST(GoldUserDefinedOp, Use_Subscript) {
  std::string Code = R"Gold(
OpTest : type = class:
  Arr:[3]int = array{ 1, 2, 3 }
  operator"[]"(Index:int):ref int!
    return Arr[Index]
  

foo (X:ref OpTest) : void!
  x:ref int = X[1]
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("[]")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_SubscriptAsLhsOfExpr) {
  std::string Code = R"Gold(
OpTest : type = class:
  Arr:[3]int = array{ 1, 2, 3 }
  operator"[]"(Index:int):ref int!
    return Arr[Index]
  

foo (X:ref OpTest) : void!
  X[1] = 5
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("[]")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}


TEST(GoldUserDefinedOp, Use_SubscriptStandingAlone) {
  std::string Code = R"Gold(
OpTest : type = class:
  Arr:[3]int = array{ 1, 2, 3 }
  operator"[]"(Index:int):ref int!
    return Arr[Index]
  

foo (X:ref OpTest) : void!
  X[1]
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("[]")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}



TEST(GoldUserDefinedOp, Use_Call) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"()"(Index:int):int!
    return 3
  

foo (X:ref OpTest) : void!
  Var:int = X(2)
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("()")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

// TODO: We need to clarify this to see if it's an expression or a declaration.
// TEST(GoldUserDefinedOp, Use_CallAsLhsOfExpr) {
//   std::string Code = R"Gold(
// OpTest : type = class:
//   operator"()"(Index:int):int!
//     return 4
  

// foo (X:ref OpTest) : void!
//   X(1) = 5
// )Gold";
//   DeclarationMatcher opMatches = translationUnitDecl(
//     hasDescendant(
//       functionDecl(hasName("foo"),
//       hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("()")))))
//   );
//   ASSERT_TRUE(matches(Code, opMatches))
//               << "Failed to declare a valid operator overload";
// }

TEST(GoldUserDefinedOp, Use_CallOperatorStandAlone) {
  std::string Code = R"Gold(
OpTest : type = class:
  operator"()"(V:int):int!
    return 5
  

foo (X:ref OpTest) : void!
  X(3)
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("foo"),
      hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("()")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

// TEST(GoldUserDefinedOp, Use_BitWiseLeftShift) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

// TEST(GoldUserDefinedOp, Use_BitWiseRightShfit) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

// TEST(GoldUserDefinedOp, Use_BitWiseLeftShiftAssign) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

// TEST(GoldUserDefinedOp, Use_BitWiseRightShfitAssign) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

// TEST(GoldUserDefinedOp, Use_BitWiseNot) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

// TEST(GoldUserDefinedOp, Use_AddressOfOperator) {
//   ASSERT_FALSE(true) << "We need to decide on this operator within gold\n";
// }

TEST(GoldUserDefinedOp, Use_OperatorDeclaredInNamespace_UsedOutsideOfIt) {
  std::string Code = R"Gold(
ns : namespace = namespace:
  OpTest : type = class:
    ;
  
  operator"^"(x:ref OpTest):OpTest!
    return OpTest()
  

MemberOperator (X:ref ns.OpTest):void!
  Y : ns.OpTest = ^X
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("MemberOperator"),
        hasDescendant( cxxOperatorCallExpr(hasOverloadedOperatorName("*")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}

TEST(GoldUserDefinedOp, Use_OutOfOrderFreeFunc) {
  std::string Code = R"Gold(
FreeUnaryOp(X:ref UnaryOpTest1): void!
  Z : UnaryOpTest1 = not X

UnaryOpTest1 : type = class:
  ;

operator"!"(X:ref UnaryOpTest1):UnaryOpTest1!
  return UnaryOpTest1()



)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    hasDescendant(
      functionDecl( hasName("FreeUnaryOp"),
        hasDescendant(cxxOperatorCallExpr(hasOverloadedOperatorName("!")))))
  );
  ASSERT_TRUE(matches(Code, opMatches))
              << "Failed to declare a valid operator overload";
}