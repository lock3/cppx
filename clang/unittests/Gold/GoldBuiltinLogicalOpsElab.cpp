//=== GoldLogicalOpsElab.cpp =====-----------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for logical operators.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

static void doLogicalOperatorTest(const std::string &TypeName,
                        const std::string &ExpectedTypeName,
                        const std::string &Constant1,
                        const std::string &Constant2,
                        clang::CastKind CastKind) {
  using namespace std::string_literals;
  std::string Code = "foo():void!\n"s +
"  x:" + TypeName + " = " + Constant1 + "\n"
"  y:" + TypeName + " = " + Constant2 + "\n"
"  a:bool = x and y\n"
"  b:bool = x and y\n"
"  c:bool = x or y\n"
"  d:bool = x || y\n"
"  e:bool = not x\n"
"  f:bool = !x\n"
"";
  DeclarationMatcher opMatches = translationUnitDecl(
    
    hasDescendant(varDecl(
      hasName("a"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind(CK_LValueToRValue)
            )
          )
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("b"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    )),

    hasDescendant(varDecl(
      hasName("c"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("d"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("e"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("f"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CastKind),
          hasSourceExpression(
            implicitCastExpr(
              hasImplicitDestinationType(asString(ExpectedTypeName)),
              hasCastKind( CK_LValueToRValue)
            )
          )
        ))
      )))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an logical operators.";
}

// Signed integers
TEST(LogicalOp, BuiltinType_int) {
  doLogicalOperatorTest("int", "int", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_int8) {
  doLogicalOperatorTest("int8", "signed char", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_int16) {
  doLogicalOperatorTest("int16", "short", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_int32) {
  doLogicalOperatorTest("int32", "int", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_int64) {
  doLogicalOperatorTest("int64", "long", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_int128) {
  doLogicalOperatorTest("int128", "__int128", "5", "6", CK_IntegralToBoolean);
}

TEST(LogicalOp, BuiltinType_uint) {
  doLogicalOperatorTest("uint", "unsigned int", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_uint8) {
  doLogicalOperatorTest("uint8", "unsigned char", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_uint16) {
  doLogicalOperatorTest("uint16", "unsigned short", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_uint32) {
  doLogicalOperatorTest("uint32", "unsigned int", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_uint64) {
  doLogicalOperatorTest("uint64", "unsigned long", "5", "6", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_uint128) {
  doLogicalOperatorTest("uint128", "unsigned __int128", "5", "6", CK_IntegralToBoolean);
}

TEST(LogicalOp, BuiltinType_char) {
  doLogicalOperatorTest("char", "char", "'5'", "'6'", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_char8) {
  doLogicalOperatorTest("char8", "signed char", "'5'", "'6'", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_char16) {
  doLogicalOperatorTest("char16", "short", "'5'", "'6'", CK_IntegralToBoolean);
}
TEST(LogicalOp, BuiltinType_char32) {
  doLogicalOperatorTest("char32", "int", "'5'", "'6'", CK_IntegralToBoolean);
}

TEST(LogicalOp, BuiltinType_float16) {
  std::string Code = R"Gold(foo():void!
  x:float16 = 2.0
  y:float16 = 1.0
  a:bool = x and y
  b:bool = x and y
  c:bool = x or y
  d:bool = x || y
  e:bool = not x
  f:bool = !x
)Gold";
  DeclarationMatcher opMatches = translationUnitDecl(
    
    hasDescendant(varDecl(
      hasName("a"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("b"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    )),

    hasDescendant(varDecl(
      hasName("c"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("d"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        )),
        hasRHS(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("e"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("f"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(implicitCastExpr(
          hasImplicitDestinationType(asString("_Bool")),
          hasCastKind(CK_FloatingToBoolean)
        ))
      )))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an logical operators.";
}
TEST(LogicalOp, BuiltinType_float32) {
  doLogicalOperatorTest("float32", "float", "5", "6",  CK_FloatingToBoolean);
}
TEST(LogicalOp, BuiltinType_float64) {
  doLogicalOperatorTest("float64", "double", "5", "6",  CK_FloatingToBoolean);
}

#if 0
TEST(LogicalOp, BuiltinType_float128) {
  doLogicalOperatorTest("float128", "__float128", "5", "6",  CK_FloatingToBoolean);
}
#endif

TEST(LogicalOp, BuiltinType_bool) {
  std::string Code = "foo():void!\n"
"  x:bool = true\n"
"  y:bool = true\n"
"  a:bool = x and y\n"
"  b:bool = x and y\n"
"  c:bool = x or y\n"
"  d:bool = x || y\n"
"  e:bool = not x\n"
"  f:bool = !x\n"
"";
  DeclarationMatcher opMatches = translationUnitDecl(
    
    hasDescendant(varDecl(
      hasName("a"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind(CK_LValueToRValue)
        )),
        hasRHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("b"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(binaryOperator(
        hasOperatorName("&&"),
        hasType(asString("_Bool")),
        hasLHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        )),
        hasRHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    )),

    hasDescendant(varDecl(
      hasName("c"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        )),
        hasRHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("d"),
      hasType(asString("_Bool")),
      hasInitializer(	findAll(binaryOperator(
        hasOperatorName("||"),
        hasType(asString("_Bool")),
        hasLHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        )),
        hasRHS(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("e"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    )),
    hasDescendant(varDecl(
      hasName("f"),
      hasType(asString("_Bool")),
      hasInitializer(findAll(unaryOperator(
        hasOperatorName("!"),
        hasType(asString("_Bool")),
        hasUnaryOperand(
          implicitCastExpr(
            hasImplicitDestinationType(asString("_Bool")),
            hasCastKind( CK_LValueToRValue)
        ))
      )))
    ))
  );
  ASSERT_TRUE(matches(Code, opMatches))
    << "Failed to locate an logical operators.";
}
