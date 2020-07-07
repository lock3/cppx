//=== GoldCastElab.cpp -----------------------------------------------------==//
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
/*
TranslationUnitDecl 0x7fffda2b8f08 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7fffda2b9838 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7fffda2b94a0 '__int128'
|-TypedefDecl 0x7fffda2b98a8 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7fffda2b94c0 'unsigned __int128'
|-TypedefDecl 0x7fffda2b9c28 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7fffda2b99a0 '__NSConstantString_tag'
|   `-CXXRecord 0x7fffda2b9900 '__NSConstantString_tag'
|-TypedefDecl 0x7fffda2b9cc0 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7fffda2b9c80 'char *'
|   `-BuiltinType 0x7fffda2b8fa0 'char'
|-TypedefDecl 0x7fffda2f8e08 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7fffda2f8db0 '__va_list_tag [1]' 1 
|   `-RecordType 0x7fffda2b9db0 '__va_list_tag'
|     `-CXXRecord 0x7fffda2b9d18 '__va_list_tag'
`-FunctionDecl 0x7fffda2f8eb8 <cpp_test.cpp:1:1, line:8:1> line:1:5 main 'int ()'
  `-CompoundStmt 0x7fffda2f9550 <col:12, line:8:1>
    |-DeclStmt 0x7fffda2f9088 <line:2:3, col:15>
    | `-VarDecl 0x7fffda2f8fe8 <col:3, col:14> col:9 used F1 'float' cinit
    |   `-ImplicitCastExpr 0x7fffda2f9070 <col:14> 'float' <IntegralToFloating>
    |     `-IntegerLiteral 0x7fffda2f9050 <col:14> 'int' 0
    |-DeclStmt 0x7fffda2f9158 <line:3:3, col:28>
    | `-VarDecl 0x7fffda2f90b8 <col:3, col:25> col:15 used ConstF4 'const float' cinit
    |   `-ImplicitCastExpr 0x7fffda2f9140 <col:25> 'const float' <FloatingCast>
    |     `-FloatingLiteral 0x7fffda2f9120 <col:25> 'double' 1.000000e+01
    |-DeclStmt 0x7fffda2f9288 <line:4:3, col:46>
    | `-VarDecl 0x7fffda2f9188 <col:3, col:45> col:7 StaticCastResult 'int' cinit
    |   `-CXXStaticCastExpr 0x7fffda2f9258 <col:26, col:45> 'int' static_cast<int> <NoOp>
    |     `-ImplicitCastExpr 0x7fffda2f9240 <col:43> 'int' <FloatingToIntegral> part_of_explicit_cast
    |       `-ImplicitCastExpr 0x7fffda2f9228 <col:43> 'float' <LValueToRValue> part_of_explicit_cast
    |         `-DeclRefExpr 0x7fffda2f91f0 <col:43> 'float' lvalue Var 0x7fffda2f8fe8 'F1' 'float'
    |-DeclStmt 0x7fffda2f93f8 <line:5:3, col:59>
    | `-VarDecl 0x7fffda2f92e0 <col:3, col:58> col:8 ReinterpretCastResult 'int *' cinit
    |   `-CXXReinterpretCastExpr 0x7fffda2f93c8 <col:32, col:58> 'int *' reinterpret_cast<int *> <BitCast>
    |     `-UnaryOperator 0x7fffda2f9398 <col:55, col:56> 'float *' prefix '&' cannot overflow
    |       `-DeclRefExpr 0x7fffda2f9348 <col:56> 'float' lvalue Var 0x7fffda2f8fe8 'F1' 'float'
    `-DeclStmt 0x7fffda2f9538 <line:7:3, col:56>
      `-VarDecl 0x7fffda2f9428 <col:3, col:55> col:10 ConstCastResult 'float *' cinit
        `-CXXConstCastExpr 0x7fffda2f9508 <col:28, col:55> 'float *' const_cast<float *> <NoOp>
          `-UnaryOperator 0x7fffda2f94d8 <col:47, col:48> 'const float *' prefix '&' cannot overflow
            `-DeclRefExpr 0x7fffda2f9490 <col:48> 'const float' lvalue Var 0x7fffda2f90b8 'ConstF4' 'const float'
*/
TEST(GoldCastElab, StaticCast) {
  std::string Code = R"Gold(foo():void!
  y:float16 = 1.0
  x:int = static_cast[int](y)
)Gold";
  DeclarationMatcher opMatches = hasDescendant(cxxStaticCastExpr());
  ASSERT_TRUE(matches(Code, opMatches))
    << "Static cast failed";
}

TEST(GoldCastElab, StaticCast_ToManyArgs) {
  std::string Code = R"Gold(foo():void!
  y:float16 = 1.0
  z:int
  x:int = static_cast[int](x, z)
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldCastElab, StaticCast_ToManyTypeArgs) {
  std::string Code = R"Gold(foo():void!
  y:float16 = 1.0
  z:int
  x:int = static_cast[int, float](x)
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldCastElab, ReinterpretCast) {
  std::string Code = R"Gold(foo():void!
  y:^float32
  x:^int = reinterpret_cast[^int](y)
)Gold";
  DeclarationMatcher opMatches = hasDescendant(cxxReinterpretCastExpr());
  ASSERT_TRUE(matches(Code, opMatches))
    << "Reinterpert cast failed";
}

TEST(GoldCastElab, ConstCast) {
  std::string Code = R"Gold(foo(i:^ const int):void!
  x:^int = const_cast[^int](i)
)Gold";
  DeclarationMatcher opMatches = hasDescendant(cxxConstCastExpr());
  ASSERT_TRUE(matches(Code, opMatches))
    << "Const cast failed";
}

TEST(GoldCastElab, DynamicCast) {
  std::string Code = R"Gold(
Base : type = class:
  destructor()<virtual>:void!
    ;
  i:int

Derived1 : type = class(Base):
  x:int

Derived2 : type = class(Base):
  y:int


foo(y:^Base):void!
  x:^Derived2 = dynamic_cast[^Derived2](y)
)Gold";
  DeclarationMatcher opMatches = hasDescendant(cxxDynamicCastExpr());
  ASSERT_TRUE(matches(Code, opMatches))
    << "dynamic cast failed";
}