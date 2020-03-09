//=== GoldParsingTest.cpp - Elaboration for Gold Nodes ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "ParseUtil.h"
#include "ASTMatchersTest.h"
// #include "clang/Frontend/ASTUnit.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassParsing, ClassDeclaration) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool

main() : int!
  return 0
  )";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


TEST(ClassParsing, ClassInstance) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
main() : int!
  q : c
  return 0
  )";

  /*
  TranslationUnitDecl 0x7ffff4900088 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7ffff49009b8 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7ffff4900620 '__int128'
|-TypedefDecl 0x7ffff4900a28 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7ffff4900640 'unsigned __int128'
|-TypedefDecl 0x7ffff4900da8 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7ffff4900b20 '__NSConstantString_tag'
|   `-CXXRecord 0x7ffff4900a80 '__NSConstantString_tag'
|-TypedefDecl 0x7ffff4900e40 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7ffff4900e00 'char *'
|   `-BuiltinType 0x7ffff4900120 'char'
|-TypedefDecl 0x7ffff493e518 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7ffff493e4c0 '__va_list_tag [1]' 1 
|   `-RecordType 0x7ffff4900f30 '__va_list_tag'
|     `-CXXRecord 0x7ffff4900e98 '__va_list_tag'
|-CXXRecordDecl 0x7ffff493e570 <bin/cpp_test.cpp:1:1, line:4:1> line:1:8 referenced struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7ffff493e698 <col:1, col:8> col:8 implicit struct c
| |-FieldDecl 0x7ffff493e748 <line:2:3, col:7> col:7 referenced a 'int'
| |-FieldDecl 0x7ffff493e7a8 <line:3:3, col:8> col:8 b 'bool'
| |-CXXConstructorDecl 0x7ffff493ea10 <line:1:8> col:8 implicit used c 'void () noexcept' inline default trivial
| | `-CompoundStmt 0x7ffff493eed8 <col:8>
| |-CXXConstructorDecl 0x7ffff493eb58 <col:8> col:8 implicit constexpr c 'void (const c &)' inline default trivial noexcept-unevaluated 0x7ffff493eb58
| | `-ParmVarDecl 0x7ffff493ec78 <col:8> col:8 'const c &'
| `-CXXConstructorDecl 0x7ffff493ed18 <col:8> col:8 implicit constexpr c 'void (c &&)' inline default trivial noexcept-unevaluated 0x7ffff493ed18
|   `-ParmVarDecl 0x7ffff493ee38 <col:8> col:8 'c &&'
`-FunctionDecl 0x7ffff493e860 <line:6:1, line:10:1> line:6:5 main 'int ()'
  `-CompoundStmt 0x7ffff493f0b8 <col:12, line:10:1>
    |-DeclStmt 0x7ffff493ef10 <line:7:3, col:6>
    | `-VarDecl 0x7ffff493e990 <col:3, col:5> col:5 used q 'c' callinit
    |   `-CXXConstructExpr 0x7ffff493eee8 <col:5> 'c' 'void () noexcept'
    |-BinaryOperator 0x7ffff493ef98 <line:8:3, col:9> 'int' lvalue '='
    | |-MemberExpr 0x7ffff493ef48 <col:3, col:5> 'int' lvalue .a 0x7ffff493e748
    | | `-DeclRefExpr 0x7ffff493ef28 <col:3> 'c' lvalue Var 0x7ffff493e990 'q' 'c'
    | `-IntegerLiteral 0x7ffff493ef78 <col:9> 'int' 5
    `-ReturnStmt 0x7ffff493f0a8 <line:9:3, col:10>
      `-IntegerLiteral 0x7ffff493f088 <col:10> 'int' 0


TranslationUnitDecl 0x7fffc8b43088 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7fffc8b439b8 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7fffc8b43620 '__int128'
|-TypedefDecl 0x7fffc8b43a28 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7fffc8b43640 'unsigned __int128'
|-TypedefDecl 0x7fffc8b43da8 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7fffc8b43b20 '__NSConstantString_tag'
|   `-CXXRecord 0x7fffc8b43a80 '__NSConstantString_tag'
|-TypedefDecl 0x7fffc8b43e40 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7fffc8b43e00 'char *'
|   `-BuiltinType 0x7fffc8b43120 'char'
|-TypedefDecl 0x7fffc8b81638 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7fffc8b815e0 '__va_list_tag [1]' 1 
|   `-RecordType 0x7fffc8b43f30 '__va_list_tag'
|     `-CXXRecord 0x7fffc8b43e98 '__va_list_tag'
|-FunctionTemplateDecl 0x7fffc8b81960 <bin/cpp_test.cpp:1:1, line:4:1> line:2:5 move
| |-TemplateTypeParmDecl 0x7fffc8b81690 <line:1:10, col:19> col:19 referenced typename depth 0 index 0 T
| |-FunctionDecl 0x7fffc8b818b8 <line:2:1, line:4:1> line:2:5 move 'T &&(T &&)'
| | |-ParmVarDecl 0x7fffc8b817b8 <col:10, col:14> col:14 referenced x 'T &&'
| | | |-LifetimeconstAttr 0x7fffc8bab5b8 <<invalid sloc>> Implicit
| | | `-LifetimeconstAttr 0x7fffc8bacef8 <<invalid sloc>> Implicit
| | `-CompoundStmt 0x7fffc8b81a78 <col:17, line:4:1>
| |   `-ReturnStmt 0x7fffc8b81a68 <line:3:3, col:28>
| |     `-CXXStaticCastExpr 0x7fffc8b81a38 <col:10, col:28> 'T' xvalue static_cast<T &&> <Dependent>
| |       `-DeclRefExpr 0x7fffc8b81a08 <col:27> 'T' lvalue ParmVar 0x7fffc8b817b8 'x' 'T &&'
| `-FunctionDecl 0x7fffc8bab8a8 <line:2:1, line:4:1> line:2:5 used move 'c &(c &)'
|   |-TemplateArgument type 'c &'
|   |-ParmVarDecl 0x7fffc8bab758 <col:10, col:14> col:14 used x 'c &'
|   | `-LifetimeconstAttr 0x7fffc8bab7c8 <<invalid sloc>> Implicit
|   `-CompoundStmt 0x7fffc8bad140 <col:17, line:4:1>
|     `-ReturnStmt 0x7fffc8bad130 <line:3:3, col:28>
|       `-CXXStaticCastExpr 0x7fffc8bad100 <col:10, col:28> 'c' lvalue static_cast<struct c &> <NoOp>
|         `-DeclRefExpr 0x7fffc8bad0e0 <col:27> 'c' lvalue ParmVar 0x7fffc8bab758 'x' 'c &'
|-CXXRecordDecl 0x7fffc8b81a90 <line:5:1, line:8:1> line:5:8 referenced struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param implicit_has_const_param
| | |-MoveAssignment exists simple trivial
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffc8b81bb8 <col:1, col:8> col:8 implicit struct c
| |-FieldDecl 0x7fffc8b81c68 <line:6:3, col:7> col:7 referenced a 'int'
| |-FieldDecl 0x7fffc8b81cc8 <line:7:3, col:8> col:8 referenced b 'bool'

| |-CXXConstructorDecl 0x7fffc8b81ee8 <line:5:8> col:8 implicit used c 'void () noexcept' inline default trivial
| | `-CompoundStmt 0x7fffc8b823a8 <col:8>
| |-CXXConstructorDecl 0x7fffc8b82028 <col:8> col:8 implicit used constexpr c 'void (const c &) noexcept' inline default trivial
| | |-ParmVarDecl 0x7fffc8b82148 <col:8> col:8 used 'const c &'
| | |-CXXCtorInitializer Field 0x7fffc8b81c68 'a' 'int'
| | | `-ImplicitCastExpr 0x7fffc8bab3a0 <col:8> 'int' <LValueToRValue>
| | |   `-MemberExpr 0x7fffc8bab370 <col:8> 'const int' lvalue .a 0x7fffc8b81c68
| | |     `-DeclRefExpr 0x7fffc8bab350 <col:8> 'const c' lvalue ParmVar 0x7fffc8b82148 '' 'const c &'
| | |-CXXCtorInitializer Field 0x7fffc8b81cc8 'b' 'bool'
| | | `-ImplicitCastExpr 0x7fffc8bab430 <col:8> 'bool' <LValueToRValue>
| | |   `-MemberExpr 0x7fffc8bab400 <col:8> 'const bool' lvalue .b 0x7fffc8b81cc8
| | |     `-DeclRefExpr 0x7fffc8bab3e0 <col:8> 'const c' lvalue ParmVar 0x7fffc8b82148 '' 'const c &'
| | `-CompoundStmt 0x7fffc8bab480 <col:8>

| |-CXXConstructorDecl 0x7fffc8b821e8 <col:8> col:8 implicit constexpr c 'void (c &&)' inline default trivial noexcept-unevaluated 0x7fffc8b821e8
| | `-ParmVarDecl 0x7fffc8b82308 <col:8> col:8 'c &&'

| |-CXXMethodDecl 0x7fffc8babbf0 <col:8> col:8 implicit used constexpr operator= 'c &(const c &) noexcept' inline default trivial
| | |-ParmVarDecl 0x7fffc8babd08 <col:8> col:8 used 'const c &'
| | `-CompoundStmt 0x7fffc8bac178 <col:8>
| |   |-BinaryOperator 0x7fffc8bac058 <col:8> 'int' lvalue '='
| |   | |-MemberExpr 0x7fffc8bac010 <col:8> 'int' lvalue ->a 0x7fffc8b81c68
| |   | | `-CXXThisExpr 0x7fffc8bac000 <col:8> 'c *' this
| |   | `-ImplicitCastExpr 0x7fffc8bac040 <col:8> 'int' <LValueToRValue>
| |   |   `-MemberExpr 0x7fffc8babfd0 <col:8> 'const int' lvalue .a 0x7fffc8b81c68
| |   |     `-DeclRefExpr 0x7fffc8babfb0 <col:8> 'const c' lvalue ParmVar 0x7fffc8babd08 '' 'const c &'
| |   |-BinaryOperator 0x7fffc8bac120 <col:8> 'bool' lvalue '='
| |   | |-MemberExpr 0x7fffc8bac0d8 <col:8> 'bool' lvalue ->b 0x7fffc8b81cc8
| |   | | `-CXXThisExpr 0x7fffc8bac0c8 <col:8> 'c *' this
| |   | `-ImplicitCastExpr 0x7fffc8bac108 <col:8> 'bool' <LValueToRValue>
| |   |   `-MemberExpr 0x7fffc8bac098 <col:8> 'const bool' lvalue .b 0x7fffc8b81cc8
| |   |     `-DeclRefExpr 0x7fffc8bac078 <col:8> 'const c' lvalue ParmVar 0x7fffc8babd08 '' 'const c &'
| |   `-ReturnStmt 0x7fffc8bac168 <col:8>
| |     `-UnaryOperator 0x7fffc8bac150 <col:8> 'c' lvalue prefix '*' cannot overflow
| |       `-CXXThisExpr 0x7fffc8bac140 <col:8> 'c *' this
| `-CXXMethodDecl 0x7fffc8babd80 <col:8> col:8 implicit constexpr operator= 'c &(c &&)' inline default trivial noexcept-unevaluated 0x7fffc8babd80
|   `-ParmVarDecl 0x7fffc8babe98 <col:8> col:8 'c &&'

`-FunctionDecl 0x7fffc8b81d80 <line:10:1, line:20:1> line:10:5 main 'int ()'
  `-CompoundStmt 0x7fffc8bad078 <col:12, line:20:1>
    |-DeclStmt 0x7fffc8b823e0 <line:11:3, col:6>
    | `-VarDecl 0x7fffc8b81e68 <col:3, col:5> col:5 used q 'c' callinit
    |   `-CXXConstructExpr 0x7fffc8b823b8 <col:5> 'c' 'void () noexcept'
    |-BinaryOperator 0x7fffc8b82468 <line:12:3, col:9> 'int' lvalue '='
    | |-MemberExpr 0x7fffc8b82418 <col:3, col:5> 'int' lvalue .a 0x7fffc8b81c68
    | | `-DeclRefExpr 0x7fffc8b823f8 <col:3> 'c' lvalue Var 0x7fffc8b81e68 'q' 'c'
    | `-IntegerLiteral 0x7fffc8b82448 <col:9> 'int' 5
    |-DeclStmt 0x7fffc8bab4c0 <line:13:3, col:10>
    | `-VarDecl 0x7fffc8bab258 <col:3, col:9> col:5 q1 'c' callinit
    |   `-CXXConstructExpr 0x7fffc8bab490 <col:5, col:9> 'c' 'void (const c &) noexcept'
    |     `-ImplicitCastExpr 0x7fffc8bab300 <col:8> 'const c' lvalue <NoOp>
    |       `-DeclRefExpr 0x7fffc8bab2c0 <col:8> 'c' lvalue Var 0x7fffc8b81e68 'q' 'c'
    |-DeclStmt 0x7fffc8babae0 <line:14:3, col:16>
    | `-VarDecl 0x7fffc8bab530 <col:3, col:15> col:5 used q2 'c' callinit
    |   `-CXXConstructExpr 0x7fffc8babab0 <col:5, col:15> 'c' 'void (const c &) noexcept'
    |     `-ImplicitCastExpr 0x7fffc8baba98 <col:8, col:14> 'const c' lvalue <NoOp>
    |       `-CallExpr 0x7fffc8baba50 <col:8, col:14> 'c' lvalue
    |         |-ImplicitCastExpr 0x7fffc8baba38 <col:8> 'c &(*)(c &)' <FunctionToPointerDecay>
    |         | `-DeclRefExpr 0x7fffc8bab9b0 <col:8> 'c &(c &)' lvalue Function 0x7fffc8bab8a8 'move' 'c &(c &)' (FunctionTemplate 0x7fffc8b81960 'move')
    |         `-DeclRefExpr 0x7fffc8bab598 <col:13> 'c' lvalue Var 0x7fffc8b81e68 'q' 'c'
    |-DeclStmt 0x7fffc8babb98 <line:15:3, col:7>
    | `-VarDecl 0x7fffc8babb08 <col:3, col:5> col:5 used q3 'c' callinit
    |   `-CXXConstructExpr 0x7fffc8babb70 <col:5> 'c' 'void () noexcept'
    |-CXXOperatorCallExpr 0x7fffc8bacd80 <line:16:3, col:8> 'c' lvalue
    | |-ImplicitCastExpr 0x7fffc8bacd68 <col:6> 'c &(*)(const c &) noexcept' <FunctionToPointerDecay>
    | | `-DeclRefExpr 0x7fffc8babf58 <col:6> 'c &(const c &) noexcept' lvalue CXXMethod 0x7fffc8babbf0 'operator=' 'c &(const c &) noexcept'
    | |-DeclRefExpr 0x7fffc8babbb0 <col:3> 'c' lvalue Var 0x7fffc8babb08 'q3' 'c'
    | `-ImplicitCastExpr 0x7fffc8babf10 <col:8> 'const c' lvalue <NoOp>
    |   `-DeclRefExpr 0x7fffc8babbd0 <col:8> 'c' lvalue Var 0x7fffc8bab530 'q2' 'c'
    |-DeclStmt 0x7fffc8bace58 <line:17:3, col:7>
    | `-VarDecl 0x7fffc8bacdc8 <col:3, col:5> col:5 used q4 'c' callinit
    |   `-CXXConstructExpr 0x7fffc8bace30 <col:5> 'c' 'void () noexcept'
    |-CXXOperatorCallExpr 0x7fffc8bad010 <line:18:3, col:15> 'c' lvalue
    | |-ImplicitCastExpr 0x7fffc8bacff8 <col:6> 'c &(*)(const c &) noexcept' <FunctionToPointerDecay>
    | | `-DeclRefExpr 0x7fffc8bacfd8 <col:6> 'c &(const c &) noexcept' lvalue CXXMethod 0x7fffc8babbf0 'operator=' 'c &(const c &) noexcept'
    | |-DeclRefExpr 0x7fffc8bace70 <col:3> 'c' lvalue Var 0x7fffc8bacdc8 'q4' 'c'
    | `-ImplicitCastExpr 0x7fffc8bacfc0 <col:8, col:15> 'const c' lvalue <NoOp>
    |   `-CallExpr 0x7fffc8bacf98 <col:8, col:15> 'c' lvalue
    |     |-ImplicitCastExpr 0x7fffc8bacf80 <col:8> 'c &(*)(c &)' <FunctionToPointerDecay>
    |     | `-DeclRefExpr 0x7fffc8bacf58 <col:8> 'c &(c &)' lvalue Function 0x7fffc8bab8a8 'move' 'c &(c &)' (FunctionTemplate 0x7fffc8b81960 'move')
    |     `-DeclRefExpr 0x7fffc8baced8 <col:13> 'c' lvalue Var 0x7fffc8babb08 'q3' 'c'
    `-ReturnStmt 0x7fffc8bad068 <line:19:3, col:10>
      `-IntegerLiteral 0x7fffc8bad048 <col:10> 'int' 0
  */

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow()))
    //, TODO: Make sure that we correctly output the move constructor? Maybe?
    // hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
    );


  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
}

// TEST(ClassParsing, MemberInitializationAndAccess) {
//   StringRef Code = R"(
// c : type = class:
//   x : int
//   y : bool
// main() : int!
//   q : c
//   q.x = 4
//   return 0
//   )";
//   SimpleGoldParseTest(Code);
// }