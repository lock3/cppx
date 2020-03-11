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
// Some possible implicit functions for a class with only the use of a default ctor.
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



// ALL possible implicit functions for a class.
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

TEST(ClassParsing, MemberInitializationAndAccess) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
main() : int!
  q : c
  q.x = 4
  return 0
  )";

  StatementMatcher StmtMatcher(compoundStmt(hasDescendant(
        binaryOperator(hasOperatorName("="),
          hasLHS(memberExpr(hasDescendant(
            declRefExpr(to(varDecl(hasType(recordDecl(hasName("c"))))))))),
          hasRHS(integerLiteral(equals(4)))
        )
      )
    )
  );


  ASSERT_TRUE(matches(Code, StmtMatcher));
}
// TODO: This is the next thing to implement. Maybe...
// TEST(ClassParsing, MemberAccessFromFunctionResult) {
//   StringRef Code = R"(
// c : type = class:
//   x : int
//   y : bool
// foo(z: int): c!
//   q : c
//   return q

// main() : int!
//   q : c
//   foo(1).x = 4
//   return 0
//   )";
//   SimpleGoldParseTest(Code);
// }