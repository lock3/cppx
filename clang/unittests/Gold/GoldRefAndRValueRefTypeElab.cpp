//=== GoldRefAndRValueRefTypeElab.cpp --------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing for declaration of reference types. Both L & R value
//  reference types.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include <string>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldRef, LValueRef) {
  std::string Code = R"Gold(
x:int = 0
y:ref int = x
)Gold";
  DeclarationMatcher opMatches = varDecl(
    hasName("y"), hasType(asString("int &")));
  ASSERT_TRUE(matches(Code, opMatches));
}

TEST(GoldRef, LValueRefParam) {
  std::string Code = R"Gold(
x:int = 0
foo(y:ref int): void
)Gold";
  DeclarationMatcher opMatches = parmVarDecl(
    hasName("y"), hasType(asString("int &")));
  ASSERT_TRUE(matches(Code, opMatches));
}


  /*
`-FunctionDecl 0x7fffd54f3fa8 <cpp_test.cpp:12:1, line:14:1> line:12:7 move 'int &&(int &)'
  |-ParmVarDecl 0x7fffd54f3ea0 <col:12, col:17> col:17 used i 'int &'
  `-CompoundStmt 0x7fffd54f4118 <col:20, line:14:1>
    `-ReturnStmt 0x7fffd54f4108 <line:13:3, col:30>
      `-CXXStaticCastExpr 0x7fffd54f40d8 <col:10, col:30> 'int' xvalue static_cast<int &&> <NoOp>
        `-DeclRefExpr 0x7fffd54f40a0 <col:29> 'int' lvalue ParmVar 0x7fffd54f3ea0 'i' 'int &'
  */
TEST(GoldRef, RValueRefParam) {
  std::string Code = R"Gold(
move(var:rref int):rref int
)Gold";
  DeclarationMatcher opMatches = parmVarDecl(
    hasName("var"), hasType(asString("int &&")));
  ASSERT_TRUE(matches(Code, opMatches));
}

// TEST(GoldRef, LValueRefAsDeclaratorOnly) {
//   std::string Code = R"Gold(
// Ty[T:type] :type = class:
//   # 

// foo() :void!
//   x:Ty[const int]
// )Gold";
//   DeclarationMatcher opMatches = varDecl(
//     hasName("y"), hasType(asString("Ty<int &>")));
//   ASSERT_TRUE(matches(Code, opMatches));
// }

// TEST(GoldRef, RValueRefAsDeclaratorOnly) {
//   std::string Code = R"Gold(
// Ty[T:type] :type = class:
//   # 

// foo() :void!
//   x:Ty[rref int]
// )Gold";
//   DeclarationMatcher opMatches = varDecl(
//     hasName("y"), hasType(asString("Ty<int &&>")));
//   ASSERT_TRUE(matches(Code, opMatches));
// }