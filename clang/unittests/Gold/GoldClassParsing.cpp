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

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

/*
TranslationUnitDecl 0x7ffff3816088 <<invalid sloc>> <invalid sloc>

|-CXXRecordDecl 0x7ffff38545e0 <bin/cpp_test.cpp:1:1, line:4:1> line:1:8 struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial needs_implicit
| | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveConstructor exists simple trivial needs_implicit
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7ffff3854708 <col:1, col:8> col:8 implicit struct c
| |-FieldDecl 0x7ffff38547b8 <line:2:3, col:7> col:7 x 'int'
| `-FieldDecl 0x7ffff3854818 <line:3:3, col:8> col:8 y 'bool'
|
`-FunctionDecl 0x7ffff38548d0 <line:6:1, line:8:1> line:6:5 main 'int ()'
  `-CompoundStmt 0x7ffff3854a20 <col:12, line:8:1>
    `-ReturnStmt 0x7ffff3854a10 <line:7:3, col:10>
      `-IntegerLiteral 0x7ffff38549f0 <col:10> 'int' 0
*/
TEST(ClassParsing, ClassDeclaration) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool

main() : int!
  return 0
  )";
  SimpleGoldParseTest(Code);
}


TEST(ClassParsing, ClassInstance) {

  /*
TranslationUnitDecl 0x7ffff5973088 <<invalid sloc>> <invalid sloc>
|-CXXRecordDecl 0x7ffff59b15e0 <bin/cpp_test.cpp:1:1, line:4:1> line:1:8 referenced struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7ffff59b1708 <col:1, col:8> col:8 implicit struct c
| |-FieldDecl 0x7ffff59b17b8 <line:2:3, col:7> col:7 x 'int'
| |-FieldDecl 0x7ffff59b1818 <line:3:3, col:8> col:8 y 'bool'
| |-CXXConstructorDecl 0x7ffff59b1a80 <line:1:8> col:8 implicit used c 'void () noexcept' inline default trivial
| | `-CompoundStmt 0x7ffff59b1f48 <col:8>
| |-CXXConstructorDecl 0x7ffff59b1bc8 <col:8> col:8 implicit constexpr c 'void (const c &)' inline default trivial noexcept-unevaluated 0x7ffff59b1bc8
| | `-ParmVarDecl 0x7ffff59b1ce8 <col:8> col:8 'const c &'
| `-CXXConstructorDecl 0x7ffff59b1d88 <col:8> col:8 implicit constexpr c 'void (c &&)' inline default trivial noexcept-unevaluated 0x7ffff59b1d88
|   `-ParmVarDecl 0x7ffff59b1ea8 <col:8> col:8 'c &&'
|
`-FunctionDecl 0x7ffff59b18d0 <line:6:1, line:9:1> line:6:5 main 'int ()'
  `-CompoundStmt 0x7ffff59b1fc8 <col:12, line:9:1>
    |-DeclStmt 0x7ffff59b1f80 <line:7:3, col:6>
    | `-VarDecl 0x7ffff59b1a00 <col:3, col:5> col:5 q 'c' callinit
    |   `-CXXConstructExpr 0x7ffff59b1f58 <col:5> 'c' 'void () noexcept'
    `-ReturnStmt 0x7ffff59b1fb8 <line:8:3, col:10>
      `-IntegerLiteral 0x7ffff59b1f98 <col:10> 'int' 0
  
  */
//   StringRef Code = R"(
// c : type = class:
//   x : int
//   y : bool

// main() : int!
//   q : c
//   return 0
//   )";
//   SimpleGoldParseTest(Code);
}