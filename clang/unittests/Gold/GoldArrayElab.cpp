//=== GoldArrayElab.cpp ----------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for automatic type deduction in Gold.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


/*
TranslationUnitDecl 0x7fffba318918 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7fffba319248 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7fffba318eb0 '__int128'
|-TypedefDecl 0x7fffba3192b8 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7fffba318ed0 'unsigned __int128'
|-TypedefDecl 0x7fffba319638 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7fffba3193b0 '__NSConstantString_tag'
|   `-CXXRecord 0x7fffba319310 '__NSConstantString_tag'
|-TypedefDecl 0x7fffba3196d0 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7fffba319690 'char *'
|   `-BuiltinType 0x7fffba3189b0 'char'
|-TypedefDecl 0x7fffba358818 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7fffba3587c0 '__va_list_tag [1]' 1 
|   `-RecordType 0x7fffba3197c0 '__va_list_tag'
|     `-CXXRecord 0x7fffba319728 '__va_list_tag'
`-FunctionDecl 0x7fffba3588c8 <cpp_test.cpp:1:1, line:4:1> line:1:5 main 'int ()'
  `-CompoundStmt 0x7fffba358cd8 <col:12, line:4:1>
    |-DeclStmt 0x7fffba358bf8 <line:2:3, col:21>
    | `-VarDecl 0x7fffba358a80 <col:3, col:20> col:7 used x 'int [3]' cinit
    |   `-InitListExpr 0x7fffba358ba0 <col:14, col:20> 'int [3]'
    |     |-IntegerLiteral 0x7fffba358ae8 <col:15> 'int' 1
    |     |-IntegerLiteral 0x7fffba358b08 <col:17> 'int' 2
    |     `-IntegerLiteral 0x7fffba358b28 <col:19> 'int' 3
    `-ReturnStmt 0x7fffba358cc8 <line:3:3, col:13>
      `-ImplicitCastExpr 0x7fffba358cb0 <col:10, col:13> 'int' <LValueToRValue>
        `-ArraySubscriptExpr 0x7fffba358c90 <col:10, col:13> 'int' lvalue
          |-ImplicitCastExpr 0x7fffba358c78 <col:10> 'int *' <ArrayToPointerDecay>
          | `-DeclRefExpr 0x7fffba358c10 <col:10> 'int [3]' lvalue Var 0x7fffba358a80 'x' 'int [3]'
          `-IntegerLiteral 0x7fffba358c30 <col:12> 'int' 1
*/
TEST(ArrayMacro, ArrayIndexingOperatr) {
  StringRef Code = R"(
main() : int!
  a :[3]int = array{0, 1, 2}
  return a[1]
)";

  ASSERT_TRUE(matches(Code.str(), arraySubscriptExpr()));
}