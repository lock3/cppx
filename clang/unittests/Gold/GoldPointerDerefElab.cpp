
//=== GoldPointerDerefElab.cpp ---------------------------------------------==//
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
`-FunctionDecl 0x7fffdb546040 <cpp_test.cpp:8:1, line:10:1> line:8:6 foo 'void (int *)'
  |-ParmVarDecl 0x7fffdb545f70 <col:10, col:15> col:15 used f 'int *'
  `-CompoundStmt 0x7fffdb546238 <col:18, line:10:1>
    `-DeclStmt 0x7fffdb546220 <line:9:3, col:13>
      `-VarDecl 0x7fffdb546150 <col:3, col:12> col:7 x 'int' cinit
        `-ImplicitCastExpr 0x7fffdb546208 <col:11, col:12> 'int' <LValueToRValue>
          `-UnaryOperator 0x7fffdb5461f0 <col:11, col:12> 'int' lvalue prefix '*' cannot overflow
            `-ImplicitCastExpr 0x7fffdb5461d8 <col:12> 'int *' <LValueToRValue>
              `-DeclRefExpr 0x7fffdb5461b8 <col:12> 'int *' lvalue ParmVar 0x7fffdb545f70 'f' 'int *'
*/

TEST(GoldPointerDeref, SimpleDereference) {
  std::string Code = R"Gold(
foo(i:^int):void!
  x = ^i
)Gold";
  DeclarationMatcher opMatches = hasDescendant(unaryOperator());
  ASSERT_TRUE(matches(Code, opMatches))
    << "Pointer deref failed";
}