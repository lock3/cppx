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


// TEST(GoldUserDefinedOperator, OperatorMemberDeclTest) {
//   std::string Code = R"Gold(
// X : type = class:
//   i:int    
//   operator'+'():X!
//     return X()

// )Gold";
//   DeclarationMatcher opMatches = hasDescendant(
//     cxxMethodDecl(hasName("operator+")));
//   ASSERT_TRUE(matches(Code, opMatches))
//     << "Failed to declare a valid operator overload";
// }

/*
|-VarDecl 0x7fffdd3ab888 <cpp_test.cpp:9:1, col:5> col:5 used x 'int'
|-VarDecl 0x7fffdd3ab980 <line:10:1, col:10> col:6 referenced y 'int &' cinit
| `-DeclRefExpr 0x7fffdd3ab9e8 <col:10> 'int' lvalue Var 0x7fffdd3ab888 'x' 'int'
`-VarDecl 0x7fffdd3aba78 <line:11:1, col:9> col:5 z 'int' cinit
  `-ImplicitCastExpr 0x7fffdd3abb00 <col:9> 'int' <LValueToRValue>
    `-DeclRefExpr 0x7fffdd3abae0 <col:9> 'int' lvalue Var 0x7fffdd3ab980 'y' 'int &' non_odr_use_constant
*/
// TEST(GoldUserDefinedOperator, FreeFunctionDeclTest) {
//   std::string Code = R"Gold(
// X : type = class:
//   i:int    
// operator'+'(y:ref const X):X!
//   return y

// )Gold";
//   DeclarationMatcher opMatches = hasDescendant(
//     functionDecl(hasName("operator+")));
//   ASSERT_TRUE(matches(Code, opMatches))
//     << "Failed to declare a valid operator overload";
// }