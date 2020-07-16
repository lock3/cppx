//=== GoldQualifiedMemberAccessElab.cpp - Testing qualified member lookup --==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests to make sure that qualified member access works
//  correctly.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

/*
TODO: Apparently this is waiting for the (x)y implementation to be completed.
TranslationUnitDecl 0x7fffdb9f6f08 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7fffdb9f7838 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7fffdb9f74a0 '__int128'
|-TypedefDecl 0x7fffdb9f78a8 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7fffdb9f74c0 'unsigned __int128'
|-TypedefDecl 0x7fffdb9f7c28 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7fffdb9f79a0 '__NSConstantString_tag'
|   `-CXXRecord 0x7fffdb9f7900 '__NSConstantString_tag'
|-TypedefDecl 0x7fffdb9f7cc0 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7fffdb9f7c80 'char *'
|   `-BuiltinType 0x7fffdb9f6fa0 'char'
|-TypedefDecl 0x7fffdba36e08 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7fffdba36db0 '__va_list_tag [1]' 1 
|   `-RecordType 0x7fffdb9f7db0 '__va_list_tag'
|     `-CXXRecord 0x7fffdb9f7d18 '__va_list_tag'
|-CXXRecordDecl 0x7fffdba36e60 <cpp_test.cpp:1:1, line:3:1> line:1:8 referenced struct A definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial
| |-CXXRecordDecl 0x7fffdba36f88 <col:1, col:8> col:8 implicit struct A
| |-FieldDecl 0x7fffdba37038 <line:2:3, col:7> col:7 referenced m1 'int'
| |-CXXConstructorDecl 0x7fffdba375a0 <line:1:8> col:8 implicit used A 'void () noexcept' inline default trivial
| | `-CompoundStmt 0x7fffdba60fd0 <col:8>
| |-CXXDestructorDecl 0x7fffdba37698 <col:8> col:8 implicit ~A 'void ()' inline default trivial noexcept-unevaluated 0x7fffdba37698
| |-CXXConstructorDecl 0x7fffdba377f8 <col:8> col:8 implicit constexpr A 'void (const A &)' inline default trivial noexcept-unevaluated 0x7fffdba377f8
| | `-ParmVarDecl 0x7fffdba37930 <col:8> col:8 'const A &'
| `-CXXConstructorDecl 0x7fffdba379d8 <col:8> col:8 implicit constexpr A 'void (A &&)' inline default trivial noexcept-unevaluated 0x7fffdba379d8
|   `-ParmVarDecl 0x7fffdba37b10 <col:8> col:8 'A &&'
|-CXXRecordDecl 0x7fffdba370a0 <line:4:1, line:6:1> line:4:8 referenced struct B definition
| |-DefinitionData pass_in_registers standard_layout trivially_copyable trivial literal
| | |-DefaultConstructor exists trivial
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-public 'A'
| |-CXXRecordDecl 0x7fffdba37208 <col:1, col:8> col:8 implicit struct B
| |-CXXConstructorDecl 0x7fffdba37490 <col:8> col:8 implicit used B 'void () noexcept' inline default trivial
| | |-CXXCtorInitializer 'A'
| | | `-CXXConstructExpr 0x7fffdba60fe0 <col:8> 'A' 'void () noexcept'
| | `-CompoundStmt 0x7fffdba61048 <col:8>
| |-CXXConstructorDecl 0x7fffdba37b88 <col:8> col:8 implicit constexpr B 'void (const B &)' inline default trivial noexcept-unevaluated 0x7fffdba37b88
| | `-ParmVarDecl 0x7fffdba60d40 <col:8> col:8 'const B &'
| `-CXXConstructorDecl 0x7fffdba60de8 <col:8> col:8 implicit constexpr B 'void (B &&)' inline default trivial noexcept-unevaluated 0x7fffdba60de8
|   `-ParmVarDecl 0x7fffdba60f20 <col:8> col:8 'B &&'
`-FunctionDecl 0x7fffdba37310 <line:7:1, line:10:1> line:7:6 foo 'void ()'
  `-CompoundStmt 0x7fffdba61318 <col:12, line:10:1>
    |-DeclStmt 0x7fffdba61080 <line:8:3, col:6>
    | `-VarDecl 0x7fffdba37410 <col:3, col:5> col:5 used b 'B' callinit
    |   `-CXXConstructExpr 0x7fffdba61058 <col:5> 'B' 'void () noexcept'
    `-BinaryOperator 0x7fffdba61170 <line:9:3, col:13> 'int' lvalue '='
      |-MemberExpr 0x7fffdba61108 <col:3, col:8> 'int' lvalue .m1 0x7fffdba37038
      | `-ImplicitCastExpr 0x7fffdba610d8 <col:3> 'A' lvalue <UncheckedDerivedToBase (A)>
      |   `-DeclRefExpr 0x7fffdba61098 <col:3> 'B' lvalue Var 0x7fffdba37410 'b' 'B'
      `-IntegerLiteral 0x7fffdba61150 <col:13> 'int' 4
*/
// TEST(QualifiedMemberAccess, ExplicitBaseClassAccess_OutsideClass) {
//   StringRef Code = R"(
// A : type = class:
//   m1:int

// B : type = class(A) {
//   m2:int;
// }

// foo() : void! {
//   b:B;
//   b.(A)m1 = 4;
// }
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(QualifiedMemberAccess, ExplicitBaseClassAccess_InsideOfClass) {
//   StringRef Code = R"(
// A : type = class {
//   m1:int
// }
// B : type = class(A) {
//   foo() :void{
//     (A)m1 = 4
//   }
// }

// foo() : void {
//   b:B;
//   b.foo();
// }
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }