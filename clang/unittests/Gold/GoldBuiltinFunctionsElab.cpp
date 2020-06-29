//=== GoldClassThisAccessElab.cpp ------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This tests elaboration for sizeof, alignof, decltype, and
//  the noexcept operators. Eventually, it might be possible to use this to 
//  evaluate the constexpr operator.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;
/*
TranslationUnitDecl 0x7fffb916a918 <<invalid sloc>> <invalid sloc>
|-TypedefDecl 0x7fffb916b248 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128'
| `-BuiltinType 0x7fffb916aeb0 '__int128'
|-TypedefDecl 0x7fffb916b2b8 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128'
| `-BuiltinType 0x7fffb916aed0 'unsigned __int128'
|-TypedefDecl 0x7fffb916b638 <<invalid sloc>> <invalid sloc> implicit __NSConstantString '__NSConstantString_tag'
| `-RecordType 0x7fffb916b3b0 '__NSConstantString_tag'
|   `-CXXRecord 0x7fffb916b310 '__NSConstantString_tag'
|-TypedefDecl 0x7fffb916b6d0 <<invalid sloc>> <invalid sloc> implicit __builtin_ms_va_list 'char *'
| `-PointerType 0x7fffb916b690 'char *'
|   `-BuiltinType 0x7fffb916a9b0 'char'
|-TypedefDecl 0x7fffb91aa998 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]'
| `-ConstantArrayType 0x7fffb91aa940 '__va_list_tag [1]' 1 
|   `-RecordType 0x7fffb916b7c0 '__va_list_tag'
|     `-CXXRecord 0x7fffb916b728 '__va_list_tag'
|-VarDecl 0x7fffb91aaa08 <cpp_test.cpp:1:1, col:36> col:21 S1 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91aaad0 <col:26, col:36> 'unsigned long' sizeof 'int'
|-VarDecl 0x7fffb91aab60 <line:2:1, col:37> col:21 S2 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91aabe0 <col:26, col:37> 'unsigned long' sizeof 'long'
|-VarDecl 0x7fffb91aac70 <line:3:1, col:39> col:21 S3 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91aacf0 <col:26, col:39> 'unsigned long' sizeof 'double'
|-CXXRecordDecl 0x7fffb91aad68 <line:5:1, col:16> col:8 referenced struct Empty definition
| |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists trivial constexpr needs_implicit defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveConstructor exists simple trivial needs_implicit
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| `-CXXRecordDecl 0x7fffb91aae88 <col:1, col:8> col:8 implicit struct Empty
|-CXXRecordDecl 0x7fffb91aaf38 <line:6:1, line:8:1> line:6:8 referenced struct X definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable pod trivial literal
| | |-DefaultConstructor exists trivial needs_implicit
| | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveConstructor exists simple trivial needs_implicit
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffb91ab058 <col:1, col:8> col:8 implicit struct X
| `-FieldDecl 0x7fffb91ab108 <line:7:3, col:7> col:7 i 'int'
|-VarDecl 0x7fffb91ab188 <line:9:1, col:38> col:21 S4 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91ab200 <col:26, col:38> 'unsigned long' sizeof 'Empty'
|-VarDecl 0x7fffb91ab350 <line:10:1, col:34> col:21 S5 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91ab3c8 <col:26, col:34> 'unsigned long' sizeof 'X'
|-VarDecl 0x7fffb91ab520 <line:12:1, col:39> col:21 S6 'const unsigned long' cinit
| `-UnaryExprOrTypeTraitExpr 0x7fffb91ab630 <col:26, col:39> 'unsigned long' sizeof 'int [3]'
`-VarDecl 0x7fffb91ab6c0 <line:13:1, col:38> col:21 S7 'const unsigned long' cinit
  `-UnaryExprOrTypeTraitExpr 0x7fffb91ab740 <col:26, col:38> 'unsigned long' sizeof 'void *'
*/
TEST(GoldBuiltinFunctionElab, SizeOf_OnTypeName) {
    StringRef Code = R"(
S1 : const int = sizeof(int)
)";
  DeclarationMatcher VarDeclWithSizeOf = varDecl(
    hasName("S1"),
    hasInitializer(has(unaryExprOrTypeTraitExpr(
      sizeOfExpr(
        hasArgumentOfType(
          asString("int")
        )
      )
    )))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithSizeOf));
}

TEST(GoldBuiltinFunctionElab, SizeOf_OnClassName) {
    StringRef Code = R"(
Cls : type = class:
  i:int
S1 : const int = sizeof(Cls)
)";
  DeclarationMatcher VarDeclWithSizeOf = varDecl(
    hasName("S1"),
    hasInitializer(has(unaryExprOrTypeTraitExpr(
      sizeOfExpr(
        hasArgumentOfType(
          asString("struct Cls")
        )
      )
    )))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithSizeOf));
}

TEST(GoldBuiltinFunctionElab, SizeOf_OnExpr) {
    StringRef Code = R"(
Cls : type = class:
  i:int
S1 : const int = sizeof(1 + 1)
)";
  DeclarationMatcher VarDeclWithSizeOf = varDecl(
    hasName("S1"),
    hasInitializer(has(unaryExprOrTypeTraitExpr(
      sizeOfExpr(
        hasArgumentOfType(
          asString("int")
        )
      )
    )))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithSizeOf));
}

TEST(GoldBuiltinFunctionElab, SizeOf_OnIncompleteTemplate) {
    StringRef Code = R"(
Cls[T:type] : type = class:
  i:int
S1 : const int = sizeof(Cls)
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, SizeOf_OnNamespace) {
    StringRef Code = R"(
Ns :namespace = namespace:
  foo():void

S1 : const int = sizeof(Ns)
)";
  GoldFailureTest(Code);
}