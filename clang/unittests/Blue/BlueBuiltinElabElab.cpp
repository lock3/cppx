//=== BlueBuiltinElabElab.cpp ----------------------------------------------==//
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


#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueBuiltinFunc, SizeOf_OnTypeName) {
  StringRef Code = R"(
S1 : const int = sizeof(int);
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

TEST(BlueBuiltinFunc, SizeOf_OnClassName) {
  StringRef Code = R"(
Cls : type = {
  i:int;
}
S1 : const int = sizeof(Cls);
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

TEST(BlueBuiltinFunc, SizeOf_OnExpr) {
  StringRef Code = R"(
Cls : type ={
  i:int;
}
S1 : const int = sizeof(1 + 1);
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

TEST(BlueBuiltinFunc, SizeOf_OnIncompleteTemplate) {
  StringRef Code = R"(
Cls : [T:type]->type= {
  i:int;
}
S1 : const int = sizeof(Cls);
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, SizeOf_OnNamespace) {
  StringRef Code = R"(
Ns : namespace = {
  foo:() -> void = { }
}
S1 : const int = sizeof(Ns);
)";
  BlueFailureTest(Code);
}

// ---------------------------------------------------------------------------//
//                                decltype
// ---------------------------------------------------------------------------//

TEST(BlueBuiltinFunc, decltype_TypeOfTypes) {
  StringRef Code = R"(
S1 : decltype(int) = int;
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, decltype_Expression) {
  StringRef Code = R"(
S1 : decltype(1) = 5;
)";
  DeclarationMatcher VarDeclWithDecltype = varDecl(
    hasName("S1"),
    hasType(isInteger())
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}

TEST(BlueBuiltinFunc, decltype_ClassName) {
  StringRef Code = R"(
Cls : type ={
  i:int;
}
S1 : decltype(Cls) = Cls;
)";
  BlueFailureTest(Code);
}


TEST(BlueBuiltinFunc, decltype_OverloadSet) {
  StringRef Code = R"(
x:int;

foo :(i:int)-> int = {
  return 4;
}

foo :(i:float32)-> float32 = {
  return 4;
}

S1 : decltype(foo(x)) = 1;
)";
  DeclarationMatcher VarDeclWithDecltype = varDecl(
    hasName("S1"),
    hasType(isInteger())
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}


TEST(BlueBuiltinFunc, NoExceptOperator_FunctionCall) {
  StringRef Code = R"(
foo:()->int = {
  return 4;
}
S1:const bool = noexcept(foo());
)";
  DeclarationMatcher VarDeclNoExcept = varDecl(
    hasName("S1"),
    hasInitializer(cxxNoexceptExpr(has(callExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclNoExcept));
}

TEST(BlueBuiltinFunc, NoExceptOperator_Namespace) {
  StringRef Code = R"(
x : namespace = {
}

S1:const bool = noexcept(x);
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, NoExceptOperator_Type) {
  StringRef Code = R"(
x : type ={
  i : int;
}

S1:const bool = noexcept(x);
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, NoExceptOperator_Template) {
  StringRef Code = R"(
x :[T:type]-> type={
  i : int;
}

S1:const bool = noexcept(x);
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, NoExceptOperator_OverloadSet) {
  StringRef Code = R"(
foo :(x:int) -> int= {
  return 5;
}
foo :(x:float32) -> float ={
  return 5;
}

S1:const bool = noexcept(foo);
)";
  BlueFailureTest(Code);
}

TEST(BlueBuiltinFunc, NoExceptOperator_Literal) {
  StringRef Code = R"(
foo:() ->int = {
  return 4;
}
S1:const bool = noexcept(1);
)";
  DeclarationMatcher VarDeclNoExcept = varDecl(
    hasName("S1"),
    hasInitializer(cxxNoexceptExpr(has(integerLiteral())))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclNoExcept));
}