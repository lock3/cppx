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

TEST(GoldBuiltinFunctionElab, SizeOf_ParameterPack) {
  StringRef Code = R"(
Cls [T:type...] = class:
  S1 <static>: const int = sizeof...(T)
)";
  auto ToMatch = sizeOfPackExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldBuiltinFunctionElab, decltype_TypeOfTypes) {
  StringRef Code = R"(
S1 : decltype(int) = int
)";
  DeclarationMatcher DeclType = typeAliasDecl(
      hasName("S1"), hasType(isInteger()));
  ASSERT_TRUE(matches(Code.str(), DeclType));
}

TEST(GoldBuiltinFunctionElab, decltype_Expression) {
  StringRef Code = R"(
S1 : decltype(1) = 5
)";
  DeclarationMatcher VarDeclWithDecltype = varDecl(
    hasName("S1"),
    hasType(isInteger())
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}

// TEST(GoldBuiltinFunctionElab, decltype_Namespace) {
//     StringRef Code = R"(
// Cls : type = class:
//   i:int
// S1 : decltype(1) = 5
// )";
//   DeclarationMatcher VarDeclWithDecltype = varDecl(
//     hasName("S1"),
//     hasType(isInteger())
//   );
//   ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
// }

TEST(GoldBuiltinFunctionElab, decltype_ClassName) {
  StringRef Code = R"(
Cls : type = class:
  i:int
S1 : decltype(Cls) = Cls
)";
  DeclarationMatcher VarDeclWithDecltype = typeAliasDecl(
    hasName("S1"),
    hasType(asString("struct Cls"))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}


TEST(GoldBuiltinFunctionElab, decltype_AccessingNestedClassName_Fail) {
  StringRef Code = R"(
Cls : type = class:
  Inner : type = class:
    i:int
  i:int

foo() : ref Cls
S1 : decltype(foo()).Inner
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, decltype_AccessingNestedClassName) {
  StringRef Code = R"(
Cls : type = class:
  Inner : type = class:
    i:int
  i:int

foo() : Cls
S1 : decltype(foo()).Inner
)";
  DeclarationMatcher VarDeclWithDecltype = varDecl(
    hasName("S1"),
    hasType(asString("struct Cls::Inner"))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}
// TEST(GoldBuiltinFunctionElab, decltype_IncompleteTemplate) {
//     StringRef Code = R"(
// Cls[T:type] : type = class:
//   i:int
// S1 : decltype(Cls) = Cls
// )";
//   DeclarationMatcher VarDeclWithDecltype = typeAliasDecl(
//     hasName("S1")  );
//   ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
// }


TEST(GoldBuiltinFunctionElab, decltype_OverloadSet) {
  StringRef Code = R"(
x:int

foo(i:int) : int
foo(i:float32) : float32

S1 : decltype(foo(x)) = 1
)";
  DeclarationMatcher VarDeclWithDecltype = varDecl(
    hasName("S1"),
    hasType(isInteger())
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclWithDecltype));
}

/*
|-VarDecl 0x7fffe79c3930 <line:2:1, col:38> col:19 V1 'const bool' static cinit
| `-CXXNoexceptExpr 0x7fffe79c3a60 <col:24, col:38> 'bool'
|   `-CallExpr 0x7fffe79c3a40 <col:33, col:37> 'int'
|     `-ImplicitCastExpr 0x7fffe79c3a28 <col:33> 'int (*)()' <FunctionToPointerDecay>
|       `-DeclRefExpr 0x7fffe79c39e0 <col:33> 'int ()' lvalue Function 0x7fffe79c3830 'foo' 'int ()' non_odr_use_unevaluated
`-VarDecl 0x7fffe79c3ae8 <line:3:1, col:34> col:19 V2 'const bool' static cinit
  `-CXXNoexceptExpr 0x7fffe79c3b70 <col:24, col:34> 'bool'
    `-IntegerLiteral 0x7fffe79c3b50 <col:33> 'int' 1
*/
TEST(GoldBuiltinFunctionElab, NoExceptOperator_FunctionCall) {
  StringRef Code = R"(
foo():int
S1:const bool = noexcept(foo())
)";
  DeclarationMatcher VarDeclNoExcept = varDecl(
    hasName("S1"),
    hasInitializer(cxxNoexceptExpr(has(callExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclNoExcept));
}

TEST(GoldBuiltinFunctionElab, NoExceptOperator_Namespace) {
  StringRef Code = R"(
x : namespace = namespace:
  #

S1:const bool = noexcept(x)
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, NoExceptOperator_Type) {
  StringRef Code = R"(
x : type = class:
  i : int

S1:const bool = noexcept(x)
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, NoExceptOperator_Template) {
  StringRef Code = R"(
x[T:type] : type = class:
  i : int

S1:const bool = noexcept(x)
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, NoExceptOperator_OverloadSet) {
  StringRef Code = R"(
foo(x:int) :int
foo(x:float32) :float

S1:const bool = noexcept(foo)
)";
  GoldFailureTest(Code);
}

TEST(GoldBuiltinFunctionElab, NoExceptOperator_Literal) {
  StringRef Code = R"(
foo():int
S1:const bool = noexcept(1)
)";
  DeclarationMatcher VarDeclNoExcept = varDecl(
    hasName("S1"),
    hasInitializer(cxxNoexceptExpr(has(integerLiteral())))
  );
  ASSERT_TRUE(matches(Code.str(), VarDeclNoExcept));
}