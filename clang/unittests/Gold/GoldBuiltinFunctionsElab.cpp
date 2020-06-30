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

// TODO: Create tests for alignof - (this is missing the alignas attribute)
TEST(GoldBuiltinFunctionElab, Alignof_TypeOfTypes) {
  ASSERT_TRUE(false) << "Need to implement alignas attribute\n";
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