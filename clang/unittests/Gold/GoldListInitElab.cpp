//=== GoldListInitElab.cpp -------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file tests the syntax  T{ args } and new [T]{ args }.
//  This is for both universal initialization, and list initialization.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

#if 0
// I need to test this for templates also?
TEST(GoldListInit, Array_DefaultCtorCall_UDT_NotAsPartOfConstruction) {
  StringRef Code = R"(
C  = class:
  ;
foo(): void!
  x : C
  x = C { }
)";
  auto ToMatch = cxxOperatorCallExpr(
    hasDescendant(materializeTemporaryExpr(
      has(cxxFunctionalCastExpr(
        has(initListExpr())
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldListInit, Array_DefaultCtorCall_UDT_NestedTpye) {
  StringRef Code = R"(
C  = class:
  X = class:
    ;
foo(): void!
  x : C.X
  x = C.X { }
)";
  auto ToMatch = cxxOperatorCallExpr(
    hasDescendant(materializeTemporaryExpr(
      has(cxxFunctionalCastExpr(
        has(initListExpr())
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldListInit, Array_DefaultCtorCall_UDT_FreeStanding) {
  StringRef Code = R"(
C  = class:
  ;
foo(): void!
  C { }
)";
  auto ToMatch = cxxFunctionalCastExpr( has(initListExpr()) );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldListInit, Array_DefaultCtorCall_UDT_AsPartOfInit) {
  StringRef Code = R"(
C  = class:
  ;
foo(): void!
  x : C = C{ }
)";
  auto ToMatch = varDecl(
    hasName("x"),
    hasDescendant(cxxFunctionalCastExpr(has(initListExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldListInit, Array_DefaultCtorCall_UDT_WithConstructorArguments) {
  StringRef Code = R"(
C  = class:
  constructor(x:int)!
    ;

foo(): void!
  x : C = C{ 1 }
)";
  auto ToMatch = varDecl(
    hasName("x"),
    hasDescendant(cxxTemporaryObjectExpr())
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldListInit, Array_DefaultCtorCall_UDT_SingleMemberInit) {
  StringRef Code = R"(
C  = class:
  x:int

foo(): void!
  x : C = C{ 1 }
)";
  auto ToMatch = varDecl(
    hasName("x"),
    hasDescendant(cxxFunctionalCastExpr(has(initListExpr())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldListInit, Array_DefaultCtorCall_UDT_WithConstructorArguments_MemberWiseInit) {
  StringRef Code = R"(
C  = class:
  x:int
  y:int
  z:float64

foo(): void!
  x : C = C{ 1, 2, 3.0 }
)";
  auto ToMatch = varDecl(
    hasName("x"),
      has(cxxFunctionalCastExpr(
        has(initListExpr())
      ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldListInit, Array_DefaultCtorCall_UDT_ArrayListInit) {
//   StringRef Code = R"(

// foo(): void!
//   x : [3]int = ([3]int){ 1, 2, 3}
// )";
//   auto ToMatch = varDecl(
//     hasName("x"),
//     hasInitializer(
//       hasDescendant(materializeTemporaryExpr(
//         has(cxxFunctionalCastExpr(
//           has(initListExpr())
//         ))
//       ))
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }
#endif
