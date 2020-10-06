//=== GoldExceptionCatchElab.cpp -------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests the syntax for the catch as part of a macro.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldCatch, OnAFunction) {
    StringRef Code = R"(
foo():void!
  i:int = 5
catch(x:int):
  ;
catch(x:float64):
  ;

)";
  auto ToMatch = namespaceDecl(
    hasName("X"),
    has(namespaceDecl(hasName("NS"))
  ));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


// TEST(GoldCatch, OnIf) {
//     StringRef Code = R"(
// foo():void!
//   if true:
//     ;
//   catch(x:int):
//     ;

// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldCatch, OnElse) {
//     StringRef Code = R"(
// foo():void!
//   if true:
//     ;
//   else:
//     ;
//   catch(x:int):
//     ;

// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldCatch, OnWhile) {
//     StringRef Code = R"(
// foo():void!
//   while(true):
//     ;
//   catch(x:int):
//     ;

// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(GoldCatch, OnBlock) {
    StringRef Code = R"(
foo():void!
  {
    h:int = 4
    {
      i:int = 5
    }
    j:int = 3
  } catch(x:int):
    ;
  k:int = 2  
)";
  auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldCatch, WithFollowingExpr) {
//     StringRef Code = R"(
// foo():void!
//   {
//   } catch(x:int):
//     ;
//   x:int = 9
// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldCatch, MultiCatch) {
//     StringRef Code = R"(
// foo():void!
//   {
//   } catch(x:int):
//     ;
//   catch(x:float64):
//     ;
//   catch:
//     ;

//   x:int = 9
// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldCatch, InsideClassBodyOnMemberFunction) {
//     StringRef Code = R"(
// x = class:
//   foo():void!
//   catch(x:int):
//     ;
//   catch(x:float64):
//     ;
//   catch:
//     ;
// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldCatch, InsideClassBodyOnConstructorFunction) {
//     StringRef Code = R"(
// x = class:
//   constructor()!
//   catch(x:int):
//     ;
//   catch(x:float64):
//     ;
//   catch:
//     ;
// )";
//   auto ToMatch = namespaceDecl(hasName("X"), has(namespaceDecl(hasName("NS"))));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }


// TEST(GoldCatch, AttachedToAClassIndentation) {
//     StringRef Code = R"(
// x = class:
//   constructor()!
//     ;
// catch(x:int):
//   ;
// catch(x:float64):
//   ;
// catch:
//   ;
// )";
//   GoldFailureTest(Code);
// }

// TEST(GoldCatch, AttachedToAnArray) {
//     StringRef Code = R"(
// foo():void!
//   i:[3]int = array{
//     1,
//     2,
//     3
//   } catch(x:int):
//       ;
// )";
//   GoldFailureTest(Code);
// }