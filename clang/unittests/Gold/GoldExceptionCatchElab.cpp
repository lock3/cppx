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


TEST(GoldCatch, UsingImplicitAnonymousArrayMacro) {
    StringRef Code = R"(
foo():void!
  {

    i:int = 5
  } catch(x:int):
    ;
  catch(x:float64):
    ;

)";
  auto ToMatch = compoundStmt(
    has(cxxTryStmt(
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      )),
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCatch, UsingExplicitAnonymousArrayMacro) {
    StringRef Code = R"(
foo():void!
  _{

    i:int = 5
  } catch(x:int):
    ;
  catch(x:float64):
    ;

)";
  auto ToMatch = compoundStmt(
    has(cxxTryStmt(
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      )),
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldCatch, UsingExplicitAnonymousIndent) {
    StringRef Code = R"(
foo():void!
  _:

    i:int = 5
  catch(x:int):
    ;
  catch(x:float64):
    ;

)";
  auto ToMatch = compoundStmt(
    has(cxxTryStmt(
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      )),
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCatch, StrangeColonBlockWithoutName) {
    StringRef Code = R"(
foo():void!
  :

    i:int = 5
  catch(x:int):
    ;
  catch(x:float64):
    ;

)";
  auto ToMatch = compoundStmt(
    has(cxxTryStmt(
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      )),
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCatch, CatchAllTest) {
    StringRef Code = R"(
foo():void!
  {

    i:int = 5
  } catch(x:int):
    ;
  catch():
    ;

)";
  auto ToMatch = compoundStmt(
    has(cxxTryStmt(
      has(cxxCatchStmt(
        has(varDecl(hasName("x"), hasType(asString("int")), isExceptionVariable()))
      )),
      has(cxxCatchStmt(isCatchAll()))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}