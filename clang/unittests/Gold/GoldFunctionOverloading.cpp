//===- unittest/Gold/GoldFunctionOverloading.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(FunctionOverloading, SimpleOverload) {

  StringRef Code = R"(
foo(i:int) : int!
  return 4 + i

foo(a:float32) :int!
  return 5

main() : int!
  return foo(3)
)";
  DeclarationMatcher CallMatcher = functionDecl(
    hasName("main"),
    isMain(),
    isDefinition(),
    hasDescendant(
      callExpr(
        callee(
          functionDecl(
            hasName("foo"),
            hasType(
              asString("int (int)")
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), CallMatcher));
}


TEST(FunctionOverloading, TemplateOverloading_SameTemplateParamTypes) {

  StringRef Code = R"(
foo[T:type](i:int) : int!
  return 4 + i

foo[T:type](a:float32) :int!
  return 5

main() : int!
  return foo[int](3)
)";
  DeclarationMatcher CallMatcher = functionDecl(
    hasName("main"),
    isMain(),
    isDefinition(),
    hasDescendant(
      callExpr(
        callee(
          functionDecl(
            hasName("foo"),
            hasType(
              asString("int (int)")
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), CallMatcher));
}

TEST(FunctionOverloading, MemberFunction_Overloading) {

  StringRef Code = R"(
c : type = class:
  foo(i:int) : int!
    return 4 + i
  foo(a:float64) :int!
    return 5


main() : int!
  b:c
  return b.foo(3)

)";
  DeclarationMatcher CallMatcher = functionDecl(
    hasName("main"),
    isMain(),
    isDefinition(),
    hasDescendant(
      callExpr(
        callee(
          functionDecl(
            hasName("foo"),
            hasType(
              asString("int (int)")
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), CallMatcher));
}

TEST(FunctionOverloading, MemberFunctionTemplate_Overloading) {
  StringRef Code = R"(
c : type = class:
  foo[T:type](i:int) : int!
    return 4 + i
  
  foo[T:type](a:float32) :int!
    return 5
  
main() : int!
  q : c
  return q.foo[int](2)
)";
  DeclarationMatcher CallMatcher = functionDecl(
    hasName("main"),
    isMain(),
    isDefinition(),
    hasDescendant(
      callExpr(
        callee(
          functionDecl(
            hasName("foo"),
            hasType(
              asString("int (int)")
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), CallMatcher));
}
