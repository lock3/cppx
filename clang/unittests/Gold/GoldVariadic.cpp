//===- unittest/Gold/GoldFunctionTemplateExec.cpp ------------------------===//
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

TEST(GoldVariadic, Simple) {
  StringRef Code = R"(
add(count : int, varargs : args)!
  vlist : __builtin_va_list
  __builtin_va_start(vlist, count)

  sum = 0
  for (i in 0 .. count):
    sum += __builtin_va_arg(vlist, int)

  __builtin_va_end(vlist)
  return sum

main() : int!
  value = add(4, 0, 1, 2, 3)
)";

  DeclarationMatcher Value = varDecl(hasName("value"),
                                  hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), Value));
}
