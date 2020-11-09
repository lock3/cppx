//===- unittest/Gold/GoldDeductExec.cpp - Compiled auto deduction ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// #include "GoldCompileRun.h"
#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

// using namespace llvm;
using namespace gold;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;


// TEST(GoldDeduct, Aliases) {
//   StringRef Code = R"(
// ns = namespace {
//   x : int = 10

//   t : type = class:
//     y : int = 16
// }

// f[T : type]()! {
//   ys = ns
//   y = ys.t
//   yy : y
//   return yy.y
// }

// main() : int! {
//   return f[int]();
// }
// )";
//   LLVMContext Context;
//   std::unique_ptr<ExecutionEngine> EE;
//   ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
//   MainSig CB = MainSig(EE->getFunctionAddress("main"));
//   ASSERT_TRUE(CB);
//   int result = CB();
//   ASSERT_EQ(result, 16);
// }


TEST(GoldDeduct, AliasesElaboration) {
  std::string Code = R"(
ns = namespace {
  x : int = 10

  t : type = class:
    y : int = 16
}

f[T : type]()! {
  ys = ns
  y = ys.t
  yy : y
  return yy.y
}

main() : int! {
  return f[int]();
}
)";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long, int, int, int) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}
