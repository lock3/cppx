//===- unittest/Gold/GoldTemplateDefaultParamsExec.cpp --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldTemplateDefaultParams, NonType) {
  StringRef Code = R"(
t[x : int = 10] : type = class:
  y : const int = x

main() : int!
  T : t[]
  return T.y
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 10);
}

TEST(GoldTemplateDefaultParams, Type) {
  StringRef Code = R"(
t[T : type = int] : type = class:
  y : T = 10

main() : int!
  T : t[]
  return T.y
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 10);
}

TEST(GoldTemplateDefaultParams, Template) {
  StringRef Code = R"(
test[T : type] : type = class:
  x : T = T(10)

F[U : type, Container[T : type] : type = test] : type = class:
  c : Container[U]

main() : int!
  f : F[int, test]
  return f.c.x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 10);
}
