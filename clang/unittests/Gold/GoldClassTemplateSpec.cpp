//===- unittest/Gold/GoldClassTemplateSpec.cpp ----------------------------===//
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

TEST(ClassTemplateSpec, Basic) {
  StringRef Code = R"(
is_void[T : type] : type = class:
  value : bool = false

is_void[void] : type = class:
  value : bool = true

main() : int!
  v : is_void[void]
  return v.value
)";

  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}

TEST(ClassTemplateSpec, Partial) {
  StringRef Code = R"(
A[T : type, U : type] : type = class:
  i : int = 0

A[T : type, U : type][^T, U] : type = class:
  i : int = 1

main() : int!
  a : A[^int, float]
  return a.i
)";

  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 1);
}
