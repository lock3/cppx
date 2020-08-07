//===- unittest/Gold/GoldFunctionTemplateExec.cpp ------------------------===//
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

TEST(GoldFunctionTemplate, Execution) {
  StringRef Code = R"(
f[z : int]() : int!
  return z

main() : int!
  return f[4]()
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));

#if 0
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 4);
#endif
}
