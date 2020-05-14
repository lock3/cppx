//===- unittest/Gold/GoldBoolImplicitConversionInit.cpp -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ExecutionEngine/GenericValue.h"

#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldBool, InitializationTest_AsTrue) {
  StringRef Code = R"(
main() : int!
  b : bool = 1
  return b
)";
  LLVMContext Context;
  std::unique_ptr<llvm::ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig Fn = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(Fn);
  int Result = Fn();
  ASSERT_EQ(Result, 1);
}

TEST(GoldBool, InitializationTest_AsFalse) {
  StringRef Code = R"(
main() : int!
  b : bool = 0
  return b
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 0);
}
