//=== GoldArrayExec.cpp ----------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements compiled/executed tests for array elaboration in
//  Gold.
//
//===----------------------------------------------------------------------===//

#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldArray, MultidimensionalArrayExec) {
  StringRef Code = R"(
x : [1, 1, 1]int = array{array{array{7}}}
main() : int!
  return x[0, 0, 0]
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 7);
}

TEST(GoldArray, MultidimensionalAutoArrayExec) {
  StringRef Code = R"(
main() : int!
  a = array{array{1,2}, array{3,4}}
  return a[1, 1]
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 4);
}
