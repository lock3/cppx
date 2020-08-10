//=== FunctionTemplateSpecExec.cpp - Execution for Gold Function Tmp Spec --==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for execution of function
//  template specializations.
//
//===----------------------------------------------------------------------===//

#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace gold;
using namespace llvm;

TEST(GoldFunctionTemplateSpec, NonExplicitCall) {
  StringRef Code = R"(
f[T : type](x : T) : T!
  return T()

f[](x : int) : int!
  return x

f[](x : double) : double!
  return 24.0

main() : int!
  return f[int](42)
)";

  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
}
