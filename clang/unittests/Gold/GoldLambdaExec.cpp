//=== GoldLambdaExec.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Compiling lambda expressions.
//
//===----------------------------------------------------------------------===//

#include "GoldCompileRun.h"
#include "GoldParseUtil.h"

using namespace llvm;
using namespace gold;

TEST(GoldLambdaExec, Variadic) {
  StringRef Code = R"(
list = lambda{}(xs : auto...) {
  return lambda{=}(access : auto) { return access(xs...); };
}

length = lambda{}(xs : auto) {
  return xs(lambda{=}(z : auto...) { return sizeof...(z); });
}

main() : int!
  return length(list(1, 2, 4))
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int Result = CB();
  ASSERT_EQ(Result, 3);
}
