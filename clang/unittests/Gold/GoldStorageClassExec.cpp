//===- unittest/Gold/GoldStorageClassExec.cpp ----===//
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



TEST(GoldStorageClassExec, Static_GlobalVariable) {
  StringRef Code = R"(
x <static> : int = 4
main() : int!
  return x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 4);
}

TEST(GoldStorageClassExec, Static_LocalVariable) {
  StringRef Code = R"(
foo() : int!
  x <static> : int = 4
  x = x + 1
  return x
main() : int!
  foo()
  return foo()
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 6);
}


TEST(GoldStorageClassExec, Static_MemberOfAClass) {
  StringRef Code = R"(
c : type = class:
  x <static><inline>: int = 4
  foo() : int!
    x = x + 1
    return x
  

main() : int!
  q:c
  q.foo()
  return c.x
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getFunctionAddress("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}
