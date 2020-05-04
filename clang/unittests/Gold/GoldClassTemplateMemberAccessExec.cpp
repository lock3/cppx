//===- unittest/Gold/GoldClassTemplateMemberAccessExec.cpp ----------------===//
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

TEST(ClassTemplateInstance, MemberAccess_NonDependentMember) {
  StringRef Code = R"(
c[T:type] : type = class:
  z : T
  y : bool = 0

main() : int!
  q : c[int]
  return q.y
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 0);
}

TEST(ClassTemplateInstance, MemberAccess_DependentMember) {
  StringRef Code = R"(
c[T:type] : type = class:
  z : T
  y : bool = 0

main() : int!
  q : c[int]
  q.z = 5
  return q.z
)";
  LLVMContext Context;
  std::unique_ptr<ExecutionEngine> EE;
  ASSERT_TRUE(CompileGoldCode(Context, Code, EE));
  MainSig CB = MainSig(EE->getPointerToNamedFunction("main"));
  ASSERT_TRUE(CB);
  int result = CB();
  ASSERT_EQ(result, 5);
}