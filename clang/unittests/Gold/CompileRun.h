//===- unittest/Gold/CompileRun.h - Matcher tests helpers ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UNITTESTS_GOLD_ASTMATCHERSTEST_H
#define LLVM_CLANG_UNITTESTS_GOLD_ASTMATCHERSTEST_H


#include "llvm/ExecutionEngine/ExecutionEngine.h"


#include "llvm/IR/LLVMContext.h"
#include "gtest/gtest.h"

namespace gold {
testing::AssertionResult CompileGoldCode(llvm::LLVMContext& Context,
    llvm::StringRef code, std::unique_ptr<llvm::ExecutionEngine>& EE);
}
#endif