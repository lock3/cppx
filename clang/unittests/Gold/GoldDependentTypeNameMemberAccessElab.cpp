//=== GoldDependentTypeNameMemberAccessElab.cpp ----------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for a dependent type name within a member access
//  expression.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldDependentTypeNameWithinMemberAccess, ConvertToBaseClass) {
  StringRef Code = R"GOLD(

new_allocator[T:type] : type = class{
  rebind[T1:type] : type = class { other : type = new_allocator[T1]; }

  init()<noexcept>:void !{ }
}

_Vector_Base[T:type, Alloc:type] : type = class:
  T_alloc_type : type = Alloc.rebind[T].other
  _Vector_Impl : type = class(T_alloc_type) {
    constructor()! {
      static_cast[^T_alloc_type](this).init();
    }
  }
  impl: _Vector_Impl;
temp() : void !{
  x:_Vector_Base[int, new_allocator[int]];
}
)GOLD";
  auto ToMatch = callExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TODO: Create a special case where this uses a chain of nested accessors.
