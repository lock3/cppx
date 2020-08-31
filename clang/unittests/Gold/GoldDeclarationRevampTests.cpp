//=== GoldDeclarationRevampTest.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration of the constexpr version of an if statement.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;
#if 0
TEST(GoldDeclRevamp, EverythingTest) {
  StringRef Code = R"(
foo() : void
foo() = 0
foo() : void!
  ;
foo[T:type]() : void
foo[T:type]() = 0
foo[T:type]() : void!
  ;

x(p:int) : void
x(p:int) : void = 0
x(p:int) : void!
  ;
x[T:type](p:int) : void
x[T:type](p:int) = 0
x[T:type](p:int) : void!
  ;
foo[T:type]() : void
foo[T:type]() = 0
foo[T:type]() : void!
  ;

foo[][int]() : void
foo[][]() : void
x[T:type][^T] = class

x = class
x : type = class
x : expr = class
x[T:type] : type = class
x = class:
  ;
x : type = class:
  ;
x : expr = class:
  ;
x[T:type] : type = class:
  ;
x = class(a):
  ;
x : type = class(a):
  ;
x : expr = class(a):
  ;
x[T:type] : type = class(a):
  ;

x = union
x : type = union
x : expr = union
x[T:type] : type = union
x = union:
  ;
x : type = union:
  ;
x : expr = union:
  ;
x[T:type] : type = union:
  ;

x = enum
x : type = enum
x : expr = enum
x[T:type] : type = enum
x = enum:
  ;
x : type = enum:
  ;
x : expr = enum:
  ;
x[T:type] : type = enum:
  ;
x = enum(int)
x : type = enum(int)
x : expr = enum(int)
x[T:type] : type = enum(int)
x = enum(int):
  ;
x : type = enum(int):
  ;
x : expr = enum(int):
  ;
x[T:type] : type = enum(int):
  ;

# Variable type
x[T:type] : T = T(y)
x[T:type] = T(y)

# Type declaration
x[T:type] : type = int
x[T:type] = bool

x : namespace = namespace:
  ;
x : namespace = x
x.y = ns
x.z = namespace:
  ;

# Anonymous namespaces.
_ = namespace:
  ;

x : type = int
x = int
x : type = int

.foo() : void
.foo() = 0

x.y(x:int) : void!
  ;
x.y(x:int) : void!
  ;
x.y(x:int)!
  ;

for (x in range):
  ;

x.y = 5
x[t:type].y : int
x[int].y : int
x[t:type][t, int].y : int
.x.y = 5

)";
  SimpleGoldParseTest(Code);
}

TEST(GoldDeclRevamp, TemplateNNSEverythingTest) {
  StringRef Code = R"(
x[foo].foo() : void!
  ;

x[z:int][bar, z].foo() : void!
  ;

x[z:int].foo() : void!
  ;

.x[z:int][bar, z].foo() : void!
  ;

.x[z:int][bar, z].foo = enum:
  ;

.foo() : void!
  ;
)";
  SimpleGoldParseTest(Code);
}

TEST(GoldDeclRevamp, FunctionDefExamples) {
  StringRef Code = R"(
x[foo].foo()!
  ;
)";
  SimpleGoldParseTest(Code);
}


TEST(GoldDeclRevamp, TemplateWithinATemplate) {
  StringRef Code = R"(
a.b.c[bar].d.x[foo].foo[T:type]()!
  ;
)";
  SimpleGoldParseTest(Code);
}
#endif