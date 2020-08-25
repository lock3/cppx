//===- unittest/Gold/GoldDeclaratorElab.cpp -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Errors associated with declarator elaboration.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldDeclarator, FunctionDecl_FunctionNestedInName) {
  StringRef Code = R"(
foo()<const>():void
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_InvalidDeclaratorWithinEnum) {
  StringRef Code = R"(
x : type = enum:
  a : int
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_DuplicateName) {
  StringRef Code = R"(
x : type = enum:
  a
  a
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_FunctionDefinition) {
  StringRef Code = R"(
x : type = enum:
  foo()!
    ;
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_FunctionDecl) {
  StringRef Code = R"(
x : type = enum:
  foo()
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_Func) {
  StringRef Code = R"(
x : type = enum:
  f!
    ;
)";
  GoldFailureTest(Code);
}


TEST(GoldDeclarator, EnumConstantDecl_Template) {
  StringRef Code = R"(
x : type = enum:
  a[T:type] : T = 3
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_Specialization) {
  StringRef Code = R"(
x : type = enum:
  a[int] : T = 3
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_GlobalNamespecifier) {
  StringRef Code = R"(
x : type = enum:
  .a
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_GlobalNamespecifier_WithAssignment) {
  StringRef Code = R"(
x : type = enum:
  .a = 4
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_NestedNameSpecifier) {
  StringRef Code = R"(
x : type = enum:
  q.a
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_NestedNameSpecifier_WithAssignment) {
  StringRef Code = R"(
x : type = enum:
  q.a = 3
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_PartialSpecialization) {
  StringRef Code = R"(
x : type = enum:
  a[T:type][^T]
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_PartialSpecialization_WithAssignment) {
  StringRef Code = R"(
x : type = enum:
  a[T:type][^T] = 3
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, EnumConstantDecl_NamespaceDecl) {
  StringRef Code = R"(
x : type = enum:
  a = namespace:
    ;
)";
  GoldFailureTest(Code);
}

TEST(GoldDeclarator, ClassDecl_ForwardDeclaration) {
  StringRef Code = R"(
w : type = class:
  ;
x : type = class(w)
)";
  GoldFailureTest(Code);
}

// // FIXME: Move into GoldEnumDecl.
// TEST(GoldDeclarator, EnumConstantDecl_NamespaceAlias) {
//   StringRef Code = R"(
// w : namespace = namespace:
//   ;

// x : type = enum:
//   a = w
// )";
//   GoldFailureTest(Code);
// }