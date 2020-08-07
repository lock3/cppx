//=== GoldCarriesDependencyElab.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the carries_dependency attribute
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "clang/Basic/AttrKinds.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// ------------------------- carries_dependency --------------------------------
TEST(GoldCXX11Attr, CarriesDependency_OnFunction) {
  StringRef Code = R"(
[carries_dependency]
foo(i:int) : int
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
      hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCXX11Attr, CarriesDependency_OnParameter) {
  StringRef Code = R"(
foo(x:^int, y<carries_dependency>:^int):int
)";
  DeclarationMatcher ToMatch =
    parmVarDecl(hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCXX11Attr, CarriesDependency_AsCall) {
  StringRef Code = R"(
f(i:int)<carries_dependency()> : int
)";
  DeclarationMatcher ToMatch =
    functionDecl(hasAttr(clang::attr::CarriesDependency));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldCXX11Attr, CarriesDependency_Invalid) {
  StringRef Code = R"(
[carries_dependency]
Code:int
)";
  GoldFailureTest(Code);
}


// ------------------------------ deprecated -----------------------------------
TEST(GoldCXX11Attr, Deprecated_ClassDecl) {
  DeclarationMatcher ToMatch = cxxRecordDecl(hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
S<deprecated>:type = class
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
S:type = class
)", ToMatch)) << "line attribute";
}


TEST(GoldCXX11Attr, Deprecated_TypeAlias) {
  DeclarationMatcher ToMatch = typeAliasDecl(hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
PS<deprecated>:type = ^int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
PS:type = ^int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_TypeAliasTemplate) {
  DeclarationMatcher ToMatch = typeAliasTemplateDecl(
    hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
PS<deprecated>[T:type] : type = ^int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
PS[T:type]:type = ^int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_VarDecl) {
  DeclarationMatcher ToMatch = varDecl(
    hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
i<deprecated>:int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
i:int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_VarTemplateDecl) {
  DeclarationMatcher ToMatch = varTemplateDecl(
    hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
i<deprecated>[T:type]:int = 5
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
i[T:type]:int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_MemberOfAUnion) {
  DeclarationMatcher ToMatch = tagDecl(isUnion(), has(fieldDecl(
    hasAttr(clang::attr::Deprecated))
  ));
  ASSERT_TRUE(matches(R"(
U :type = union:
  n<deprecated>:int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
U :type = union:
  [deprecated]
  n:int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_FunctionDecl) {
  DeclarationMatcher ToMatch = functionDecl(
    hasName("f"),
    hasAttr(clang::attr::Deprecated));
  ASSERT_TRUE(matches(R"(
f()<deprecated>:void
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
f():void
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_NamespaceDecl) {
  DeclarationMatcher ToMatch = namespaceDecl(
    hasName("NS"), hasAttr(clang::attr::Deprecated)
  );
  ASSERT_TRUE(matches(R"(
NS<deprecated> = namespace:
  ;
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
NS = namespace:
  ;
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_EnumDecl) {
  DeclarationMatcher ToMatch = tagDecl(isEnum(),
    hasName("X"), hasAttr(clang::attr::Deprecated)
  );
  ASSERT_TRUE(matches(R"(
X<deprecated> : type = enum { ; }
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated]
X : type = enum { ; }
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_EnumMember_ImplicitValue) {
  DeclarationMatcher ToMatch = enumConstantDecl(
      hasAttr(clang::attr::Deprecated)
  );
  ASSERT_TRUE(matches(R"(
X : type= enum:
  A<deprecated>
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
X : type = enum:
  [deprecated]
  A
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_EnumMember_SetValue) {
  DeclarationMatcher ToMatch = enumConstantDecl(
      hasAttr(clang::attr::Deprecated)
  );
  ASSERT_TRUE(matches(R"(
X : type= enum:
  A<deprecated> = 4
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
X : type = enum:
  [deprecated]
  A = 4
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, Deprecated_WithMessage) {
  DeclarationMatcher ToMatch = functionDecl(
      hasAttr(clang::attr::Deprecated)
  );
  ASSERT_TRUE(matches(R"(
f()<deprecated("message")>:void
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[deprecated("message")]
f():void
)", ToMatch)) << "line attribute";
}

// ------------------------------ maybe_unused ---------------------------------
TEST(GoldCXX11Attr, MaybeUnused_ClassDecl) {
  DeclarationMatcher ToMatch = cxxRecordDecl(hasAttr(clang::attr::Unused));
  ASSERT_TRUE(matches(R"(
S<maybe_unused>:type = class
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[maybe_unused]
S:type = class
)", ToMatch)) << "line attribute";
}


TEST(GoldCXX11Attr, MaybeUnused_TypeAlias) {
  DeclarationMatcher ToMatch = typeAliasDecl(hasAttr(clang::attr::Unused));
  ASSERT_TRUE(matches(R"(
PS<maybe_unused>:type = ^int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[maybe_unused]
PS:type = ^int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_VarDecl) {
  DeclarationMatcher ToMatch = varDecl(
    hasAttr(clang::attr::Unused));
  ASSERT_TRUE(matches(R"(
i<maybe_unused>:int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[maybe_unused]
i:int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_MemberOfAUnion) {
  DeclarationMatcher ToMatch = tagDecl(isUnion(), has(fieldDecl(
    hasAttr(clang::attr::Unused))
  ));
  ASSERT_TRUE(matches(R"(
U :type = union:
  n<maybe_unused>:int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
U :type = union:
  [maybe_unused]
  n:int
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_FunctionDecl) {
  DeclarationMatcher ToMatch = functionDecl(
    hasName("f"),
    hasAttr(clang::attr::Unused));
  ASSERT_TRUE(matches(R"(
f()<maybe_unused>:void
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[maybe_unused]
f():void
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_EnumDecl) {
  DeclarationMatcher ToMatch = tagDecl(isEnum(),
    hasName("X"), hasAttr(clang::attr::Unused)
  );
  ASSERT_TRUE(matches(R"(
X<maybe_unused> : type = enum { ; }
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[maybe_unused]
X : type = enum { ; }
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_EnumMember_ImplicitValue) {
  DeclarationMatcher ToMatch = enumConstantDecl(
      hasAttr(clang::attr::Unused)
  );
  ASSERT_TRUE(matches(R"(
X : type= enum:
  A<maybe_unused>
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
X : type = enum:
  [maybe_unused]
  A
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, MaybeUnused_EnumMember_SetValue) {
  DeclarationMatcher ToMatch = enumConstantDecl(
      hasAttr(clang::attr::Unused)
  );
  ASSERT_TRUE(matches(R"(
X : type= enum:
  A<maybe_unused> = 4
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
X : type = enum:
  [maybe_unused]
  A = 4
)", ToMatch)) << "line attribute";
}

// ------------------------------- nodiscard -----------------------------------
TEST(GoldCXX11Attr, NoDiscard_Class) {
  DeclarationMatcher ToMatch = cxxRecordDecl(
      hasAttr(clang::attr::WarnUnusedResult)
  );
  ASSERT_TRUE(matches(R"(
error_info <nodiscard> : type = class { ; }
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[nodiscard]
error_info : type = class { ; }
)", ToMatch)) << "line attribute";
}

TEST(GoldCXX11Attr, NoDiscard_FunctionDecl) {
  DeclarationMatcher ToMatch = functionDecl(
      hasAttr(clang::attr::WarnUnusedResult)
  );
  ASSERT_TRUE(matches(R"(
foo()<nodiscard> : ^int
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[nodiscard]
foo() : ^int
)", ToMatch)) << "line attribute";
}

// ------------------------------- noreturn ------------------------------------

TEST(GoldCXX11Attr, NoReturn_FunctionDecl) {
  DeclarationMatcher ToMatch = functionDecl(
      hasAttr(clang::attr::CXX11NoReturn)
  );
  ASSERT_TRUE(matches(R"(
abort()<noreturn> : void
)", ToMatch)) << "name attribute";

  ASSERT_TRUE(matches(R"(
[noreturn]
abort() : void
)", ToMatch)) << "line attribute";
}