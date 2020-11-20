//=== GoldUsing.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing using declarations and using directives
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldUsingDeclaration, Decl) {
  StringRef Code = R"(
ns = namespace {
  y : int;
}

main() : int! {
  using {ns.y};
}
)";

  DeclarationMatcher Match =
    usingDecl(hasAnyUsingShadowDecl(hasName("y")));
  ASSERT_TRUE(matches(Code.str(), Match));
}

TEST(GoldUsingDeclaration, Ambiguous) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
}

ys = namespace {
  y() : int!
    return 0
}

main() : int! {
  using {ys.y, ns.y};
  return y()
}
)";

  GoldFailureTest(Code);
}

TEST(GoldUsingDeclaration, Overload) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
  y(p : int) : int!
    return p
}

main() : int! {
  using {ns.y};
  x = y(42)
}
)";

  DeclarationMatcher X = varDecl(hasName("x"),
                                 hasType(asString("int")));
  ASSERT_TRUE(matches(Code.str(), X));
}


TEST(GoldUsingDeclaration, OverloadMistype) {
  StringRef Code = R"(
ns = namespace {
  y() : int!
    return 0
  y(p : int) : int!
    return p
}

main() : int! {
  using {ns.y};
  return y
}
)";

  GoldFailureTest(Code);
}

TEST(GoldUsingDeclaration, VarInClass) {
  StringRef Code = R"(
foo = class {
  baz : int = 3
}

bar = class {
  baz : char = 'a'
}

qux = class(foo, bar) {
  using {bar.baz}
}

main() : int!
  corge : qux
  test = corge.baz
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldUsingDeclaration, FnInClass) {
  StringRef Code = R"(
foo = class {
  baz(f : int)!
    return f
}

bar = class {
  baz(f : int)!
    return 'a'
}

qux = class(foo, bar) {
  using {bar.baz}
}

main() : int!
  corge : qux
  test = corge.baz(4)
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldUsingDeclaration, OverloadInClass) {
  StringRef Code = R"(
foo = class {
  baz(f : int)!
    return f
  baz()!
    return 10
}

bar = class {
  baz()!
    return 'a'
  baz(f : int)!
    return 'a'
}

qux = class(foo, bar) {
  using {bar.baz}
}

main() : int!
  corge : qux
  test = corge.baz(4)
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldUsingDeclaration, TypeInClass) {
  StringRef Code = R"(
foo = class {
  baz : int = 3
  quux = class {
    x : int = 11
  }
}

bar = class {
  baz : int = 4
  quux = class {
    x : char = 'a'
  }
}

qux = class(foo, bar) {
  using {bar.quux}
}

main() : int!
  corge : qux.quux
  test = corge.x
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldUsingDeclaration, UnresolvedInClass) {
  StringRef Code = R"(
foo[T : type] = class {
  baz(f : T)!
    return f
  baz()!
    return 10
}

bar[T : type] = class {
  baz()!
    return 'a'
  baz(f : T)!
    return f
}

qux[T : type, U : type] = class(foo[U], bar[T]) {
  using {bar[T].baz}
}

main() : int !
  q : qux[int, char]
  test = q.baz()
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}


TEST(GoldUsingDirective, Basic) {
  StringRef Code = R"(
ns = namespace {
  x : int = 10
}

ys = namespace {
  x = 'a'
}

main() : int!
  using {ys}
  test = x
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test)); 
}

TEST(GoldUsingDirective, DirectBasic) {
  StringRef Code = R"(
ns = namespace {
  x : int = 10
}

ys = namespace {
  x = 'a'
}

main() : int!
  using {ys.x}
  test = x
)";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test)); 
}

TEST(GoldUsingDirective, InClass) {
  StringRef Code = R"(
ns = namespace {
  x : int = 10
}

s = class {
  using {ns}
})";

  GoldFailureTest(Code.str());
}

TEST(GoldUsingDirective, DirectInClass) {
  StringRef Code = R"(
ns = namespace {
  x : int = 10
}

s = class {
  using {ns.x}
})";

  GoldFailureTest(Code.str());
}

TEST(GoldUsingDirective, Overload) {
  StringRef Code = R"(
ys : namespace = namespace {
  y() : int!
    return 10
  y(p : char) :char!
    return p
  x()!
    return 10
}

main() : int!
  using {ys.y}
  test = y('a')
})";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test)); 
}

TEST(GoldUsingDirective, Ambiguous) {
  StringRef Code = R"(
ys : namespace = namespace {
  y() : int!
    return 10
  y(p : int) :int!
    return p
  x()!
    return 10
}

ns = namespace {
  y() : int!
    return 13
}

main() : int!
  using {ys.y, ns.y}
  return y()
})";

  GoldFailureTest(Code.str());
}

TEST(GoldUsingDirective, GlobalNNS) {
  StringRef Code = R"(
foo() : char!
  return 'a'

_ = namespace {
  foo() : int!
    return 42
}

main() : int!
  using {.foo}
  test = foo()
})";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  ASSERT_TRUE(matches(Code.str(), Test));
}

TEST(GoldUsingDirective, GlobalNNSInClass) {
  StringRef Code = R"(
foo() : int!
  return 1

A = class {
  foo() : int!
    return 0
}

B = class(A<protected>) {
  using {.foo}
}
})";

  GoldFailureTest(Code.str());
}

TEST(GoldAnonymousNamespace, Basic) {
  StringRef Code = R"(
_ = namespace {
  y(x : char) : char!
    return x
  y() : int!
    return 13
}

main() : int!
  test = y('a');
})";

  DeclarationMatcher Test =
    varDecl(hasName("test"), hasType(asString("unsigned char")));
  DeclarationMatcher Anon =
    namespaceDecl(isAnonymous());
  ASSERT_TRUE(matches(Code.str(), Test));
  ASSERT_TRUE(matches(Code.str(), Anon));
}
