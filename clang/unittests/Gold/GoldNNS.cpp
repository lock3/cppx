//=== GoldWNNS.cpp - Test Gold nested name specifiers ----------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for parsing of nested name specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(NNS, SimpleNNS) {
  StringRef Code = R"(
ns : namespace = namespace:
  ns2 : namespace = namespace:
    x : int = 1
  y : int = 1

main() : int!
  return ns.ns2.x + ns.y
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, GlobalNNS) {
  StringRef Code = R"(
x : int = 9

main() : int!
  x : int = 10
  return x + .x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, GlobalNested) {
  StringRef Code = R"(
ns : namespace = namespace:
  x : int = 0

main() : int!
  x : int = 10
  return .ns.x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, AutoNNS) {
  StringRef Code = R"(
ns = namespace:
  x : int = 5

main() : int!
  x : int = 4
  return .ns.x
)";

  SimpleGoldParseTest(Code);
}

TEST(NNS, PrefixBaseSpecifier) {
  StringRef Code = R"(
A : type = class:
  i : int = 9

B: type = class (A):
  ;

main() : int!
  b : B
  result = b.(A)i
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(NNS, PrefixBaseSpecifierThroughPointer) {
  StringRef Code = R"(
A : type = class:
  i : int = 9

B: type = class (A):
  ;

main() : int!
  b : ^B
  result = b.(A)i
)";

  ASSERT_TRUE(matches(Code.str(), memberExpr(member(hasName("i")))
    ));
}

TEST(NNS, CallingAnExplicitBaseClassMethod) {
  StringRef Code = R"(
A : type = class:
  i : int = 9
  foo() : void!
    ;

B: type = class (A):
  foo() : void!
    ;

main() : int!
  b : ^B
  result = b.(A)i
  (b.(A)foo)()
)";

  ASSERT_TRUE(matches(Code.str(), memberExpr(member(hasName("foo")))
    ));
}

TEST(NNS, CallingAnExplicitBaseClassMethodWithOverloadedFunction) {
  StringRef Code = R"(
A : type = class:
  i : int = 9
  foo() : void!
    ;

  foo(x:int) : void!
    ;

B: type = class (A):
  foo() : void!
    ;

main() : int!
  b : ^B
  result = b.(A)i
  (b.(A)foo)()
)";

  ASSERT_TRUE(matches(Code.str(), memberExpr(member(hasName("foo")))
    ));
}


// TEST(NNS, DoubleDisambiguation) {
//   StringRef Code = R"(
// D : type = class:
//   x : int
// C : type = class(D):
//   something : int

// A : type = class:
//   i : int = 9
//   x : C

// B: type = class (A):
//   ;

// main() : int!
//   b : ^B
//   result = b.(A)x.(D)x

// )";
//   // TODO: I need to fix this so it disambiguates as expected.
//   ASSERT_TRUE(matches(Code.str(), memberExpr(member(hasName("i")))
//     ));
// }

TEST(NNS, PrefixBaseSpecifierTempl) {
  StringRef Code = R"(
A[T : type] : type = class:
  i : T = 9

B[T : type] : type = class (A[T]):
  ;

main() : int!
  b : B[int]
  result = b.(A[int])i
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(NNS, PrefixBaseSpecifierRefParam) {
  StringRef Code = R"(
Base0 : type = class:
  x : int = 28

DerivedEx0 : type = class(Base0):
  ;

f0(I:ref DerivedEx0):int!
  return I.x

main() : int!
  d : DerivedEx0
  result = f0(d)
  result2 = d.(Base0)x
  result3 = d.(.Base0)x
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  StatementMatcher Matcher2(hasDescendant(
                             varDecl(hasName("result2"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  StatementMatcher Matcher3(hasDescendant(
                             varDecl(hasName("result3"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
  ASSERT_TRUE(matches(Code.str(), Matcher2));
  ASSERT_TRUE(matches(Code.str(), Matcher3));
}

TEST(NNS, PrefixBaseSpecifierNested) {
  StringRef Code = R"(
Base0 : type = class:
  x : int = 29

Base1 : type = class(Base0):
  y : int

DerivedEx1 : type = class(Base1):
  ;

f1(I:ref DerivedEx1):int!
  return I.(Base1.Base0)x

main() : int!
  d : DerivedEx1
  result = f1(d)
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(NNS, PrefixBaseSpecifierNestedTemplate) {
  StringRef Code = R"(
Base0[T : type] : type = class:
  x : T = 20

Base1 : type = class:
  y : int

Base2 : type = class(Base1):
  z : int

Base3[T:type] : type = class(Base0[T]):
  someeType : type = int
  a : T = T() + 1

DerivedEx2 : type = class(Base2, Base3[int]):
  ;

f(I:ref DerivedEx2):int!
  return I.(Base3[int].Base0[int])x

main() : int!
  d : DerivedEx2
  result = f(d)
  result2 = d.(Base3[int])a
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  StatementMatcher Matcher2(hasDescendant(
                             varDecl(hasName("result2"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
  ASSERT_TRUE(matches(Code.str(), Matcher2));
}

TEST(NNS, RawBaseSpecifier) {
  StringRef Code = R"(
A : type = class:
  i : int

B : type = class:
  i : float

C : type = class(A):
  foo() : void!
    (A)i = 5

main() : int!
  c : C
  c.foo()
  result = c.(A)i
)";

  StatementMatcher Matcher(hasDescendant(
                             varDecl(hasName("result"),
                                     hasType(asString("int"))
                               )
                             )
                   );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}
