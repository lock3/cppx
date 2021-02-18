//=== BlueConstructorElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for constructor declaration and use.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueOperatorOverloading, EqualityMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator==:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator=="), hasType(asString("_Bool (const struct C &) const"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, EqualityFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator==:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator=="),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, InequalityMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator!=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator!=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, InequalityFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator!=:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator!="),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator<")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator<:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator<"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator>")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator>:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator>"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, LessEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator<=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessEqualFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator<=:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator<="),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator>=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterEqualFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator>=:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator>="),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, UnaryPlusMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+:(in this) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator+")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, UnaryMinusMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-:(in this) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator-")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryPlusMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator+")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryPlusFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator+:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator+"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryMinusMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator-")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryMinusFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator-:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator-"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator*")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator*:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator*"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator/")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator/:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator/"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator%")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusFreeDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator%:(in x:C, in y:C) bool = { return false; }
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator%"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, PlusEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator+=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MinusEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator-=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator*=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator/=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusEqualMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%=:(in this, in other:C) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator%=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(BlueOperatorOverloading, LeftShiftMemberDecl) {
//   StringRef Code = R"BLUE(
// type C : class = {
//   func operator<<:(in this, in other:C) bool = { return false; }
// }
// )BLUE";
//   auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
//     hasName("C"),
//     has(cxxMethodDecl(isDefaultConstructor(),
//                   unless(isImplicit())))
//     ))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(BlueOperatorOverloading, RightShiftMemberDecl) {
//   StringRef Code = R"BLUE(
// type C : class = {
//   func operator>>:(in this, in other:C) bool = { return false; }
// }
// )BLUE";
//   auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
//     hasName("C"),
//     has(cxxMethodDecl(isDefaultConstructor(),
//                   unless(isImplicit())))
//     ))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// -----------------------------------------------------------------------------
//                          Operator lookup tests.
// -----------------------------------------------------------------------------
TEST(BlueOperatorOverloading, EqualityMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator==:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x == y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("=="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, EqualityFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator==:(in x:C, in y:C) bool = { return false; }
foo:(in x:C, in y:C) = {
  return x == y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("=="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, InequalityMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator!=:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x != y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("!="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, InequalityFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator!=:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x != y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("!="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x < y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("<"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator<:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x < y;
}
)BLUE";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator<"),
      hasType(asString("_Bool (const struct C &, const struct C &)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x > y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName(">"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator>:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x > y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName(">"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, LessEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<=:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x <= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("<="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, LessEqualFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator<=:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x <= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("<="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>=:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x >= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName(">="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterEqualFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator>=:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x >= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName(">="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, UnaryPlusMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+:(in this) bool = { return false; }
}
foo:(x:C) = {
  return +x;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("+"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, UnaryMinusMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-:(in this) bool = { return false; }
}
foo:(x:C) = {
  return -x;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("-"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryPlusMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x + y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("+"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryPlusFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator+:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x + y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("+"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryMinusMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x - y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("-"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryMinusFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator-:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x - y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("-"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x * y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("*"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator*:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x * y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("*"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x / y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("/"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator/:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x / y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("/"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%:(in this, in other:C) bool = { return false; }
}
foo:(x:C,y:C) = {
  return x % y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("%"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusFreeUse) {
  StringRef Code = R"BLUE(
type C : class = { }
func operator%:(in x:C, in y:C) bool = { return false; }
foo:(x:C,y:C) = {
  return x % y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("%"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, PlusEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+=:(in this, in other:C) bool = { return false; }
}
foo:(inout x:C,y:C) void = {
   x += y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("+="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MinusEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-=:(in this, in other:C) bool = { return false; }
}
foo:(inout x:C,y:C) void = {
   x -= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("-="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*=:(in this, in other:C) bool = { return false; }
}
foo:(inout x:C,y:C) void = {
   x *= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("*="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/=:(in this, in other:C) bool = { return false; }
}
foo:(inout x:C,y:C) void = {
   x /= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("/="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusEqualMemberUse) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%=:(in this, in other:C) bool = { return false; }
}
foo:(inout x:C,y:C) void = {
   x %= y;
}
)BLUE";
  auto ToMatch = cxxOperatorCallExpr(hasOverloadedOperatorName("%="));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}