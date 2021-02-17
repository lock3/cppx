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

TEST(BlueOperatorOverloading, Equality) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator==:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator==")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, Inequality) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator!=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator!=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, Less) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator<")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, Greater) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator>")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueOperatorOverloading, LessEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator<=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator<=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, GreaterEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator>=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator>=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, UnaryPlus) {
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

TEST(BlueOperatorOverloading, UnaryMinus) {
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

TEST(BlueOperatorOverloading, BinaryPlus) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator+")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, BinaryMinus) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator-")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, Multiply) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator*")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, Divide) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator/")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, Modulus) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator%")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, PlusEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator+=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator+=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MinusEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator-=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator-=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, MultiplyEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator*=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator*=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, DivideEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator/=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator/=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueOperatorOverloading, ModulusEqual) {
  StringRef Code = R"BLUE(
type C : class = {
  func operator%=:(in this, other) bool = { return false; }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("operator%=")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(BlueOperatorOverloading, LeftShift) {
//   StringRef Code = R"BLUE(
// type C : class = {
//   func operator<<:(in this, other) bool = { return false; }
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

// TEST(BlueOperatorOverloading, RightShift) {
//   StringRef Code = R"BLUE(
// type C : class = {
//   func operator>>:(in this, other) bool = { return false; }
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
