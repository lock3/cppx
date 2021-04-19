//=== BlueVirtualMethodElab.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration of type aliases
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueVirtual, VirtualMemberFunctionDecl) {
  StringRef Code = R"(
c : type = {
  foo: (virtual in this, i:int) -> int = {
    return 4;
  }
})";
  auto ToMatch = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isVirtual()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueVirtual, MultipleVirtualSpecified) {
  StringRef Code = R"(
c : type = {
  foo: (virtual virtual in this, i:int) -> int = {
    return 4;
  }
})";
  BlueFailureTest(Code);
}


TEST(BlueVirtual, FinalMemberFunctionDecl) {
  StringRef Code = R"(

a : type = {
  foo: (virtual in this, i:int) -> int = {
    return 4;
  }
}
c : type = {
  :a;
  foo: (override final in this, i:int) -> int = {
    return 4;
  }
})";
  auto ToMatch = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isFinal()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueVirtual, MultipleFinalGiven) {
  StringRef Code = R"(

a : type = {
  foo: (virtual in this, i:int) -> int = {
    return 4;
  }
}
c : type = {
  :a;

  foo: (final final in this, i:int) -> int = {
    return 4;
  }
})";
  BlueFailureTest(Code);
}

TEST(BlueVirtual, OverrideMemberFunctionDecl) {
  StringRef Code = R"(
a : type = {
  foo: (virtual in this, i:int) -> int = {
    return 4;
  }
}
c : type = {
  :a;
  foo: (override in this, i:int) -> int = {
    return 4;
  }
})";
  auto ToMatch = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isOverride()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueVirtual, InvalidOverride) {
  StringRef Code = R"(
c : type = {
  foo: (override in this, i:int) -> int = {
    return 4;
  }
})";
  BlueFailureTest(Code);
}

TEST(BlueVirtual, MissingOverrideMemberFunctionDecl) {
  StringRef Code = R"(
a : type = {
  foo: (virtual in this, i:int) -> int = {
    return 4;
  }
}
c : type = {
  :a;
  foo: (in this, i:int) -> int = {
    return 4;
  }
})";
  BlueFailureTest(Code);
}