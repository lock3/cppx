//=== ClassMemberVisibilityTest.cpp - Testing member visibility attributes -==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements some of the tests for the gold language parser.
//
//===----------------------------------------------------------------------===//


#include "ParseUtil.h"
#include "ASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassParsing, Visibility_PrivateMember) {
  StringRef Code = R"(
c : type = class:
  x <private>: int
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


TEST(ClassParsing, Visibility_PublicMember) {
  StringRef Code = R"(
c : type = class:
  x <public>: int
)";
  DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPublic())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ImplicitPublicMember) {
  StringRef Code = R"(
c : type = class:
  x : int
)";
  DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPublic())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ProtectedMember) {
  StringRef Code = R"(
c : type = class:
  x <protected>: int
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isProtected())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


// Class Member function visibility testing.
TEST(ClassParsing, Visibility_PrivateMemberFunctions) {
  StringRef Code = R"(
c : type = class:
  foo()<private>: int!
    return 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ProtectedMemberFunctions) {
  StringRef Code = R"(
c : type = class:
  foo()<protected>: int!
    return 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_PublicMemberFunctions) {
  StringRef Code = R"(
c : type = class:
  foo()<public>: int!
    return 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ImplicitPublicMemberFunctions) {
  StringRef Code = R"(
c : type = class:
  foo(): int!
    return 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}



// Testing constructor visibility test.
TEST(ClassParsing, Visibility_PrivateConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <private> : void!
  

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ProtectedConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <protected> : void!

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_PublicConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <public> : void!
  
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ImplicitPublicConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <protected>: void!

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

// Testing destructor visibility.
TEST(ClassParsing, Visibility_PrivateDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <private> : void!

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ProtectedDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <protected> : void!

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_PublicDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <public> : void!

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Visibility_ImplicitPublicDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor(): void!
  
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}