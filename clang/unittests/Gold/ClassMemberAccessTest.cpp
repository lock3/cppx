//=== ClassMemberAccessTest.cpp - Testing member Access attributes -==//
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

TEST(ClassParsing, Access_PrivateMember) {
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

// TEST(ClassParsing, Access_PrivateMember_NoType) {
//   StringRef Code = R"(
// c : type = class:
//   x <private> = 9
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }


TEST(ClassParsing, Access_PublicMember) {
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

TEST(ClassParsing, Access_ImplicitPublicMember) {
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

TEST(ClassParsing, Access_ProtectedMember) {
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


// Class Member function Access testing.
TEST(ClassParsing, Access_PrivateMemberFunctions) {
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

TEST(ClassParsing, Access_ProtectedMemberFunctions) {
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

TEST(ClassParsing, Access_PublicMemberFunctions) {
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

TEST(ClassParsing, Access_ImplicitPublicMemberFunctions) {
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



// Testing constructor Access test.
TEST(ClassParsing, Access_PrivateConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <private> : void!
    return
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ProtectedConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <protected> : void!
    return
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_PublicConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <public> : void! 
    return 
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ImplicitPublicConstructor) {
  StringRef Code = R"(
c : type = class:
  constructor() : void! 
    return
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

// Testing destructor Access.
TEST(ClassParsing, Access_PrivateDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <private> : void! 
    return
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ProtectedDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <protected> : void! 
    return 
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_PublicDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <public> : void! 
    return
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ImplicitPublicDestructor) {
  StringRef Code = R"(
c : type = class:
  destructor(): void! 
    return 
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


// Testing nested type Access.
TEST(ClassParsing, Access_PublicType) {
  StringRef Code = R"(
c : type = class:
  c2<public> : type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ImplicitPublicType) {
  StringRef Code = R"(
c : type = class:
  c2 : type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_PrivateType) {
  StringRef Code = R"(
c : type = class:
  c2 <private>: type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


TEST(ClassParsing, Access_ProtectedType) {
  StringRef Code = R"(
c : type = class:
  c2 <protected>: type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


// Testing nested templated types
TEST(ClassParsing, Access_PublicTemplateType) {
  StringRef Code = R"(
c : type = class:
  c2[T:type]<public> : type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_ImplicitPublicTemplateType) {
  StringRef Code = R"(
c : type = class:
  c2[T:type] : type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Access_PrivateTemplateType) {
  StringRef Code = R"(
c : type = class:
  c2 [T:type]<private>: type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


TEST(ClassParsing, Access_ProtectedTemplateType) {
  StringRef Code = R"(
c : type = class:
  c2 [T:type]<protected>: type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isProtected()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}


// Testing templated member function access?
TEST(ClassParsing, Access_PrivateNestedClass) {
  StringRef Code = R"(
c : type = class:
  c2 <private>: type = class:
    z : int
    y : bool
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code, ClassC));
}
