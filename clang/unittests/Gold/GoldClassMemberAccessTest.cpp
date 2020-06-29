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


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

// TODO: I need to create failure tests for public, private, and protected
// method and member access.

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
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
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
  ASSERT_TRUE(matches(Code.str(), ClassC));
}



// Testing Nested using statements.
TEST(ClassParsing, Access_PublicUsingUsingType) {
  StringRef Code = R"(
c : type = class:
  c2<public> : type = int

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPublic(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(ClassParsing, Access_ImplicitPublicUsingType) {
  StringRef Code = R"(
c : type = class:
  c2: type = int

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPublic(),
        hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(ClassParsing, Access_ProtectedUsingType) {
  StringRef Code = R"(
c : type = class:
  c2 <protected >: type = int

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isProtected(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(ClassParsing, Access_PrivateUsingType) {
  StringRef Code = R"(
c : type = class:
  c2 <private>: type = int

)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPrivate(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Testing Inheritance visiblity
TEST(ClassParsing, Access_PrivateBaseClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a<private>):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_private, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(ClassParsing, Access_ProtectedBaseClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a<protected>):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_protected, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(ClassParsing, Access_PublicBaseClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a<public>):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_public, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(ClassParsing, Access_ImplicitPublicBaseClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_public, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}