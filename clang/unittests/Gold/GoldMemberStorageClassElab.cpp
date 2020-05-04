//===- unittest/Gold/GoldMemberStorageClassElab.cpp ==---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(ClassParsing, Static_MemberVariable) {
  StringRef Code = R"(
c : type = class:
  x <static>: int = 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      varDecl(hasName("x"), hasType(asString("int")), isPublic(), isStaticStorageClass())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Static_StringLiteralVariable) {
  StringRef Code = R"(
c : type = class:
  x <static>: ^char = "something"
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      varDecl(hasName("x"), hasType(asString("char *")), isPublic(), isStaticStorageClass())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
}

TEST(ClassParsing, Static_MemberFunction) {
   StringRef Code = R"(
c : type = class:
  x() <static> : ^char
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      cxxMethodDecl(hasName("x"), hasType(asString("char *(void)")), isPublic(), isStaticStorageClass())
    )
  );
  ASSERT_TRUE(matches(Code, ClassC));
} 
// TEST(ClassParsing, Access_PublicMember) {
//   StringRef Code = R"(
// c : type = class:
//   x <public>: int
// )";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

// TEST(ClassParsing, Access_ImplicitPublicMember) {
//   StringRef Code = R"(
// c : type = class:
//   x : int
// )";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

// TEST(ClassParsing, Access_ProtectedMember) {
//   StringRef Code = R"(
// c : type = class:
//   x <protected>: int
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       fieldDecl(hasName("x"), hasType(asString("int")), isProtected())
//     )
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }


// // Class Member function Access testing.
// TEST(ClassParsing, Access_PrivateMemberFunctions) {
//   StringRef Code = R"(
// c : type = class:
//   foo()<private>: int!
//     return 4
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(cxxMethodDecl(hasName("foo"), isPrivate()))
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

// TEST(ClassParsing, Access_ProtectedMemberFunctions) {
//   StringRef Code = R"(
// c : type = class:
//   foo()<protected>: int!
//     return 4
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(cxxMethodDecl(hasName("foo"), isProtected()))
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

// TEST(ClassParsing, Access_PublicMemberFunctions) {
//   StringRef Code = R"(
// c : type = class:
//   foo()<public>: int!
//     return 4
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

// TEST(ClassParsing, Access_ImplicitPublicMemberFunctions) {
//   StringRef Code = R"(
// c : type = class:
//   foo(): int!
//     return 4
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
//   );
//   ASSERT_TRUE(matches(Code, ClassC));
// }

