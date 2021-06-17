//=== BlueAccessSpecifier.cpp ------------------------------------------------===//
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

TEST(BlueClass, Access_PrivateMember) {
  StringRef Code = R"(
c : type = {
  private x: int;
}
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PublicMember) {
  StringRef Code = R"(
c : type = {
  public x : int;
}
)";
  DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPublic())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPrivateMember) {
  StringRef Code = R"(
c : type = {
  x : int;
}
)";
  DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isPrivate())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ProtectedMember) {
  StringRef Code = R"(
c : type = {
  protected x : int;
}
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      fieldDecl(hasName("x"), hasType(asString("int")), isProtected())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Class Member function Access testing.
TEST(BlueClass, Access_PrivateMemberFunctions) {
  StringRef Code = R"(
c : type = {
  private foo: (in this) -> int = {
    return 4;
  }
}
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ProtectedMemberFunctions) {
  StringRef Code = R"(
c : type = {
  protected foo:() -> int={
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PublicMemberFunctions) {
  StringRef Code = R"(
c : type = {
  public foo:(in this) -> int = {
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicMemberFunctions) {
  StringRef Code = R"(
c : type = {
  foo: (in this) -> int = {
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxMethodDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Class template Member function
TEST(BlueClass, Access_PrivateMemberFunctionTemplate) {
  StringRef Code = R"(
c : type = {
  private foo: [T:type] -> (in this, i:T) -> int= {
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(functionTemplateDecl(hasName("foo"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ProtectedMemberFunctionTemplate) {
  StringRef Code = R"(
c : type = {
  protected foo: [T:type] -> (in this, i:T) -> int ={
    return 4;
  }
}
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(functionTemplateDecl(hasName("foo"), isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PublicMemberFunctionTemplate) {
  StringRef Code = R"(
c : type = {
  public foo:[T:type] -> (in this, i:T) -> int = {
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(functionTemplateDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicMemberFunctionTemplate) {
  StringRef Code = R"(
c : type = {
  foo: [T:type] -> (in this, i:T) -> int = {
    return 4;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(functionTemplateDecl(hasName("foo"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}



// Testing constructor Access test.
TEST(BlueClass, Access_PrivateConstructor) {
  StringRef Code = R"(
c : type = {
  private operator= : (out this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ProtectedConstructor) {
  StringRef Code = R"(
c : type = {
  protected operator= : (out this) = { }

})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PublicConstructor) {
  StringRef Code = R"(
c : type = {
  public operator= : (out this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicConstructor) {
  StringRef Code = R"(
c : type = {
  operator= : (out this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

// Testing destructor Access.
TEST(BlueClass, Access_PrivateDestructor) {
  StringRef Code = R"(
c : type = {
  private operator= : (move this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ProtectedDestructor) {
  StringRef Code = R"(
c : type = {
  protected operator= : (move this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PublicDestructor) {
  StringRef Code = R"(
c : type = {
  public operator= : (move this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicDestructor) {
  StringRef Code = R"(
c : type = {
  operator= : (move this) = { }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(cxxDestructorDecl(isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Testing nested type Access.
TEST(BlueClass, Access_PublicType) {
  StringRef Code = R"(
c : type = {
  public c2 : type = {
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicType) {
  StringRef Code = R"(
c : type = {
  c2 : type = {
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PrivateType) {
  StringRef Code = R"(
c : type = {
  private c2 : type = {
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(BlueClass, Access_ProtectedType) {
  StringRef Code = R"(
c : type = {
  protected c2 : type = {
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(recordDecl(hasName("c2"), isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Testing nested templated types
TEST(BlueClass, Access_PublicTemplateType) {
  StringRef Code = R"(
c : type = {
  public c2 : [T:type] -> type ={
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicTemplateType) {
  StringRef Code = R"(
c : type = {
  c2 : [T:type] -> type ={
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PrivateTemplateType) {
  StringRef Code = R"(
c : type = {
  private c2 : [T:type] -> type ={
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isPrivate()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(BlueClass, Access_ProtectedTemplateType) {
  StringRef Code = R"(
c : type = {
  protected c2 : [T:type] -> type ={
    z : int;
    y : bool;
  }
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(classTemplateDecl(hasName("c2"), isProtected()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Testing Nested using statements.
TEST(BlueClass, Access_PublicUsingUsingType) {
  StringRef Code = R"(
c : type = {
  public c2 : type = int;
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPublic(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_ImplicitPublicUsingType) {
  StringRef Code = R"(
c : type = {
  c2 : type = int;
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPublic(),
        hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


TEST(BlueClass, Access_ProtectedUsingType) {
  StringRef Code = R"(
c : type = {
  protected c2 : type = int;
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isProtected(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(BlueClass, Access_PrivateUsingType) {
  StringRef Code = R"(
c : type = {
  private c2 : type = int;
})";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(typeAliasDecl(hasName("c2"), isPrivate(),
      hasType(asString("int"))))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}


// Testing Inheritance visiblity
TEST(BlueClass, Access_PrivateBaseClass) {
  StringRef Code = R"(
a : type = {
  i : int = 0;
}

c : type = {
  private :a;
  y : bool = 0;
})";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_private, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(BlueClass, Access_ProtectedBaseClass) {
  StringRef Code = R"(
a : type = {
  i : int = 0;
}

c : type = {
  protected :a;
  y : bool = 0;
})";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_protected, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(BlueClass, Access_PublicBaseClass) {
  StringRef Code = R"(a : type = {
  i : int = 0;
}

c : type = {
  public :a;
  y : bool = 0;
})";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_public, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(BlueClass, Access_ImplicitPublicBaseClass) {
  StringRef Code = R"(
a : type = {
  i : int = 0;
}

c : type = {
  :a;
  y : bool = 0;
})";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({false, AS_public, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}



// TODO: Implement these once we have syntax for them?
// // Variable Template
// TEST(BlueClass, Access_VarTemplate_Private) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><private> : const T = 4
// })";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplate_PublicMember) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><public> : const T = 4
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_ImplicitPublic_VarTemplate) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type] <static>: const T = 4
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_Protected_VarTemplate) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><protected> : const T = 4
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isProtected())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }


// // Variable Template specialization
// TEST(BlueClass, Access_VarTemplateSpecialization_Private) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><private> : const T = 4
//   x[int]<static><private> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPrivate())
//     ),
//     hasDescendant(
//       varTemplateSpecializationDecl(hasName("x"), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplateSpecialization_Public) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><public> : const T = 4
//   x[int]<static><public> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     ),
//     hasDescendant(
//       varTemplateSpecializationDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplateSpecialization_ImplicitPublic) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type] <static>: const T = 4
//   x[int]<static> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     ),
//     hasDescendant(
//       varTemplateSpecializationDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplateSpecialization_Protected) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><protected> : const T = 4
//   x[int]<static><protected> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isProtected())
//     ),
//     hasDescendant(
//       varTemplateSpecializationDecl(hasName("x"), isProtected())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// // Variable Template specialization
// TEST(BlueClass, Access_VarTemplatePartialSpecialization_Private) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><private> : const T = 4
//   x[T:type][^T]<static><private> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPrivate())
//     ),
//     hasDescendant(
//       varTemplatePartialSpecializationDecl(hasName("x"), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplatePartialSpecialization_Public) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><public> : const T = 4
//   x[T:type][^T]<static><public> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     ),
//     hasDescendant(
//       varTemplatePartialSpecializationDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplatePartialSpecialization_ImplicitPublic) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type] <static>: const T = 4
//   x[T:type][^T]<static> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isPublic())
//     ),
//     hasDescendant(
//       varTemplatePartialSpecializationDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_VarTemplatePartialSpecialization_Protected) {
//   StringRef Code = R"(
// c : type = {
//   x[T:type]<static><protected> : const T = 4
//   x[T:type][^T]<static><protected> : const int = 2
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       varTemplateDecl(hasName("x"), isProtected())
//     ),
//     hasDescendant(
//       varTemplatePartialSpecializationDecl(hasName("x"), isProtected())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }


// // Type Alias Template
// TEST(BlueClass, Access_TypeAliasTemplate_Private) {
//   StringRef Code = R"(
// Z[T:type] = class:
//   ;

// c : type = {
//   x[T:type]<private> : type = Z[T]
// })";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       typeAliasTemplateDecl(hasName("x"), isPrivate())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_TypeAliasTemplate_PublicMember) {
//   StringRef Code = R"(
// Z[T:type] = class:
//   ;

// c : type = {
//   x[T:type]<public> : type = Z[T]
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       typeAliasTemplateDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_ImplicitPublic_TypeAliasTemplate) {
//   StringRef Code = R"(
// Z[T:type] = class:
//   ;

// c : type = {
//   x[T:type] : type = Z[T]
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       typeAliasTemplateDecl(hasName("x"), isPublic())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(BlueClass, Access_Protected_TypeAliasTemplate) {
//   StringRef Code = R"(
// Z[T:type] = class:
//   ;

// c : type = {
//   x[T:type]<protected> : type = Z[T]
// })";
//   DeclarationMatcher ClassC = recordDecl(recordDecl(hasName("c")),
//     hasDescendant(
//       typeAliasTemplateDecl(hasName("x"), isProtected())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

// TEST(DependentMember, NonExistentMemberCall) {
//   StringRef Code = R"(
// new_allocator[T:type] : type = class:
//   allocate(n:uint64, ignored:^const void = null): ^T !{
//     if (n > this.max_size()) {
//       ;
//     }
// }
// })";

//   GoldFailureTest(Code.str());
// }
