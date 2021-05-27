//=== BlueDependentMemberAccessElab.cpp =----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for member access, and member access with nested name specifiers, within 
//  dependent contexts
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueDependentMemberAccess, AccessingNonDependentBase_InClass) {
  StringRef Code = R"BLUE(
B : type = {
  i:int;
}

C : [T:type] -> type = {
  :B;
  i:int;
  foo:(inout this) -> void = {
    this.(B).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, AccessingDependentBaseClass_InClass) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    this.(B[T]).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, AccessingDependentBaseClass_ImplicitThis) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    B[T].i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, BaseClassMemberDisambiguationAccess) {
  StringRef Code = R"BLUE(
B : type = {
  i:int;
}

C : type = {
  :B;
  i:int;
}

foo:() = {
  v:C;
  v.(B).i = 4;
}
)BLUE";
  auto ToMatch =
    memberExpr(
      hasDescendant(
        declRefExpr(
          to(
            varDecl(
              hasName("v")
            )
          )
        )
      )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, AccessingNonDependentBase_OutsideOfClass) {
  StringRef Code = R"BLUE(
B : type = {
  i:int;
}

C : [T:type] -> type = {
  :B;
  i:int;
}
foo:()->void = {
  x:C[int];
  x.(B).i = 4;
}
)BLUE";
  auto ToMatch =
    memberExpr(
      hasDescendant(
        declRefExpr(
          to(
            varDecl(
              hasName("x")
            )
          )
        )
      )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, AccessingDependentBaseClass_OutsideOfClass) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
}
foo:()->void = {
  x:C[int];
  x.(B[int]).i = 4;
}
)BLUE";
 auto ToMatch =
    memberExpr(
      hasDescendant(
        declRefExpr(
          to(
            varDecl(
              hasName("x")
            )
          )
        )
      )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguation) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B :[T:type] -> type = {
    i:T;
  }
}

C : [T:type] -> type = {
  :ns.B[T];
  i:T;
  foo: (inout this) -> void = {
    this.(ns.B[T]).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(BlueDependentMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguation_ImplicitThis) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : [T:type] -> type = {
    i:int;
  }
}

C : [T:type] -> type = {
  :ns.B[T];
  i:int;
  foo: (this) -> void = {
    ns.B[T].i = 4;
  }
}

foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    memberExpr(
      hasDescendant(
        cxxThisExpr()
      )
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguationNonMemberCall) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : [T:type] -> type = {
    i:int;
  }
  bar:()->void= { }
}

C : [T:type] -> type = {
  :ns.B[T];
  i:int;
  foo: (inout this) -> void = {
    ns.bar();
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = callExpr(callee(functionDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NameDisambiguationThroughTypeAliasTemplate) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : [T:type] -> type = {
    i:int;
  }
}

C : [T:type] -> type = {
  :ns.B[T];
  A :[U:type] -> type = ns.B[U];
  i:int;
  foo: (this) -> void = {
    A[T].i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    memberExpr(member(hasName("i")),
      hasDescendant(
        cxxThisExpr()
      )
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, FunctionCallWithDisambiguationExprImplicitThis) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
  bar:(inout this) -> void = { }
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    B[T].bar();
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = cxxMemberCallExpr(callee(cxxMethodDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, FunctionCallWithDisambiguationExpr) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
  bar:(inout this) -> void = { }
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    this.(B[T]).bar();
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = cxxMemberCallExpr(callee(cxxMethodDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, CallToFunctionOverloadInsideNamespace) {
  StringRef Code = R"BLUE(
ns:namespace = {
  foo:(i:int)->int = {
    return 3;
  }
  foo:(f:float32)->float32 = {
    return 3;
  }
}

C : [T:type] -> type = {
  i:int;
  foo:(this) -> void = {
    ns.foo(1);
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
})BLUE";
  auto ToMatch = callExpr(
    callee(functionDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, BaseClassThroughLocalAlias_InsideClass) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  Base :[X:type] -> type = B[X];
  i:int;
  foo:(inout this) -> void = {
    this.(Base[T]).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, BaseClassThroughLocalAlias_InsideCurrentCall) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    Base :[X:type] -> type = B[X];
    this.(Base).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, BaseClassThroughLocalAlias_InsideUnrelatedNamespace) {
  StringRef Code = R"BLUE(

NS:namespace = {
  Base : [T:type] -> type = B[T];
}
B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    this.(NS.Base[T]).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, BaseClassThroughLocalAlias_InsideUnrelatedClass) {
  StringRef Code = R"BLUE(

A : type = {
  Base : [T:type] -> type = B[T];
}

B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    this.(A.Base[T]).i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, BaseClassThroughLocalAlias_InsideCurrentFunction) {
  StringRef Code = R"BLUE(

A : type = {
  Base : [T:type] -> type = B[T];
}

B : [T:type] -> type = {
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo:(inout this) -> void = {
    Ty:[U:type]->type = B[U];
    this.Ty[T].i = 4;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

// FIXME: This is a problem that may need to be revisited in the future.
// TEST(BlueDependentMemberAccess, CallToOverload_InsideOfBaseClass) {
//   StringRef Code = R"BLUE(
// B: [T:type] -> type = {
//   foo:(this, i:int)->int = {
//     return 3;
//   }

//   foo:(this, f:float32)->float32 = {
//     return 3;
//   }
// }

// C : [T:type] -> type = {
//   : B[T];
//   i:int;
//   bar:(this) -> void = {
//     this.B[T].foo(1);
//   }
// }
// foo:()->void = {
//   x:C[int];
//   x.foo();
// }
// )BLUE";
//   auto ToMatch = callExpr(
//     callee(cxxMethodDecl(hasName("foo")))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(BlueDependentMemberAccess, NestedNameSpecifier_Declaration) {
  StringRef Code = R"BLUE(
B: [T:type] -> type = {
  S : type = T;
}

C : [T:type] -> type = {
  : B[T];
  i:B[T].S;
}
foo:()->void = {
  x:C[int];
}
)BLUE";
  auto ToMatch = fieldDecl(hasName("i"), hasType(asString("B<int>::S")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, GlobalVariableAccess_InsideNamespace) {
  StringRef Code = R"BLUE(
NS:namespace = {
  x:int64 = 4;
}

C : [T:type] -> type = {
  foo:(inout this) -> void = {
    NS.x = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = declRefExpr(to(varDecl(hasName("x"), hasType(asString("long")))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NamespaceAlias_InsideNamespace) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:[T:type] -> type = {
    i:int;
  }
}

NS2 :namespace = NS;

C : [T:type] -> type = {
  :NS.B[T];
  foo:(inout this) -> void = {
    this.(NS2.B[T]).i = 4;
  }
}

foo:()->void = {
  x:C[int];
  x.foo();
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NamespaceAlias_InsideNamespace_ImplicitThis) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:[T:type] -> type = {
    i:int;
  }
}

NS2 :namespace = NS;

C : [T:type] -> type = {
  :NS.B[T];
  foo:(inout this) -> void = {
    NS2.B[T].i = 4;
  }
}

foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(BlueDependentMemberAccess, NamespaceAlias_InsideNamespace_DeclaredInFunction) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:[T:type] -> type = {
    i:int;
  }
}

C : [T:type] -> type = {
  :NS.B[T];
  foo:(inout this) -> void = {
    NS2 :namespace = NS;
    this.(NS2.B[T]).i = 4;
  }
}

foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NamespaceAlias_InsideNamespace_ImplicitThis_DeclaredInFunction) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:[T:type] -> type = {
    i:int;
  }
}

C : [T:type] -> type = {
  :NS.B[T];
  i:int;
  foo:(inout this) -> void = {
    NS2 :namespace = NS;
    NS2.B[T].i = 4;
  }
}

foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, PointerToAMemberOfAnotherClass_NoOverload) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  foo:(inout this) -> void = {
  }
}
C : [T:type] -> type = {
  foo:(inout this) -> void = {
    memberPtr : auto = ^B[T].foo;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = varDecl(hasName("memberPtr"), hasType(asString("void (struct B<int>::*)(void)")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(BlueDependentMemberAccess, PointerToAMemberOfAnotherClass_WithOverload) {
//   StringRef Code = R"BLUE(
// B : type = {
//   foo:() -> void = {
//   }
//   foo:(i:int)-> void = {
//   }
// }
// C : type = {
//   foo:(inout this) -> void = {
//     memberPtr : auto = ^B.foo;
//   }
// }
// )BLUE";
//   // FIXME: I don't know how to write a member function pointer in blue/what
//   // they actually look like
//   auto ToMatch = varDecl(hasName("memberPtr"), hasType(asString("long long int")));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(BlueDependentMemberAccess, PointerToAMemberOfAnotherClass_Field) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  i:int;
}
C : [T:type] -> type = {
  foo:(inout this) -> void = {
    memberPtr : auto = ^B[T].i;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = varDecl(hasName("memberPtr"), hasType(asString("int struct B<int>::*")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NameDisambiguationForTemplateArguments) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

B : [T:type] -> type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    var:A[B[T].IntegerType].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<int>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, TemplateArgument_ClassNameAsTemplateTemplateParameter) {
  StringRef Code = R"BLUE(

A :[X:[T:type] ->type, OtherT:type] ->type = {
  Ty : type = X[OtherT].X;
}

B : [T:type] -> type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B[T];
  X : type = int64;
  i:int;
  foo: (inout this) -> void = {
    var:A[C, int].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<C, int>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, ClassTemplate_PointerToMySelf) {
  StringRef Code = R"BLUE(

C : [T:type] -> type = {
  child : ^C = null;
}

foo:()->void = {
  x:C[int];
}

)BLUE";
  auto ToMatch =
    fieldDecl(
      hasName("child"),
      hasType(asString("struct C<int> *"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, NameDisambiguationForTemplateArguments_TemplateTemplateArg) {
  StringRef Code = R"BLUE(

A :[Template:[T:type]->type] ->type = {
  Ty : type = Template[int];
}

A2 :[T:type] ->type = {
  i:int;
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    var:A[A2].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<A2>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, NameDisambiguationForTemplateArguments_TemplateTemplateArg_FromInsideANamespace) {
  StringRef Code = R"BLUE(

A :[Template:[T:type]->type] ->type = {
  Ty : type = Template[int];
}

NS:namespace = {

  A2 :[T:type] -> type = {
    i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    var:A[NS.A2].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<A2>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueDependentMemberAccess, BaseClassDisambiguationThroughTemplateAlias_InDerived) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  i:int;
}

C : [T:type] -> type = {
  :B[T];
  TAlias : [X:type] -> type = B[X];
  i:int;
  foo: (inout this) -> void = {
    this.(TAlias[T]).i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, BaseClassDisambiguationThroughTemplateAlias_Global) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  i:int;
}

TAlias : [T:type] -> type = B[T];

C : [T:type] -> type = {
  :B[int];
  i:int;
  foo: (inout this) -> void = {
    this.(TAlias[int]).i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueDependentMemberAccess, BaseClassDisambiguationThroughTemplateAlias_Namespace) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  i:int;
}
NS:namespace = {

  TAlias : [T:type] -> type = B[T];
}

C : [T:type] -> type  = {
  :B[int];
  i:int;
  foo: (inout this) -> void = {
    this.(NS.TAlias[int]).i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C<int> *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// -------------------------- Error cases --------------------------------------
TEST(BlueDependentMemberAccess, BaseTypeErr_MemberNotFound) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = { }

C : [T:type] -> type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    B[T].IntegerType = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_LocalVar) {
  StringRef Code = R"BLUE(
C : [T:type] -> type = {
  i:int;
  foo: (inout this) -> void = {
    x:int;
    this.x = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_Parameter) {
  StringRef Code = R"BLUE(
C : [T:type] -> type = {
  i:int;
  foo: (inout this, x:int) -> void = {
    this.x = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_Global) {
  StringRef Code = R"BLUE(
x:int;
C : [T:type] -> type = {
  i:int;
  foo: (inout this) -> void = {
    this.x = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, IncompleteTemplateTypeErr_IncompleteType_AsBaseClass) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
}
C : [T:type] -> type = {
  :B[int];
  i:int;
  foo: (inout this) -> void = {
    this.(B).i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, IncompleteTemplateTypeErr_IncompleteType_AsAnUnrelatedClass) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
}
C : [T:type] -> type = {
  i:int;
  foo: (inout this) -> void = {
    B.i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, TemplateArg_InvalidNameExpr_WithValue_Namespace) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    var:A[this.NS].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueDependentMemberAccess, TemplateArg_InvalidNameExpr_WithoutValue_Namespace) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] -> type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    var:A[NS].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, TemplateArg_InvalidNameExpr_WithValue_BaseType) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] ->type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    var:A[this.B[T]].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueDependentMemberAccess, TemplateArg_InvalidNameExpr_WithValue_MemberExpression) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : [T:type] ->type = {
  :B[T];
  i:int;
  foo: (inout this) -> void = {
    var:A[this.i].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, TemplateArg_InvalidNameExpr_WithValue_TemplatedClass) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

B : [T:type] -> type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    var:A[this.B].Ty;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}



TEST(BlueDependentMemberAccess, InvalidInstantiation_Namespace) {
  StringRef Code = R"BLUE(

NS:namespace = {

}
C : [T:type] -> type = {
  i:int;
  foo: (inout this) -> void = {
    this.(NS[int]).y = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, InvalidInstantiation_NonTemplateBaseClass) {
  StringRef Code = R"BLUE(
B : type = {
  i:int;
}
C : [T:type] -> type = {
  :B;
  i:int;
  foo: (inout this) -> void = {
    this.(B[int]).i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueDependentMemberAccess, InvalidInstantiation_NonBaseClassNonTemplate) {
  StringRef Code = R"BLUE(
B : type = {
  i:int;
}
C : [T:type] -> type = {
  i:int;
  foo: (inout this) -> void = {
    B[int].i = 5;
  }
}
foo:()->void = {
  x:C[int];
  x.foo();
}
)BLUE";
  BlueFailureTest(Code);
}