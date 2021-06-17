//=== BlueMemberAccessElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for member access, and member access with nested name specifiers.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;


TEST(BlueMemberAccess, BaseClassMemberDisambiguationAccess_InsideClass) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo:(inout this) -> void = {
    this.(B).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, BaseClassMemberDisambiguationAccess) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
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


TEST(BlueMemberAccess, ImplicitThisBaseClassMemberDisambiguationAccess) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo: (this) -> void = {
    B.i = 4;
  }
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


TEST(BlueMemberAccess, TemplateInstantiationInsideOfBaseDisambiguationExpr_ImplicitThis) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  public i:int;
}

C : type = {
  :B[int];
  public i:int;
  foo: (this) -> void = {
    B[int].i = 4;
  }
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

TEST(BlueMemberAccess, TemplateInstantiationInsideOfBaseDisambiguationExpr) {
  StringRef Code = R"BLUE(
B : [T:type] -> type = {
  public i:int;
}

C : type = {
  :B[int];
  public i:int;
  foo: (inout this) -> void = {
    this.(B[int]).i = 4;
  }
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


TEST(BlueMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguation) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : type = {
    public i:int;
  }
}

C : type = {
  :ns.B;
  public i:int;
  foo: (inout this) -> void = {
    this.(ns.B).i = 4;
  }
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(BlueMemberAccess, NamespaceSpecifierNotInsideOfParens) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : type = {
    public i:int;
  }
}

C : type = {
  :ns.B;
  public i:int;
  foo: (inout this) -> void = {
    this.ns.B.i = 4;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguation_ImplicitThis) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : type = {
    public i:int;
  }
}

C : type = {
  :ns.B;
  public i:int;
  foo: (this) -> void = {
    ns.B.i = 4;
  }
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

TEST(BlueMemberAccess, NamespaceSpecifierForAsPartOfNameDisambiguationNonMemberCall) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : type = {
    public i:int;
  }
  bar:()->void= { }
}

C : type = {
  :ns.B;
  public i:int;
  foo: (inout this) -> void = {
    ns.bar();
  }
}
)BLUE";
  auto ToMatch = callExpr(callee(functionDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NameDisambiguationThroughTypeAlias) {
  StringRef Code = R"BLUE(
ns:namespace = {
  B : type = {
    public i:int;
  }
  bar:(this)->void= { }
}

C : type = {
  :ns.B;
  A :type = ns.B;
  public i:int;
  foo: (this) -> void = {
    A.i = 4;
  }
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

TEST(BlueMemberAccess, FunctionCallWithDisambiguationExprImplicitThis) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
  bar:(inout this) -> void = { }
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    B.bar();
  }
}
)BLUE";
  auto ToMatch = cxxMemberCallExpr(callee(cxxMethodDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, FunctionCallWithDisambiguationExpr) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
  bar:(inout this) -> void = { }
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    this.(B).bar();
  }
}
)BLUE";
  auto ToMatch = cxxMemberCallExpr(callee(cxxMethodDecl(hasName("bar"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, DependentExpressionDisambiguation) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C :[T:type] -> type = {
  :B;
  public i:int;

  foo:(this) -> void = {
    this.(B).i = 4;
  }
}


)BLUE";
  auto ToMatch = cxxDependentScopeMemberExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, AccessingNonDependent_ImplicitThis) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C :[T:type] -> type = {
  :B;
  public i:int;

  foo:(this) -> void = {
    B.i = 4;
  }
}


)BLUE";
  auto ToMatch = cxxDependentScopeMemberExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, DependentDisambiguationExpression) {
  StringRef Code = R"BLUE(
B :[T:type] -> type = {
  public i:int;
}

C : [T:type] -> type = {
  :B[T];
  public i:int;
  foo:(this) -> void = {
    this.(B[T]).i = 4;
  }
}

)BLUE";
  auto ToMatch = cxxDependentScopeMemberExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, DependendBaseClassFunctionCall) {
  StringRef Code = R"BLUE(
B :[T:type] -> type = {
  public i:int;
}

C : [T:type] -> type = {
  :B[T];
  public i:int;
  foo:(this) -> void = {
    this.(B[T]).foo() = 4;
  }
}

)BLUE";
  auto ToMatch = cxxDependentScopeMemberExpr();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, CallToFunctionOverloadInsideNamespace) {
  StringRef Code = R"BLUE(
ns:namespace = {
  foo:(i:int)->int = {
    return 3;
  }
  foo:(f:float32)->float32 = {
    return 3;
  }
}

C : type = {
  public i:int;
  foo:(this) -> void = {
    ns.foo(1);
  }
}

)BLUE";
  auto ToMatch = callExpr(
    callee(functionDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, BaseClassThroughLocalAlias_InsideClass) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  Base :type = B;
  public i:int;
  foo:(inout this) -> void = {
    this.(Base).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, BaseClassThroughLocalAlias_InsideCurrentCall) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo:(inout this) -> void = {
    Base :type = B;
    this.(Base).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, BaseClassThroughLocalAlias_InsideUnrelatedNamespace) {
  StringRef Code = R"BLUE(

NS:namespace = {
  Base : type = B;
}
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo:(inout this) -> void = {
    this.(NS.Base).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, BaseClassThroughLocalAlias_InsideUnrelatedClass) {
  StringRef Code = R"BLUE(

A : type = {
  Base : type = B;
}

B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo:(inout this) -> void = {
    this.(A.Base).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, CallToOverload_InsideOfBaseClass) {
  StringRef Code = R"BLUE(
B: type = {
  foo:(this, i:int)->int = {
    return 3;
  }

  foo:(this, f:float32)->float32 = {
    return 3;
  }
}

C : type = {
  : B;
  public i:int;
  bar:(this) -> void = {
    this.(B).foo(1);
  }
}

)BLUE";
  auto ToMatch = callExpr(
    callee(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NestedNameSpecifier_Declaration) {
  StringRef Code = R"BLUE(
B: type = {
  S : type = int64;
}

C : type = {
  : B;
  i:B.S;
}

)BLUE";
  auto ToMatch = fieldDecl(hasName("i"), hasType(asString("B::S")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, GlobalVariableAccess_InsideNamespace) {
  StringRef Code = R"BLUE(
NS:namespace = {
  x:int64 = 4;
}

C : type = {
  foo:(inout this) -> void = {
    NS.x = 5;
  }
}

)BLUE";
  auto ToMatch = declRefExpr(to(varDecl(hasName("x"), hasType(asString("long")))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NamespaceAlias_InsideNamespace) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:type = {
    public i:int;
  }
}

NS2 :namespace = NS;

C : type = {
  :NS.B;
  foo:(inout this) -> void = {
    this.(NS2.B).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NamespaceAlias_InsideNamespace_ImplicitThis) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:type = {
    public i:int;
  }
}

NS2 :namespace = NS;

C : type = {
  :NS.B;
  foo:(inout this) -> void = {
    NS2.B.i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NamespaceAlias_InsideNamespace_DeclaredInFunction) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:type = {
    public i:int;
  }
}

C : type = {
  :NS.B;
  foo:(inout this) -> void = {
    NS2 :namespace = NS;
    this.(NS2.B).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NamespaceAlias_InsideNamespace_ImplicitThis_DeclaredInFunction) {
  StringRef Code = R"BLUE(
NS:namespace = {
  B:type = {
    public i:int;
  }
}

C : type = {
  :NS.B;
  public i:int;
  foo:(inout this) -> void = {
    NS2 :namespace = NS;
    NS2.B.i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, PointerToAMemberOfAnotherClass_NoOverload) {
  StringRef Code = R"BLUE(
B : type = {
  foo:(inout this) -> void = {
  }
}
C : type = {
  foo:(inout this) -> void = {
    memberPtr : auto = ^B.foo;
  }
}

)BLUE";
  auto ToMatch = varDecl(hasName("memberPtr"), hasType(asString("void (struct B::*)(void)")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


// TEST(BlueMemberAccess, PointerToAMemberOfAnotherClass_WithOverload) {
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

TEST(BlueMemberAccess, PointerToAMemberOfAnotherClass_Field) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}
C : type = {
  foo:(inout this) -> void = {
    memberPtr : auto = ^B.i;
  }
}

)BLUE";
  auto ToMatch = varDecl(hasName("memberPtr"), hasType(asString("int struct B::*")));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NameDisambiguationForTemplateArguments) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[B.IntegerType].Ty;
  }
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<int>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, NameDisambiguationForTemplateArguments_TemplateTemplateArg) {
  StringRef Code = R"BLUE(

A :[Template:[T:type]->type] ->type = {
  Ty : type = Template[int];
}

A2 :[T:type] ->type = {
  public i:int;
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[A2].Ty;
  }
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<A2>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, NameDisambiguationForTemplateArguments_TemplateTemplateArg_FromInsideANamespace) {
  StringRef Code = R"BLUE(

A :[Template:[T:type]->type] ->type = {
  Ty : type = Template[int];
}

NS:namespace = {

  A2 :[T:type] -> type = {
    public i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[NS.A2].Ty;
  }
}
)BLUE";
  auto ToMatch =
    varDecl(
      hasName("var"),
      hasType(asString("A<A2>::Ty"))
    );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueMemberAccess, BaseClassDisambiguationThroughTemplateAlias_InDerived) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  public i:int;
}

C : type = {
  :B[int];
  TAlias : [T:type] -> type = B[T];
  public i:int;
  foo: (inout this) -> void = {
    this.(TAlias[int]).i = 5;
  }
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, BaseClassDisambiguationThroughTemplateAlias_Global) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  public i:int;
}

TAlias : [T:type] -> type = B[T];

C : type = {
  :B[int];
  public i:int;
  foo: (inout this) -> void = {
    this.(TAlias[int]).i = 5;
  }
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, BaseClassDisambiguationThroughTemplateAlias_Namespace) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
  public i:int;
}
NS:namespace = {

  TAlias : [T:type] -> type = B[T];
}

C : type = {
  :B[int];
  public i:int;
  foo: (inout this) -> void = {
    this.(NS.TAlias[int]).i = 5;
  }
}
)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// -------------------------- Error cases --------------------------------------
TEST(BlueMemberAccess, BaseTypeErr_MemberNotFound) {
  StringRef Code = R"BLUE(
B : type = { }

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    B.IntegerType = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_LocalVar) {
  StringRef Code = R"BLUE(
C : type = {
  public i:int;
  foo: (inout this) -> void = {
    x:int;
    this.x = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_Parameter) {
  StringRef Code = R"BLUE(
C : type = {
  public i:int;
  foo: (inout this, x:int) -> void = {
    this.x = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, BaseTypeErr_VariableFoundIsNotAMember_Global) {
  StringRef Code = R"BLUE(
x:int;
C : type = {
  public i:int;
  foo: (inout this) -> void = {
    this.x = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, IncompleteTemplateTypeErr_IncompleteType_AsBaseClass) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
}
C : type = {
  :B[int];
  public i:int;
  foo: (inout this) -> void = {
    this.(B).i = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, IncompleteTemplateTypeErr_IncompleteType_AsAnUnrelatedClass) {
  StringRef Code = R"BLUE(

B : [T:type] -> type = {
  IntegerType : type = int;
}
C : type = {
  public i:int;
  foo: (inout this) -> void = {
    B.i = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, TemplateArg_InvalidNameExpr_WithValue_Namespace) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    public i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[this.NS].Ty;
  }
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueMemberAccess, TemplateArg_InvalidNameExpr_WithoutValue_Namespace) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    public i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[NS].Ty;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, TemplateArg_InvalidNameExpr_WithValue_BaseType) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    public i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[this.B].Ty;
  }
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueMemberAccess, TemplateArg_InvalidNameExpr_WithValue_MemberExpression) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

NS:namespace = {
  A2 :[T:type] ->type = {
    public i:int;
  }
}

B : type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[this.i].Ty;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, TemplateArg_InvalidNameExpr_WithValue_TemplatedClass) {
  StringRef Code = R"BLUE(

A :[T:type] ->type = {
  Ty : type = T;
}

B : [T:type] -> type = {
  IntegerType : type = int;
}

C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    var:A[this.B].Ty;
  }
}
)BLUE";
  BlueFailureTest(Code);
}



TEST(BlueMemberAccess, InvalidInstantiation_Namespace) {
  StringRef Code = R"BLUE(

NS:namespace = {

}
C : type = {
  public i:int;
  foo: (inout this) -> void = {
    this.NS[int].y = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, InvalidInstantiation_NonTemplateBaseClass) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}
C : type = {
  :B;
  public i:int;
  foo: (inout this) -> void = {
    this.B[int].i = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}

TEST(BlueMemberAccess, InvalidInstantiation_NonBaseClassNonTemplate) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}
C : type = {
  public i:int;
  foo: (inout this) -> void = {
    B[int].i = 5;
  }
}
)BLUE";
  BlueFailureTest(Code);
}


TEST(BlueMemberAccess, GloballyQualifiedMemberAccess) {
  StringRef Code = R"BLUE(
B : type = {
  public i:int;
}

C : type = {
  :B;
  public i:int;
  foo:(inout this) -> void = {
    this.(.B).i = 4;
  }
}

)BLUE";
  auto ToMatch = memberExpr(
    member(hasName("i")),
    has(implicitCastExpr(
      has(
        cxxThisExpr(hasType(asString("struct C *")))
      )
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueMemberAccess, GloballyQualifiedName) {
  StringRef Code = R"BLUE(
foo:()->void = { }

bar:()->void = {
  .foo();
}

)BLUE";
  auto ToMatch = callExpr(callee(functionDecl(hasName("foo"))));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// point: type = {
//     public x: int = 1;
//     public y: int = 2;
//     operator=: (out this) = { x = 1; y = 2; }
// }

// main: () -> int = {
//     p: point = ();
//     return p.x + p.y;
// }
