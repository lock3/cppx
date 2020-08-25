//=== GoldUnionELab.cpp ---------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing union declarations.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldUnion, SimpleDecl) {
  StringRef Code = R"(
U : type = union:
  x:int
)";

  DeclarationMatcher ToMatch = tagDecl(hasName("U"), isUnion(), isDefinition());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, DeclWithTwoPODTypes) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool
)";
  DeclarationMatcher ToMatch = recordDecl( isUnion(), hasName("U"),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, NonPODTypes) {
  StringRef Code = R"(
Cls : type = class:
  destructor()!
    ;
U : type = union:
  x : int
  y : Cls
)";
  DeclarationMatcher ToMatch = recordDecl(hasName("U"), isUnion(), 
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("struct Cls")),
      isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, Instance) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool

main() : int!
  q : U
  return 0
)";
  DeclarationMatcher UnionInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic()))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("union U")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(UnionInfo),
    hasDescendant(MainFnMatcher)
  );


  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}

TEST(GoldUnion, MemberInitializationAndAccess) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool

main() : int!
  q : U
  q.x = 4
  return 0
  )";

  StatementMatcher StmtMatcher(compoundStmt(hasDescendant(
        binaryOperator(hasOperatorName("="),
          hasLHS(memberExpr(hasDescendant(
            declRefExpr(to(varDecl(hasType(recordDecl(hasName("U"))))))))),
          hasRHS(integerLiteral(equals(4)))
        )
      )
    )
  );


  ASSERT_TRUE(matches(Code.str(), StmtMatcher));
}

TEST(GoldUnion, MemberInitializers)  {
  StringRef Code = R"(
U : type = union:
  x : int = 4
  y : bool = 1
)";
  GoldFailureTest(Code);
}

TEST(GoldUnion, MemberFunction_NoMemberUse) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool
  foo() : int!
    return 0
  
main() : int!
  return 0
)";
  DeclarationMatcher ToMatch = recordDecl( 
    hasDescendant(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, MemberFunction_MemberUse) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool
  foo() : int!
    return x
  
main() : int!
  return 0
)";
  DeclarationMatcher ToMatch = recordDecl( 
    hasDescendant(
      cxxMethodDecl(
        hasName("foo"),
        hasDescendant(cxxThisExpr(hasType(asString("union U *"))))
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, MemberFunction_ImplicitThisCall) {
  StringRef Code = R"(
U : type = union:
  x : int = 5
  y : bool
  foo() : int!
    return x
  
  bar() : int!
    return foo()
  

main() : int!
  q : U
  return q.foo()
)";
  DeclarationMatcher ToMatch = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxMethodDecl(hasName("foo"))),
    hasDescendant(cxxMethodDecl(hasName("bar"),
      hasDescendant(returnStmt(has(
        cxxMemberCallExpr()
      )))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, MemberFunction_OutsideOfClassCall) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool
  foo() : int!
    return x
  
main() : int!
  q : U
  return q.foo()
)";
  StatementMatcher ToMatch(compoundStmt(has(
    returnStmt(
      hasDescendant(
        cxxMemberCallExpr(
          hasDescendant(
            memberExpr(
              hasDescendant(
                declRefExpr()
              )
            )
          )
        )
      )
    )
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, ConstructorDeclWithinClass_AfterMembers) {
  StringRef Code = R"(
U : type = union:
  x : int
  y : bool
  constructor() : void!
    x = 4
  
main() : int!
  q : U
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit())))
  );
  ASSERT_TRUE(matches(Code.str(), UnionUInfo));
}


TEST(GoldUnion, ConstructorDeclWithinClass_BeforeMembers) {
  StringRef Code = R"(
U : type = union:
  constructor() : void!
    x = 4
  x : int
  y : bool


main() : int!
  q : U
  return 0
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit()),
      unless(isDefaulted())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, ConstructorAssignmentInitialization) {
  StringRef Code = R"(
U : type = union:
  constructor() : void!
    x = 4
  x : int
  y : bool

main() : int!
  q : U = U()
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit()),
      unless(isDefaulted())))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("union U")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );
  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, ConstructorWithNonTrivalConstructorAndDestructor) {
  StringRef Code = R"(
Cls : type = class:
  constructor()!
    ;
  destructor()!
    ;

U : type = union:
  constructor() : void!
    x = 4
  x : int
  y : Cls

main() : int!
  q : U = U()
  return 0
)";

  GoldFailureTest(Code);
}

TEST(GoldUnion, ConstructorWithNonTrivalConstructorAndDestructor_ValidCtorDtorInUnion) {
  StringRef Code = R"(
Cls : type = class:
  constructor()!
    ;
  destructor()!
    ;

U : type = union:
  constructor() : void!
    x = 4
  destructor()!
    ;
  x : int
  y : Cls

main() : int!
  q : U = U()
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("struct Cls")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit()),
      unless(isDefaulted())))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("union U")),
        hasName("q")
      )
    )
  );
  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(GoldUnion, ConstructorWithParameter) {
  StringRef Code = R"(
U : type = union:
  constructor(q : int) : void!
    x = 4 + q
  x : int
  y : bool


main() : int!
  q : U = U(3)
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(parameterCountIs(1)))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("union U")),
        hasName("q"),
        hasInitializer(hasDescendant(cxxConstructExpr(argumentCountIs(1))))
      )
    )
  );

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, UserDefinedDestructor) {
  StringRef Code = R"(
U : type = union:
  constructor(q : int) : void!
    x = 4 + q
  destructor() : void!
    ;
  x : int
  y : bool

main() : int!
  q : U = U(3)
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(parameterCountIs(1))),
    has(cxxDestructorDecl(unless(isImplicit())))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("union U")),
        hasName("q"),
        hasInitializer(hasDescendant(cxxConstructExpr(argumentCountIs(1))))
      )
    )
  );

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}



TEST(GoldUnion, UserDefinedDestructorDeclOnly) {
  StringRef Code = R"(
U : type = union:
  constructor(q : int) : void!
    x = 4 + q
  destructor() : void

)";

  DeclarationMatcher ToMatch = recordDecl(
    has(cxxDestructorDecl(unless(isImplicit())))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, NestedTypeDefinition) {
  StringRef Code = R"(
outer : type = union:
  nested : type = class:
    a : int
    b : float32
  x: int

main() : int!
  u : outer.nested
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("outer"), isUnion(),
    has(recordDecl(hasName("nested"),
      hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
        isPublic()))
    ))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct outer::nested")),
        hasName("u")
      )
    )
  );

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, MultipleNestedTypeDefinition) {
  StringRef Code = R"(
U : type = union:
  nested : type = class:
    nested2 : type = class:
      a : int
      b : float32

main() : int!
  u : U.nested.nested2
  return 0
)";

  DeclarationMatcher UnionUInfo = recordDecl(
    hasName("U"), isUnion(),
    has(recordDecl(hasName("nested"),
      has(recordDecl(hasName("nested2"),
        hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
          isPublic())),
        hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
          isPublic()))
      ))
    ))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct U::nested::nested2")),
        hasName("u")
      )
    )
  );

  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(UnionUInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, TemplatedMemberFunction) {
  StringRef Code = R"(
U : type = union:
  foo[T : type](x : T) : T!
    return x
)";
  DeclarationMatcher ToMatch = recordDecl( hasName("U"), isUnion(),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, TemplateMemberFunction_Call) {
  StringRef Code = R"(
U : type = union:
  foo[i:int]() : int!
    return i

main() :int! 
  a : U = U()
  return a.foo[3]()

)";
  DeclarationMatcher ToMatch = recordDecl( hasName("U"), isUnion(),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, TemplateMemberFunction_Call2) {
  StringRef Code = R"(
U : type = union:
  y : bool = 0
  foo[i:int]() :int!
    return i

main() : int!
  q : U
  return q.foo[3]()
)";
  DeclarationMatcher ToMatch = recordDecl( hasName("U"),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, ClassTypeUsedAsFunctionParameter) {

  StringRef Code = R"(
U : type = union:
  y : bool

foo(x : U) : bool!
  return x.y

)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("union U"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}

TEST(GoldUnion, NestedTypeAliasOfAClass) {

  StringRef Code = R"(
a : type = class:
  i : int = 0

U : type = union:
  x : type = a
  y : bool

foo(x : U.x) : bool!
  return x.i
)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("U::x"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}


TEST(GoldUnion, NestedTypeAliasOfABuiltInType) {
  StringRef Code = R"(
U : type = union:
  x : type = int
  y : bool

foo(x : U.x) : bool!
  return x
)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("U::x"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}



TEST(GoldUnion, InheritedClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

U : type = union(a):
  y : bool

)";
  GoldFailureTest(Code);
}


TEST(GoldUnion, NestedUnionWithMemberFunction) {
  StringRef Code = R"(
outer : type = union:
  bar() : int!
    return 4
  nested : type = union:
    a : int
    b : float32
    foo() : int!
      return b
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("outer"), isUnion(),
    has(recordDecl(hasName("nested"), isUnion(),
      hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, NestedUnionTemplateWithDependentMember) {
  StringRef Code = R"(
outer[T : type] : type = union:
  bar() : int!
    return 4
  nested[U : type] : type = union:
    a : int
    b : U
    foo() : U!
      return b
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("outer"), isUnion(),
    has(classTemplateDecl(has(recordDecl(hasName("nested"), isUnion(),
      hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("U")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    )) ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, VirtuallyInheritedClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

U : type = union (a<virtual>):
  y : bool = 0

)";
  GoldFailureTest(Code);
}


TEST(GoldUnion, MemberElabOrder_UseBeforeDecl_Type) {
  StringRef Code = R"(
outer : type = union:
  bar() : x!
    return 4
  y : x
  x : type = int
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("outer"), isUnion(),
    hasDescendant(cxxMethodDecl(hasName("bar"))),
    hasDescendant(typeAliasDecl(hasName("x"))),
    hasDescendant(fieldDecl(hasName("y")))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, MemberElabOrder_UseBeforeDecl_UnionDef) {
  StringRef Code = R"(
outer : type = union:
  y : x
  x : type = union:
    z : int
    b : float32
    foo() : int!
      return z

)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("outer"), isUnion(),
    has(recordDecl(hasName("x"),
      hasDescendant(fieldDecl(hasName("z"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    )),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("union outer::x"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, OutOfOrderUnions) {
  StringRef Code = R"(
c1 : type = union:
  y : c2

c2 : type = union:
  y : int

)";
  DeclarationMatcher ClassC2 = recordDecl(
    hasName("c2"), isUnion(),
      hasDescendant(fieldDecl(hasName("y"), hasType(asString("int")),
        isPublic()))
  );

  DeclarationMatcher ClassC1 = recordDecl(
    hasName("c1"),
    unless(ClassC2),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("union c2")),
      isPublic()))
  );
  DeclarationMatcher ToMatch = translationUnitDecl(
    hasDescendant(ClassC2),
    hasDescendant(ClassC1)
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldUnion, ReferenceToTypeInReturnTypeOfMemberFunction) {
  StringRef Code = R"(
Test : type = union:
  foo() :ref Test!
    return ^this
)";
  DeclarationMatcher ToMatch = recordDecl(
    hasName("Test"),
    has(cxxMethodDecl(hasName("foo"),
      hasType(asString("union Test &(void)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, ClassNameConflict) {
  StringRef Code = R"(
a : type = union

a : type = class:
  i:int

)";
  GoldFailureTest(Code);
}

TEST(GoldUnion, ForwardClassDeclaration) {
  StringRef Code = R"(
a : type = union
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("a"),
      unless(isDefinition()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, ForwardClassDeclarationWithDefinition) {
  StringRef Code = R"(
a : type = union

a : type = union:
  ;

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), isUnion(), unless(isDefinition()))),
    has(cxxRecordDecl(hasName("a"), isUnion(), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, TagTypeMismatchBetweenDeclandDef) {
  StringRef Code = R"(
a : type = union

a : type = class:
  ;

)";
  GoldFailureTest(Code);
}

TEST(GoldUnion, WithUseBeforeSecondDecl) {
  StringRef Code = R"(
a : type = union

foo(x:a):void

a : type = union:
  ;

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), isUnion(), unless(isDefinition()))),
    has(cxxRecordDecl(hasName("a"), isUnion(), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, WithUseBeforeSecondDeclInstanceUsed) {
  StringRef Code = R"(
a : type = union

foo(x:ref a):void!
  x.i

a : type = union:
  i:int

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), isUnion(), unless(isDefinition()))),
    has(cxxRecordDecl(hasName("a"), isUnion(), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldUnion, ConstructorDeclOnly) {
  StringRef Code = R"(
U : type = union:
  constructor() : void
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("U"), isUnion(),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(),
                  unless(isImplicit()),
                  unless(isDefinition())))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

