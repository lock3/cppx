//=== GoldParsingTest.cpp - Elaboration for Gold Nodes ----------------------==//
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


TEST(ClassParsing, ClassDeclaration) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool

main() : int!
  return 0
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(ClassParsing, ClassInstance) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
main() : int!
  q : c
  return 0
)";
  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );


  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}

TEST(ClassParsing, MemberInitializationAndAccess) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
main() : int!
  q : c
  q.x = 4
  return 0
  )";

  StatementMatcher StmtMatcher(compoundStmt(hasDescendant(
        binaryOperator(hasOperatorName("="),
          hasLHS(memberExpr(hasDescendant(
            declRefExpr(to(varDecl(hasType(recordDecl(hasName("c"))))))))),
          hasRHS(integerLiteral(equals(4)))
        )
      )
    )
  );


  ASSERT_TRUE(matches(Code.str(), StmtMatcher));
}

TEST(ClassParsing, MemberInitializers)  {
  StringRef Code = R"(
c : type = class:
  x : int = 4
  y : bool = 1

main() : int!
  q : c
  q.x = 4
  return 0
  )";
  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow(),
      hasDescendant(cxxCtorInitializer(forField(hasName("x")),
        isMemberInitializer() )),
      hasDescendant(cxxCtorInitializer(forField(hasName("y")),
        isMemberInitializer()))
    )),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow()))
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo));

  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}



TEST(ClassParsing, MemberFunction_NoMemberUse) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo() : int!
    return 0
  
main() : int!
  return 0
)";
  DeclarationMatcher MemberFunctionMatch = recordDecl( 
    hasDescendant(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}

TEST(ClassParsing, MemberFunction_MemberUse) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo() : int!
    return x
  
main() : int!
  return 0
)";
  DeclarationMatcher MemberFunctionMatch = recordDecl( 
    hasDescendant(
      cxxMethodDecl(
        hasName("foo"),
        hasDescendant(cxxThisExpr(hasType(asString("struct c *"))))
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}


TEST(ClassParsing, MemberFunction_ImplicitThisCall) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool
  foo() : int!
    return x
  
  bar() : int!
    return foo()
  

main() : int!
  q : c
  return q.foo()
)";
  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
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

  ASSERT_TRUE(matches(Code.str(), ClassCInfo));
}

TEST(ClassParsing, MemberFunction_OutsideOfClassCall) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool
  foo() : int!
    return x
  
main() : int!
  q : c
  return q.foo()
)";
  StatementMatcher StmtMatcher(compoundStmt(has(
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
  ASSERT_TRUE(matches(Code.str(), StmtMatcher));
}


TEST(ClassParsing, ConstructorDeclWithinClass_AfterMembers) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool = 3
  constructor() : void!
    x = 4
  
main() : int!
  q : c
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit()),
      unless(isDefaulted()),
      has(
        cxxCtorInitializer(
          forField(
            hasName("x")
          ),
          isMemberInitializer()
        )
      ),
      has(
        cxxCtorInitializer(
          forField(
            hasName("y")
          ),
          isMemberInitializer()
        )
      ),
      has(compoundStmt(
        hasDescendant(
          binaryOperator(
            hasOperatorName("="),
            hasLHS(
              memberExpr(
                hasObjectExpression(hasType(pointsTo(recordDecl(hasName("c"))))),
                member(
                  hasName("x")
                )
              )
            ),
            hasRHS(integerLiteral(equals(4)))
          )
        )
      ))
    )
  )
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, ConstructorDeclWithinClass_BeforeMembers) {
  StringRef Code = R"(
c : type = class:
  constructor() : void!
    x = 4
  x : int = 5
  y : bool = 3


main() : int!
  q : c
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
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
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, ConstructorAssignmentInitialization) {
  StringRef Code = R"(
c : type = class:
  constructor() : void!
    x = 4
  x : int = 5
  y : bool = 3


main() : int!
  q : c = c()
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
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
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(cxxConstructExpr(argumentCountIs(0)))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}



TEST(ClassParsing, ConstructorWithParameter) {
  StringRef Code = R"(
c : type = class:
  constructor(q : int) : void!
    x = 4 + q
  x : int = 5
  y : bool = 3


main() : int!
  q : c = c(3)
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
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
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(hasDescendant(cxxConstructExpr(argumentCountIs(1))))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, UserDefinedDestructor) {
  StringRef Code = R"(
c : type = class:
  constructor(q : int) : void!
    x = 4 + q
  destructor() : void!
    x = 0
  x : int = 5
  y : bool = 3


main() : int!
  q : c = c(3)
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
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
        hasType(asString("struct c")),
        hasName("q"),
        hasInitializer(hasDescendant(cxxConstructExpr(argumentCountIs(1))))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}



TEST(ClassParsing, UserDefinedDestructorDeclOnly) {
  StringRef Code = R"(
c : type = class:
  constructor(q : int) : void!
    x = 4 + q
  destructor() : void


)";

  DeclarationMatcher ClassCInfo = recordDecl(
    has(cxxDestructorDecl(unless(isImplicit())))
  );

  ASSERT_TRUE(matches(Code.str(), ClassCInfo));
}


TEST(ClassParsing, NestedTypeDefinition) {
  StringRef Code = R"(
outer : type = class:
  nested : type = class:
    a : int
    b : float32
  
main() : int!
  u : outer.nested
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("outer"),
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

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}

TEST(ClassParsing, MultipleNestedTypeDefinition) {
  StringRef Code = R"(
c : type = class:
  nested : type = class:
    nested2 : type = class:
      a : int
      b : float32
    
  

main() : int!
  u : c.nested.nested2
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("c"),
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
        hasType(asString("struct c::nested::nested2")),
        hasName("u"),
        hasInitializer(hasDescendant(cxxConstructExpr()))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)//,
    // hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, TemplatedMemberFunction) {
  StringRef Code = R"(
c : type = class:
  foo[T : type](x : T) : T!
    return x
)";
  DeclarationMatcher MemberFunctionMatch = recordDecl( hasName("c"),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}

// FIXME: WOrking on long term fix to this.
TEST(ClassParsing, TemplateMemberFunction_Call) {
  StringRef Code = R"(
c : type = class:
  foo[i:int]() : int!
    return i
  

main() :int! 
  a : c = c()
  return a.foo[3]()

)";
  DeclarationMatcher MemberFunctionMatch = recordDecl( hasName("c"),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}

TEST(ClassParsing, TemplateMemberFunction_Call2) {

  StringRef Code = R"(
c : type = class:
  y : bool = 0
  foo[i:int]() :int!
    return i
  

main() : int!
  q : c
  return q.foo[3]()

)";
  DeclarationMatcher MemberFunctionMatch = recordDecl( hasName("c"),
    hasDescendant(functionTemplateDecl(hasName("foo"), has(cxxMethodDecl())))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}


TEST(ClassParsing, MemberToMemberToMember) {
  StringRef Code = R"(
a : type = class:
  z : int = 5

b : type = class:
  x : a

c : type = class:
  y : b  

main() : int!
  q : c
  return q.y.x.z
)";
  StatementMatcher StmtMatcher(compoundStmt(has(
    returnStmt(
      hasDescendant(
        memberExpr(
          has(
            memberExpr(
              has(
                memberExpr(
                  has(
                    declRefExpr(
                      to(
                        varDecl(
                          hasName("q")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )));
  ASSERT_TRUE(matches(Code.str(), StmtMatcher));
}


TEST(ClassParsing, ClassTypeUsedAsFunctionParameter) {

  StringRef Code = R"(
c : type = class:
  y : bool = 0

foo(x : c) : bool!
  return x.y

)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("struct c"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}



TEST(ClassParsing, NestedTypeAliasOfAClass) {

  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class:
  x : type = a
  y : bool = 0

foo(x : c.x) : bool!
  return x.i
)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("c::x"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}


TEST(ClassParsing, NestedTypeAliasOfABuiltInType) {

  StringRef Code = R"(
c : type = class:
  x : type = int
  y : bool = 0

foo(x : c.x) : bool!
  return x
)";
  DeclarationMatcher MemberFunctionMatch = functionDecl( hasName("foo"),
    hasAnyParameter(hasName("x")),
    hasDescendant(varDecl(hasType(asString("c::x"))))
  );
  ASSERT_TRUE(matches(Code.str(), MemberFunctionMatch));
}



TEST(ClassParsing, InheritedClass) {

  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a"))
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}

TEST(ClassParsing, InheritedClass_BaseMemberAccessOutsideOfClass) {

  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a):
  y : bool = 0

main() : int!
  q : c
  return q.i
)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a"))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q")
      )
    ),
    hasDescendant(
      returnStmt(
        has(implicitCastExpr(
          has(memberExpr(
            has(implicitCastExpr(
              has(declRefExpr(
                to(varDecl(hasName("q")))
              ))
            ))
          ))
        ))
      )
    )
  );
  DeclarationMatcher MemberBaseAccessOutsideOfClass = translationUnitDecl(
    hasDescendant(BaseClassMatch),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), MemberBaseAccessOutsideOfClass));
}


TEST(ClassParsing, InheritedClass_BaseMemberAccessInsideOfClass) {
  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a):
  y : bool = 0
  foo() : int!
    return i
  

main() : int!
  q : c
  return q.i
)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    // Verifying that we are infact using the right constructor.
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(),
      has(cxxCtorInitializer(
        unless(isMemberInitializer())
      )),
      has(
        cxxCtorInitializer(
          forField(
            hasName("y")
          ),
          isMemberInitializer()
        )
      )
    )),
    // Attempting to verify that we have an actual return statment at the
    // very least.
    hasDescendant(cxxMethodDecl(hasName("foo"), hasDescendant(returnStmt())))
  );
  DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q")
      )
    ),
    hasDescendant(
      returnStmt(
        has(implicitCastExpr(
          has(memberExpr(
            has(implicitCastExpr(
              has(declRefExpr(
                to(varDecl(hasName("q")))
              ))
            ))
          ))
        ))
      )
    )
  );
  DeclarationMatcher MemberBaseAccessOutsideOfClass = translationUnitDecl(
    hasDescendant(BaseClassMatch),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), MemberBaseAccessOutsideOfClass));
}

TEST(ClassParsing, NestedClassWithMemberFunction) {
  StringRef Code = R"(
outer : type = class:
  bar() : int!
    return 4
  nested : type = class:
    a : int
    b : float32
    foo() : int!
      return b
    
  
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("outer"),
    has(recordDecl(hasName("nested"),
      hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    ))
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}

TEST(ClassParsing, NestedClassTemplateWithDependentMember) {
  StringRef Code = R"(
outer[T : type] : type = class:
  bar() : int!
    return 4
  nested[U : type] : type = class:
    a : int
    b : U
    foo() : U!
      return b
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("outer"),
    has(classTemplateDecl(has(recordDecl(hasName("nested"),
      hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("type-parameter-1-0")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    )) ))
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}

TEST(ClassParsing, VirtuallyInheritedClass) {

  StringRef Code = R"(
a : type = class:
  i : int = 0

c : type = class (a<virtual>):
  y : bool = 0

)";
  DeclarationMatcher BaseClassMatch = cxxRecordDecl(hasName("c"),
    isDirectlyDerivedFrom(hasName("a")),
    hasBaseSpecifier({true, AS_public, "struct a"})
  );
  ASSERT_TRUE(matches(Code.str(), BaseClassMatch));
}


TEST(ClassParsing, MemberElabOrder_UseBeforeDecl_Type) {
  StringRef Code = R"(
outer : type = class:
  bar() : x!
    return 4
  y : x = 5
  x : type = int
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("outer"),
    hasDescendant(cxxMethodDecl(hasName("bar"))),
    hasDescendant(typeAliasDecl(hasName("x"))),
    hasDescendant(fieldDecl(hasName("y")))
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, MemberElabOrder_UseBeforeDecl_ClassDef) {
  StringRef Code = R"(
outer : type = class:
  y : x
  x : type = class:
    z : int
    b : float32
    foo() : int!
      return z
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("outer"),
    has(recordDecl(hasName("x"),
      hasDescendant(fieldDecl(hasName("z"), hasType(asString("int")),
        isPublic())),
      hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
        isPublic())),
      hasDescendant(cxxMethodDecl(hasName("foo")))
    )),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("struct outer::x"))))
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)
  );
  ASSERT_TRUE(matches(Code.str(), ClassImplicitsAndCalls));
}


TEST(ClassParsing, ClassesOutOfOrder) {
  StringRef Code = R"(
c1 : type = class:
  y : c2

c2 : type = class:
  y : int

)";
  DeclarationMatcher ClassC2 = recordDecl(
    hasName("c2"),
      hasDescendant(fieldDecl(hasName("y"), hasType(asString("int")),
        isPublic()))
  );

  DeclarationMatcher ClassC1 = recordDecl(
    hasName("c1"),
    unless(ClassC2),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("struct c2")),
      isPublic()))
  );


  DeclarationMatcher Matches = translationUnitDecl(
    hasDescendant(ClassC2),
    hasDescendant(ClassC1)
  );
  ASSERT_TRUE(matches(Code.str(), Matches));
}


TEST(ClassParsing, ReferenceToTypeInReturnTypeOfMemberFunction) {
  StringRef Code = R"(
Test : type = class:
  foo() :ref Test!
    return ^this
)";

  DeclarationMatcher ToMatch = recordDecl(
    hasName("Test"),
    has(cxxMethodDecl(hasName("foo"),
      hasType(asString("struct Test &(void)"))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(ClassParsing, NonClassTypeMemberAccess) {
  StringRef Code = R"Gold(
foo(x:int) : void!
  x.y
)Gold";
  GoldFailureTest(Code);
}

TEST(ClassParsing, ConstructorWithInvalidReturnType) {
  StringRef Code = R"(
c : type = class:
  constructor() : int!
    ;
  
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, DestructorWithInvalidReturnType) {
  StringRef Code = R"(
c : type = class:
  destructor() : int!
    ;
  
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, DestructorWithParams) {
  StringRef Code = R"(
c : type = class:
  destructor(x:int) : void!
    ;
  
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, DoubleDedent) {
  StringRef Code = R"(
A : type = class:
  B : type = class:
    x : int = 0

main() : int!
  a : A
)";

  SimpleGoldParseTest(Code);
}

TEST(ClassParsing, DedentAfterSemi) {
  StringRef Code = R"(
A : type = class:
  foo() :void!
    ;

main():int !
  return 0
)";

  SimpleGoldParseTest(Code);
}


TEST(ClassParsing, ClassNameConflict) {
  StringRef Code = R"(
a : type = class:
  ;
a : type = class:
  i:int

)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, ForwardClassDeclaration) {
  StringRef Code = R"(
a : type = class

)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("a"),
      unless(isDefinition()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, ForwardClassDeclarationWithDefinition) {
  StringRef Code = R"(
a : type = class

a : type = class:
  ;

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), unless(isDefinition()))),

    has(cxxRecordDecl(hasName("a"), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, WithUseBeforeSecondDecl) {
  StringRef Code = R"(
a : type = class

foo(x:a):void

a : type = class:
  ;

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), unless(isDefinition()))),

    has(cxxRecordDecl(hasName("a"), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, WithUseBeforeSecondDeclInstanceUsed) {
  StringRef Code = R"(
a : type = class

foo(x:ref a):void!
  x.i

a : type = class:
  i:int

)";
  DeclarationMatcher ToMatch = translationUnitDecl(
    has(cxxRecordDecl(hasName("a"), unless(isDefinition()))),

    has(cxxRecordDecl(hasName("a"), isDefinition()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, ConstructorDeclOnly) {
  StringRef Code = R"(
c : type = class:
  constructor() : void
)";

  DeclarationMatcher ToMatch = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(),
                  unless(isImplicit()),
                  unless(isDefinition())))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(ClassParsing, FiveDeepNestedClassDecl) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    T3 = class:
      T4 = class:
        T5 = class:
          ;
)";

  auto ToMatch = cxxRecordDecl(hasName("T1"),
    has(cxxRecordDecl(hasName("T2"),
      has(cxxRecordDecl(hasName("T3"),
        has(cxxRecordDecl(hasName("T4"),
          has(cxxRecordDecl(hasName("T5")))
        ))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, FiveDeepNestedClassDecl_WithFunction) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    T3 = class:
      T4 = class:
        T5 = class:
          foo()<static>:void!
            ;
)";

  auto ToMatch = cxxRecordDecl(hasName("T1"),
    has(cxxRecordDecl(hasName("T2"),
      has(cxxRecordDecl(hasName("T3"),
        has(cxxRecordDecl(hasName("T4"),
          has(cxxRecordDecl(hasName("T5"),
            has(cxxMethodDecl(hasName("foo")))
          ))
        ))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, FiveDeepNestedClassDecl_WithFunctionTemplate) {
  StringRef Code = R"(
T1 = class:
  T2 = class:
    T3 = class:
      T4 = class:
        T5 = class:
          foo[T:type]()<static>:void!
            ;
)";

  auto ToMatch = cxxRecordDecl(hasName("T1"),
    has(cxxRecordDecl(hasName("T2"),
      has(cxxRecordDecl(hasName("T3"),
        has(cxxRecordDecl(hasName("T4"),
          has(cxxRecordDecl(hasName("T5"),
            has(functionTemplateDecl(
              has(cxxMethodDecl(hasName("foo")))
            ))
          ))
        ))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, FiveDeepNestedClassDecl_WithCallableInlineSataticVar) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;
T1 = class:
  T2 = class:
    T3 = class:
      T4 = class:
        T5 = class:
          foo<static> <inline> : Callable
)Gold";

  auto ToMatch = cxxRecordDecl(hasName("T1"),
    has(cxxRecordDecl(hasName("T2"),
      has(cxxRecordDecl(hasName("T3"),
        has(cxxRecordDecl(hasName("T4"),
          has(cxxRecordDecl(hasName("T5"),
            has(varDecl(hasName("foo")))
          ))
        ))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(ClassParsing, NestedStaticMemberVariableDecl) {
  StringRef Code = R"Gold(
Callable = class:
  operator"()"():void!
    ;
T1 = class:
  T2 = class:
    foo<static> <inline> : Callable

)Gold";

  auto ToMatch = cxxRecordDecl(hasName("T1"),
    has(cxxRecordDecl(hasName("T2"),
        has(varDecl(hasName("foo")))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(ClassParsing, LocalClassTest) {
  StringRef Code = R"Gold(

foo():void!
  Callable = class:
    operator"()"():void!
      ;
  T1 = class:
    T2 = class:
      foo<static> <inline> : Callable

)Gold";

  auto ToMatch =functionDecl(hasName("foo"),
    hasDescendant( cxxRecordDecl(
      hasName("T1"),
      has(cxxRecordDecl(hasName("T2"),
          has(varDecl(hasName("foo")))
      ))
    ))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}