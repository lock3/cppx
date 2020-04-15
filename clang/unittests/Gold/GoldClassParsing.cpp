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


#include "ParseUtil.h"
#include "ASTMatchersTest.h"

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
  ASSERT_TRUE(matches(Code, ClassC));
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


  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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


  ASSERT_TRUE(matches(Code, StmtMatcher));
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

  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, MemberFunctionMatch));
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
  ASSERT_TRUE(matches(Code, MemberFunctionMatch));
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
  ASSERT_TRUE(matches(Code, StmtMatcher));
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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
}



TEST(ClassParsing, NestedTypeDefinition) {
  StringRef Code = R"(
c : type = class:
  nested : type = class:
    a : int
    b : float
  
main() : int!
  u : c.nested
  return 0
)";

  DeclarationMatcher ClassCInfo = recordDecl(
    hasName("c"),
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
        hasType(asString("struct c::nested")),
        hasName("u"),
        hasInitializer(hasDescendant(cxxConstructExpr()))
      )
    )
  );

  DeclarationMatcher ClassImplicitsAndCalls = translationUnitDecl(
    hasDescendant(ClassCInfo)//,
    // hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
}


TEST(ClassParsing, MultipleNestedTypeDefinition) {
  StringRef Code = R"(
c : type = class:
  nested : type = class:
    nested2 : type = class:
      a : int
      b : float
    
  

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
  ASSERT_TRUE(matches(Code, ClassImplicitsAndCalls));
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
  ASSERT_TRUE(matches(Code, MemberFunctionMatch));
}

TEST(ClassParsing, TemplateMemberFunction_Call) {
/*
|-CXXRecordDecl 0x7fffbb93b590 <bin/cpp_test.cpp:1:1, line:7:1> line:1:8 referenced struct c definition
| |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists trivial constexpr defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffbb93b6b8 <col:1, col:8> col:8 implicit struct c
| |-FunctionTemplateDecl 0x7fffbb93b910 <line:2:3, line:5:3> line:3:7 foo
| | |-NonTypeTemplateParmDecl 0x7fffbb93b768 <line:2:12, col:16> col:16 referenced 'int' depth 0 index 0 i
| | |-CXXMethodDecl 0x7fffbb93b860 <line:3:3, line:5:3> line:3:7 foo 'int ()'
| | | `-CompoundStmt 0x7fffbb93b9b0 <col:13, line:5:3>
| | |   `-ReturnStmt 0x7fffbb93b9a0 <line:4:5, col:12>
| | |     `-DeclRefExpr 0x7fffbb93b980 <col:12> 'int' NonTypeTemplateParm 0x7fffbb93b768 'i' 'int'
| | `-CXXMethodDecl 0x7fffbb93c280 <line:3:3, line:5:3> line:3:7 used foo 'int ()'
| |   |-TemplateArgument integral 3
| |   `-CompoundStmt 0x7fffbb9656d0 <col:13, line:5:3>
| |     `-ReturnStmt 0x7fffbb9656c0 <line:4:5, col:12>
| |       `-SubstNonTypeTemplateParmExpr 0x7fffbb9656a0 <col:12> 'int'
| |         `-IntegerLiteral 0x7fffbb965680 <col:12> 'int' 3
| |-CXXConstructorDecl 0x7fffbb93bba0 <line:1:8> col:8 implicit used constexpr c 'void () noexcept' inline default trivial
| | `-CompoundStmt 0x7fffbb93c068 <col:8>
| |-CXXConstructorDecl 0x7fffbb93bce8 <col:8> col:8 implicit constexpr c 'void (const c &)' inline default trivial noexcept-unevaluated 0x7fffbb93bce8
| | `-ParmVarDecl 0x7fffbb93be08 <col:8> col:8 'const c &'
| `-CXXConstructorDecl 0x7fffbb93bea8 <col:8> col:8 implicit constexpr c 'void (c &&)' inline default trivial noexcept-unevaluated 0x7fffbb93bea8
|   `-ParmVarDecl 0x7fffbb93bfc8 <col:8> col:8 'c &&'
`-FunctionDecl 0x7fffbb93b9f0 <line:9:1, line:13:1> line:9:5 main 'int ()'
  `-CompoundStmt 0x7fffbb965658 <col:12, line:13:1>
    |-DeclStmt 0x7fffbb93c0a0 <line:10:3, col:6>
    | `-VarDecl 0x7fffbb93bb20 <col:3, col:5> col:5 used t 'c' callinit
    |   `-CXXConstructExpr 0x7fffbb93c078 <col:5> 'c' 'void () noexcept'
    |-CXXMemberCallExpr 0x7fffbb965608 <line:11:3, col:12> 'int'
    | `-MemberExpr 0x7fffbb965580 <col:3, col:10> '<bound member function type>' .foo 0x7fffbb93c280
    |   `-DeclRefExpr 0x7fffbb93c0b8 <col:3> 'c' lvalue Var 0x7fffbb93bb20 't' 'c'
    `-ReturnStmt 0x7fffbb965648 <line:12:3, col:10>
      `-IntegerLiteral 0x7fffbb965628 <col:10> 'int' 0
*/
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
  ASSERT_TRUE(matches(Code, MemberFunctionMatch));
}