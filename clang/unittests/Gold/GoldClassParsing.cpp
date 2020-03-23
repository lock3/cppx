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
      isDefaulted(), isNoThrow()))
    //, TODO: Make sure that we correctly output the move constructor? Maybe?
    // hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
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
  //  
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
  /*
`-ReturnStmt 0x7fffc8c3bd08 <line:10:3, col:16>
  `-CXXMemberCallExpr 0x7fffc8c3bce8 <col:10, col:16> 'int'
    `-MemberExpr 0x7fffc8c3bcb8 <col:10, col:12> '<bound member function type>' .foo 0x7fffc8c10a90
      `-DeclRefExpr 0x7fffc8c3bc98 <col:10> 'C':'C' lvalue Var 0x7fffc8c10d70 'b' 'C':'C'
  */
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
// TODO: This is the next thing to implement. Maybe...
// TEST(ClassParsing, MemberAccessFromFunctionResult) {
//   StringRef Code = R"(
// c : type = class:
//   x : int
//   y : bool
// foo(z: int): c!
//   q : c
//   return q

// main() : int!
//   q : c
//   foo(1).x = 4
//   return 0
//   )";
//   SimpleGoldParseTest(Code);
// }
// }


TEST(ClassParsing, ConstructorDeclWithinClass_BeforeMembers) {
/*
Expected
| |-CXXConstructorDecl 0x7fffcdf04a98 <line:3:3, line:5:3> line:3:3 used C 'void ()'
| | |-CXXCtorInitializer Field 0x7fffcdf049b8 'x' 'int'
| | | `-CXXDefaultInitExpr 0x7fffcdf04b88 <col:3> 'int'
| | `-CompoundStmt 0x7fffcdf04c58 <col:7, line:5:3>
| |   `-BinaryOperator 0x7fffcdf04c38 <line:4:5, col:9> 'int' lvalue '='
| |     |-MemberExpr 0x7fffcdf04be8 <col:5> 'int' lvalue ->x 0x7fffcdf049b8
| |     | `-CXXThisExpr 0x7fffcdf04bd8 <col:5> 'C *' implicit this
| |     `-IntegerLiteral 0x7fffcdf04c18 <col:9> 'int' 5

Actual
|-CXXConstructorDecl 0x7fffedecece0 <line:3:23, line:4:7> line:3:23 c 'void (void)' inline
| `-CompoundStmt 0x7fffedecf008 <line:4:7>
|   `-DeclStmt 0x7fffedeceff0 <col:7, col:5>
|     `-VarDecl 0x7fffedecee90 <col:7, col:9> col:7 x 'int':'int' auto listinit
|       `-IntegerLiteral 0x7fffedeceef8 <col:9> 'int' 4
*/
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

  // TODO: Figure out what's supposed to be happening here.
  DeclarationMatcher ClassCInfo = recordDecl(
    recordDecl(hasName("c")),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic())),
    hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
      isPublic())),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), unless(isImplicit()),
      unless(isDefaulted()), isNoThrow()))
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
