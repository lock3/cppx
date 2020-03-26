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
