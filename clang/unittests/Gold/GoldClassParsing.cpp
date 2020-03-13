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
// #include "clang/Frontend/ASTUnit.h"

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
  /*
|-CXXRecordDecl 0x7fffe45a0570 <bin/cpp_test.cpp:2:1, line:5:1> line:2:8 referenced struct c definition
| |-DefinitionData pass_in_registers aggregate standard_layout trivially_copyable literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists non_trivial constexpr defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffe45a0698 <col:1, col:8> col:8 implicit struct c
| |
| |-FieldDecl 0x7fffe45a0748 <line:3:3, col:11> col:7 a 'int'
| | `-IntegerLiteral 0x7fffe45a0838 <col:11> 'int' 5
| |
| |-FieldDecl 0x7fffe45a07a8 <line:4:3, col:12> col:8 b 'bool'
| | `-CXXBoolLiteralExpr 0x7fffe45a0858 <col:12> 'bool' true
| |
| |-CXXConstructorDecl 0x7fffe45a0a70 <line:2:8> col:8 implicit used constexpr c 'void () noexcept' inline default
| | |-CXXCtorInitializer Field 0x7fffe45a0748 'a' 'int'
| | | `-CXXDefaultInitExpr 0x7fffe45a0f38 <col:8> 'int'
| | |-CXXCtorInitializer Field 0x7fffe45a07a8 'b' 'bool'
| | | `-CXXDefaultInitExpr 0x7fffe45a0f80 <col:8> 'bool'
| | `-CompoundStmt 0x7fffe45a0fd8 <col:8>
| |
| |-CXXConstructorDecl 0x7fffe45a0bb8 <col:8> col:8 implicit constexpr c 'void (const c &)' inline default trivial noexcept-unevaluated 0x7fffe45a0bb8
| | `-ParmVarDecl 0x7fffe45a0cd8 <col:8> col:8 'const c &'
| |
| `-CXXConstructorDecl 0x7fffe45a0d78 <col:8> col:8 implicit constexpr c 'void (c &&)' inline default trivial noexcept-unevaluated 0x7fffe45a0d78
|   `-ParmVarDecl 0x7fffe45a0e98 <col:8> col:8 'c &&'
|
`-FunctionDecl 0x7fffe45a08c0 <line:7:1, line:10:1> line:7:5 main 'int ()'
  `-CompoundStmt 0x7fffe45a1058 <col:12, line:10:1>
    |-DeclStmt 0x7fffe45a1010 <line:8:3, col:6>
    | `-VarDecl 0x7fffe45a09f0 <col:3, col:5> col:5 q 'c' callinit
    |   `-CXXConstructExpr 0x7fffe45a0fe8 <col:5> 'c' 'void () noexcept'
    `-ReturnStmt 0x7fffe45a1048 <line:9:3, col:10>
      `-IntegerLiteral 0x7fffe45a1028 <col:10> 'int' 0
  */
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

