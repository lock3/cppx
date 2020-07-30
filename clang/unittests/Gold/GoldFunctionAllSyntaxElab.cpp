//=== GoldFunctionAllSyntaxElab.cpp ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file tests all of the different ways we can declare/define a function.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldFuncSyntax, FunctionDecl) {
  StringRef Code = R"(
foo():void
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
    unless(hasBody(compoundStmt())));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, FunctionDef) {
  StringRef Code = R"(
foo():void!
  ;
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
    hasBody(compoundStmt()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, MemberFunctionDecl) {
  StringRef Code = R"(
C : type = class:
  foo():void
)";
  DeclarationMatcher ToMatch = cxxMethodDecl(hasName("foo"),
    unless(hasBody(compoundStmt())));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, MemberFunctionDef) {
  StringRef Code = R"(
C :type = class:
  foo():void!
    ;
)";
  DeclarationMatcher ToMatch = cxxMethodDecl(hasName("foo"),
    hasBody(compoundStmt()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldFuncSyntax, PureVirtualDecl) {
//   StringRef Code = R"(
// C :type = class:
//   foo()<virtual>:void = 0
// )";
//   DeclarationMatcher ToMatch = cxxMethodDecl(hasName("foo"), isPure());
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(GoldFuncSyntax, FreeFunctionDeclLooksLikePureVirtual) {
  StringRef Code = R"(
foo():int = 0
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"),
    hasBody(compoundStmt()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, PureVirtualDestructor) {
  StringRef Code = R"(
C :type = class:
  destructor()<virtual>:void = 0
)";
  DeclarationMatcher ToMatch = cxxDestructorDecl(isPure());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, InvalidPureVirtualDestructor) {
  StringRef Code = R"(
C :type = class:
  destructor():void = 0
)";
  GoldFailureTest(Code);
}

TEST(GoldFuncSyntax, InvalidPureVirtualSpecification) {
  StringRef Code = R"(
C :type = class:
  foo():void = 0
)";
  GoldFailureTest(Code);
}

TEST(GoldFuncSyntax, PureVirtualAppliedToConstructor) {
  StringRef Code = R"(
C :type = class:
  constructor() : void = 0
)";
  GoldFailureTest(Code);
}

TEST(GoldFuncSyntax, ImplicitVirtualSpecifier) {
  StringRef Code = R"(
Base : type = class:
  foo()<virtual>:void = 0

C :type = class(Base):
  foo():void = 0
)";
  DeclarationMatcher ToMatch = cxxRecordDecl(hasName("C"),
    has(cxxMethodDecl(hasName("foo"), isPure()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DeletedFunction) {
  StringRef Code = R"(
foo() : void = delete
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), isDeleted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedDefaultConstructor) {
  StringRef Code = R"(
C :type = class:
  constructor() : void = default

)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isDefaultConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedCopyConstructor) {
  StringRef Code = R"(
C :type = class:
  constructor(Other:const ref C) : void = default

)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isCopyConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedMoveConstructor) {
  StringRef Code = R"(
C :type = class:
  constructor(Other:rref C) : void = default

)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isMoveConstructor(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedCopyAssignment) {
  StringRef Code = R"(
C :type = class:
  operator"="(Other:const ref C) : void = default

)";
  DeclarationMatcher ToMatch = cxxMethodDecl(
    hasOverloadedOperatorName("="), isCopyAssignmentOperator(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedMoveAssignment) {
  StringRef Code = R"(
C :type = class:
  operator"="(Other:rref C) : void = default

)";
  DeclarationMatcher ToMatch = cxxMethodDecl(
    hasOverloadedOperatorName("="), isMoveAssignmentOperator(), isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DeletedDefaultConstructord) {
  StringRef Code = R"(
C :type = class:
  constructor() : void = delete
)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isDefaultConstructor(), isDeleted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedDestructor) {
  StringRef Code = R"(
C :type = class:
  destructor() : void = default

)";
  DeclarationMatcher ToMatch = cxxDestructorDecl(isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, InvalidDefaultedFunction) {
  StringRef Code = R"(
something() : void = default
)";
  GoldFailureTest(Code);
}

TEST(GoldFuncSyntax, FunctionDef_BodyFromAssignedExpr_WithReturnType) {
  StringRef Code = R"(
foo() : int = 1 + 2
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), isDefinition());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, FunctionDef_Bang_NoReturnType) {
  StringRef Code = R"(
foo() !
  return 1 + 2
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), isDefinition());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
// FIXME: I may need to revisit this in the future.
// TEST(GoldFuncSyntax, FunctionTypeDecl) {
//   StringRef Code = R"(
// FTy : type = (int) -> void

// foo(x:int):void!
//   ;

// FuncPtr:^FTy = foo
// )";
//   DeclarationMatcher ToMatch = translationUnitDecl(
//     has(typeAliasDecl(hasName("FTy"), hasType(asString("void(int)")))),
//     has(varDecl(hasName("FuncPtr"), hasType(asString("void (*)(int)"))))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// FIXME: We need to implement this functionality, using speculative/possible
// declaration statements. Because this could be a function declaration OR
// a function call and assignement.

TEST(GoldFuncSyntax, FunctionDef_BodyFromAssignedExpr_NoReturnType) {
  StringRef Code = R"(
foo() = 1 + 2
)";
  DeclarationMatcher ToMatch = functionDecl(hasName("foo"), 
    hasBody(compoundStmt()));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}
TEST(GoldFuncSyntax, DefaultedConstructor_NoReturnType) {
  StringRef Code = R"(
C :type = class:
  constructor() = default

)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isDefaultConstructor(),isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DefaultedDestructor_NoReturnType) {
  StringRef Code = R"(
C :type = class:
  destructor() = default

)";
  DeclarationMatcher ToMatch = cxxDestructorDecl(isDefaulted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DeletedDefaultConstructord_NoReturnType) {
  StringRef Code = R"(
C :type = class:
  constructor() = delete

)";
  DeclarationMatcher ToMatch = cxxConstructorDecl(
    isDefaultConstructor(), isDeleted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, DeletedDestructord_NoReturnType) {
  StringRef Code = R"(
C :type = class:
  destructor() = delete

)";
  DeclarationMatcher ToMatch = cxxDestructorDecl(isDeleted());
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldFuncSyntax, InvalidPureVirtualFunctionDecl_NoReturnType) {
  StringRef Code = R"(
C :type = class:
  foo()<virtual> = 0
)";
  GoldFailureTest(Code);
}

TEST(GoldFuncSyntax, UseOfADeletedFunction) {
  StringRef Code = R"(
foo() : void = delete

main() : int!
  foo()
)";
  GoldFailureTest(Code);
}