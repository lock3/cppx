//=== BlueClassElab.cpp =------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Tests for basic class elaboration.
//
//===----------------------------------------------------------------------===//
#include "BlueParseUtil.h"
#include "BlueASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace blue;

TEST(BlueClass, SimpleClassDecl) {
  StringRef Code = R"BLUE(
type C : class = { }
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("C"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassWithMemberDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  var x:int;
}
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("C"),
    hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
      isPublic()))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassDeclUse_DefaultInit) {
  StringRef Code = R"BLUE(
type C : class = {
}
var x:C;

)BLUE";
  auto ToMatch = translationUnitDecl(has(cxxRecordDecl(
    hasName("C"),
    hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
      isDefaulted(), isNoThrow())),
    hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
    )),
    has(varDecl(hasName("x"), hasType(asString("struct C"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, InclassMemberInitializers)  {
  StringRef Code = R"BLUE(
type C : class = {
  var y : bool = 1;
  var x : int = 4;
}
var x:C;
  )BLUE";
  auto ToMatch = translationUnitDecl(has(
      cxxRecordDecl(
        hasName("C"),
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
      )
    ),
    has(varDecl(hasName("x"), hasType(asString("struct C"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ReferenceToSelfAsPtr)  {
  StringRef Code = R"BLUE(
type C : class = {
  var x : ^C = null;
}
var x:C;
  )BLUE";
  auto ToMatch = translationUnitDecl(has(
      cxxRecordDecl(
        hasName("C"),
        hasDescendant(fieldDecl(hasName("x"), hasType(asString("struct C *")),
          isPublic())),
        hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
          isDefaulted(), isNoThrow(),
          hasDescendant(cxxCtorInitializer(forField(hasName("x")),
            isMemberInitializer() ))
        )),
        hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
          isDefaulted(), isNoThrow()))
      )
    ),
    has(varDecl(hasName("x"), hasType(asString("struct C"))))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MemberUse)  {
  StringRef Code = R"BLUE(
type C : class = {
  var x : int = 4;
  var y : bool = 1;
}
var x:C;
var z:=x.x;
  )BLUE";
  auto ToMatch = translationUnitDecl(has(
      cxxRecordDecl(
        hasName("C"),
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
      )
    ),
    has(varDecl(hasName("x"), hasType(asString("struct C")))),
    hasDescendant(
      memberExpr(
        member(hasName("x")),
        unless(isArrow()),
        hasDescendant(
          declRefExpr(
            to(
              varDecl(
                hasType(
                  cxxRecordDecl(hasName("C"))
                )
              )
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MemberUse_ThroughPtr)  {
  StringRef Code = R"BLUE(
type C : class = {
  var x : int = 4;
  var y : bool = 1;
}
var x:^C;
var z:=x.x;
  )BLUE";
  auto ToMatch = translationUnitDecl(has(
      cxxRecordDecl(
        hasName("C"),
        hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
          isPublic())),
        hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
          isPublic()))
      ))
    ,
    has(varDecl(hasName("x"), hasType(asString("struct C *"))))
    ,
    hasDescendant(
      memberExpr(
        // member(hasName("x")),
        isArrow(),
        hasDescendant(
          declRefExpr(
            to(
              varDecl(
                hasName("x")
              )
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, NestedTypeDefinition) {
  StringRef Code = R"BLUE(
type outer : class = {
  type nested : class = {
    var a : int;
    var b : float32;
  }
}
var u : outer.nested;
)BLUE";

  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("outer"),
      has(recordDecl(hasName("nested"),
        hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
          isPublic())),
        hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
          isPublic()))
      ))
    )),
    has(varDecl(
      hasType(asString("struct outer::nested")),
      hasName("u")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MultipleNestedTypeDefinition) {
  StringRef Code = R"(
type c : class = {
  type nested : class = {
    type nested2 : class = {
      var a : int;
      var b : float32;
    }
  }
}

var u : c.nested.nested2;

)";

  auto ToMatch = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      has(recordDecl(hasName("nested"),
        has(recordDecl(hasName("nested2"),
          hasDescendant(fieldDecl(hasName("a"), hasType(asString("int")),
            isPublic())),
          hasDescendant(fieldDecl(hasName("b"), hasType(asString("float")),
            isPublic()))
        ))
      ))
    )),
    has(varDecl(
      hasType(asString("struct c::nested::nested2")),
      hasName("u")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, TypeAliasDecl) {
  StringRef Code = R"BLUE(
type outer : class = {
  var x : type = int;
}
)BLUE";

  auto ToMatch = cxxRecordDecl(
    hasName("outer"),
    has(typeAliasDecl(hasName("x"), hasType(asString("int"))))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, TypeAliasDecl_OutOfOrderUse) {
  StringRef Code = R"BLUE(
type outer : class = {
  var y : x;
  var x : type = int;
}
)BLUE";

  auto ToMatch = cxxRecordDecl(
    hasName("outer"),
    has(typeAliasDecl(hasName("x"), hasType(asString("int")), isPublic())),
    has(fieldDecl(hasName("y"), hasType(asString("outer::x")), isPublic()))
  );

  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MemberFunctionDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  var x:int;
  func foo:(inout this) void ={ }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("foo")))
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MemberFunctionTemplateDecl) {
  StringRef Code = R"BLUE(
type C : class = {
  var x:int;
  func foo:[T:type](inout this) void = { }
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(functionTemplateDecl(has(cxxMethodDecl(hasName("foo")))))
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(BlueClass, MemberFunctionDecl_Use) {
  StringRef Code = R"BLUE(
type C : class = {
  var x:int;
  func foo:(inout this) void ={ }
}
func bar:(x:^C) void = {
  x.foo()
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(cxxMethodDecl(hasName("foo")))
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, MemberFunctionTemplateDecl_Use) {
  StringRef Code = R"BLUE(
type C : class = {
  var x:int;
  func foo:[T:type](inout this) void = { }
}
func bar:(x:^C) void = {
  x.foo[int]()
}
)BLUE";
  auto ToMatch = translationUnitDecl(hasDescendant(cxxRecordDecl(
    hasName("C"),
    has(functionTemplateDecl(has(cxxMethodDecl(hasName("foo")))))
  )));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, BaseClasses) {
  StringRef Code = R"BLUE(
type B : class = { }
type C : class = {
  super :B;
}
)BLUE";
  auto ToMatch = cxxRecordDecl(
    hasName("C"),
    isDirectlyDerivedFrom(hasName("B"))
  );
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}