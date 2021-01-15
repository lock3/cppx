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
C : class { };
)BLUE";
  auto ToMatch = cxxRecordDecl(hasName("C"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(BlueClass, ClassWithMemberDecl) {
  StringRef Code = R"BLUE(
C : class {
  x:int;
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
C : class {
}
x:C;

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
C : class{
  x : int = 4;
  y : bool = 1;
}
x:C;
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
C : class{
  x : ^C = null;
}
x:C;
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
C : class{
  x : int = 4;
  y : bool = 1;
}
x:C;
z:=x.x;
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
C : class{
  x : int = 4;
  y : bool = 1;
}
x:^C;
z:=x.x;
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

// TODO: IMplement me eventually.
// TEST(BlueClass, ClassDeclUse_DefaultCtor) {
//   StringRef Code = R"BLUE(
// C : class {
//   x:int;
// }
// x:C = ();

// )BLUE";
//   auto ToMatch = translationUnitDecl(has(cxxRecordDecl(
//     hasName("C"),
//     hasDescendant(cxxConstructorDecl(isDefaultConstructor(), isImplicit(),
//       isDefaulted(), isNoThrow())),
//     hasDescendant(cxxConstructorDecl(isCopyConstructor(), isImplicit(),
//       isDefaulted(), isNoThrow())),
//     hasDescendant(cxxConstructorDecl(isMoveConstructor(), isImplicit()))
//     )),
//     varDecl(hasName("x"), hasType(asString("struct C")))
//   );
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }