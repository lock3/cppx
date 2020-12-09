//=== GoldNewAndDeleteElab.cpp ---------------------------------------========//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the elaboration for the inplace new operator.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

#if 0
// ===---------------------------------------------------------------------===//
//                          Free function overloading
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, NewOverload) {
  std::string Code = R"Gold(
operator"new"(sz:uint64, ptr:^void):^void!
  return null
)Gold";
  auto ToMatch = functionDecl(
    hasName("operator new"),
    unless(isImplicit()),
    hasType(asString("void *(unsigned long, void *)"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, DeleteOverload) {
  std::string Code = R"Gold(
operator"delete"(ptr:^void)<noexcept>:void!
  ;
)Gold";
  auto ToMatch = functionDecl(
    hasName("operator delete"),
    unless(isImplicit()),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, ArrayNewOverload) {
  std::string Code = R"Gold(
operator"new[]"(sz:uint64):^void!
  return null
)Gold";
  auto ToMatch = functionDecl(
    hasName("operator new[]"),
    unless(isImplicit()),
    hasType(asString("void *(unsigned long)"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, ArrayDeleteOverload) {
  std::string Code = R"Gold(
operator"delete[]"(ptr:^void):void!
  ;
)Gold";
  auto ToMatch = functionDecl(
    hasName("operator delete[]"),
    unless(isImplicit()),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, PlacementDeleteDecl) {
  std::string Code = R"Gold(
operator"delete"(ptr:^void, i:int)<noexcept>:void!
  ;
)Gold";
  auto ToMatch = functionDecl(hasName("operator delete"),
    hasType(asString("void (void *, int) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

// ===---------------------------------------------------------------------===//
//                     Explicit calls to new and delete
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, ExplicitCallNewAndDelete) {
  std::string Code = R"Gold(
operator"new"(sz:uint64, ptr:^void):^void!
  return ptr

foo():void!
  x:^void = operator"new"(0, null)
  operator"delete"(x)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator new"),
      unless(isImplicit()),
      hasType(asString("void *(unsigned long, void *)"))
    )),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(callExpr(
        hasType(asString("void *")),
        has(implicitCastExpr(
          hasType(asString("void *(*)(unsigned long, void *)")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator new")))
          ))
        ))
      )),
      hasDescendant(callExpr(
        hasType(asString("void")),
        has(implicitCastExpr(
          hasType(asString("void (*)(void *) noexcept")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator delete")))
          ))
        ))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, ExplicitCallArrayNewAndDelete) {
  std::string Code = R"Gold(
operator"new[]"(sz:uint64, ptr:^void):^void!
  return ptr

operator"delete[]"(ptr:^void)<noexcept>:void!
  ;

foo():void!
  x:^void = operator"new[]"(0, null)
  operator"delete[]"(x)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("operator new[]"),
      unless(isImplicit()),
      hasType(asString("void *(unsigned long, void *)"))
    )),
    has(functionDecl(
      hasName("foo"),
      hasDescendant(callExpr(
        hasType(asString("void *")),
        has(implicitCastExpr(
          hasType(asString("void *(*)(unsigned long, void *)")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator new[]")))
          ))
        ))
      )),
      hasDescendant(callExpr(
        hasType(asString("void")),
        has(implicitCastExpr(
          hasType(asString("void (*)(void *) noexcept")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator delete[]")))
          ))
        ))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, ExplicitCallToBuiltInNewAndDelete) {
  std::string Code = R"Gold(
foo():void!
  x:^void = operator"new"(0)
  operator"delete"(x)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("foo"),
      hasDescendant(callExpr(
        hasType(asString("void *")),
        has(implicitCastExpr(
          hasType(asString("void *(*)(unsigned long)")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator new")))
          ))
        ))
      )),
      hasDescendant(callExpr(
        hasType(asString("void")),
        has(implicitCastExpr(
          hasType(asString("void (*)(void *) noexcept")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator delete")))
          ))
        ))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, ExplicitCallToBuiltInArrayNewAndDelete) {
  std::string Code = R"Gold(
foo():void!
  x:^void = operator"new[]"(0)
  operator"delete[]"(x)
)Gold";
  auto ToMatch = translationUnitDecl(
    has(functionDecl(
      hasName("foo"),
      hasDescendant(callExpr(
        hasType(asString("void *")),
        has(implicitCastExpr(
          hasType(asString("void *(*)(unsigned long)")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator new[]")))
          ))
        ))
      )),
      hasDescendant(callExpr(
        hasType(asString("void")),
        has(implicitCastExpr(
          hasType(asString("void (*)(void *) noexcept")),
          has(declRefExpr(
            hasDeclaration(functionDecl(hasName("operator delete[]")))
          ))
        ))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

// ===---------------------------------------------------------------------===//
//                Operator syntax use of operator new and delete
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, BasicNewDelete) {
  std::string Code = R"Gold(
foo():void!
  x:^int = new [int](4)
  delete x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, PlacementNewExpr) {
  std::string Code = R"Gold(
operator"new"(s:uint64, ptr:^void)<noexcept>: ^void!
  return ptr

foo(x:^int):void!
  new (x) [int](4)
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        unless(isImplicit()),
        hasType(asString("void *(unsigned long, void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, DeleteAsAnExpression) {
  std::string Code = R"Gold(
foo(x:^int):void!
  return delete x
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, BasicNewDeleteArray) {
  std::string Code = R"Gold(
foo():void!
  x:^int = new [[3]int]
  delete [] x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete[]"),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, BasicNewDeleteArray_WithCall) {
  std::string Code = R"Gold(
foo():void!
  x:^int = new [[3]int]()
  delete [] x
)Gold";
  GoldFailureTest(Code);
}


TEST(GoldNewDelete, ArrayNewPlacementArguments) {
  std::string Code = R"Gold(
operator"new[]"(s:uint64, a:int, b:int, c:int)<noexcept>: ^void!
  return null

foo():void!
  x:^int = new (1, 2, 3) [[3]int]
  delete [] x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long, int, int, int) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}



// ===---------------------------------------------------------------------===//
//              Member overloads and static member overloads
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, NewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new"(sz:uint64)<noexcept>:^void!
    return null

)Gold";
  auto ToMatch = cxxMethodDecl( hasName("operator new"),
    hasType(asString("void *(unsigned long) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, DeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete"(ptr:^void)<noexcept>:void!
    ;
)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator delete"),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, ArrayNewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new[]"(sz:uint64)<noexcept>:^void!
    return null

)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator new[]"),
    hasType(asString("void *(unsigned long) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, ArrayDeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete[]"(ptr:^void)<noexcept>:void!
    ;
)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator delete[]"),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, StaticNewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new"(sz:uint64)<static><noexcept>:^void!
    return null

)Gold";
  auto ToMatch = cxxMethodDecl( hasName("operator new"),
    isStaticStorageClass(),
    hasType(asString("void *(unsigned long) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, StaticDeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete"(ptr:^void)<static><noexcept>:void!
    ;
)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator delete"),
    isStaticStorageClass(),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, StaticArrayNewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new[]"(sz:uint64)<static><noexcept>:^void!
    return null

)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator new[]"),
    isStaticStorageClass(),
    hasType(asString("void *(unsigned long) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, StaticArrayDeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete[]"(ptr:^void)<static><noexcept>:void!
    ;
)Gold";
  auto ToMatch = cxxMethodDecl(hasName("operator delete[]"),
    isStaticStorageClass(),
    hasType(asString("void (void *) noexcept"))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}



// ===---------------------------------------------------------------------===//
//                      Member overload lookups
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, Use_NewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new"(sz:uint64)<noexcept>:^void!
    return null

foo():void!
  x:^C = new [C];
)Gold";
  auto ToMatch = cxxNewExpr(
    hasDeclaration(cxxMethodDecl( hasName("operator new"),
      hasType(asString("void *(unsigned long) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, Use_DeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete"(ptr:^void)<noexcept>:void!
    ;

foo(x:^C):void!
   delete x
)Gold";
  auto ToMatch = cxxDeleteExpr(
    deleteFunction(cxxMethodDecl(hasName("operator delete"),
      hasType(asString("void (void *) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, Use_ArrayNewMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"new[]"(sz:uint64)<noexcept>:^void!
    return null

foo():void!
  x:^C = new [[3]C];
)Gold";
  auto ToMatch = cxxNewExpr(
    hasDeclaration(cxxMethodDecl(hasName("operator new[]"),
      hasType(asString("void *(unsigned long) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, Use_ArrayDeleteMemberOverload) {
  std::string Code = R"Gold(
C = class:
  operator"delete[]"(ptr:^void)<noexcept>:void!
    ;
foo(x:^C):void!
   delete [] x
)Gold";
  auto ToMatch = cxxDeleteExpr(
    deleteFunction(cxxMethodDecl(hasName("operator delete[]"),
      hasType(asString("void (void *) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

// ===---------------------------------------------------------------------===//
//                      Lookup inside of a base class
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, Use_NewFromBaseClass) {
  std::string Code = R"Gold(
B = class:
  operator"new"(sz:uint64)<noexcept>:^void!
    return null
C = class(B):
  ;
foo():void!
  x:^C = new [C];
)Gold";
  auto ToMatch = cxxNewExpr(
    hasDeclaration(cxxMethodDecl( hasName("operator new"),
      hasType(asString("void *(unsigned long) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, Use_DeleteFromBaseClass) {
  std::string Code = R"Gold(
B = class:
  operator"delete"(ptr:^void)<noexcept>:void!
    ;
C = class(B):
  ;

foo(x:^C):void!
   delete x
)Gold";
  auto ToMatch = cxxDeleteExpr(
    deleteFunction(cxxMethodDecl(hasName("operator delete"),
      hasType(asString("void (void *) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, Use_ArrayNewFromBaseClass) {
  std::string Code = R"Gold(
B = class:
  operator"new[]"(sz:uint64)<noexcept>:^void!
    return null
C = class(B):
  ;
foo():void!
  x:^C = new [[3]C];
)Gold";
  auto ToMatch = cxxNewExpr(
    hasDeclaration(cxxMethodDecl(hasName("operator new[]"),
      hasType(asString("void *(unsigned long) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, Use_ArrayDeleteFromBaseClass) {
  std::string Code = R"Gold(
B = class:
  operator"delete[]"(ptr:^void)<noexcept>:void!
    ;
C = class(B):
  ;
foo(x:^C):void!
   delete [] x
)Gold";
  auto ToMatch = cxxDeleteExpr(
    deleteFunction(cxxMethodDecl(hasName("operator delete[]"),
      hasType(asString("void (void *) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


// ===---------------------------------------------------------------------===//
//                   Lookup of new from use inside of a type
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, UsingNewInsideAClassWhereNewIsDefinedButNotUsed) {
  std::string Code = R"Gold(
B = class:
  operator"new"(sz:uint64)<noexcept>:^void!
    return null
  foo():void!
    x:^int = new [int];
)Gold";
  auto ToMatch = cxxNewExpr(
    hasDeclaration(functionDecl(hasName("operator new"),
      isImplicit(),
      hasType(asString("void *(unsigned long)"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, UsingDeleteInsideAClassWhereNewIsDefinedButNotUsed) {
  std::string Code = R"Gold(
B = class:
  operator"delete"(ptr:^void)<noexcept>:void!
    ;
  foo(x:^int):void!
    delete x
)Gold";
  auto ToMatch = cxxDeleteExpr(
    deleteFunction(functionDecl(hasName("operator delete"),
      isImplicit(),
      hasType(asString("void (void *) noexcept"))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, NewOverloadWithPlacementArgsUsedInsideType) {
  std::string Code = R"Gold(
B = class:
  operator"new"(sz:uint64, s:int)<noexcept>:^void!
    return null
  foo():void!
    x:^int = new (1) [int];
)Gold";
  GoldFailureTest(Code);
}

// TODO: Create an explicit Initialization argument
// ===---------------------------------------------------------------------===//
//                   Lookup of new from use inside of a type
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, DynamicallySizedArray) {
  std::string Code = R"Gold(
foo():void!
  y = 3
  x:^int = new [[y]int]
  delete [] x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete[]"),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, NestedDynamicallySizedArray) {
  std::string Code = R"Gold(
foo():void!
  y = 3
  x = new [[100, y]int]
  delete [] x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(varDecl(hasName("x"), hasType(asString("int (*)[100]")))),
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete[]"),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, InvalidDynamicallySizedArray) {
  std::string Code = R"Gold(
foo():void!
  y = 3
  x = new [[y, 100]int]
  delete [] x
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldNewDelete, ArraySizeThroughUsing) {
  std::string Code = R"Gold(
Ty : type = [4]int
foo():void!
  x:^int = new [Ty]
  delete [] x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      hasDeclaration(functionDecl(
        hasName("operator new[]"),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete[]"),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

// Create one for template, and member types.
TEST(GoldNewDelete, ParenTypeExpr) {
  std::string Code = R"Gold(
foo():void!
  x:^int = new () [(int)]
  delete x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, NewDeleteMemberTy) {
  std::string Code = R"Gold(
B = class:
  C = class:
    ;
  foo() :void!
    v:^C = new [C]
    delete v
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, TemplateTypeExpr) {
  std::string Code = R"Gold(
Template[T:type] = class:
  ;

foo():void!
  x = new [Template[int]]
  delete x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}


TEST(GoldNewDelete, TemplateTypeIncompleteExpr) {
  std::string Code = R"Gold(
Template[T:type] = class:
  ;

foo():void!
  x = new [Template]
  delete x
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldNewDelete, NamespaceType) {
  std::string Code = R"Gold(
foo():void!
  x = new [namespace]
  delete x
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldNewDelete, KindType) {
  std::string Code = R"Gold(
foo():void!
  x  = new [type]
  delete x
)Gold";
  GoldFailureTest(Code);
}

TEST(GoldNewDelete, NewFunction) {
  std::string Code = R"Gold(
foo():void!
  x = new [(int)->void]
)Gold";
  GoldFailureTest(Code);
}



TEST(GoldNewDelete, NewAuto) {
  std::string Code = R"Gold(
foo():void!
  x = new [auto](4)
  delete x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      ))
    )),
    hasDescendant(cxxDeleteExpr(
      deleteFunction(functionDecl(
        hasName("operator delete"),
        isImplicit(),
        hasType(asString("void (void *) noexcept"))
      ))
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}



// ===---------------------------------------------------------------------===//
//                Universal init operator new
// ===---------------------------------------------------------------------===//
TEST(GoldNewDelete, InitList_NewDelete) {
  std::string Code = R"Gold(
foo():void!
  x:^int = new [int]{4}
  delete x
)Gold";
  auto ToMatch = functionDecl(
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        isImplicit(),
        hasType(asString("void *(unsigned long)"))
      )),
      hasDescendant(initListExpr())
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

TEST(GoldNewDelete, InitList_PlacementNewExpr) {
  std::string Code = R"Gold(
operator"new"(s:uint64, ptr:^void)<noexcept>: ^void!
  return ptr

foo(x:^int):void!
  new (x) [int]{ 4 }
)Gold";
  auto ToMatch = functionDecl(
    hasName("foo"),
    hasDescendant(cxxNewExpr(
      unless(isArray()),
      hasDeclaration(functionDecl(
        hasName("operator new"),
        unless(isImplicit()),
        hasType(asString("void *(unsigned long, void *) noexcept"))
      )),
      hasDescendant(initListExpr())
    ))
  );
  ASSERT_TRUE(matches(Code, ToMatch));
}

// TEST(GoldNewDelete, InitList_BasicNewDeleteArray) {
//   std::string Code = R"Gold(
// foo():void!
//   x:^int = new [[3]int]
//   delete [] x
// )Gold";
//   auto ToMatch = functionDecl(
//     hasDescendant(cxxNewExpr(
//       hasDeclaration(functionDecl(
//         hasName("operator new[]"),
//         hasType(asString("void *(unsigned long)"))
//       ))
//     )),
//     hasDescendant(cxxDeleteExpr(
//       deleteFunction(functionDecl(
//         hasName("operator delete[]"),
//         hasType(asString("void (void *) noexcept"))
//       ))
//     ))
//   );
//   ASSERT_TRUE(matches(Code, ToMatch));
// }
#endif
