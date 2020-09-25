//=== GoldNestedNameSpecifierOnDeclElab.cpp --------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file implements tests for elaboration of declarations with nested name
//  specifiers.
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldCompileRun.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldNestedNameDecl, NamespaceNameFunctionDef) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x.foo():void!
  i :int  = 5
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(functionDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, NamespaceOperatorDeclDef) {
  StringRef Code = R"(
x : namespace = namespace:
  Y = class:
    ;
  operator"=="(a:ref Y, b:ref Y):bool

X.operator"=="(a:ref Y, b:ref Y):bool
  return false
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(functionDecl(hasName("operator=="), unless(isDefinition())))
    )),
    has(functionDecl(hasName("operator=="),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, NamespaceNameVarTempalteDeclDef) {
  StringRef Code = R"(
x : namespace = namespace:
  VTD[T:type]<extern> : T

x.VTD[T:type] : T = 34
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(varTemplateDecl(hasName("VTD")))
    )),
    has(varTemplateDecl(hasName("VTD")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, NamespaceNameFunctionTempalteDeclDef) {
  StringRef Code = R"(
x : namespace = namespace:
  Fn[T:type](x:T):void

x.Fn[T:type](x:T):void!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      hasDescendant(functionDecl(hasName("Fn")))
    )),
    has(functionTemplateDecl(hasName("Fn")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, NamespaceNameFunctionDefThroughNSAlias) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

X2:namespace = x
X2.foo():void!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(functionDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImproperFunctionLookup) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x.foo():void!
  ;
test():void!
  foo()
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, NamespaceNameClassDef) {
  StringRef Code = R"(
x : namespace = namespace:
  Ty : type = class

x.Ty : type = class:
  ;
)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(cxxRecordDecl(hasName("Ty"), unless(isDefinition())))
    )),
    has(cxxRecordDecl(hasName("Ty"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, NamespaceExternVariableWithLaterDecl) {
  StringRef Code = R"(
x : namespace = namespace:
  Var<extern>:int

x.Var:int = 5

)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      hasName("x"),
      has(varDecl(hasName("Var"), unless(isDefinition())))
    )),
    has(varDecl(hasName("Var"),
      has(nestedNameSpecifier(specifiesNamespace(hasName("x"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, FunctionDeclWithinClassDefOutside) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo() : int

c.foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      has(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, StaticMemberOutsideOfClass) {
  StringRef Code = R"(
c : type = class:
  x<static>:float64

c.x : float64 = 10.0
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      has(varDecl(hasName("x"), unless(isDefinition())))
    )),
    has(varDecl(hasName("x")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

c[T:type].foo() : T!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassTemplate_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

c[T:type].foo() : T!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, Method_FromClassTemplateParameterNameSwitch) {
  StringRef Code = R"(
c[T:type, U:type] : type = class:
  x : T
  y : bool
  foo() : T

c[U:type, T:type].foo() : T!
  return x
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, Method_FromClassExplicitSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[int] : type = class {
  x : int
  y : bool
  foo() : int
}

c[int].foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassExplicitSpecialization_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[int] : type = class {
  x : int
  y : bool
  foo() : int
}

Q: type = c[int]

Q.foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassPartialSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[T:type][^T] : type = class:
  x : ^T
  y : bool
  foo() : T

c[T:type][^T].foo() : T!
  return ^x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, FunctionTemplate_DefOutsideOfClass) {
  StringRef Code = R"(
c : type = class:
  x : int
  y : bool
  foo[T:type]() : int

c.foo[T:type]() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(functionTemplateDecl(has(cxxMethodDecl(hasName("foo"),
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ConstructorDefOutsideOfClass) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c.constructor()!
  ;
)";
  auto Matcher = translationUnitDecl(
    has(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxConstructorDecl(unless(isDefinition())))
    )),
    has(cxxConstructorDecl(
      has(nestedNameSpecifier(specifiesType(asString("struct c"))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, Method_FromClassPartialSpecialization_ThroughAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  ;

c[T:type][^T] : type = class:
  x : ^T
  y : bool
  foo() : T

Q[T:type] : type = c[^T]

Q[T:type].foo() : T!
  return ^x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}



// Special error cases
TEST(GoldNestedNameDecl, InvalidNamespaceWithTemplateParameters) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x[T:type].foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidUsingVariableAsNNS) {
  StringRef Code = R"(
x = 4
x.foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidNamespaceWithSpecializtion) {
  StringRef Code = R"(
x : namespace = namespace:
  foo():void

x[int].foo():void!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, TypeCannotBeUsedAsANNS) {
  StringRef Code = R"(
int.something()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidTypeNotATemplate) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c[T:type].constructor()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, InvalidTypeNotATemplateSpecialization) {
  StringRef Code = R"(
c : type = class:
  constructor()
  y : bool

c[int].constructor()!
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_MemberFunctionTest) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():void

c[int].bar():void!
  bar()
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_MemberTest) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():void
  x : bool

c[int].bar():void!
  x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}



TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_StaticMember) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():void
  x<static> : const T = 5

c[int].bar():void!
  y = x + 3
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_ThroughStaticMember) {
  StringRef Code = R"(
c[T:type] : type = class:
  x<static> : T

c[int].x : int = 5
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl()),
    has(varDecl(hasName("x"))//,
      // has(nestedNameSpecifier(specifiesNamespace(hasName("c"))))
    )
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_Constructor) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():void
  constructor()!
    ;

c[int].bar():void!
  ;
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_Destructor) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():void
  destructor()!
    ;

c[int].bar():void!
  ;
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_Operator) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar():bool
  operator"=="(other:const ref c[T])<const>:bool!
    return false

c[int].bar():bool!
  x: c[int]
  y : c[int]
  return ^this == y
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NestedTypeDefinition) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  Ty : type = class:
    y : bool = false

c[int].bar():bool!
  x : Ty
  return x.y
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_ThroughTypeDefinedOutsideOfClass) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  Ty : type = class

c[int].Ty : type = class:
  y : bool = false
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NestedClassTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  TemplateType[U:type] : type = class:
    y :bool = false

c[int].bar():bool!
  x = TemplateType[float64]()
  return x.y
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NestedClassTemplate_W_StaticMember) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : int
  TemplateType[U:type] : type = class:
    z <static><inline>: const int = 5

c[int].bar():int!
  return TemplateType[float64].z
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NestedClassTemplateSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  TemplateType[U:type] : type = class:
    y :bool = false

  TemplateType[int] : type = class:
    z : bool

c[int].bar():bool!
  x = TemplateType[int]()
  return x.z
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NestedClassTemplatePartialSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  TemplateType[U:type] : type = class:
    y :bool = false

  TemplateType[U:type][^U] : type = class:
    z : bool

c[int].bar():bool!
  x = TemplateType[^int]()
  return x.z
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_FunctionTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  foo[U:type](p:U) : bool!
    return false

c[int].bar() : bool!
  return foo(5)
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

// TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_FunctionTemplateImplicitExplicitSpecialization) {
//   StringRef Code = R"(
// c[T:type] : type = class:
//   bar() : bool
//   foo[U:type](p:U) : bool!
//     return false
//   foo[](p:int) : bool!
//     return false

// c[int].bar() : bool!
//   return foo(4)
// )";
//   auto Matcher = translationUnitDecl(
//     hasDescendant(classTemplateSpecializationDecl())
//   );
//   ASSERT_TRUE(matches(Code.str(), Matcher));
// }

// TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_FunctionTemplateExplicitSpecialization) {
//   StringRef Code = R"(
// c[T:type] : type = class:
//   bar() : bool
//   foo[U:type](p:U) : bool!
//     return false

//   foo[int](p:int) : bool!
//     return true

// c[int].bar() : bool!
//   return foo(5)
// )";
//   auto Matcher = translationUnitDecl(
//     hasDescendant(classTemplateSpecializationDecl())
//   );
//   ASSERT_TRUE(matches(Code.str(), Matcher));
// }

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_TypeAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : type = bool
  bar() : x

c[int].bar() : x!
  ret : x = true
  return ret
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_NamespaceAlias) {
  StringRef Code = R"(
NS : namespace = namespace:
  x : type = bool

c[T:type] : type = class:
  Q : namespace = NS
  bar() : Q.x

c[int].bar() : Q.x!
  return true
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_VarTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  TmpltVar[U:type] <static>: const U = 12345
  bar() : bool

c[int].bar() : bool!
  return TmpltVar[bool]
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_VarTemplate_NonConstStaticMemberInitErr) {
  StringRef Code = R"(
c[T:type] : type = class:
  TmpltVar[U:type] <static>: U = 12345
  bar() : bool

c[int].bar() : bool!
  return TmpltVar[bool]
)";
  GoldFailureTest(Code);
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_VarTemplateSpecialization) {
  StringRef Code = R"(
c[T:type] : type = class:
  TmpltVar[U:type] : U
  TmpltVar[int]<static> : const int = 54321
  bar() : bool

c[int].bar() : bool!
  return TmpltVar[int]
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_Enum) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  Enum :type = enum:
    a
    b
    c

c[int].bar() : bool!
  return bool(Enum.a)
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_Union) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  UnionType : type = union:
    a : int
    b : float64
    c : ^int

c[int].bar() : bool!
  X :UnionType
  return X.a
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_UnionTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  bar() : bool
  UnionTemplateType[U:type] : type = union:
    a : U
    b : float64
    c : ^int

c[int].bar() : bool!
  X :UnionTemplateType[^float32]
  return X.a
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_TemplateAlias) {
  StringRef Code = R"(
X[T:type] : type = class:
  a : bool

c[T:type] : type = class:
  bar() : bool
  TemplateAlias[U:type] : type = X[U]

c[int].bar() : bool!
  X : TemplateAlias[^float32]
  return X.a
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_DependentTemplateTypeAlias) {
  StringRef Code = R"(
X[T:type, U:type] : type = class:
  a :int
c[T:type] : type = class:
  bar() : bool
  TemplateAlias[U:type] : type = X[T, U]

c[int].bar() : bool!
  X : TemplateAlias[float32]
  return X.a
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(classTemplateSpecializationDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

// TODO: Add a conversion operator to this once we implement it.


TEST(GoldNestedNameDecl, ConstructorOutsideOfClass) {
  StringRef Code = R"(
c[T:type] : type = class:
  constructor()

c[int].constructor()!
  ;
)";
  // Figure out how to check for the implicit specialization
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxConstructorDecl(unless(isDefinition())))
    )),
    has(cxxConstructorDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, DestructorOutsideOfClass) {
  StringRef Code = R"(
c[T:type] : type = class:
  destructor()

c[int].destructor()!
  ;
)";
  // Figure out how to check for the implicit specialization
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxDestructorDecl(unless(isDefinition())))
    )),
    has(cxxDestructorDecl())
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_ThroughTypeAlias) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

X : type = c[int]

X.foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldNestedNameDecl, ImplicitSpecialization_CToG_ThroughTypeAliasTemplate) {
  StringRef Code = R"(
c[T:type] : type = class:
  x : T
  y : bool
  foo() : T

X[T:type] : type = c[T]

X[int].foo() : int!
  return x
)";
  auto Matcher = translationUnitDecl(
    hasDescendant(cxxRecordDecl(
      hasName("c"),
      hasDescendant(cxxMethodDecl(hasName("foo"), unless(isDefinition())))
    )),
    has(cxxMethodDecl(hasName("foo")))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}