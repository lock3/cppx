//===- unittest/Gold/GoldVarDeclStorageClass.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"


using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldVarDeclStorageClass, Static) {
  StringRef Code = R"(
foo<static> : int
)";
  DeclarationMatcher StaticVar = varDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}

TEST(GoldVarDeclStorageClass, ConflictingStorageClass) {
  StringRef Code = R"(
foo<static><extern> : int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, External) {
  StringRef Code = R"(
foo<extern>: int
)";
  DeclarationMatcher ExternVar = varDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}


TEST(GoldVarDeclStorageClass, LineAttr_Static) {
  StringRef Code = R"(
[static]
foo : int
)";
  DeclarationMatcher StaticVar = varDecl(
    hasName("foo"), isStaticStorageClass());
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}

TEST(GoldVarDeclStorageClass, LineAttr_External) {
  StringRef Code = R"(
[extern]
foo: int
)";
  DeclarationMatcher ExternVar = varDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}


TEST(GoldVarDeclStorageClass, ExternC_VarDecl) {
  StringRef Code = R"(
foo<extern("C")>: int
)";
  DeclarationMatcher ExternVar = varDecl(hasName("foo"), isExternC());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}

TEST(GoldVarDeclStorageClass, ExternC_FunctionDecl) {
  StringRef Code = R"(
foo()<extern("C")>: int
)";
  DeclarationMatcher ExternVar = functionDecl(hasName("foo"), isExternC());
  ASSERT_TRUE(matches(Code.str(), ExternVar));
}

TEST(GoldVarDeclStorageClass, ExternC_VarTemplateDecl) {
  StringRef Code = R"(
foo[T:type]<extern("C")>: int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_VarTemplateDeclSpecialization) {
  StringRef Code = R"(
foo[T:type]<extern("C")>: int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Class) {
  StringRef Code = R"(
C<extern("C")>: type = class:
  i:int
)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    cxxRecordDecl(hasName("C"))));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_ClassTemplate) {
  StringRef Code = R"(
C[T:type]<extern("C")>: type = class:
  ;
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Enum) {
  StringRef Code = R"(
C<extern("C")>: type = enum:
  ;
)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(enumDecl(hasName("C"))));
  ASSERT_TRUE(matches(Code.str(), Matcher));

}

TEST(GoldVarDeclStorageClass, ExternC_NamespaceDecl) {
  StringRef Code = R"(
NS<extern("C")>: namespace = namespace:
  ;
)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(namespaceDecl(
    hasName("NS")
  )));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_TypeAlias) {
  StringRef Code = R"(
C<extern("C")>: type = int
)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    typeAliasDecl(hasName("C"))));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_TypeAliasTemplate) {
  StringRef Code = R"(
Cls[T:type]: type = class:
  ;

C[T:type]<extern("C")>: type = Cls[T]
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_NamespaceAlias) {
  StringRef Code = R"(
NS: namespace = namespace:
  ;
Alias<extern("C")>:namespace = NS
)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    namespaceAliasDecl(hasName("Alias"))));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_Parameter) {
  StringRef Code = R"(
foo(X<extern("C")>:int) : void
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_TemplateParameter) {
  StringRef Code = R"(
foo[T<extern("C")>:type](X:int) : void
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_NonTypeTemplateParameter) {
  StringRef Code = R"(
foo[T<extern("C")>:int](X:int) : void
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Constructor) {
  StringRef Code = R"(
C : type = class:
  constructor()<extern("C")>
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_StaticMember) {
  StringRef Code = R"(
C : type = class:
  Something<static><extern("C")>: const int = 5
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Field) {
  StringRef Code = R"(
C : type = class:
  Something<extern("C")>: const int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Destructor) {
  StringRef Code = R"(
C : type = class:
  destructor()<extern("C")>
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_Method) {
  StringRef Code = R"(
C : type = class:
  foo()<extern("C")>:void
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_NNSMethodDefinition) {
  StringRef Code = R"(
C : type = class:
  foo():void

C.foo()<extern("C")>:void!
  ;

)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    cxxMethodDecl(hasName("foo"))));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSConstructorDefinition) {
  StringRef Code = R"(
C : type = class:
  constructor()

C.constructor()<extern("C")>!
  ;

)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    cxxConstructorDecl()));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSDestructorDefinition) {
  StringRef Code = R"(
C : type = class:
  destructor()

C.destructor()<extern("C")>!
  ;

)";
  DeclarationMatcher Matcher = linkageSpecDecl(has(
    cxxDestructorDecl()));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldVarDeclStorageClass, ExternC_NNSFunctionDeclDef) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo()<extern("C")>: void

NS.foo() : void!
  ;

)";
  DeclarationMatcher Matcher = namespaceDecl(has(linkageSpecDecl(has(
    functionDecl(hasName("foo"))))));
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSFunctionDeclDefBothWithExternC) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo()<extern("C")>: void

NS.foo()<extern("C")>: void!
  ;

)";
  // FIXME:: Not sure if this should be an error or not.
  auto StaticVar = translationUnitDecl(
    has(namespaceDecl(
      has(linkageSpecDecl(
        has(functionDecl(
          hasName("foo")
        ))
      ))
    )),
    has(linkageSpecDecl(
      has(functionDecl(
        hasName("foo")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSFunctionOnlyDefWithExternC) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo(): void

NS.foo()<extern("C")>: void!
  ;

)";
  GoldFailureTest(Code);
}



TEST(GoldVarDeclStorageClass, ExternC_NNSVarDeclDeclDef) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo<extern("C")>: int

NS.foo : int = 4
)";

  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      has(linkageSpecDecl(
        has(varDecl(
          hasName("foo")
        ))
      ))
    )),
    has(varDecl(
      hasName("foo")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSVarDeclDeclDefBothWithExternC) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo<extern("C")>: int

NS.foo<extern("C")>: int = 4

)";
  auto Matcher = translationUnitDecl(
    has(namespaceDecl(
      has(linkageSpecDecl(
        has(varDecl(
          hasName("foo")
        ))
      ))
    )),
    has(linkageSpecDecl(
      has(varDecl(
        hasName("foo")
      ))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_NNSVarDeclOnlyDefWithExternC) {
  StringRef Code = R"(
NS : namespace = namespace:
  foo: int

NS.foo<extern("C")>: int = 4
)";
  GoldFailureTest(Code);
}


TEST(GoldVarDeclStorageClass, ExternC_StaticVarDecl) {
  StringRef Code = R"(
foo<static><extern("C")>: int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_LocalVarDecl) {
  StringRef Code = R"(
foo() : void!
  var<extern("C")>: int
)";
  // TODO: Figure out if this will work or not.
  // DeclarationMatcher StaticVar = varDecl(
  //   hasName("var"), isStaticStorageClass());
  // ASSERT_TRUE(matches(Code.str(), StaticVar));
  GoldFailureTest(Code);
}


TEST(GoldVarDeclStorageClass, ExternC_GlobalVarDeclUse) {
  StringRef Code = R"(
var<extern("C")>: int
foo() : int!
  return var + 1
)";
  auto Matcher = translationUnitDecl(
    has(linkageSpecDecl(
      has(varDecl(
        hasName("var")
      ))
    )),
    hasDescendant(declRefExpr(to(varDecl(hasName("var")))))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}


TEST(GoldVarDeclStorageClass, ExternC_FunctionDeclUse) {
  StringRef Code = R"(
bar()<extern("C")>: int

foo() : int!
  return bar() + 1
)";
  auto Matcher = translationUnitDecl(
    has(linkageSpecDecl(
      has(functionDecl(
        hasName("bar")
      ))
    )),
    hasDescendant(callExpr(
      hasDescendant(declRefExpr(to(functionDecl(hasName("bar")))))
    ))
  );
  ASSERT_TRUE(matches(Code.str(), Matcher));
}

TEST(GoldVarDeclStorageClass, ExternC_ConflictingFunctionDecl_ExternCSecond) {
  StringRef Code = R"(
bar() : int
bar()<extern("C")>: int
)";
  GoldFailureTest(Code);
}

TEST(GoldVarDeclStorageClass, ExternC_ConflictingFunctionDecl_ExterCFirst) {
  StringRef Code = R"(
bar()<extern("C")>: int
bar() : int
)";
  auto StaticVar = translationUnitDecl(
    has(linkageSpecDecl(
      has(functionDecl(
        hasName("bar")
      ))
    )),
    has(functionDecl(
      hasName("bar")
    ))
  );
  ASSERT_TRUE(matches(Code.str(), StaticVar));
}