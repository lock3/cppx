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

// TEST(GoldNewDelete, BasicNewDelete) {
//   std::string Code = R"Gold(
// foo():void!
//   x:^int = new int(4)
//   delete x
// )Gold";
//   auto ToMatch = functionDecl(
//     hasDescendant(callExpr(
//       hasType(asString("void *")),
//       has(implicitCastExpr(
//         hasType(asString("void *(*)(unsigned long)")),
//         has(declRefExpr(
//           hasDeclaration(functionDecl(hasName("operator new")))
//         ))
//       ))
//     )),
//     hasDescendant(callExpr(
//       hasType(asString("void")),
//       has(implicitCastExpr(
//         hasType(asString("void (*)(void *) noexcept")),
//         has(declRefExpr(
//           hasDeclaration(functionDecl(hasName("operator delete")))
//         ))
//       ))
//     ))
//   );
//   ASSERT_TRUE(matches(Code, ToMatch));
// }

// TEST(GoldNewDelete, NewStorageOverload) {
//   std::string Code = R"Gold(
// new_storage(x:uint64) : ^void!
//   return null

// )Gold";
//   auto ToMatch = functionDecl(
//     hasName("operator new"), unless(isImplicit())
//   );
//   ASSERT_TRUE(matches(Code, ToMatch));
// }

// TEST(GoldNewDelete, NewStorageOverload_WithUse) {
//   std::string Code = R"Gold(
// new_storage(x:uint64) : ^void!
//   return null

// foo(): void!
//   x:^void = new_storage(4)
//   delete_storage(x)

// )Gold";
//   auto ToMatch = functionDecl(
//     hasName("foo"),
//     hasDescendant(callExpr(
//       hasType(asString("void *")),
//       has(implicitCastExpr(
//         hasType(asString("void *(*)(unsigned long)")),
//         has(declRefExpr(
//           hasDeclaration(functionDecl(hasName("operator new"),
//             unless(isImplicit())))
//         ))
//       ))
//     ))
//   );
//   ASSERT_TRUE(matches(Code, ToMatch));
// }