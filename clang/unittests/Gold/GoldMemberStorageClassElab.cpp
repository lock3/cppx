//===- unittest/Gold/GoldMemberStorageClassElab.cpp ==---------------------===//
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

TEST(ClassParsing, Static_MemberVariable) {
  StringRef Code = R"(
c : type = class:
  x <static><inline>: int = 4
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      varDecl(hasName("x"), hasType(asString("int")), isPublic(), isStaticStorageClass())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
}

TEST(ClassParsing, Static_Constructor) {
  StringRef Code = R"(
c : type = class:
  constructor() <static>: void!
    ;
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, Static_Destructor) {
  StringRef Code = R"(
c : type = class:
  destructor() <static>: void!
    ;
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, Static_AsACall) {
  StringRef Code = R"(
c : type = class:
  x <static()>: int = 4
)";
  GoldFailureTest(Code);
}

TEST(ClassParsing, Static_AppliedToAType) {
  StringRef Code = R"(
c : type = class:
  x <static()>: type = int
)";
  GoldFailureTest(Code);
}

// TEST(ClassParsing, Static_StringLiteralVariable) {
//   StringRef Code = R"(
// c : type = class:
//   x <static>: ^char = "something"
// )";
//   DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
//     hasDescendant(
//       varDecl(hasName("x"), hasType(asString("char *")), isPublic(),
//               isStaticStorageClass())
//     )
//   );
//   ASSERT_TRUE(matches(Code.str(), ClassC));
// }

TEST(ClassParsing, Static_MemberFunction) {
   StringRef Code = R"(
c : type = class:
  x() <static> : ^char
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      cxxMethodDecl(hasName("x"), hasType(asString("char *(void)")),
                    isPublic(), isStaticStorageClass())
    )
  );
  ASSERT_TRUE(matches(Code.str(), ClassC));
} 

TEST(ClassParsing, Static_VariableUseTest) {
   StringRef Code = R"(
c : type = class:
  x <inline><static>: int = 4
  foo() : int!
    x = x + 1
    return x
  

main() : int!
  q:c
  q.foo()
  return c.x
)";
  DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
    hasDescendant(
      cxxMethodDecl(hasName("foo"), hasType(asString("int (void)")),
                    isPublic())
    )
  );
DeclarationMatcher MainFnMatcher = functionDecl(hasName("main"), isMain(),
    isDefinition(),
    hasDescendant(
      varDecl(
        hasType(asString("struct c")),
        hasName("q")
      )
    ),
    hasDescendant(
      returnStmt(
        has(implicitCastExpr(
          has(declRefExpr(
            to(varDecl(hasName("x"), hasType(asString("int")), isStaticStorageClass()))
          ))
        ))
      )
    )
  );

  DeclarationMatcher CompleteMatch = translationUnitDecl(
    hasDescendant(ClassC),
    hasDescendant(MainFnMatcher)
  );
  ASSERT_TRUE(matches(Code.str(), CompleteMatch));
}


