#include "ParseUtil.h"
#include "ASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(FunctionOverloading, SimpleOverload) {

  StringRef Code = R"(
foo(i:int) : int!
  return 4 + i

foo(a:float) :int!
  return 5

main() : int!
  return foo(3)
)";
  DeclarationMatcher CallMatcher = functionDecl(
    hasName("main"),
    isMain(),
    isDefinition(),
    hasDescendant(
      callExpr(
        callee(
          functionDecl(
            hasName("foo"),
            hasType(
              asString("int (int)")
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code, CallMatcher));
}


// TEST(FunctionOverloading, TemplateOverloading_SameTemplateParamTypes) {

//   StringRef Code = R"(
// foo[T:type](i:int) : int!
//   return 4 + i

// foo[T:type](a:float) :int!
//   return 5

// main() : int!
//   return foo[int](3)
// )";
//   DeclarationMatcher CallMatcher = functionDecl(
//     hasName("main"),
//     isMain(),
//     isDefinition(),
//     hasDescendant(
//       callExpr(
//         callee(
//           functionDecl(
//             hasName("foo"),
//             hasType(
//               asString("int (int)")
//             )
//           )
//         )
//       )
//     )
//   );
//   ASSERT_TRUE(matches(Code, CallMatcher));
// }