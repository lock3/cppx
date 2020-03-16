#include "ParseUtil.h"
#include "ASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;


TEST(GoldParseConstructorCall, BuiltIn_ExplicitCtorCall) {
  StringRef Code = R"(
main() : int!
  x = int(5)
  return 0
)";
  // DeclarationMatcher ClassC = recordDecl( recordDecl(hasName("c")),
  //   hasDescendant(fieldDecl(hasName("x"), hasType(asString("int")),
  //     isPublic())),
  //   hasDescendant(fieldDecl(hasName("y"), hasType(asString("_Bool")),
  //     isPublic()))
  // );
  // ASSERT_TRUE(matches(Code, ClassC));
  SimpleGoldParseTest(Code);
}