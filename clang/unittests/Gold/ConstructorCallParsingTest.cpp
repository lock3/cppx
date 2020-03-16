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
  // TODO: Figure out if casting works for this.
  SimpleGoldParseTest(Code);
}