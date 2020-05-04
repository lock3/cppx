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
  ASSERT_TRUE(matches(Code, StaticVar));
}

TEST(GoldVarDeclStorageClass, External) {
  StringRef Code = R"(
foo<extern>: int
)";
  DeclarationMatcher ExternVar = varDecl(
    hasName("foo"), isExternStorageClass());
  ASSERT_TRUE(matches(Code, ExternVar));
}