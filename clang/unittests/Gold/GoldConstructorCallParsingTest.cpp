#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

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

  StatementMatcher CastExprStmt(compoundStmt(hasDescendant(
        varDecl(hasName("x"), hasType(asString("int")),
          hasDescendant(
            cxxFunctionalCastExpr(
              hasCastKind(CK_NoOp),
              has(integerLiteral(equals(5)))
            )
          )
        )
      )
    )
  );
  ASSERT_TRUE(matches(Code, CastExprStmt));
}

TEST(GoldParseConstructorCall, UDT_CallToImplicitCtor) {
  StringRef Code = R"(
c : type = class:
  x : int = 5
  y : bool = 3
main() : int!
  q = c()
  return q.x
)";
  // TODO: Figure out what's supposed to be happening here.
  StatementMatcher HasConstructorCallSearch(
    compoundStmt(
      hasDescendant(
        declStmt(
          has(
            varDecl(
              hasName("q"),
              hasType(
                asString("struct c")
              ),
              has(
                cxxTemporaryObjectExpr()
              )
            )
          )
        )
      ),
      hasDescendant(returnStmt())
    )
  );
  ASSERT_TRUE(matches(Code, HasConstructorCallSearch));
}