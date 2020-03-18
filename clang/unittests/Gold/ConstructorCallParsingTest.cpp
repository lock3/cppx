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

/*
Expected
`-FunctionDecl 0x7fffbb593ac0 <line:5:1, line:8:1> line:5:5 main 'int ()'
  `-CompoundStmt 0x7fffbb594588 <col:12, line:8:1>
    |-DeclStmt 0x7fffbb594540 <line:6:3, col:15>
    | `-VarDecl 0x7fffbb593c10 <col:3, col:14> col:8 b 'C':'C' cinit
    |   `-ExprWithCleanups 0x7fffbb594528 <col:12, col:14> 'C':'C'
    |     `-CXXConstructExpr 0x7fffbb5944f8 <col:12, col:14> 'C':'C' 'void (C &&) noexcept' elidable
    |       `-MaterializeTemporaryExpr 0x7fffbb5943c0 <col:12, col:14> 'C' xvalue
    |         `-CXXTemporaryObjectExpr 0x7fffbb5941c8 <col:12, col:14> 'C' 'void () noexcept' zeroing
    `-ReturnStmt 0x7fffbb594578 <line:7:3, col:10>
      `-IntegerLiteral 0x7fffbb594558 <col:10> 'int' 0


Actual:
`-FunctionDecl 0x7fffdc24d668 <line:5:13, line:7:3> line:5:13 main 'int (void)'
  `-CompoundStmt 0x7fffdc24e200 <line:6:5, line:7:3>
    `-DeclStmt 0x7fffdc24e1e8 <line:6:5, col:3>
      `-VarDecl 0x7fffdc24d9c8 <col:5, col:7> col:5 q 'struct c':'struct c' auto listinit
        `-ExprWithCleanups 0x7fffdc24e1d0 <col:5, col:7> 'struct c':'struct c'
          `-CXXConstructExpr 0x7fffdc24e1a0 <col:5, col:7> 'struct c':'struct c' 'void (const struct c &) throw()' elidable
            `-MaterializeTemporaryExpr 0x7fffdc24e018 <<invalid sloc>, col:7> 'const struct c' lvalue
              `-ImplicitCastExpr 0x7fffdc24e000 <<invalid sloc>, col:7> 'const struct c' <NoOp>
                `-CXXTemporaryObjectExpr 0x7fffdc24de08 <<invalid sloc>, col:7> 'struct c' 'void (void) throw()' zeroing
*/
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