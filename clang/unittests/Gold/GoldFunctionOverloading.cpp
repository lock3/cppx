#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

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


TEST(FunctionOverloading, TemplateOverloading_SameTemplateParamTypes) {

  StringRef Code = R"(
foo[T:type](i:int) : int!
  return 4 + i

foo[T:type](a:float) :int!
  return 5

main() : int!
  return foo[int](3)
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

TEST(FunctionOverloading, MemberFunction_Overloading) {

  StringRef Code = R"(
c : type = class:
  foo(i:int) : int!
    return 4 + i
  foo(a:double) :int!
    return 5
  

main() : int!
  b:c
  return b.foo(3)

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

TEST(FunctionOverloading, MemberFunctionTemplate_Overloading) {
/*

// Expected
TranslationUnitDecl 0x7fffba82a2b8 <<invalid sloc>> <invalid sloc>
|-CXXRecordDecl 0x7fffba8687c0 <bin/cpp_test.cpp:1:1, line:10:1> line:1:8 struct C definition
| |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists trivial constexpr needs_implicit defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveConstructor exists simple trivial needs_implicit
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffba8688e8 <col:1, col:8> col:8 implicit struct C
| |-FunctionTemplateDecl 0x7fffba868c10 <line:2:3, line:5:3> line:3:7 foo
| | |-TemplateTypeParmDecl 0x7fffba868980 <line:2:13, col:22> col:22 typename depth 0 index 0 T
| | `-CXXMethodDecl 0x7fffba868b60 <line:3:3, line:5:3> line:3:7 foo 'int (int)'
| |   |-ParmVarDecl 0x7fffba868a58 <col:11, col:15> col:15 i 'int'
| |   `-CompoundStmt 0x7fffba868f48 <col:18, line:5:3>
| |     `-ReturnStmt 0x7fffba868f38 <line:4:5, col:12>
| |       `-IntegerLiteral 0x7fffba868f18 <col:12> 'int' 3
| `-FunctionTemplateDecl 0x7fffba868ea0 <line:6:3, line:9:3> line:7:7 foo
|   |-TemplateTypeParmDecl 0x7fffba868c70 <line:6:13, col:22> col:22 typename depth 0 index 0 T
|   `-CXXMethodDecl 0x7fffba868df0 <line:7:3, line:9:3> line:7:7 foo 'int (double)'
|     |-ParmVarDecl 0x7fffba868d18 <col:11, col:18> col:18 i 'double'
|     `-CompoundStmt 0x7fffba868f90 <col:21, line:9:3>
|       `-ReturnStmt 0x7fffba868f80 <line:8:5, col:12>
|         `-IntegerLiteral 0x7fffba868f60 <col:12> 'int' 2



// Actual
TranslationUnitDecl 0x7fffd40a19a8 <<invalid sloc>> <invalid sloc>
`-CXXRecordDecl 0x7fffd40df080 <temp.usyntax:2:12, col:1> col:1 struct c definition
  |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
  | |-DefaultConstructor exists trivial constexpr needs_implicit defaulted_is_constexpr
  | |-CopyConstructor simple trivial has_const_param needs_implicit implicit_has_const_param
  | |-MoveConstructor exists simple trivial needs_implicit
  | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
  | |-MoveAssignment exists simple trivial needs_implicit
  | `-Destructor simple irrelevant trivial needs_implicit
  |-CXXRecordDecl 0x7fffd40df1a8 <col:12, col:1> col:1 implicit struct c
  |-FunctionTemplateDecl 0x7fffd40df530 <<invalid sloc>, line:4:5> <invalid sloc> foo
  | |-TemplateTypeParmDecl 0x7fffd40df280 <line:3:8> col:8 typename depth 0 index 0 T
  | `-CXXMethodDecl 0x7fffd40df468 <col:27, line:4:5> line:3:27 foo 'int (int)' inline
  |   |-ParmVarDecl 0x7fffd40df378 <col:16> col:16 used i 'int'
  |   `-CompoundStmt 0x7fffd40df918 <line:4:5>
  |     `-ReturnStmt 0x7fffd40df908 <col:5, col:16>
  |       `-BinaryOperator 0x7fffd40df8e8 <col:12, col:16> 'int' '+'
  |         |-IntegerLiteral 0x7fffd40df890 <col:12> 'int' 4
  |         `-ImplicitCastExpr 0x7fffd40df8d0 <col:16> 'int' <LValueToRValue>
  |           `-DeclRefExpr 0x7fffd40df8b0 <col:16> 'int' lvalue ParmVar 0x7fffd40df378 'i' 'int'
  `-FunctionTemplateDecl 0x7fffd40df830 <<invalid sloc>, line:7:5> <invalid sloc> foo
    |-TemplateTypeParmDecl 0x7fffd40df5b0 <line:6:8> col:8 typename depth 0 index 0 T
    `-CXXMethodDecl 0x7fffd40df768 <col:28, line:7:5> line:6:28 foo 'int (float)' inline
      |-ParmVarDecl 0x7fffd40df678 <col:16> col:16 a 'float'
      `-CompoundStmt 0x7fffd40df960 <line:7:5>
        `-ReturnStmt 0x7fffd40df950 <col:5, col:12>
          `-IntegerLiteral 0x7fffd40df930 <col:12> 'int' 5


// Expected With single function call.
|-CXXRecordDecl 0x7fffd1baa8c0 <bin/cpp_test.cpp:1:1, line:10:1> line:1:8 referenced struct C definition
| |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists trivial constexpr defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffd1baa9e8 <col:1, col:8> col:8 implicit struct C
| |-FunctionTemplateDecl 0x7fffd1baad10 <line:2:3, line:5:3> line:3:7 foo
| | |-TemplateTypeParmDecl 0x7fffd1baaa80 <line:2:13, col:22> col:22 typename depth 0 index 0 T
| | |-CXXMethodDecl 0x7fffd1baac60 <line:3:3, line:5:3> line:3:7 foo 'int (int)'
| | | |-ParmVarDecl 0x7fffd1baab58 <col:11, col:15> col:15 i 'int'
| | | `-CompoundStmt 0x7fffd1bab048 <col:18, line:5:3>
| | |   `-ReturnStmt 0x7fffd1bab038 <line:4:5, col:12>
| | |     `-IntegerLiteral 0x7fffd1bab018 <col:12> 'int' 3
| | `-CXXMethodDecl 0x7fffd1bd4338 <line:3:3, line:5:3> line:3:7 used foo 'int (int)'
| |   |-TemplateArgument type 'int'
| |   |-ParmVarDecl 0x7fffd1bd4298 <col:11, col:15> col:15 i 'int'
| |   `-CompoundStmt 0x7fffd1bd47d0 <col:18, line:5:3>
| |     `-ReturnStmt 0x7fffd1bd47c0 <line:4:5, col:12>
| |       `-IntegerLiteral 0x7fffd1bab018 <col:12> 'int' 3
/// 2nd overload
| |-FunctionTemplateDecl 0x7fffd1baafa0 <line:6:3, line:9:3> line:7:7 foo
| | |-TemplateTypeParmDecl 0x7fffd1baad70 <line:6:13, col:22> col:22 typename depth 0 index 0 T
| | |-CXXMethodDecl 0x7fffd1baaef0 <line:7:3, line:9:3> line:7:7 foo 'int (double)'
| | | |-ParmVarDecl 0x7fffd1baae18 <col:11, col:18> col:18 i 'double'
| | | `-CompoundStmt 0x7fffd1bab090 <col:21, line:9:3>
| | |   `-ReturnStmt 0x7fffd1bab080 <line:8:5, col:12>
| | |     `-IntegerLiteral 0x7fffd1bab060 <col:12> 'int' 2
| | `-CXXMethodDecl 0x7fffd1bd45a8 <line:7:3, line:9:3> line:7:7 foo 'int (double)'
| |   |-TemplateArgument type 'int'
| |   `-ParmVarDecl 0x7fffd1bd4508 <col:11, col:18> col:18 i 'double'
|
`-FunctionDecl 0x7fffd1bab100 <line:12:1, line:16:1> line:12:5 main 'int ()'
  `-CompoundStmt 0x7fffd1bd4798 <col:12, line:16:1>
    |-DeclStmt 0x7fffd1bd3fb0 <line:13:3, col:6>
    | `-VarDecl 0x7fffd1bab230 <col:3, col:5> col:5 used x 'C' callinit
    |   `-CXXConstructExpr 0x7fffd1bd3f88 <col:5> 'C' 'void () noexcept'
    |-CXXMemberCallExpr 0x7fffd1bd4740 <line:14:3, col:15> 'int'
    | |-MemberExpr 0x7fffd1bd46b8 <col:3, col:12> '<bound member function type>' .foo 0x7fffd1bd4338
    | | `-DeclRefExpr 0x7fffd1bd3fc8 <col:3> 'C' lvalue Var 0x7fffd1bab230 'x' 'C'
    | `-IntegerLiteral 0x7fffd1bd41b8 <col:14> 'int' 4
    `-ReturnStmt 0x7fffd1bd4788 <line:15:3, col:10>
      `-IntegerLiteral 0x7fffd1bd4768 <col:10> 'int' 0

// Actual with single call.
|-CXXRecordDecl 0x7fffe77345b0 <temp.usyntax:2:12, col:1> col:1 struct c definition
| |-DefinitionData pass_in_registers empty aggregate standard_layout trivially_copyable pod trivial literal has_constexpr_non_copy_move_ctor can_const_default_init
| | |-DefaultConstructor exists trivial constexpr defaulted_is_constexpr
| | |-CopyConstructor simple trivial has_const_param implicit_has_const_param
| | |-MoveConstructor exists simple trivial
| | |-CopyAssignment trivial has_const_param needs_implicit implicit_has_const_param
| | |-MoveAssignment exists simple trivial needs_implicit
| | `-Destructor simple irrelevant trivial needs_implicit
| |-CXXRecordDecl 0x7fffe7734868 <col:12, col:1> col:1 implicit struct c
| |-FunctionTemplateDecl 0x7fffe7734bd0 <<invalid sloc>, line:4:5> <invalid sloc> foo
| | |-TemplateTypeParmDecl 0x7fffe7734920 <line:3:8> col:8 typename depth 0 index 0 T
| | |-CXXMethodDecl 0x7fffe7734b08 <col:27, line:4:5> line:3:27 foo 'int (int)' inline
| | | |-ParmVarDecl 0x7fffe7734a18 <col:16> col:16 used i 'int'
| | | `-CompoundStmt 0x7fffe7734fb8 <line:4:5>
| | |   `-ReturnStmt 0x7fffe7734fa8 <col:5, col:16>
| | |     `-BinaryOperator 0x7fffe7734f88 <col:12, col:16> 'int' '+'
| | |       |-IntegerLiteral 0x7fffe7734f30 <col:12> 'int' 4
| | |       `-ImplicitCastExpr 0x7fffe7734f70 <col:16> 'int' <LValueToRValue>
| | |         `-DeclRefExpr 0x7fffe7734f50 <col:16> 'int' lvalue ParmVar 0x7fffe7734a18 'i' 'int'
| | `-CXXMethodDecl 0x7fffe773fc00 <line:3:27, line:4:5> line:3:27 used foo 'int (int)' inline
| |   |-TemplateArgument type 'int'
| |   |-ParmVarDecl 0x7fffe773fb60 <col:16> col:16 i 'int'
| |   `-CompoundStmt 0x7fffe7740128 <line:4:5>
| |     `-ReturnStmt 0x7fffe7740118 <col:5, col:16>
| |       `-BinaryOperator 0x7fffe77400f8 <col:12, col:16> 'int' '+'
| |         |-IntegerLiteral 0x7fffe7734f30 <col:12> 'int' 4
| |         `-ImplicitCastExpr 0x7fffe77400e0 <col:16> 'int' <LValueToRValue>
| |           `-DeclRefExpr 0x7fffe7734f50 <col:16> 'int' lvalue ParmVar 0x7fffe7734a18 'i' 'int'

// 2nd overload.
| |-FunctionTemplateDecl 0x7fffe7734ed0 <<invalid sloc>, line:7:5> <invalid sloc> foo
| | |-TemplateTypeParmDecl 0x7fffe7734c50 <line:6:8> col:8 typename depth 0 index 0 T
| | |-CXXMethodDecl 0x7fffe7734e08 <col:28, line:7:5> line:6:28 foo 'int (float)' inline
| | | |-ParmVarDecl 0x7fffe7734d18 <col:16> col:16 a 'float'
| | | `-CompoundStmt 0x7fffe7735000 <line:7:5>
| | |   `-ReturnStmt 0x7fffe7734ff0 <col:5, col:12>
| | |     `-IntegerLiteral 0x7fffe7734fd0 <col:12> 'int' 5
| | `-CXXMethodDecl 0x7fffe773fef0 <line:6:28, line:7:5> line:6:28 foo 'int (float)' inline
| |   |-TemplateArgument type 'int'
| |   `-ParmVarDecl 0x7fffe773fe50 <col:16> col:16 a 'float'



Call from Sema::BuildMemberReferenceExpr(Expr *BaseExpr, QualType BaseExprType,


// Expected compiler output for member function template look up contents after being created
We are building a member expression and we are either an overload or unresolved
Name info = 
foo
Dumping memExpr contents
UnresolvedMemberExpr 0x7fffe057f208 '<bound member function type>' lvalue
`-DeclRefExpr 0x7fffe057f1b8 'struct C' lvalue Var 0x7fffe0556310 'x' 'struct C'
Selected Decl
FunctionTemplateDecl 0x7fffe0555d10 <bin/cpp_test.cpp:2:3, line:5:3> line:3:7 foo
|-TemplateTypeParmDecl 0x7fffe0555a80 <line:2:13, col:22> col:22 typename depth 0 index 0 T
`-CXXMethodDecl 0x7fffe0555c60 <line:3:3, line:5:3> line:3:7 foo 'int (int)'
  |-ParmVarDecl 0x7fffe0555b58 <col:11, col:15> col:15 referenced i 'int'
  `-CompoundStmt 0x7fffe05560a0 <col:18, line:5:3>
    `-ReturnStmt 0x7fffe0556090 <line:4:5, col:16>
      `-BinaryOperator 0x7fffe0556070 <col:12, col:16> 'int' '+'
        |-IntegerLiteral 0x7fffe0556018 <col:12> 'int' 3
        `-ImplicitCastExpr 0x7fffe0556058 <col:16> 'int' <LValueToRValue>
          `-DeclRefExpr 0x7fffe0556038 <col:16> 'int' lvalue ParmVar 0x7fffe0555b58 'i' 'int'

Selected Decl
FunctionTemplateDecl 0x7fffe0555fa0 <bin/cpp_test.cpp:6:3, line:9:3> line:7:7 foo
|-TemplateTypeParmDecl 0x7fffe0555d70 <line:6:13, col:22> col:22 typename depth 0 index 0 T
`-CXXMethodDecl 0x7fffe0555ef0 <line:7:3, line:9:3> line:7:7 foo 'int (double)'
  |-ParmVarDecl 0x7fffe0555e18 <col:11, col:18> col:18 referenced i 'double'
  `-CompoundStmt 0x7fffe0556170 <col:21, line:9:3>
    `-ReturnStmt 0x7fffe0556160 <line:8:5, col:16>
      `-ImplicitCastExpr 0x7fffe0556148 <col:12, col:16> 'int' <FloatingToIntegral>
        `-BinaryOperator 0x7fffe0556128 <col:12, col:16> 'double' '+'
          |-ImplicitCastExpr 0x7fffe0556110 <col:12> 'double' <IntegralToFloating>
          | `-IntegerLiteral 0x7fffe05560b8 <col:12> 'int' 2
          `-ImplicitCastExpr 0x7fffe05560f8 <col:16> 'double' <LValueToRValue>
            `-DeclRefExpr 0x7fffe05560d8 <col:16> 'double' lvalue ParmVar 0x7fffe0555e18 'i' 'double'


Call from Sema::BuildMemberReferenceExpr(Expr *BaseExpr, QualType BaseExprType,

// Actual 
We are building a member expression and we are either an overload or unresolved
Name info = 
foo
Dumping memExpr contents
UnresolvedMemberExpr 0x7fffe1bed8b8 '<bound member function type>' lvalue
`-DeclRefExpr 0x7fffe1bed898 'struct c' lvalue Var 0x7fffe1be3050 'q' 'struct c'
Selected Decl
FunctionTemplateDecl 0x7fffe1be2bd0 <<invalid sloc>, temp.usyntax:4:5> <invalid sloc> foo
|-TemplateTypeParmDecl 0x7fffe1be2920 <line:3:8> col:8 typename depth 0 index 0 T
`-CXXMethodDecl 0x7fffe1be2b08 <col:27, line:4:5> line:3:27 foo 'int (int)' inline
  |-ParmVarDecl 0x7fffe1be2a18 <col:16> col:16 used i 'int'
  `-CompoundStmt 0x7fffe1be2fb8 <line:4:5>
    `-ReturnStmt 0x7fffe1be2fa8 <col:5, col:16>
      `-BinaryOperator 0x7fffe1be2f88 <col:12, col:16> 'int' '+'
        |-IntegerLiteral 0x7fffe1be2f30 <col:12> 'int' 4
        `-ImplicitCastExpr 0x7fffe1be2f70 <col:16> 'int' <LValueToRValue>
          `-DeclRefExpr 0x7fffe1be2f50 <col:16> 'int' lvalue ParmVar 0x7fffe1be2a18 'i' 'int'

Selected Decl
FunctionTemplateDecl 0x7fffe1be2ed0 <<invalid sloc>, temp.usyntax:7:5> <invalid sloc> foo
|-TemplateTypeParmDecl 0x7fffe1be2c50 <line:6:8> col:8 typename depth 0 index 0 T
`-CXXMethodDecl 0x7fffe1be2e08 <col:28, line:7:5> line:6:28 foo 'int (float)' inline
  |-ParmVarDecl 0x7fffe1be2d18 <col:16> col:16 a 'float'
  `-CompoundStmt 0x7fffe1be3000 <line:7:5>
    `-ReturnStmt 0x7fffe1be2ff0 <col:5, col:12>
      `-IntegerLiteral 0x7fffe1be2fd0 <col:12> 'int' 5


/// DUmp from actual compilation.
Creating DeclRefExpression With DeclarationNameInfo.
Referes to enclosing var = 0
ExprValueKind = 1
FoundD = 
ParmVarDecl 0x7fffb961bb58 <bin/cpp_test.cpp:3:11, col:15> col:15 i 'int'
We have a Template args list
Nope no template args?!
Dumping type
BuiltinType 0x7fffb95dd3b0 'int'

Is depentent Type = 0
NonORD use reason = 0
Name info =
i



Creating DeclRefExpression With DeclarationNameInfo.
Referes to enclosing var = 0
ExprValueKind = 1
FoundD = 
VarDecl 0x7fffb961c280 <bin/cpp_test.cpp:13:3, col:5> col:5 x 'C' callinit
`-CXXConstructExpr 0x7fffb9644fb8 <col:5> 'C' 'void () noexcept'
We have a Template args list
Nope no template args?!
Dumping type
RecordType 0x7fffb961b960 'struct C'
`-CXXRecord 0x7fffb961b8c0 'C'

Is depentent Type = 0
NonORD use reason = 0
Name info =
x



// DUMP from actual exec
Creating DeclRefExpression With DeclarationNameInfo.
Referes to enclosing var = 0
ExprValueKind = 1
FoundD = 
ParmVarDecl 0x7fffdcb5da18 <temp.usyntax:3:16> col:16 i 'int'
We have a Template args list
Nope no template args?!
Dumping type
BuiltinType 0x7fffdcb1fad0 'int'

Is depentent Type = 0
NonORD use reason = 0
Name info =
i

Creating DeclRefExpression With DeclarationNameInfo.
Referes to enclosing var = 0
ExprValueKind = 1
FoundD = 
VarDecl 0x7fffdcb5e050 <temp.usyntax:10:5> col:5 q 'struct c' auto callinit
`-CXXConstructExpr 0x7fffdcb68858 <col:5> 'struct c' 'void (void) noexcept'
We have a Template args list
Nope no template args?!
Dumping type
RecordType 0x7fffdcb5d650 'struct c'
`-CXXRecord 0x7fffdcb5d5b0 'c'

Is depentent Type = 0
NonORD use reason = 0
Name info =
q

We are building a member expression and we are either an overload or unresolved
Name info = 
foo
Dumping memExpr contents
UnresolvedMemberExpr 0x7fffdcb688b8 '<bound member function type>' lvalue
`-DeclRefExpr 0x7fffdcb68898 'struct c' lvalue Var 0x7fffdcb5e050 'q' 'struct c'


*/
  StringRef Code = R"(
c : type = class:
  foo[T:type](i:int) : int!
    return 4 + i
  
  foo[T:type](a:float) :int!
    return 5
  
main() : int!
  q : c
  return q.foo[int](2)
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