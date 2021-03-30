//=== GoldParseCommentTest.cpp ---------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing the different possible locations a comment block could occur and
//  that we make sure that we check each one of them because comments
//  are part of the language in gold, and not just annotation. Granted that they
//  don't have symantic associated with them.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldComment_Block, Contents_StandALone_Empty) {
  StringRef Code = R"GOLD(
<# #>
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Line, Contents_StandALone_Empty) {
  StringRef Code = R"GOLD(
#
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Block, Contents_StandALone_SimpleText) {
  StringRef Code = R"GOLD(
<# test #>
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Line, Contents_StandALone_SimpleTest) {
  StringRef Code = R"GOLD(
# test
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Block, Contents_StandALone_NestedBlockComments) {
  StringRef Code = R"GOLD(
<# <# hi#> test <# #> #>
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Block, Contents_StandALone_MultipleLines) {
  StringRef Code = R"GOLD(
<#
a
b
c
d
#>
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Block, Contents_StandALone_TextTouchingPound) {
  StringRef Code = R"GOLD(
<#a#>
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Block, Unterminated) {
  StringRef Code = R"GOLD(
<#
)GOLD";
  GoldFailureTest(Code);
}

TEST(GoldComment_Line, Contents_StandALone_TextTouchingPound) {
  StringRef Code = R"GOLD(
#a
)GOLD";
  SimpleGoldParseTest(Code);
}

TEST(GoldComment_Line, Contents_StandALone_SemiColon) {
  StringRef Code = R"GOLD(
# ;
)GOLD";
  SimpleGoldParseTest(Code);
}



// -----------------------------------------------------------------------------
//  Parsing comments as part of variable declarations.
// -----------------------------------------------------------------------------
TEST(GoldComment_Block, Location_VariableDecl_AfterIdentifier_SingleLine) {
  StringRef Code = R"GOLD(
x <# comment #> : int;

)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterIdentifier_MultiLine) {
  StringRef Code = R"GOLD(
x <#
comment
#> : int
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_BeforeIdentifier_SingleLine) {
  StringRef Code = R"GOLD(
<# comment #> x  : int
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_BeforeIdentifier_MultiLine) {
  StringRef Code = R"GOLD(
<#
comment
#>x : int
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterColon_SingleLine) {
  StringRef Code = R"GOLD(
x  : <# comment #> int
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterColon_MultiLine) {
  StringRef Code = R"GOLD(
x : <#
comment
#>int
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterType_SingleLine) {
  StringRef Code = R"GOLD(
x  : int <# comment #>
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterType_MultiLine) {
  StringRef Code = R"GOLD(
x : int <#
comment
#>
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_BeforeAssignment_SingleLine) {
  StringRef Code = R"GOLD(
x  : int <# comment #> = 5
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_BeforeAssignment_MultiLine) {
  StringRef Code = R"GOLD(
x : int <#
comment
#> = 5
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterAssignment_SingleLine) {
  StringRef Code = R"GOLD(
x  : int =  <# comment #> 5
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterAssignment_MultiLine) {
  StringRef Code = R"GOLD(
x : int =<#
comment
#>  5
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterExpression_SingleLine) {
  StringRef Code = R"GOLD(
x  : int = 5 <# comment #>
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_VariableDecl_AfterExpression_MultiLine) {
  StringRef Code = R"GOLD(
x : int = 5 <#
comment
#>
)GOLD";
  auto ToMatch = varDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldComment_Line, Location_VariableDecl_AfterIdentifier) {
//   StringRef Code = R"GOLD(
// x # Comment
// : int
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_VariableDecl_AfterColon) {
//   StringRef Code = R"GOLD(
// x :# Comment
// int
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_VariableDecl_AfterType) {
//   StringRef Code = R"GOLD(
// x : int # Comment
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_VariableDecl_BeforeAssignment) {
//   StringRef Code = R"GOLD(
// x : int # Comment
// = 5
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_VariableDecl_AfterAssignment) {
//   StringRef Code = R"GOLD(
// x : int = 5 # Comment
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_VariableDecl_AfterExpression) {
//   StringRef Code = R"GOLD(
// x : int = 5 # Comment
// )GOLD";
//   auto ToMatch = varDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }


// -----------------------------------------------------------------------------
//          Parsing comments as part of function decl
// -----------------------------------------------------------------------------
TEST(GoldComment_Block, Location_FunctionDecl_AfterIdentifier) {
  StringRef Code = R"GOLD(
x <# comment #>() : void;

)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDecl_InsideEmptyParameters) {
  StringRef Code = R"GOLD(
x (<# comment #>) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDecl_AfterParameters) {
  StringRef Code = R"GOLD(
x ()<# comment #> : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDecl_AfterColon) {
  StringRef Code = R"GOLD(
x () : <# comment #> void;

)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDecl_AfterReturnType) {
  StringRef Code = R"GOLD(
x () : void <# comment #>;

)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldComment_Line, Location_FunctionDecl_AfterIdentifier) {
//   StringRef Code = R"GOLD(
// x # Comment
// () : void;

// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_FunctionDecl_InsideEmptyParameters) {
//   StringRef Code = R"GOLD(
// x (# Comment
// ) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_FunctionDecl_AfterParameters) {
//   StringRef Code = R"GOLD(
// x ()# Comment
// : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_FunctionDecl_AfterColon) {
//   StringRef Code = R"GOLD(
// x () : # comment
// void;

// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_FunctionDecl_AfterReturnType) {
//   StringRef Code = R"GOLD(
// x () : void # comment

// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }


// --- ParmVarDecl ----
// TEST(GoldComment_Block, Location_ParmVarDecl_BeforeSingleIdentifier) {
//   StringRef Code = R"GOLD(
// x (<# comment #>y) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Block, Location_ParmVarDecl_AfterSingleIdentifier) {
//   StringRef Code = R"GOLD(
// x (y<# comment #>) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(GoldComment_Block, Location_ParmVarDecl_AfterIdentifierBeforeColon) {
  StringRef Code = R"GOLD(
x (y<# comment #>:int) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_AfterColon) {
  StringRef Code = R"GOLD(
x (y:<# comment #> int) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_AfterType) {
  StringRef Code = R"GOLD(
x (y:int <# comment #>) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_BeforeAssignment) {
  StringRef Code = R"GOLD(
x (y:int <# comment #> = 5) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_AfterAssignment) {
  StringRef Code = R"GOLD(
x (y:int = <# comment #> 5) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_AfterExpression) {
  StringRef Code = R"GOLD(
x (y:int = 5<# comment #>) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_BeforeComma) {
  StringRef Code = R"GOLD(
x (y:int<# comment #>, z:int) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_ParmVarDecl_AfterComma) {
  StringRef Code = R"GOLD(
x (y:int,<# comment #> z:int) : void;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


// TEST(GoldComment_Line, Location_ParmVarDecl_BeforeSingleIdentifier) {
//   StringRef Code = R"GOLD(
// x (# comment
// y) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterSingleIdentifier) {
//   StringRef Code = R"GOLD(
// x (y# comment
// ) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterIdentifierBeforeColon) {
//   StringRef Code = R"GOLD(
// x (y # comment
// :int) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterColon) {
//   StringRef Code = R"GOLD(
// x (y:# comment
// int) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterType) {
//   StringRef Code = R"GOLD(
// x (y:int # comment
// ) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_BeforeAssignment) {
//   StringRef Code = R"GOLD(
// x (y:int # comment
// = 5) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterAssignment) {
//   StringRef Code = R"GOLD(
// x (y:int = # comment
//     5) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterExpression) {
//   StringRef Code = R"GOLD(
// x (y:int = 5# comment
// ) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_BeforeComma) {
//   StringRef Code = R"GOLD(
// x (y:int # comment
// , z:int) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

// TEST(GoldComment_Line, Location_ParmVarDecl_AfterComma) {
//   StringRef Code = R"GOLD(
// x (y:int, # comment
//   z:int) : void;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }


// -----------------------------------------------------------------------------
//          Parsing comments as part of function definition
// -----------------------------------------------------------------------------
TEST(GoldComment_Block, Location_FunctionDef_BeforeExclamationPoint) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void <# comment #>!
  ;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldComment_Line, Location_FunctionDef_BeforeExclamationPoint) {
//   StringRef Code = R"GOLD(
// x (y:int, z:int) : void # comment
// !
//   ;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }

TEST(GoldComment_Block, Location_FunctionDef_BetweenExclamationPointAndBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void! <# comment #> { }
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Line, Location_FunctionDef_BetweenExclamationPointAndBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void! # comment
{ }
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDef_AfterExplicitBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void! { } <# comment #>
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Line, Location_FunctionDef_AfterExplicitBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void! { }# comment
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldComment_Block, Location_FunctionDef_AfterIndentBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void!
  ;
<# comment #>
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Line, Location_FunctionDef_AfterIndentBlock) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void!
  ;
# comment
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Line, Location_FunctionDef_AfterExclamationPoint) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void ! # comment
  ;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDef_AfterExclamationPoint) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void ! <# comment #>
  ;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldComment_Block, Location_FunctionDef_AsEmptyBodyOfFunction) {
  StringRef Code = R"GOLD(
x (y:int, z:int) : void!
  <# comment #>
  ;
)GOLD";
  auto ToMatch = functionDecl(hasName("x"));
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// TEST(GoldComment_Line, Location_FunctionDef_AsEmptyBodyOfFunction) {
//   StringRef Code = R"GOLD(
// x (y:int, z:int) : void!
//   # comment
//   ;
// )GOLD";
//   auto ToMatch = functionDecl(hasName("x"));
//   ASSERT_TRUE(matches(Code.str(), ToMatch));
// }