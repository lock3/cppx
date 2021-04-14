//=== GoldDocAttrParsing.cpp -----------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file does simple parsing of docattrs from Tim's grammar.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

TEST(GoldDocAttr, SimpleParse) {
  StringRef Code = R"GOLD(
x:int <|Im a doc attribute>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, TwoDocAttributes) {
  StringRef Code = R"GOLD(
x:int <|\n attribute ><|Attr>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, EscapedCharacters) {
  StringRef Code = R"GOLD(
x:int <|\n attribute ><|\<>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldDocAttr, InterpolatedReference) {
  StringRef Code = R"GOLD(
x:int <|& Something; >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, IncompleteReference) {
  StringRef Code = R"GOLD(
x:int <|&Something >
)GOLD";
  GoldFailureTest(Code);
}

TEST(GoldDocAttr, ComplexExpression) {
  StringRef Code = R"GOLD(
x:int <|&a +b + c; >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}


TEST(GoldDocAttr, CommentInside) {
  StringRef Code = R"GOLD(
x:int <|<# #> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, UnterminatedBlockComment) {
  StringRef Code = R"GOLD(
x:int <|<#
)GOLD";
  GoldFailureTest(Code);
}

TEST(GoldDocAttr, UnterminatedBlockCommentWithGTInside) {
  StringRef Code = R"GOLD(
x:int <|<# >
)GOLD";
  GoldFailureTest(Code);
}

TEST(GoldDocAttr, NewLineInsde) {
  StringRef Code = R"GOLD(
x:int <|
>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, NewLineCommentInside) {
  StringRef Code = R"GOLD(
x:int <|<#


#> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

/*
markup    = '<' space tags (':>' ind content ded
            | ':' content '>' | '>' content '<' {'/' ident space} '>')



The "markup" production works as follows:
        Markup like <B,I:Hello> nests as if it were <B:<I:Hello>>.
        <B:Hello> is equivalent to markup'B'{content="Hello"}.
        <B{weight=3}:Hello> is equivalent to markup'B'{weight=3,content="Hello"}.

The "content" production for markup has two cases, depending on
the presence of '~':
        - If '~' is absent,  the markup content is an array concatenating its
          contents, starting from the beginning.
        - If '~' is present, the markup content is an array of arrays
          concatenating the contents of each "~" item, discarding spaces and
          lines before the first '~'.

In "elements" production, all text consumed by the "printable" production is
captured literally, except:
        - Empty lines (meaning: lines that contain only spaces, tabs, and line
          or block comments are captured as blank lines.
        - If markup initiates a significant indentation or exists inside of a
          prior indented construct (e.g. top=0 according to the state of the
          parser), the first non-blank line following the line where the markup
          begins is analyzed to determine its leading indent (consisting of tabs
          and spaces). This leading indent is removed from the first and all
          subsequent lines that are at least as indented. For subsequent lines
          that are less indented, all leading spaces and tabs are removed.

  # Markup Examples
  <Markup>Feeling <B>bold</B>?</Markup> # HTML style.
  <Markup:Feeling <B:bold>?>            # Markdown style.
  <List: ~Apples ~Oranges>              # List.
  <Markup:>                             # Significant indent.
          Indented style markup.
          Healthy foods include<List:>
                  ~Apples
                  ~Oranges
  <https://www.epicgames.com>           # Markup syntax assists to construct URLs.
  <Markup:>
          Markup supports arbitrary injection of code
          via string interpolation like {name} or {0u1234}
          and also PHP style code injection.
  &for(i->m:messages) do <M:>
      Message {i}: {m.subject}

markup      <B:Hello>       macro, markup'B'{content="Hello"}.
markup      <B{a=b}:Hello>  macro, markup'B'{a=b,content="Hello"}.
markup      <B,I:Hello>     macro, markup'B'{content=markup'I'{
                                                      content="Hello"}}.
*/

TEST(GoldDocAttr, Markup_HtmlStyleTags) {
  StringRef Code = R"GOLD(
x:int <|<Markup>Feeling <B>bold</B>?</Markup> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_MarkdownStyle) {
  StringRef Code = R"GOLD(
x:int <|<Markup:Feeling <B:bold>?> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_List) {
  StringRef Code = R"GOLD(
x:int <| <List: ~Apples ~Oranges> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_IndentationSensitiveMarkup) {
  StringRef Code = R"GOLD(
x:int <| <Markup:>
    Markup supports arbitrary injection of code
    via string interpolation like {name} or {0u1234}
    and also PHP style code injection.
>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_IndentationSensitiveMarkup_WithNesting) {
  StringRef Code = R"GOLD(
x:int <| <Markup:>
  Indented style markup.
  Healthy foods include<List:>
          ~Apples
          ~Oranges
>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_SimplestMarkdownExample) {
  StringRef Code = R"GOLD(
x:int <| <B:Hello> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_MarkdownImplicitNesting) {
  StringRef Code = R"GOLD(
x:int <|<B,I:Hello> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_NestedNonContentsExpression) {
  StringRef Code = R"GOLD(
x:int <| <B{weight=3}:Hello>  >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_HtmlWithIntegratedListSyntax) {
  StringRef Code = R"GOLD(
x:int <| <Markup>
			Please eat healthy
			<List>
				~Apples
				~Oranges
			</List>
			Thanks!
		</Markup>  >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, Markup_MarkdownURL) {
  StringRef Code = R"GOLD(
x:int <| <https://www.epicgames.com> >
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

TEST(GoldDocAttr, ReferenceToStatementWithMarkup) {
  StringRef Code = R"GOLD(
x:int <|&for(i->m:messages) do <M:>
  Message {i}: {m.subject}
;>
)GOLD";
  auto ToMatch = varDecl();
  ASSERT_TRUE(matches(Code.str(), ToMatch));
}

// content   = space {line} ('~' {element} {'~' {element}} | {element})
// tags      = [base {postfix | {calls} brace} '.' scan] qualident space [brace] [',' scan tags]
// Markup like <B,I:Hello> nests as if it were <B:<I:Hello>>.
// <B:Hello> is equivalent to markup'B'{content="Hello"}.
// <B{weight=3}:Hello> is equivalent to markup'B'{weight=3,content="Hello"}.
