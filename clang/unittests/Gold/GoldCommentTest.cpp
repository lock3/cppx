//=== GoldCommentTest.cpp --------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  Testing if comments are handed off to the C preprocessor correctly.
//  This is to ensure that llvm-lit will work correctly with our system.
//  Because in order to use llvm-lit we need to correctly process comments and
//  give them to the C preprocessor which hands them off to a comment handler.
//
//===----------------------------------------------------------------------===//


#include "GoldParseUtil.h"
#include "GoldASTMatchersTest.h"
#include "GoldTestVisitor.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace gold;

struct Comment {
  Comment(const std::string &Message, unsigned Line, unsigned Col)
    : Message(Message), Line(Line), Col(Col) { }

  std::string Message;
  unsigned Line, Col;
};

class CommentVerifier;
typedef std::vector<Comment> CommentList;

class GoldCommentHandlerVisitor : public GoldTestVisitor<GoldCommentHandlerVisitor>,
                                  public CommentHandler {
typedef GoldTestVisitor<GoldCommentHandlerVisitor> base;

public:
  GoldCommentHandlerVisitor() : base(), PP(nullptr), Verified(false) {}

  ~GoldCommentHandlerVisitor() override {
    EXPECT_TRUE(Verified) << "CommentVerifier not accessed";
  }


  bool HandleComment(Preprocessor &PP, SourceRange Loc) override {
    assert(&PP == this->PP && "Preprocessor changed!");
    SourceLocation Start = Loc.getBegin();
    SourceManager &SM = PP.getSourceManager();
    std::string C(SM.getCharacterData(Start),
                  SM.getCharacterData(Loc.getEnd()));

    bool Invalid;
    unsigned CLine = SM.getSpellingLineNumber(Start, &Invalid);
    EXPECT_TRUE(!Invalid) << "Invalid line number on comment " << C;

    unsigned CCol = SM.getSpellingColumnNumber(Start, &Invalid);
    EXPECT_TRUE(!Invalid) << "Invalid column number on comment " << C;

    assert (C != "\n") ;
    Comments.push_back(Comment(C, CLine, CCol));
    return false;
  }

  CommentVerifier GetVerifier();

protected:
  std::unique_ptr<ASTFrontendAction> CreateTestAction() override {
    return std::make_unique<CommentHandlerAction>(this);
  }

private:
  Preprocessor *PP;
  CommentList Comments;
  bool Verified;

  class CommentHandlerAction : public base::TestAction {
  public:
    CommentHandlerAction(GoldCommentHandlerVisitor *Visitor)
        : TestAction(Visitor) { }

    bool BeginSourceFileAction(CompilerInstance &CI) override {
      GoldCommentHandlerVisitor *V =
          static_cast<GoldCommentHandlerVisitor*>(this->Visitor);
      V->PP = &CI.getPreprocessor();
      V->PP->addCommentHandler(V);
      return true;
    }

    void EndSourceFileAction() override {
      GoldCommentHandlerVisitor *V =
          static_cast<GoldCommentHandlerVisitor*>(this->Visitor);
      V->PP->removeCommentHandler(V);
    }
  };
};

class CommentVerifier {
  CommentList::const_iterator Current;
  CommentList::const_iterator End;
  Preprocessor *PP;

public:
  CommentVerifier(const CommentList &Comments, Preprocessor *PP)
      : Current(Comments.begin()), End(Comments.end()), PP(PP)
    { }

  CommentVerifier(CommentVerifier &&C) : Current(C.Current), End(C.End), PP(C.PP) {
    C.Current = C.End;
  }

  ~CommentVerifier() {
    if (Current != End) {
      EXPECT_TRUE(Current == End) << "Unexpected comment \""
        << Current->Message << "\" at line " << Current->Line << ", column "
        << Current->Col;
    }
  }

  void Match(const char *Message, unsigned Line, unsigned Col) {
    EXPECT_TRUE(Current != End) << "Comment " << Message << " not found";
    if (Current == End) return;

    const Comment &C = *Current;
    EXPECT_TRUE(C.Message == Message && C.Line == Line && C.Col == Col)
      <<   "Expected comment \"" << Message
      << "\" at line " << Line   << ", column " << Col
      << "\nActual comment   \"" << C.Message
      << "\" at line " << C.Line << ", column " << C.Col;

    ++Current;
  }
};


CommentVerifier GoldCommentHandlerVisitor::GetVerifier() {
  Verified = true;
  return CommentVerifier(Comments, PP);
}


TEST(GoldCommentHandling, NoComments) {
  GoldCommentHandlerVisitor Visitor;
  EXPECT_TRUE(Visitor.runOver(R"(x = class)"));
  CommentVerifier Verifier = Visitor.GetVerifier();
}

TEST(GoldCommentHandling, SingleLineConmments) {
  GoldCommentHandlerVisitor Visitor;
  EXPECT_TRUE(Visitor.runOver(R"(x = class # comment)"));
  CommentVerifier Verifier = Visitor.GetVerifier();
  Verifier.Match("# comment", 1, 11);
}

TEST(GoldCommentHandling, SingleLineConmments_WithNewlineAtTheEnd) {
  GoldCommentHandlerVisitor Visitor;
  EXPECT_TRUE(Visitor.runOver(R"(x = class # comment
)"));
  CommentVerifier Verifier = Visitor.GetVerifier();
  Verifier.Match("# comment", 1, 11);
}


TEST(GoldCommentHandling, MultiLineComment) {
  GoldCommentHandlerVisitor Visitor;
  EXPECT_TRUE(Visitor.runOver(R"(x = class <# comment #>)"));
  CommentVerifier Verifier = Visitor.GetVerifier();
  Verifier.Match("<# comment #>", 1, 11);
}

TEST(GoldCommentHandling, MultipleLineConmments) {
  GoldCommentHandlerVisitor Visitor;
  EXPECT_TRUE(Visitor.runOver(R"(x = class # comment 1
# comment 2
)"));
  CommentVerifier Verifier = Visitor.GetVerifier();
  Verifier.Match("# comment 1", 1, 11);
  Verifier.Match("# comment 2", 2, 1);
}

TEST(GoldCommentHandling, NestingAndSeparation) {
  StringRef Code = R"(
x : int = 0
<#
  Defining the new allocator.
  <# doubly nested comment #>
#>

main() : int! <# test #>
  return x <#test<#test#>#>
)";

  SimpleGoldParseTest(Code);
}
