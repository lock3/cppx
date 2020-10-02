//=== GoldGoldTestVisitor.h ----------------------------------------------------==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file helps test interactions with the different aspects of the compiler
//  in order to verify interactions between gold and the clang
//  compiler infastructure.
//
//===----------------------------------------------------------------------===//


#ifndef LLVM_CLANG_UNITTESTS_GOLD_GOLDTESTVISITOR_H
#define LLVM_CLANG_UNITTESTS_GOLD_GOLDTESTVISITOR_H

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "gtest/gtest.h"
#include <vector>

namespace gold {

template <typename T>
class GoldTestVisitor : public clang::RecursiveASTVisitor<T> {
public:
  GoldTestVisitor() { }

  virtual ~GoldTestVisitor() { }

  /// \brief Runs the current AST visitor over the given code.
  bool runOver(llvm::StringRef Code) {
    return clang::tooling::runToolOnCodeWithArgs(CreateTestAction(), Code,
                                                 {"-x", "gold", "-c"},
                                                 "temp.usyntax");
  }

  bool shouldVisitTemplateInstantiations() const {
    return true;
  }

  bool shouldVisitImplicitCode() const {
    return true;
  }

protected:
  virtual std::unique_ptr<clang::ASTFrontendAction> CreateTestAction() {
    return std::make_unique<TestAction>(this);
  }

  class FindConsumer : public clang::ASTConsumer {
  public:
    FindConsumer(GoldTestVisitor *Visitor) : Visitor(Visitor) {}

    void HandleTranslationUnit(clang::ASTContext &Context) override {
      Visitor->Context = &Context;
      Visitor->TraverseDecl(Context.getTranslationUnitDecl());
    }

  private:
    GoldTestVisitor *Visitor;
  };

  class TestAction : public clang::ASTFrontendAction {
  public:
    TestAction(GoldTestVisitor *Visitor) : Visitor(Visitor) {}

    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &,
                      llvm::StringRef dummy) override {
      /// TestConsumer will be deleted by the framework calling us.
      return std::make_unique<FindConsumer>(Visitor);
    }

  protected:
    GoldTestVisitor *Visitor;
  };

  clang::ASTContext *Context;
};

/// \brief A RecursiveASTVisitor to check that certain matches are (or are
/// not) observed during visitation.
///
/// This is a RecursiveASTVisitor for testing the RecursiveASTVisitor itself,
/// and allows simple creation of test visitors running matches on only a small
/// subset of the Visit* methods.
template <typename T, template <typename> class Visitor = GoldTestVisitor>
class ExpectedLocationVisitor : public Visitor<T> {
public:
  /// \brief Expect 'Match' *not* to occur at the given 'Line' and 'Column'.
  ///
  /// Any number of matches can be disallowed.
  void DisallowMatch(clang::Twine Match, unsigned Line, unsigned Column) {
    DisallowedMatches.push_back(MatchCandidate(Match, Line, Column));
  }

  /// \brief Expect 'Match' to occur at the given 'Line' and 'Column'.
  ///
  /// Any number of expected matches can be set by calling this repeatedly.
  /// Each is expected to be matched 'Times' number of times. (This is useful in
  /// cases in which different AST nodes can match at the same source code
  /// location.)
  void ExpectMatch(clang::Twine Match, unsigned Line, unsigned Column,
                   unsigned Times = 1) {
    ExpectedMatches.push_back(ExpectedMatch(Match, Line, Column, Times));
  }

  /// \brief Checks that all expected matches have been found.
  ~ExpectedLocationVisitor() override {
    for (typename std::vector<ExpectedMatch>::const_iterator
             It = ExpectedMatches.begin(), End = ExpectedMatches.end();
         It != End; ++It) {
      It->ExpectFound();
    }
  }

protected:
  /// \brief Checks an actual match against expected and disallowed matches.
  ///
  /// Implementations are required to call this with appropriate values
  /// for 'Name' during visitation.
  void Match(llvm::StringRef Name, clang::SourceLocation Location) {
    const clang::FullSourceLoc FullLocation = this->Context->getFullLoc(Location);

    for (typename std::vector<MatchCandidate>::const_iterator
             It = DisallowedMatches.begin(), End = DisallowedMatches.end();
         It != End; ++It) {
      EXPECT_FALSE(It->Matches(Name, FullLocation))
          << "Matched disallowed " << *It;
    }

    for (typename std::vector<ExpectedMatch>::iterator
             It = ExpectedMatches.begin(), End = ExpectedMatches.end();
         It != End; ++It) {
      It->UpdateFor(Name, FullLocation, this->Context->getSourceManager());
    }
  }

 private:
  struct MatchCandidate {
    std::string ExpectedName;
    unsigned LineNumber;
    unsigned ColumnNumber;

    MatchCandidate(llvm::Twine Name, unsigned LineNumber, unsigned ColumnNumber)
      : ExpectedName(Name.str()), LineNumber(LineNumber),
        ColumnNumber(ColumnNumber) {
    }

    bool Matches(llvm::StringRef Name, clang::FullSourceLoc const &Location) const {
      return MatchesName(Name) && MatchesLocation(Location);
    }

    bool PartiallyMatches(llvm::StringRef Name, clang::FullSourceLoc const &Location) const {
      return MatchesName(Name) || MatchesLocation(Location);
    }

    bool MatchesName(llvm::StringRef Name) const {
      return Name == ExpectedName;
    }

    bool MatchesLocation(clang::FullSourceLoc const &Location) const {
      return Location.isValid() &&
          Location.getSpellingLineNumber() == LineNumber &&
          Location.getSpellingColumnNumber() == ColumnNumber;
    }

    friend std::ostream &operator<<(std::ostream &Stream,
                                    MatchCandidate const &Match) {
      return Stream << Match.ExpectedName
                    << " at " << Match.LineNumber << ":" << Match.ColumnNumber;
    }
  };

  struct ExpectedMatch {
    ExpectedMatch(llvm::Twine Name, unsigned LineNumber, unsigned ColumnNumber,
                  unsigned Times)
        : Candidate(Name, LineNumber, ColumnNumber), TimesExpected(Times),
          TimesSeen(0) {}

    void UpdateFor(llvm::StringRef Name, clang::FullSourceLoc Location,
                   clang::SourceManager &SM) {
      if (Candidate.Matches(Name, Location)) {
        EXPECT_LT(TimesSeen, TimesExpected);
        ++TimesSeen;
      } else if (TimesSeen < TimesExpected &&
                 Candidate.PartiallyMatches(Name, Location)) {
        llvm::raw_string_ostream Stream(PartialMatches);
        Stream << ", partial match: \"" << Name << "\" at ";
        Location.print(Stream, SM);
      }
    }

    void ExpectFound() const {
      EXPECT_EQ(TimesExpected, TimesSeen)
          << "Expected \"" << Candidate.ExpectedName
          << "\" at " << Candidate.LineNumber
          << ":" << Candidate.ColumnNumber << PartialMatches;
    }

    MatchCandidate Candidate;
    std::string PartialMatches;
    unsigned TimesExpected;
    unsigned TimesSeen;
  };

  std::vector<MatchCandidate> DisallowedMatches;
  std::vector<ExpectedMatch> ExpectedMatches;
};
}

#endif