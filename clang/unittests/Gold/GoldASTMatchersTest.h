//===- unittest/Gold/ASTMatchersTest.h - Matcher tests helpers ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UNITTESTS_GOLD_ASTMATCHERSTEST_H
#define LLVM_CLANG_UNITTESTS_GOLD_ASTMATCHERSTEST_H

#include "clang/Gold/GoldFrontend.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/AST/Attr.h"
#include "clang/Tooling/Tooling.h"
#include "gtest/gtest.h"
#include "clang/AST/DeclTemplate.h"

namespace clang {
namespace ast_matchers {
extern const internal::VariadicDynCastAllOfMatcher<Decl, VarTemplateDecl> varTemplateDecl;
extern const internal::VariadicDynCastAllOfMatcher<
  Decl, VarTemplateSpecializationDecl> varTemplateSpecializationDecl;
extern const internal::VariadicDynCastAllOfMatcher<
  Decl, VarTemplatePartialSpecializationDecl> varTemplatePartialSpecializationDecl;

  

extern const internal::VariadicDynCastAllOfMatcher<Decl, TemplateTemplateParmDecl>
    templateTemplateParmDecl;

extern const internal::VariadicDynCastAllOfMatcher<Stmt, PackExpansionExpr>
    packExpansionExpr;

extern const internal::VariadicDynCastAllOfMatcher<Stmt, CXXFoldExpr>
    cxxFoldExpr;

extern const internal::VariadicDynCastAllOfMatcher<Stmt, SizeOfPackExpr>
    sizeOfPackExpr;

// I created this because it didn't exist before this and I acutally needed it
// for a particular test.
AST_POLYMORPHIC_MATCHER(isExternStorageClass,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(FunctionDecl,
                                                        VarDecl)) {
  return Node.getStorageClass() == SC_Extern;
}

AST_POLYMORPHIC_MATCHER(isParameterPack,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(TemplateTypeParmDecl,
                                                        NonTypeTemplateParmDecl,
                                                        TemplateTemplateParmDecl)) {
  return Node.isParameterPack();
}

AST_POLYMORPHIC_MATCHER_P(stringHasValue,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(StringLiteral),
                          std::string, ToCmpWith) {
  return Node.getString() == ToCmpWith;
}

AST_POLYMORPHIC_MATCHER(isInlinedVar,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(VarDecl)) {
  return Node.isInlineSpecified();
}

AST_POLYMORPHIC_MATCHER(isMutable,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(FieldDecl)) {
  return Node.isMutable();
}

AST_POLYMORPHIC_MATCHER(isAligned,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(FieldDecl, VarDecl,
                                                   RecordDecl, CXXRecordDecl)) {
  return Node.template hasAttr<AlignedAttr>();
}

AST_POLYMORPHIC_MATCHER_P(valueDeclAlignedTo,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(ValueDecl
                                                          ),
                          unsigned, ExpectedAlignment) {
  if (!Node.template hasAttr<AlignedAttr>()) {
    return false;
  }

  // AlignedAttr *Attr = Node.template getAttr<AlignedAttr>();
  TypeInfo Ti = Node.getASTContext().getTypeInfo(Node.getType());
  // unsigned ActualAlignment = Attr->getAlignment(Node.getASTContext());
  return Ti.Align == ExpectedAlignment;
}

AST_POLYMORPHIC_MATCHER_P(underlyingIntegerType,
                        AST_POLYMORPHIC_SUPPORTED_TYPES(EnumDecl),
                        internal::Matcher<QualType>, InnerMatcher) {
  QualType QT = Node.getIntegerType();
  if (!QT.isNull())
    return InnerMatcher.matches(QT, Finder, Builder);
  return false;
}

AST_POLYMORPHIC_MATCHER_P(typeDeclAlignedTo,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(TypeDecl),
                          unsigned, ExpectedAlignment) {
  if (!Node.template hasAttr<AlignedAttr>()) {
    return false;
  }

  // AlignedAttr *Attr = Node.template getAttr<AlignedAttr>();
  TypeInfo Ti = Node.getASTContext().getTypeInfo(
      Node.getASTContext().getTypeDeclType(&Node));
  // unsigned ActualAlignment = Attr->getAlignment(Node.getASTContext());
  return Ti.Align == ExpectedAlignment;
}


struct BaseMatcher {
  bool IsVirtual = false;
  AccessSpecifier AS = AS_none;
  llvm::StringRef Name;
};

AST_MATCHER_P(CXXRecordDecl, hasBaseSpecifier, BaseMatcher, BaseCheck) {
  // Node.IgnoreParenCasts()
  if (BaseCheck.AS == AS_none) {
    for (const CXXBaseSpecifier &B : Node.bases())
      if (B.isVirtual() == BaseCheck.IsVirtual
        && B.getType().getAsString() == BaseCheck.Name) {
        return true;
      }
  } else
    for (const CXXBaseSpecifier &B : Node.bases())
      if (B.isVirtual() == BaseCheck.IsVirtual
          && B.getType().getAsString() == BaseCheck.Name
          && B.getAccessSpecifier() == BaseCheck.AS) {
        return true;
      }
  return false;
}

AST_POLYMORPHIC_MATCHER(methodHasRRefQualifier,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(CXXMethodDecl)) {
  const FunctionProtoType *FPT= Node.getType()->template getAs<FunctionProtoType>();
  return FPT->getExtProtoInfo().RefQualifier == RQ_RValue;
}

AST_POLYMORPHIC_MATCHER(methodHasRefQualifier,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(CXXMethodDecl)) {
  const FunctionProtoType *FPT= Node.getType()->template getAs<FunctionProtoType>();
  return FPT->getExtProtoInfo().RefQualifier == RQ_LValue;
}


AST_POLYMORPHIC_MATCHER_P(hasLHSExpr,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(CXXFoldExpr),
                          internal::Matcher<Expr>, InnerMatcher) {
  const Expr *LeftHandSide = Node.getLHS();
  return (LeftHandSide != nullptr &&
          InnerMatcher.matches(*LeftHandSide, Finder, Builder));
}

AST_POLYMORPHIC_MATCHER_P(hasRHSExpr,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(CXXFoldExpr),
                          internal::Matcher<Expr>, InnerMatcher) {
  const Expr *RightHandSide = Node.getRHS();
  return (RightHandSide != nullptr &&
          InnerMatcher.matches(*RightHandSide, Finder, Builder));
}

AST_POLYMORPHIC_MATCHER_P(hasOperator,
                          AST_POLYMORPHIC_SUPPORTED_TYPES(CXXFoldExpr),
                          BinaryOperatorKind, OpKind) {
  return Node.getOperator() == OpKind;
}

AST_MATCHER_P(CXXDeleteExpr, deleteFunction, internal::Matcher<Decl>,
              InnerMatcher) {
  const FunctionDecl *OpDel = Node.getOperatorDelete();
  return (OpDel != nullptr &&
          InnerMatcher.matches(*OpDel, Finder, Builder));
}

using clang::tooling::buildASTFromCodeWithArgs;
using clang::tooling::newFrontendActionFactory;
using clang::tooling::runToolOnCodeWithArgs;
using clang::tooling::FrontendActionFactory;
using clang::tooling::FileContentMappings;

class BoundNodesCallback {
public:
  virtual ~BoundNodesCallback() {}
  virtual bool run(const BoundNodes *BoundNodes) = 0;
  virtual bool run(const BoundNodes *BoundNodes, ASTContext *Context) = 0;
  virtual void onEndOfTranslationUnit() {}
};

// If 'FindResultVerifier' is not NULL, sets *Verified to the result of
// running 'FindResultVerifier' with the bound nodes as argument.
// If 'FindResultVerifier' is NULL, sets *Verified to true when Run is called.
class VerifyMatch : public MatchFinder::MatchCallback {
public:
  VerifyMatch(std::unique_ptr<BoundNodesCallback> FindResultVerifier, bool *Verified)
      : Verified(Verified), FindResultReviewer(std::move(FindResultVerifier)) {}

  void run(const MatchFinder::MatchResult &Result) override {
    if (FindResultReviewer != nullptr) {
      *Verified |= FindResultReviewer->run(&Result.Nodes, Result.Context);
    } else {
      *Verified = true;
    }
  }

  void onEndOfTranslationUnit() override {
    if (FindResultReviewer)
      FindResultReviewer->onEndOfTranslationUnit();
  }

private:
  bool *const Verified;
  const std::unique_ptr<BoundNodesCallback> FindResultReviewer;
};

enum class LanguageMode {
  Cxx11,
  Cxx14,
  Cxx17,
  Cxx2a,
  Cxx11OrLater,
  Cxx14OrLater,
  Cxx17OrLater,
  Cxx2aOrLater
};

template <typename T>
testing::AssertionResult matchesConditionally(
    const std::string &Code, const T &AMatcher, bool ExpectMatch,
    llvm::ArrayRef<llvm::StringRef> CompileArgs,
    const FileContentMappings &VirtualMappedFiles = FileContentMappings(),
    const std::string &Filename = "temp.usyntax") {
  bool Found = false, DynamicFound = false;
  MatchFinder Finder;
  VerifyMatch VerifyFound(nullptr, &Found);
  Finder.addMatcher(AMatcher, &VerifyFound);
  VerifyMatch VerifyDynamicFound(nullptr, &DynamicFound);
  if (!Finder.addDynamicMatcher(AMatcher, &VerifyDynamicFound))
    return testing::AssertionFailure() << "Could not add dynamic matcher";

  class SimpleFrontendActionFactoryWithFinder : public FrontendActionFactory {
  public:
    MatchFinder *Finder = nullptr;  
    SimpleFrontendActionFactoryWithFinder(MatchFinder *F)
      :Finder(F)
    { }
    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<gold::GoldSyntaxAction>(Finder);
    }
  };

  std::unique_ptr<FrontendActionFactory> Factory(
      new SimpleFrontendActionFactoryWithFinder(&Finder));
  
  // Some tests need rtti/exceptions on.  Use an unknown-unknown triple so we
  // don't instantiate the full system toolchain.  On Linux, instantiating the
  // toolchain involves stat'ing large portions of /usr/lib, and this slows down
  // not only this test, but all other tests, via contention in the kernel.
  //
  // FIXME: This is a hack to work around the fact that there's no way to do the
  // equivalent of runToolOnCodeWithArgs without instantiating a full Driver.
  // We should consider having a function, at least for tests, that invokes cc1.
  std::vector<std::string> Args;
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "gold", "-c"},
      Filename)) {
    return testing::AssertionFailure() << "Parsing error in \"" << Code << "\"";
  }
  if (Found != DynamicFound) {
    return testing::AssertionFailure() << "Dynamic match result ("
                                       << DynamicFound
                                       << ") does not match static result ("
                                       << Found << ")";
  }
  if (!Found && ExpectMatch) {
    class DumpingFrontEndAction : public FrontendActionFactory {
    public:
      DumpingFrontEndAction() { }

      std::unique_ptr<FrontendAction> create() override {
        return std::make_unique<gold::GoldSyntaxActionDumper>();
      }
    };

    std::unique_ptr<FrontendActionFactory> DumpingActionFactory(
        new DumpingFrontEndAction());
    runToolOnCodeWithArgs(DumpingActionFactory->create(), Code,
        {"-x", "gold", "-c"}, Filename);
    return testing::AssertionFailure()
      << "Could not find match in \"" << Code << "\"";
  } else if (Found && !ExpectMatch) {
    return testing::AssertionFailure()
      << "Found unexpected match in \"" << Code << "\"";
  }
  return testing::AssertionSuccess();
}

template <typename T>
testing::AssertionResult matchesConditionally(
    const std::string &Code, const T &AMatcher, bool ExpectMatch,
    llvm::StringRef CompileArg,
    const FileContentMappings &VirtualMappedFiles = FileContentMappings(),
    const std::string &Filename = "temp.usyntax") {
  return matchesConditionally(Code, AMatcher, ExpectMatch,
                              llvm::makeArrayRef(CompileArg),
                              VirtualMappedFiles, Filename);
}

template <typename T>
testing::AssertionResult
matchesConditionally(const std::string &Code, const T &AMatcher,
                     bool ExpectMatch, const LanguageMode &Mode) {
  // std::vector<LanguageMode> LangModes;
  // switch (Mode) {
  // case LanguageMode::Cxx11:
  // case LanguageMode::Cxx14:
  // case LanguageMode::Cxx17:
  // case LanguageMode::Cxx2a:
  //   LangModes = {Mode};
  //   break;
  // case LanguageMode::Cxx11OrLater:
  //   LangModes = {LanguageMode::Cxx11, LanguageMode::Cxx14, LanguageMode::Cxx17,
  //                LanguageMode::Cxx2a};
  //   break;
  // case LanguageMode::Cxx14OrLater:
  //   LangModes = {LanguageMode::Cxx14, LanguageMode::Cxx17, LanguageMode::Cxx2a};
  //   break;
  // case LanguageMode::Cxx17OrLater:
  //   LangModes = {LanguageMode::Cxx17, LanguageMode::Cxx2a};
  //   break;
  // case LanguageMode::Cxx2aOrLater:
  //   LangModes = {LanguageMode::Cxx2a};
  // }

  // for (auto Mode : LangModes) {
  //   std::string LangModeArg;
  //   switch (Mode) {
  //   case LanguageMode::Cxx11:
  //     LangModeArg = "-std=c++11";
  //     break;
  //   case LanguageMode::Cxx14:
  //     LangModeArg = "-std=c++14";
  //     break;
  //   case LanguageMode::Cxx17:
  //     LangModeArg = "-std=c++17";
  //     break;
  //   case LanguageMode::Cxx2a:
  //     LangModeArg = "-std=c++2a";
  //     break;
  //   default:
  //     llvm_unreachable("Invalid language mode");
  //   }

  auto Result =
      matchesConditionally(Code, AMatcher, ExpectMatch, "Gold");
  if (!Result)
    return Result;
  // }

  return testing::AssertionSuccess();
}


template <typename T>
testing::AssertionResult
matches(const std::string &Code, const T &AMatcher) {
  return matchesConditionally(Code, AMatcher, true, "");
}

template <typename T>
testing::AssertionResult
notMatches(const std::string &Code, const T &AMatcher) {
  return matchesConditionally(Code, AMatcher, false, "");
}




template <typename T>
testing::AssertionResult
matchAndVerifyResultConditionally(const std::string &Code, const T &AMatcher,
                                  std::unique_ptr<BoundNodesCallback> FindResultVerifier,
                                  bool ExpectResult) {
  bool VerifiedResult = false;
  MatchFinder Finder;
  VerifyMatch VerifyVerifiedResult(std::move(FindResultVerifier), &VerifiedResult);
  Finder.addMatcher(AMatcher, &VerifyVerifiedResult);
  // std::unique_ptr<FrontendActionFactory> Factory(
  //     newFrontendActionFactory(&Finder));
  class SimpleFrontendActionFactoryWithFinder : public FrontendActionFactory {
  public:
    MatchFinder *Finder = nullptr;  
    SimpleFrontendActionFactoryWithFinder(MatchFinder *F)
      :Finder(F)
    { }
    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<gold::GoldSyntaxAction>(Finder);
    }
  };

  std::unique_ptr<FrontendActionFactory> Factory(
      new SimpleFrontendActionFactoryWithFinder(&Finder));
  

  // Some tests use typeof, which is a gnu extension.  Using an explicit
  // unknown-unknown triple is good for a large speedup, because it lets us
  // avoid constructing a full system triple.
  std::vector<std::string> Args = {"gold"};
  if (!runToolOnCodeWithArgs(Factory->create(), Code, {"-x", "gold", "-c"}, "temp.usyntax")) {
    return testing::AssertionFailure() << "Parsing error in \"" << Code << "\"";
  }
  if (!VerifiedResult && ExpectResult) {
    return testing::AssertionFailure()
      << "Could not verify result in \"" << Code << "\"";
  } else if (VerifiedResult && !ExpectResult) {
    return testing::AssertionFailure()
      << "Verified unexpected result in \"" << Code << "\"";
  }

  VerifiedResult = false;
  std::unique_ptr<ASTUnit> AST(buildASTFromCodeWithArgs(Code, Args, "temp.usyntax"));
  if (!AST.get())
    return testing::AssertionFailure() << "Parsing error in \"" << Code
                                       << "\" while building AST";
  Finder.matchAST(AST->getASTContext());
  if (!VerifiedResult && ExpectResult) {
    return testing::AssertionFailure()
      << "Could not verify result in \"" << Code << "\" with AST";
  } else if (VerifiedResult && !ExpectResult) {
    return testing::AssertionFailure()
      << "Verified unexpected result in \"" << Code << "\" with AST";
  }

  return testing::AssertionSuccess();
}

// FIXME: Find better names for these functions (or document what they
// do more precisely).
template <typename T>
testing::AssertionResult
matchAndVerifyResultTrue(const std::string &Code, const T &AMatcher,
                         std::unique_ptr<BoundNodesCallback> FindResultVerifier) {
  return matchAndVerifyResultConditionally(
      Code, AMatcher, std::move(FindResultVerifier), true);
}

template <typename T>
testing::AssertionResult
matchAndVerifyResultFalse(const std::string &Code, const T &AMatcher,
                          std::unique_ptr<BoundNodesCallback> FindResultVerifier) {
  return matchAndVerifyResultConditionally(
      Code, AMatcher, std::move(FindResultVerifier), false);
}

// Implements a run method that returns whether BoundNodes contains a
// Decl bound to Id that can be dynamically cast to T.
// Optionally checks that the check succeeded a specific number of times.
template <typename T>
class VerifyIdIsBoundTo : public BoundNodesCallback {
public:
  // Create an object that checks that a node of type \c T was bound to \c Id.
  // Does not check for a certain number of matches.
  explicit VerifyIdIsBoundTo(llvm::StringRef Id)
    : Id(Id), ExpectedCount(-1), Count(0) {}

  // Create an object that checks that a node of type \c T was bound to \c Id.
  // Checks that there were exactly \c ExpectedCount matches.
  VerifyIdIsBoundTo(llvm::StringRef Id, int ExpectedCount)
    : Id(Id), ExpectedCount(ExpectedCount), Count(0) {}

  // Create an object that checks that a node of type \c T was bound to \c Id.
  // Checks that there was exactly one match with the name \c ExpectedName.
  // Note that \c T must be a NamedDecl for this to work.
  VerifyIdIsBoundTo(llvm::StringRef Id, llvm::StringRef ExpectedName,
                    int ExpectedCount = 1)
    : Id(Id), ExpectedCount(ExpectedCount), Count(0),
      ExpectedName(ExpectedName) {}

  void onEndOfTranslationUnit() override {
    if (ExpectedCount != -1) {
      EXPECT_EQ(ExpectedCount, Count);
    }
    if (!ExpectedName.empty()) {
      EXPECT_EQ(ExpectedName, Name);
    }
    Count = 0;
    Name.clear();
  }

  ~VerifyIdIsBoundTo() override {
    EXPECT_EQ(0, Count);
    EXPECT_EQ("", Name);
  }

  bool run(const BoundNodes *Nodes) override {
    const BoundNodes::IDToNodeMap &M = Nodes->getMap();
    if (Nodes->getNodeAs<T>(Id)) {
      ++Count;
      if (const NamedDecl *Named = Nodes->getNodeAs<NamedDecl>(Id)) {
        Name = Named->getNameAsString();
      } else if (const NestedNameSpecifier *NNS =
        Nodes->getNodeAs<NestedNameSpecifier>(Id)) {
        llvm::raw_string_ostream OS(Name);
        NNS->print(OS, PrintingPolicy(LangOptions()));
      }
      BoundNodes::IDToNodeMap::const_iterator I = M.find(Id);
      EXPECT_NE(M.end(), I);
      if (I != M.end()) {
        EXPECT_EQ(Nodes->getNodeAs<T>(Id), I->second.get<T>());
      }
      return true;
    }
    EXPECT_TRUE(M.count(Id) == 0 ||
      M.find(Id)->second.template get<T>() == nullptr);
    return false;
  }

  bool run(const BoundNodes *Nodes, ASTContext *Context) override {
    return run(Nodes);
  }

private:
  const std::string Id;
  const int ExpectedCount;
  int Count;
  const std::string ExpectedName;
  std::string Name;
};

} // namespace ast_matchers
} // namespace clang

#endif  // LLVM_CLANG_UNITTESTS_AST_MATCHERS_AST_MATCHERS_TEST_H
