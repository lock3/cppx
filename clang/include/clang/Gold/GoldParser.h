//===- GoldParser.h - Gold Language Parser --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// Copyright (c) Lock3 Software 2019, all rights reserved.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the GoldParser interface.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_GOLD_GOLDPARSER_H
#define CLANG_GOLD_GOLDPARSER_H

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/SmallVector.h"

#include "clang/Gold/GoldLexer.h"

namespace clang {

class DiagnosticsEngine;

} // namespace clang


namespace gold
{
  struct Attribute;
  struct Syntax;
  struct LiteralSyntax;
  class SyntaxContext;

  /// A pair of tokens.
  using TokenPair = std::pair<Token, Token>;

  enum FoldDirection {
    FD_Right,
    FD_Left
  };

  /// The parser transforms sequences of tokens into uninterpreted syntax
  /// trees.
  ///
  /// \todo Lift common parsing functions into a parameterized base class?
  struct Parser
  {
    Parser(SyntaxContext &Context, clang::SourceManager &SM, File const& F,
          clang::Preprocessor &PP);

    void fetchToken()
    {
      Toks.push_back(Lex());
    }

    Token const& peekToken() const {
      return FusionToks.empty() ?
        Toks.front() : FusionToks.front();
    }

    Token const& peekToken(std::size_t N)
    {
      if (!FusionToks.empty() && N < FusionToks.size())
          return FusionToks[N];

      // The token is in the lookahead buffer.
      if (N < Toks.size())
        return Toks[N];

      // Load tokens until we reach the nth token.
      N = N - Toks.size() + 1;
      while (N != 0) {
        fetchToken();
        --N;
      }

      return Toks.back();
    }

    TokenKind getLookahead() const {
      return peekToken().getKind();
    }

    TokenKind getLookahead(std::size_t N) {
      return peekToken(N).getKind();
    }

    clang::SourceLocation getInputLocation() const {
      return peekToken().getLocation();
    }

    bool atEndOfFile() {
      return peekToken().isEndOfFile();
    }

    bool nextTokenIs(TokenKind K) const {
      return getLookahead() == K;
    }

    bool nextTokenIs(char const* Id) const {
      return nextTokenIs(tok::Identifier) && peekToken().hasSpelling(Id);
    }

    bool nthTokenIs(std::size_t N, TokenKind K) {
      return getLookahead(N) == K;
    }

    bool nthTokenIs(std::size_t N, char const* Id) {
      Token const& Tok = peekToken(N);
      return Tok.hasKind(tok::Identifier) && Tok.hasSpelling(Id);
    }

    bool nextTokenIsNot(TokenKind K) {
      return !nextTokenIs(K);
    }

    bool nextTokenIsNot(char const* Id) {
      return !nextTokenIs(Id);
    }

    bool nthTokenIsNot(std::size_t N, TokenKind K) {
      return !nthTokenIs(N, K);
    }

    bool nthTokenIsNot(std::size_t N, char const* Id) {
      return !nthTokenIs(N, Id);
    }

    Token consumeToken() {
      // Take the front token.
      if (!FusionToks.empty()) {
        Token Tok = FusionToks.front();
        FusionToks.pop_front();
        return Tok;
      }

      Token Tok = Toks.front();
      Toks.pop_front();

      if (!Tok.hasKind(tok::Space))
        PreviousToken = Tok;

      // Refresh the queue.
      if (Toks.empty())
        fetchToken();

      LastTokWasClose = false;
      return Tok;
    }

    Token matchToken(TokenKind K)
    {
      if (nextTokenIs(K))
        return consumeToken();
      return {};
    }

    Token matchToken(char const* Id)
    {
      if (nextTokenIs(Id))
        return consumeToken();
      return {};
    }

    Token matchNthToken(std::size_t N, TokenKind K)
    {
      if (nthTokenIs(N, K))
        return consumeToken();
      return {};
    }

    Token matchNthToken(std::size_t N, char const* Id)
    {
      if (nthTokenIs(N, Id))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchTokenIf(Predicate Pred) {
      if (Pred(getLookahead()))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchNthTokenIf(std::size_t N, Predicate Pred) {
      if (Pred(getLookahead()))
        return consumeToken();
      return {};
    }

    template<typename Predicate>
    Token matchTokens(Predicate Pred, Parser &P) {
      if (Pred(P))
        return consumeToken();
      return {};
    }

    Token expectToken(TokenKind K);

    Token expectToken(char const* Id);

    Token requireToken(TokenKind k)
    {
      assert(nextTokenIs(k));
      return consumeToken();
    }

    Syntax *parseFile();

    /// Determines how arrays and lists are flattened during parsing.
    /// By "flattened" we mean removing (i.e., not representing) empty
    /// sequences, and replacing singletons with their elements.
    enum ArraySemantic {
      /// A block array does not contain empty or singleton lists.
      BlockArray,

      /// A singleton array is actually a list.
      ArgArray,
    };

    Syntax *parseArray(ArraySemantic S);
    void parseArray(ArraySemantic S, llvm::SmallVectorImpl<Syntax *> &Vec);
    Syntax *parseList(ArraySemantic S);
    void parseList(llvm::SmallVectorImpl<Syntax *> &Vec);

    Syntax *parseExpr();
    bool parsePreattr();
    Syntax *parseDef();
    Syntax *parseDefFold(Syntax *E1);
    Syntax *parseExpansion();
    Syntax *parseOr();
    Syntax *parseOrFold(Syntax *E1);
    Syntax *parseAnd();
    Syntax *parseAndFold(Syntax *E1);
    Syntax *parseBitShift();
    Syntax *parseBitShiftFold(Syntax *E1);
    Syntax *parseCmp();
    Syntax *parseCmpFold(Syntax *E1);
    Syntax *parseTo();
    // Syntax *parseToFold(Syntax *E1);
    Syntax *parseAdd();
    Syntax *parseAddFold(Syntax *E1);
    Syntax *parseMul();
    Syntax *parseMulFold(Syntax *E1);

    enum FoldKind {
      FK_None, // Not found
      FK_Unary_Left, // Unary left
      FK_Unary_Right, // Unary right.
      FK_Binary // Binary direction unknown
    };

    Syntax *parseFoldExpr(FoldKind ExprKind);

    Syntax *parseMacro();
    Syntax *parseIf();
    Syntax *parseWhile();
    Syntax *parseFor();
    Syntax *parseNew();
    Syntax *parseDelete();
    Syntax *parseLambda();
    Syntax *parseBlockLoop(Token KWTok);

    Syntax *parseThrow();
    Syntax *parsePre();
    Syntax *parsePost();
    Syntax *parseCall(Syntax *fn);
    Syntax *parseElem(Syntax *map);
    Syntax *parseDot(Syntax *Obj);
    Syntax *parseDotCaret(Syntax *Obj);
    Syntax *parseArrayPrefix();
    Syntax *parseNNSPrefix();
    Syntax *parsePostAttr(Syntax *Pre);

    Syntax *parseExpansionOperator(Syntax *Obj);
  private:
    Attribute *parsePostAttr();

  public:
    Syntax *parsePrimary();
    Syntax *parseId();
    Syntax *parseParen();


    Syntax *parseOf();
    Syntax *parseImm();

    Syntax *parseBlock();
    Syntax *parseBracedArray();
    Syntax *parseNestedArray();
    Syntax *parsePrimaryBlock();
    Syntax *parseCatchSequence(Syntax *Contents);
    Syntax *parseCatch();

    bool parsePreAttr();
    Syntax *parseDocAttr();

    // Primary expressions
    Syntax *parseReserved();
    Syntax *parseKey();
    Syntax *parseWord();
    Syntax *parseChar();
    Syntax *parseString();
    Syntax *parseNum();

    // Semantic actions
    Syntax *onAtom(const Token &Tok);
    Syntax *onAtom(const Token &Tok, const tok::FusionKind K, Syntax *Data);
    Syntax *onLiteral(const Token& tok);
    Syntax *onUserDefinedLiteral(Syntax *Base, const Token &Lit);
    Syntax *onArray(ArraySemantic S, const llvm::SmallVectorImpl<Syntax*>& Vec);
    Syntax *onList(ArraySemantic S, const llvm::SmallVectorImpl<Syntax*>& Vec);
    Syntax *onBinary(const Token& tok, Syntax *e1, Syntax *e2);
    Syntax *onUnary(const Token& tok, Syntax *e1);
    Syntax *onUnaryOrNull(const Token& tok, Syntax *e1);
    Syntax *onCall(const TokenPair& toks, Syntax *e1, Syntax *e2);
    Syntax *onCall(Syntax *e1, Syntax *e2);
    Syntax *onElem(const TokenPair& toks, Syntax *e1, Syntax *e2);
    Syntax *onMacro(Syntax *e1, Syntax *e2);
    Syntax *onMacro(Syntax *e1, Syntax *e2, Syntax *e3);
    Syntax *onCatch(const Token &Catch, Syntax *Args, Syntax *Block);
    Syntax *onIf(const Token& tok, Syntax *e1, Syntax *e2, Syntax *e3);
    Syntax *onElse(const Token& tok, Syntax *e1);
    Syntax *onLoop(const Token& tok, Syntax *e1, Syntax *e2);
    Syntax *onFile(const llvm::SmallVectorImpl<Syntax*>& Vec);
    Syntax *onUnaryFoldExpr(FoldDirection Dir, const Token &Operator,
                            const Token &Ellipsis, Syntax *E);
    Syntax *onBinaryFoldExpr(const Token &OperatorToken, const Token &Ellipsis,
                             Syntax *LHS, Syntax *RHS);
    Syntax *onInvalidRightUnaryFoldOperator();
    Syntax *onError() const;

    /// Whether the '>' token acts as an operator or not. This will be
    /// true except when we are parsing an expression within a post-attribute,
    /// where the '>' closes the attribute.
    bool GreaterThanIsOperator = true;

    /// The lexer.
    Lexer Lex;

    /// Lookahead tokens.
    std::deque<Token> Toks;

    /// Diagnostics.
    clang::DiagnosticsEngine &Diags;

    // Keeps track of information and memory associated with our Gold AST.
    SyntaxContext &Context;

    // Lookahead for a parse of fused token data.
    std::deque<Token> FusionToks;

    std::size_t ParenCount = 0, BracketCount = 0,
      BraceCount = 0, IndentCount = 0;

    // Keep track of whether or not the previous token was a enclosure closing,
    // such as a right parenthesis, dedent, etc.
    bool LastTokWasClose = false;

    // The token before the current lookahead.
    Token PreviousToken;

    // The last identifier we parsed.
    Syntax *LastIdentifier = nullptr;

    /// Tracker for '<' tokens that might have been intended to be treated as an
    /// angle bracket instead of a less-than comparison.
    ///
    /// This happens when the user intends to form a attribute.
    ///
    /// We track these locations from the point where we see a '<' with a
    /// name-like expression on its left until we see a '>' that might
    /// match it.
    struct AngleBracketTracker {
      enum EnclosureKind : unsigned {
        Parens, Brackets, Braces, Indents, EnclosureSize
      };

      // The nesting depth of the tracker.
      std::size_t EnclosureCounts[EnclosureSize] = {0, 0, 0, 0};

      // Represents an actual token.
      struct Loc {
        clang::SourceLocation SourceLoc;
        std::size_t EnclosureCounts[EnclosureSize];

        // True when this location is shallower than RHS
        inline bool operator<(const Loc &RHS) const {
          for (unsigned K = Parens; K < EnclosureSize; ++K)
            if (EnclosureCounts[K] < RHS.EnclosureCounts[K])
              return true;
          return false;
        }

        // True when this location is deeper than RHS
        inline bool operator>(const Loc &RHS) const {
          for (unsigned K = Parens; K < EnclosureSize; ++K)
            if (EnclosureCounts[K] > RHS.EnclosureCounts[K])
              return true;
          return false;
        }

        // True when this location has the same depth as RHS in enclosure tokens
        inline bool operator==(const Loc &RHS) const {
          for (unsigned K = Parens; K < EnclosureSize; ++K)
            if (EnclosureCounts[K] != RHS.EnclosureCounts[K])
              return false;
          return true;
        }

        inline bool operator!=(const Loc &RHS) const {
          return !this->operator==(RHS);
        }
      };

      // True when we are "inside" a potential angle bracket.
      inline bool isOpen() const {
        return !Angles.empty();
      }

      void clear() {
        Angles.clear();
        Enclosures.clear();
        for (auto &I : EnclosureCounts)
          I = 0;
      }

      // The amount of open angle tokens we have encountered.
      llvm::SmallVector<Loc, 4> Angles;

      // The amount of other open enclosure tokens we have encountered.
      llvm::SmallVector<Loc, 4> Enclosures;
    };

  private:
    bool scanNNSPrefix();

  private:
    AngleBracketTracker Angles;

    bool scanAngles(Syntax *Base);
    void startPotentialAngleBracket(const Token &OpToken);
    void finishPotentialAngleBracket(const Token &OpToken);

  private:
    AngleBracketTracker Folds;

    FoldKind scanForFoldExpr();
    bool InsideKnownFoldExpr = false;
    bool ParseFoldOp = false;

    struct FoldSubExprRAII {
      Parser &P;
      bool PreviousState;
      bool PreviousParsedFoldOp;
      FoldSubExprRAII(Parser &P, bool EnterFoldExpr)
        :P(P),
        PreviousState(P.InsideKnownFoldExpr),
        PreviousParsedFoldOp(P.ParseFoldOp)
      {
        P.InsideKnownFoldExpr = EnterFoldExpr;
      }

      ~FoldSubExprRAII() {
        P.ParseFoldOp = PreviousParsedFoldOp;
        P.InsideKnownFoldExpr = PreviousState;
      }
    };

    template<typename Pred>
    bool nextOperatorIsFold(Pred Predicate) {
      return Predicate(*this) && nthTokenIs(1, tok::Ellipsis);
    }
    bool nextTokensMatchBinaryFoldOp(TokenKind TK);
    bool nextTokensMatchBinaryFoldOp();

    // void startPotentialFold(const Token &EllipsisTok);
    // void finishPotentialFold(const Token &NextTok);


  public:
    void incrementEnclosureCount(unsigned Enclosure);
    void decrementEnclosureCount(unsigned Enclosure);

  private:
    /// Holds onto each pre-attribute we parse until finishing the declaration.
    llvm::SmallVector<Attribute *, 4> Preattributes;

    /// Attach the current stack of preattrs to a Syntax.
    void attachPreattrs(Syntax *S);

    /// True when we are parsing an attribute
    bool InAttribute = false;

    struct AttributeScope {
      AttributeScope(bool &InAttribute)
        : InAttribute(InAttribute) {
        InAttribute = !InAttribute;
      }

      ~AttributeScope() {
        InAttribute = !InAttribute;
      }

    private:
      bool &InAttribute;
    };
  };

  /// RAII object that makes '>' behave either as an operator
  /// or as the closing angle bracket for a post-attribute.
  class GreaterThanIsOperatorScope
  {
    bool &GreaterThanIsOperator;
    bool OldGreaterThanIsOperator;
  public:
    GreaterThanIsOperatorScope(bool &GTIO, bool Val)
      : GreaterThanIsOperator(GTIO), OldGreaterThanIsOperator(GTIO) {
      GreaterThanIsOperator = Val;
    }

    ~GreaterThanIsOperatorScope() {
      GreaterThanIsOperator = OldGreaterThanIsOperator;
    }
  };
} // namespace gold

#endif
