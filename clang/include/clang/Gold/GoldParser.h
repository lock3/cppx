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
  class SyntaxContext;

  /// A pair of tokens.
  using TokenPair = std::pair<Token, Token>;

  /// The parser transforms sequences of tokens into uninterpreted syntax
  /// trees.
  ///
  /// \todo Lift common parsing functions into a parameterized base class?
  struct Parser
  {
    Parser(SyntaxContext &Context, clang::SourceManager &SM, File const& F);

    void fetchToken()
    {
      Toks.push_back(Lex());
    }

    Token const& peekToken() const {
      return Toks.front();
    }

    Token const& peekToken(std::size_t N)
    {
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
    Syntax *parseOr();
    Syntax *parseAnd();
    Syntax *parseCmp();
    Syntax *parseTo();
    Syntax *parseAdd();
    Syntax *parseMul();

    Syntax *parseMacro();
    Syntax *parseIf();
    Syntax *parseWhile();
    Syntax *parseFor();
    Syntax *parseBlockLoop(Token KWTok);

    Syntax *parsePre();
    Syntax *parsePost();
    Syntax *parseCall(Syntax *fn);
    Syntax *parseElem(Syntax *map);
    Syntax *parseDot(Syntax *obj);
    Syntax *parseArrayPrefix();
    Syntax *parseNNSPrefix();
    Syntax *parsePostAttr(Syntax *Pre);
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
    Syntax *onAtom(const Token& tok);
    Syntax *onLiteral(const Token& tok);
    Syntax *onArray(ArraySemantic S, const llvm::SmallVectorImpl<Syntax*>& Vec);
    Syntax *onList(ArraySemantic S, const llvm::SmallVectorImpl<Syntax*>& Vec);
    Syntax *onBinary(const Token& tok, Syntax *e1, Syntax *e2);
    Syntax *onUnary(const Token& tok, Syntax *e1);
    Syntax *onUnaryOrNull(const Token& tok, Syntax *e1);
    Syntax *onCall(const TokenPair& toks, Syntax *e1, Syntax *e2);
    Syntax *onCall(Syntax *e1, Syntax *e2);
    Syntax *onElem(const TokenPair& toks, Syntax *e1, Syntax *e2);
    Syntax *onMacro(Syntax *e1, Syntax *e2);
    Syntax *onCatch(const Token &Catch, Syntax *Args, Syntax *Block);
    Syntax *onIf(const Token& tok, Syntax *e1, Syntax *e2, Syntax *e3);
    Syntax *onElse(const Token& tok, Syntax *e1);
    Syntax *onLoop(const Token& tok, Syntax *e1, Syntax *e2);
    Syntax *onFile(const llvm::SmallVectorImpl<Syntax*>& Vec);
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
      };

      // True if two locations have the same depth in enclosing tokens.
      inline bool hasSameDepth(const Loc &LHS, const Loc &RHS) const {
        for (unsigned K = Parens; K < EnclosureSize; ++K)
          if (LHS.EnclosureCounts[K] != RHS.EnclosureCounts[K])
            return false;
        return true;
      }

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

    /// Keep track of the depth of enclosure tokens when scanning for
    /// attributes.
    void trackEnclosureDepth(Token Enclosure);
    bool scanAngles(Syntax *Base);
    void startPotentialAngleBracket(const Token &OpToken);
    void finishPotentialAngleBracket(const Token &OpToken);

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
