#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticParse.h"

#include "clang/Green/GreenParser.h"
#include "clang/Green/Syntax.h"

#include <iostream>

namespace green
{
  // [[maybe_unused]] static
  // void dump_parser_trace(parser& p, char const* fn)
  // {
  //   trace(p.input_location(), "in {}", fn);
  // }

  namespace
  {
    // TODO: This should be made a general feature since it appears in
    // virtually every parser.

    enum enclosure_kind
    {
      enc_parens,
      enc_braces,
      enc_brackets,
      enc_tabs,
    };

    token_kind open_tokens[]
    {
      tok_left_paren,
      tok_left_brace,
      tok_left_bracket,
      tok_indent,
    };

    token_kind close_tokens[]
    {
      tok_right_paren,
      tok_right_brace,
      tok_right_bracket,
      tok_dedent,
    };

    /// A class to help match enclosing tokens.
    template<enclosure_kind K>
    struct enclosing_tokens
    {
      enclosing_tokens(parser& p)
        : p(p)
      { }

      bool expect_open()
      {
        open = p.expect(open_tokens[K]);
        return (bool)open;
      }

      bool expect_close()
      {
        // Allow end-of-file where dedents are expected.
        if (K == enc_tabs)
        {
          if (p.eof())
          {
            close = p.peek();
            return true;
          }
        }

        close = p.expect(close_tokens[K]);
        if (!close)
        {
          // note(open.loc, "matching '{}'' here", spelling(open.kind()));
        }
        return (bool)close;
      }

      parser& p;
      token open;
      token close;
    };

    struct enclosing_parens : enclosing_tokens<enc_parens>
    {
      using enclosing_tokens<enc_parens>::enclosing_tokens;
    };

    struct enclosing_braces : enclosing_tokens<enc_braces>
    {
      using enclosing_tokens<enc_braces>::enclosing_tokens;
    };

    struct enclosing_brackets : enclosing_tokens<enc_brackets>
    {
      using enclosing_tokens<enc_brackets>::enclosing_tokens;
    };

    struct enclosing_tabs : enclosing_tokens<enc_tabs>
    {
      using enclosing_tokens<enc_tabs>::enclosing_tokens;
    };
  } // namespace

  parser::parser(clang::SourceManager &SM, clang::DiagnosticsEngine &Diags, file const& f)
    : lex(SM, Diags, f), Diags(Diags)
  {
    la = lex();
  }

  token parser::expect(token_kind k)
  {
    if (next_token_is(k))
      return consume();

    char const* exp = spelling(k);
    char const* got = spelling(lookahead());
    Diags.Report(input_location(), clang::diag::err_empty_enum);
    // error(input_location(), "expected '{}' but got '{}'", exp, got);
    return {};
  }

  // file:
  //    array?
  Syntax *parser::parse_file()
  {
    // trace_parser();

    if (!eof())
      return parse_array();
    return nullptr;
  }

  static
  void append(std::vector<Syntax *>& vec, Syntax *s)
  {
    if (s && s != Syntax::error)
      vec.push_back(s);
  }

  static
  Syntax **create(std::vector<Syntax *> const& vec)
  {
    Syntax **Array = new Syntax *[vec.size()];
    std::copy(vec.begin(), vec.end(), Array);
    return Array;
  }

  static
  Syntax **create(std::initializer_list<Syntax *> list)
  {
    Syntax **Array = new Syntax *[list.size()];
    std::copy(list.begin(), list.end(), Array);
    return Array;
  }

  auto is_sequencer = [](token_kind k) -> bool
  {
    return k == tok_separator || k == tok_semicolon;
  };

  // array:
  //    list
  //    array sequencer list
  //
  // sequencer:
  //    ;
  //    separator
  //
  // Note that the original expression allows arrays to be empty. I think
  // it's better to make that optionality part of the lower-precedence rule
  // that contains this one.
  //
  // FIXME: It seems like this could be refactored into a more C++-like
  // statement sequence where each statement is terminated by a separator
  // or semi-colon. The original specification is:
  //
  //    list  = lookahead(';') | expr {',' scan expr}
  //    array = push lineprefix='' scan [list {(';'|ending) scan list}] pop
  //
  // The fact that ';' is allowed as part of a list implies the
  // existence of an empty statement -- probably to support things like this:
  //
  //    if (expr)
  //      ;
  //
  //
  // and this:
  //
  //    ;;;
  //
  // In this case, we have a list (';') followed by a sequencer (';') followed
  // by another list ';'.
  //
  // But it's not entirely obvious if that's true. Note that this also allows
  // some weirdness like ';;;;<newline>'.
  //
  // I think it would be better to define an explicit stmt-level construct.
  //
  //    stmts = {stmt}
  //    stmt = [expr] (';'|ending)
  //
  // Or something like that.
  Syntax *parser::parse_array()
  {
    // trace_parser();

    std::vector<Syntax *> lists;

    append(lists, parse_list());
    while (match_if(is_sequencer))
    {
      // Handle a newline before the end of file.
      if (eof())
        break;
      append(lists, parse_list());
    }

    return on_array(lists);
  }

  // list:
  //    ;
  //    expr
  //    list , expr
  //
  // Note that a ';' is interpreted as an empty list, so we return nullptr,
  // in that case.
  Syntax *parser::parse_list()
  {
    // trace_parser();

    if (match(tok_semicolon))
      return nullptr;

    std::vector<Syntax *> exprs;
    append(exprs, parse_expr());
    while (match(tok_comma))
      append(exprs, parse_expr());

    return on_list(exprs);
  }

  // expr:
  //    def
  //    def => block
  //    def => expr
  //    def where-clause
  //
  // where-clause:
  //    block
  //    def-list
  //
  // def-list:
  //    def
  //    def-list , def
  Syntax *parser::parse_expr()
  {
    // trace_parser();

    if (next_token_is(tok_left_bracket))
    {
      // FIXME: Match the 'pre-attr expr' production.
      llvm_unreachable("attributed expressions not supported");
    }

    Syntax *def = parse_def();

    if (token op = match(tok_equal_greater))
    {
      // Note that '=>' is the mapping operator (e.g., a => 5). I'm not at
      // all sure what this means semantically.
      //
      // Note that this is right associative.
      //
      // TODO: Why is this a sequence of "trailers" on definitions. We end
      // up allowing things like: 'x => e1 => e2 => { ... }'.
      Syntax *val;
      if (next_token_is(tok_left_brace))
        val = parse_braced_array();
      else if (next_token_is(tok_indent))
        val = parse_nested_array();
      else
        val = parse_expr();

      // FIXME: Skip to the end of the list or stmt/line.
      if (val == Syntax::error)
        return Syntax::error;

      return on_binary(op, def, val);
    }

    if (match("where"))
    {
      // FIXME: Try to match this:
      //
      //    scankey 'where' word (block | def {',' scan def} !',')}
      //
      // I'm not sure 'word' is supposed to match. Also note that there's
      // a potential ambiguity depending on how we interpret negation in
      // the grammar.
      //
      // Nominally, we want to support expressions of the form:
      //
      //    a where b, c, d
      //
      // and presumably b is some kind of boolean expression. Of course,
      // the grammar being recursive allows things like:
      //
      //    a where b where c where d
      //
      // which may or may not be equivalent to the above? Also, what happens
      // if any of the conditions are false?
      llvm_unreachable("where clauses not supported");
    }

    // FIXME: Support trailing docattrs.

    return def;
  }

  auto is_assignment_operator = [](token_kind k) -> bool
  {
    switch (k)
    {
    default:
      return false;
    case tok_equal:
    case tok_colon_equal:
    case tok_plus_equal:
    case tok_minus_equal:
    case tok_star_equal:
    case tok_slash_equal:
      return true;
    }
  };

  // def:
  //    or assignment-operator def
  //    or ! braced-array
  //    or ! nested-array
  //
  // The original specification was:
  //
  //    def = or { ('='|':='|'+='|'-='|'*='|'/='  ) scan def | ... }
  //
  // Where the ... is the ! production for function definitions. I'm not
  // sure the repetition is desirable here because the it's already right
  // recursive. If you have;
  //
  //    x = y = z
  //
  // that will naturally parse as
  //
  //    x = (y = z)
  //
  // And the repetition never matches. Note that this is likely true for all
  // uses of right recursion in the grammar.
  Syntax *parser::parse_def()
  {
    // trace_parser();

    Syntax *def = parse_or();

    if (token op = match_if(is_assignment_operator))
    {
      Syntax *val = parse_def();
      return on_binary(op, def, val);
    }

    // FIXME: Is the only way to define a function to follow the declarator
    // with a '!'? It seems like that would work better as a suffix operator
    // on the declarator (it also leads naturally to factorials!).
    if (token op = match(tok_bang))
    {
      // FIXME: This should probably not be inside the loop. It allows
      // weirdness like this: 'f ! { ...} ! { ... } ! ...'. This would also
      // be interspersed with assignments: 'f ! { ... } = expr'
      Syntax *body;
      if (next_token_is(tok_left_brace))
      {
        body = parse_braced_array();
      }
      else if (next_token_is(tok_indent))
      {
        body = parse_nested_array();
      }
      else
      {
        // FIXME: Skip to the end of the list or stmt/line.
        Diags.Report(input_location(), clang::diag::err_empty_enum);
        // error(input_location(), "expected '{{' or indent");
        return Syntax::error;
      }

      return on_binary(op, def, body);
    }

    return def;
  }

  auto is_or_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_bar_bar) || p.next_token_is("or");
  };

  // or:
  //    and
  //    or or-operator and
  //
  // or-operator:
  //    ||
  //    "or"
  Syntax *parser::parse_or()
  {
    // trace_parser();

    Syntax *e1 = parse_and();
    while (token op = match_if(is_or_operator, *this))
    {
      Syntax *e2 = parse_and();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  auto is_and_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_ampersand_ampersand) || p.next_token_is("and");
  };

  // and:
  //    cmp
  //    and and-operator cmp
  //
  // and-operator:
  //    &&
  //    "and"
  Syntax *parser::parse_and()
  {
    // trace_parser();

    Syntax *e1 = parse_cmp();
    while (token op = match_if(is_and_operator, *this))
    {
      Syntax *e2 = parse_cmp();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  auto is_logical_unary_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_ampersand)
        || p.next_token_is(tok_dot_dot)
        || p.next_token_is(tok_bang)
        || p.next_token_is("not");
  };

  auto is_relational_operator = [](parser& p) -> bool
  {
    switch (p.lookahead())
    {
    default:
      return false;
    case tok_equal_equal:
    case tok_bang_equal:
    case tok_less_greater:
    case tok_less:
    case tok_greater:
    case tok_less_equal:
    case tok_greater_equal:
      return true;
    }
  };

  /// cmp:
  ///    to
  ///    cmp relational-operator to
  ///    unary-operator cmp
  Syntax *parser::parse_cmp()
  {
    // trace_parser();

    if (token op = match_if(is_logical_unary_operator, *this))
    {
      Syntax *e1 = parse_cmp();
      return on_unary(op, e1);
    }

    Syntax *e1 = parse_to();
    while (token op = match_if(is_relational_operator, *this))
    {
      Syntax *e2 = parse_to();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  auto is_to_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_colon)
        || p.next_token_is(tok_dot_dot)
        || p.next_token_is(tok_minus_greater);
  };

  // to:
  //    add
  //    to to-operator add
  //
  // to-operator:
  //    :
  //    ..
  //    ->
  //
  // TODO: -> is at the wrong level of precedence and has the wrong
  // associativity. Also, what's the behavior.
  Syntax *parser::parse_to()
  {
    // trace_parser();

    Syntax *e1 = parse_add();
    while (token op = match_if(is_to_operator, *this))
    {
      Syntax *e2 = parse_add();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  auto is_add_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_plus) || p.next_token_is(tok_minus);
  };

  /// add:
  ///   mul
  ///   add add-operator mul
  ///
  /// add-operator:
  ///   +
  ///   -
  Syntax *parser::parse_add()
  {
    // trace_parser();

    Syntax *e1 = parse_mul();
    while (token op = match_if(is_add_operator, *this))
    {
      Syntax *e2 = parse_mul();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  auto is_mul_operator = [](parser& p) -> bool
  {
    return p.next_token_is(tok_star) || p.next_token_is(tok_slash);
  };

  /// mul:
  ///   call
  ///   mul mul-operator call
  Syntax *parser::parse_mul()
  {
    // trace_parser();

    Syntax *e1 = parse_macro();
    while (token op = match_if(is_mul_operator, *this))
    {
      Syntax *e2 = parse_macro();
      e1 = on_binary(op, e1, e2);
    }

    return e1;
  }

  /// macro:
  ///   post
  ///   post block
  ///   if ( list ) block
  ///   if ( list ) block else block
  ///   while ( list ) block
  ///   for ( list ) block
  ///   block
  ///
  /// catch:
  ///   catch ( list ) block
  ///
  /// \todo We can parse a macro as a list of (post block) chains if we
  /// had an continuation token (e.g., 'else').
  ///
  /// \todo Implement catch blocks. We should probably only allow these
  /// on the 'block' production, and possibly explicitly allow a 'try'
  /// statement. Note that catch-blocks are really a sequence of catches,
  /// possibly followed by a finally.
  ///
  /// \note Macros are essentially right associative in structure, so
  /// that a naive post-order traversal will not produce the correct typing
  /// or evaluation. They must be traversed pre-order (i.e., analyze the
  /// outermost properties, followed by the innermost).
  Syntax *parser::parse_macro()
  {
    // trace_parser();

    if (next_token_is("if"))
      return parse_if();

    if (next_token_is("while"))
      return parse_while();

    if (next_token_is("for"))
      return parse_for();

    if (next_token_is(tok_left_brace) || next_token_is(tok_colon))
      return parse_block();

    Syntax *e1 = parse_pre();

    // TODO: Support a continued chain of macros.
    if (next_token_is(tok_left_brace))
    {
      Syntax *e2 = parse_block();
      return on_macro(e1, e2);
    } else if (next_token_is(tok_colon)) {
      // We need to make sure that we have a sequence of newlines followed
      // by a sequence of indents in order to parse a nested block.
      unsigned la = 1;
      while(lex.char_lookahead(la) == '\n')
        ++la;

      if (la > 1) {
        // Record the lookahead where we see the first indent.
        unsigned indent_la = la;
        while (lex.char_lookahead(la) == '\t' || lex.char_lookahead(la) == ' ')
          ++la;

        // if we had both a newline and an indent, we're good to parse a block.
        if (la > indent_la) {
          Syntax *e2 = parse_block();
          return on_macro(e1, e2);
        }

        assert(false && "Parsing block without indent.");
      }
    }

    return e1;
  }

  Syntax *parser::parse_if()
  {
    // trace_parser();

    token if_tok = expect("if");
    Syntax *cond = parse_paren();

    // FIXME: Allow an optional 'then' keyword?
    Syntax *then_block = parse_block();

    Syntax *else_macro;
    if (token else_tok = match("else"))
    {
      Syntax *else_block = parse_block();
      else_macro = on_else(else_tok, else_block);
    }
    else
    {
      else_macro = nullptr;
    }

    return on_if(if_tok, cond, then_block, else_macro);
  }

  Syntax *parser::parse_while()
  {
    // trace_parser();

    token tok = expect("while");
    Syntax *cond = parse_paren();

    // FIXME: Allow an optional 'do' keyword?
    Syntax *block = parse_block();

    return on_loop(tok, cond, block);
  }

  Syntax *parser::parse_for()
  {
    // trace_parser();

    token tok = expect("for");
    Syntax *cond = parse_paren();

    // FIXME: Allow an optional 'do' keyword?
    Syntax *block = parse_block();

    return on_loop(tok, cond, block);
  }

  auto is_unary_operator = [](token_kind k) -> bool
  {
    switch (k)
    {
    case tok_question:
    case tok_caret:
    case tok_plus:
    case tok_minus:
    case tok_star:
      return true;
    default:
      return false;
    }
  };

  Syntax *parser::parse_pre()
  {
    // trace_parser();

    if (token op = match_if(is_unary_operator))
    {
      Syntax *e = parse_pre();
      return on_unary(op, e);
    }

    return parse_post();
  }

  /// postfix:
  ///   base
  ///   postfix ( list )
  ///   postfix [ list ]
  ///   postfix < list >
  ///   postfix . identifier
  ///   postfix suffix-operator
  ///
  /// suffix-operator:
  ///   ?
  ///   ^
  ///   @
  Syntax *parser::parse_post()
  {
    // trace_parser();

    Syntax *e = parse_primary();
    while (true)
    {
      switch (lookahead())
      {
      case tok_left_paren:
        e = parse_call(e);
        break;

      case tok_left_bracket:
        e = parse_elem(e);
        break;

      case tok_less:
        llvm_unreachable("attributes not implemented");
        break;

      case tok_dot:
        e = parse_dot(e);
        break;

      case tok_question:
      case tok_caret:
      case tok_at:
        llvm_unreachable("suffix operators not implemented");
        consume();
        break;

      default:
        return e;
      }
    }

    // We should never reach this point.
    assert(false);
    return nullptr;
  }

  Syntax *parser::parse_call(Syntax *fn)
  {
    // trace_parser();

    enclosing_parens parens(*this);
    if (!parens.expect_open())
      return Syntax::error;

    // TODO: If this is an Syntax::error, should we skip to the next paren or
    // to the the nearest comma? separator? What?
    Syntax *args = nullptr;

    // Begin parsing function parameters.
    ParsingParams = true;

    // Don't parse an array if the parens are empty.
    if (lookahead() != tok_right_paren)
      args = parse_array();
    else
      args = on_array({});

    // Finish parsing function parameters.
    ParsingParams = false;

    if (!parens.expect_close())
      return Syntax::error;

    return on_call({parens.open, parens.close}, fn, args);
  }

  Syntax *parser::parse_elem(Syntax *map)
  {
    // trace_parser();

    enclosing_brackets brackets(*this);
    if (!brackets.expect_open())
      return Syntax::error;

    Syntax *sel = parse_array();

    if (!brackets.expect_close())
      return Syntax::error;

    return on_elem({brackets.open, brackets.close}, map, sel);
  }

  Syntax *parser::parse_dot(Syntax *obj)
  {
    token op = expect(tok_dot);

    // FIXME: This is somehow a qualified-id, except that I don't know
    // what that means.
    Syntax *sub = parse_id();

    return on_binary(op, obj, sub);
  }

  Syntax *parser::parse_primary()
  {
    // trace_parser();

    switch (lookahead())
    {
    case tok_identifier:
      return parse_id();

    case tok_binary_integer:
    case tok_decimal_integer:
    case tok_hexadecimal_integer:
    case tok_decimal_float:
    case tok_hexadecimal_float:
    case tok_character:
    case tok_string:
      return on_atom(consume());

    case tok_left_paren:
      return parse_paren();

    default:
      break;
    }

    Diags.Report(input_location(), clang::diag::err_empty_enum);
    // error(input_location(), "expected primary-expression");
    return Syntax::error;
  }

  /// id:
  ///   identifier
  Syntax *parser::parse_id()
  {
    // trace_parser();

    token id = expect(tok_identifier);
    if (!id)
      return Syntax::error;
    return on_atom(id);
  }

  Syntax *parser::parse_paren()
  {
    // trace_parser();

    enclosing_parens parens(*this);
    if (!parens.expect_open())
      return Syntax::error;

    // TODO: If this is an Syntax::error, should we skip to the next paren or
    // to the the nearest comma? separator? What?
    Syntax *seq = parse_array();

    if (!parens.expect_close())
      return Syntax::error;

    return seq;
  }

  // braced-array:
  //    { array }
  Syntax *parser::parse_braced_array()
  {
    // trace_parser();

    enclosing_braces braces(*this);
    if (!braces.expect_open())
      return Syntax::error;

    // FIXME: How do we recover from errors?
    Syntax *ret = parse_array();

    if (!braces.expect_close())
      return Syntax::error;

    return ret;
  }

  // nested-array:
  //    indent array dedent
  Syntax *parser::parse_nested_array()
  {
    // trace_parser();

    enclosing_tabs tabs(*this);
    if (!tabs.expect_open())
      return Syntax::error;

    Syntax *ret = parse_array();

    if (!tabs.expect_close())
      return Syntax::error;

    return ret;
  }

  /// block:
  ///   braced-array
  ///   : nested-array
  Syntax *parser::parse_block()
  {
    // trace_parser();

    if (next_token_is(tok_left_brace))
      return parse_braced_array();

    expect(tok_colon);
    return parse_nested_array();
  }

  // Semantic actions

  // Returns the identifier 'operator\'<op>\''.
  static
  Syntax *make_operator(clang::SourceLocation loc, char const* op)
  {
    // FIXME: Make this a fused operator?
    std::string name = "operator'" + std::string(op) + "'";
    symbol sym = get_symbol(name);
    token tok(tok_identifier, loc, sym);
    return new AtomSyntax(tok, clang::SourceLocation());
  }

  static
  Syntax *make_operator(token const& tok)
  {
    return make_operator(tok.loc, tok.spelling());
  }

  static
  Syntax *make_list(std::initializer_list<Syntax *> list)
  {
    assert(std::none_of(list.begin(), list.end(), [](Syntax *s) { return !s; }));
    return new ListSyntax(create(list), list.size(), clang::SourceLocation());
  }

  static
  Syntax *make_call(token const& tok)
  {
    return new CallSyntax(make_operator(tok), make_list({}), clang::SourceLocation());
  }

  static
  Syntax *make_call(token const& tok, Syntax *args)
  {
    return new CallSyntax(make_operator(tok), args, clang::SourceLocation());
  }

  Syntax *parser::on_atom(token const& tok)
  {
    return new AtomSyntax(tok, clang::SourceLocation(), ParsingParams);
  }

  Syntax *parser::on_array(std::vector<Syntax *> const& vec)
  {
    return new ArraySyntax(create(vec), vec.size(), clang::SourceLocation());
  }

  Syntax *parser::on_list(std::vector<Syntax *> const& vec)
  {
    return new ListSyntax(create(vec), vec.size(), clang::SourceLocation());
  }

  Syntax *parser::on_binary(token const& tok, Syntax *e1, Syntax *e2)
  {
    return new CallSyntax(make_operator(tok), make_list({e1, e2}), clang::SourceLocation());
  }

  Syntax *parser::on_unary(token const& tok, Syntax *e1)
  {
    return new CallSyntax(make_operator(tok), make_list({e1}), clang::SourceLocation());
  }

  Syntax *parser::on_call(token_pair const& tok, Syntax *e1, Syntax *e2)
  {
    return new CallSyntax(e1, e2, clang::SourceLocation(), ParsingParams);
  }

  Syntax *parser::on_elem(token_pair const& tok, Syntax *e1, Syntax *e2)
  {
    return new ElemSyntax(e1, e2, clang::SourceLocation());
  }

  Syntax *parser::on_macro(Syntax *e1, Syntax *e2)
  {
    return new MacroSyntax(e1, e2, nullptr, clang::SourceLocation());
  }

  Syntax *parser::on_else(token const& tok, Syntax *e1)
  {
    return new MacroSyntax(make_call(tok), e1, nullptr, clang::SourceLocation());
  }

  Syntax *parser::on_if(token const& tok, Syntax *e1, Syntax *e2, Syntax *e3)
  {
    return new MacroSyntax(make_call(tok, e1), e2, e3, clang::SourceLocation());
  }

  Syntax *parser::on_loop(token const& tok, Syntax *e1, Syntax *e2)
  {
    return new MacroSyntax(make_call(tok, e1), e2, nullptr, clang::SourceLocation());
  }

} // namespace green
