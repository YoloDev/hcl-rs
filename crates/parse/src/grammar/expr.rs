use crate::{
  grammar::tmpl,
  parser::{CompletedMarker, Marker, Parser},
  token_set::TokenSet,
  SyntaxKind::{self, *},
};

/// Binary operator groups are listed in order of precedence, with
/// the *lowest* precedence first. Operators within the same group
/// have left-to-right associativity.
const BINARY_OPS: &[TokenSet] = &[
  TokenSet::new(&[T![||]]),
  TokenSet::new(&[T![&&]]),
  TokenSet::new(&[T![==], T![!=]]),
  TokenSet::new(&[T![>], T![>=], T![<], T![<=]]),
  TokenSet::new(&[T![+], T![-]]),
  TokenSet::new(&[T![*], T![/], T![%]]),
];

pub(super) fn expression(p: &mut Parser) -> Option<CompletedMarker> {
  ternary_conditional(p)
}

fn ternary_conditional(p: &mut Parser) -> Option<CompletedMarker> {
  // The ternary conditional operator (.. ? .. : ..) behaves somewhat
  // like a binary operator except that the "symbol" is itself
  // an expression enclosed in two punctuation characters.
  // The middle expression is parsed as if the ? and : symbols
  // were parentheses. The "rhs" (the "false expression") is then
  // treated right-associatively so it behaves similarly to the
  // middle in terms of precedence.

  let cond = binary_ops(p, BINARY_OPS)?;

  if !p.eat(T![?]) {
    return Some(cond);
  }

  let true_expr = expression(p)?;

  if !p.eat(T![:]) {
    p.error(
      "Missing false expression in conditional",
      "The conditional operator (...?...:...) requires a false expression, delimited by a colon.",
    );
    return None;
  }

  let false_expr = expression(p)?;
  let m = cond.precede(p);
  Some(m.complete(p, EXPR_TERNARY))
}

// binary_ops calls itself recursively to work through all of the
// operator precedence groups, and then eventually calls expression_term
// for each operand.
fn binary_ops(p: &mut Parser, ops: &[TokenSet]) -> Option<CompletedMarker> {
  if ops.is_empty() {
    // We've run out of operators, so now we'll just try to parse a term.
    return expression_with_traversals(p);
  }

  let this_level = ops[0];
  let remaining = &ops[1..];

  // Parse a term that might be the first operand of a binary
  // operation or it might just be a standalone term.
  // We won't know until we've parsed it and can look ahead
  // to see if there's an operator token for this level.
  let mut lhs = binary_ops(p, remaining)?;

  // We'll keep eating up operators until we run out, so that operators
  // with the same precedence will combine in a left-associative manner:
  // a+b+c => (a+b)+c, not a+(b+c)
  //
  // Should we later want to have right-associative operators, a way
  // to achieve that would be to call back up to expression here
  // instead of iteratively parsing only the remaining operators.
  loop {
    if !this_level.contains(p.current()) {
      break;
    }

    p.bump_any();
    let rhs = binary_ops(p, ops)?;
    let m = lhs.precede(p);
    lhs = m.complete(p, EXPR_BINARY_OP);
  }

  Some(lhs)
}

fn expression_with_traversals(p: &mut Parser) -> Option<CompletedMarker> {
  let term = expression_term(p)?;
  expression_traversals(p, term)
}

fn expression_traversals(p: &mut Parser, mut from: CompletedMarker) -> Option<CompletedMarker> {
  'traversal: loop {
    match p.current() {
      T![.] => {
        p.eat(T![.]);
        match p.current() {
          IDENTIFIER => {
            p.eat(IDENTIFIER);
            from = from.precede(p).complete(p, EXPR_RELATIVE_TRAVERSAL);
          }

          LIT_NUMBER => {
            // This is a weird form we inherited from HIL, allowing numbers
            // to be used as attributes as a weird way of writing [n].
            // This was never actually a first-class thing in HIL, but
            // HIL tolerated sequences like .0. in its variable names and
            // calling applications like Terraform exploited that to
            // introduce indexing syntax where none existed.
            p.eat(LIT_NUMBER);

            // This syntax is ambiguous if multiple indices are used in
            // succession, like foo.0.1.baz: that actually parses as
            // a fractional number 0.1. Since we're only supporting this
            // syntax for compatibility with legacy Terraform
            // configurations, and Terraform does not tend to have lists
            // of lists, we'll choose to reject that here with a helpful
            // error message, rather than failing later because the index
            // isn't a whole number.
            // TODO: deal with non-whole literals here somehow? different
            // literal tokens?

            from = from.precede(p).complete(p, EXPR_RELATIVE_TRAVERSAL);
          }

          T![*] => {
            // "Attribute-only" splat expression.
            // This is a kinda weird construct inherited from HIL, which
            // behaves a bit like a [*] splat except that it is only able
            // to do attribute traversals into each of its elements,
            // whereas foo[*] can support _any_ traversal.
            p.eat(T![*]);
            while p.eat(T![.]) {
              if p.eat(LIT_NUMBER) {
                // Continuing the "weird stuff inherited from HIL"
                // theme, we also allow numbers as attribute names
                // inside splats and interpret them as indexing
                // into a list, for expressions like:
                // foo.bar.*.baz.0.foo
                // TODO: deal with non-whole literals here somehow? different
                // literal tokens?
                from = from.precede(p).complete(p, EXPR_RELATIVE_TRAVERSAL);
                continue;
              }

              if p.at(IDENTIFIER) {
                if !p.recovery() {
                  if p.at(T![*]) {
                    p.error("Nested splat expression not allowed", "A splat expression (*) cannot be used inside another attribute-only splat expression.");
                  } else {
                    p.error(
                      "Invalid attribute name",
                      "An attribute name is required after a dot.",
                    );
                  }
                }

                p.set_recovery(true);
                continue 'traversal;
              }

              p.bump_any();
              from = from.precede(p).complete(p, EXPR_RELATIVE_TRAVERSAL);
            }

            from = from.precede(p).complete(p, EXPR_SPLAT);
          }

          _ => {
            p.error(
              "Invalid attribute name",
              "An attribute name is required after a dot.",
            );

            // This leaves the peeker in a bad place, so following items
            // will probably be misparsed until we hit something that
            // allows us to re-sync.
            //
            // We will probably need to do something better here eventually
            // in order to support autocomplete triggered by typing a
            // period.
            p.set_recovery(true);
          }
        }
      }

      T!['['] => {
        // Indexing of a collection.
        // This may or may not be a hcl.Traverser, depending on whether
        // the key value is something constant.

        p.eat(T!['[']);
        match p.current() {
          T![*] => {
            // This is a full splat expression, like foo[*], which consumes
            // the rest of the traversal steps after it using a recursive
            // call to this function.
            p.eat(T![*]);
            if !p.eat(T![']']) {
              p.error("Missing close bracket on splat index", "The star for a full splat operator must be immediately followed by a closing bracket (\"]\").");
              p.recover(T![']']);
            }

            // Splat expressions use a special "anonymous symbol"  as a
            // placeholder in an expression to be evaluated once for each
            // item in the source expression.
            // Now we'll recursively call this same function to eat any
            // remaining traversal steps against the anonymous symbol.
            from = expression_traversals(p, from)?;
          }

          _ => {
            // arbitrary newlines allowed in brackets
            p.push_include_newlines(false);
            let has_errors = expression(p).is_none();
            if p.recovery() && has_errors {
              p.recover(T![']']);
            } else if !p.eat(T![']']) {
              p.bump_any();
              p.error(
                "Missing close bracket on index",
                "The index operator must end with a closing bracket (\"]\").",
              );
              p.recover(T![']']);
            }
            p.pop_include_newlines();

            from = from.precede(p).complete(p, EXPR_RELATIVE_TRAVERSAL);
          }
        }
      }

      _ => break Some(from),
    }
  }
}

pub(crate) fn expression_term(p: &mut Parser) -> Option<CompletedMarker> {
  let m = p.start();

  match p.current() {
    T!['('] => {
      p.eat(T!['(']);

      p.push_include_newlines(false);
      if let None = expression(p) {
        // attempt to place the peeker after our closing paren
        // before we return, so that the next parser has some
        // chance of finding a valid expression.
        p.recover(T![')']);
        p.pop_include_newlines();
        return None;
      }

      if !p.eat(T![')']) {
        p.error(
          "Unbalanced parentheses",
          "Expected a closing parenthesis to terminate the expression.",
        );
        p.set_recovery(true);

        p.bump_any();
      }

      p.pop_include_newlines();
      Some(m.complete(p, EXPR_PAREN))
    }

    LIT_NUMBER => {
      p.eat(LIT_NUMBER);
      Some(m.complete(p, EXPR_LITERAL))
    }

    IDENTIFIER => {
      if p.nth_at(1, T!['(']) {
        p.eat(IDENTIFIER);
        return finish_function_call(p, m);
      }

      if p.at_contextual_kw("true") {
        p.bump_remap(LIT_TRUE);
        Some(m.complete(p, EXPR_LITERAL))
      } else if p.at_contextual_kw("false") {
        p.bump_remap(LIT_FALSE);
        Some(m.complete(p, EXPR_LITERAL))
      } else if p.at_contextual_kw("null") {
        p.bump_remap(LIT_NULL);
        Some(m.complete(p, EXPR_LITERAL))
      } else {
        p.eat(IDENTIFIER);
        Some(m.complete(p, EXPR_VARIABLE))
      }
    }

    SYM_O_QUOTE | SYM_O_HEREDOC | SYM_O_HEREDOC_INDENTED => {
      let closer = match p.current() {
        SYM_O_QUOTE => {
          p.eat(SYM_O_QUOTE);
          SYM_C_QUOTE
        }

        SYM_O_HEREDOC => {
          p.eat(SYM_O_HEREDOC);
          SYM_C_HEREDOC
        }

        SYM_O_HEREDOC_INDENTED => {
          p.eat(SYM_O_HEREDOC_INDENTED);
          SYM_C_HEREDOC
        }

        _ => unreachable!(),
      };

      tmpl::template_inner(p, closer);
      Some(m.complete(p, EXPR_TEMPLATE))
    }

    T![-] => {
      p.eat(T![-]);

      // Important to use expression_with_traversals rather than expression
      // here, otherwise we can capture a following binary expression into
      // our negation.
      // e.g. -46+5 should parse as (-46)+5, not -(46+5)
      expression_with_traversals(p);
      Some(m.complete(p, EXPR_UNARY_OP))
    }

    T![!] => {
      p.eat(T![!]);

      // Important to use expression_with_traversals rather than expression
      // here, otherwise we can capture a following binary expression into
      // our negation.
      expression_with_traversals(p);
      Some(m.complete(p, EXPR_UNARY_OP))
    }

    T!['['] => tuple_cons(p, m),
    T!['{'] => object_cons(p, m),

    _ => {
      if !p.recovery() {
        p.error(
          "Invalid expression",
          "Expected the start of an expression, but found an invalid expression token.",
        );
      }
      p.set_recovery(true);

      // Return a placeholder so that the AST is still structurally sound
      // even in the presence of parse errors.
      Some(m.complete(p, EXPR_LITERAL))
    }
  }
}

// finish_function_call parses a function call assuming that the function
// name was already read, and so the peeker should be pointing at the opening
// parenthesis after the name.
fn finish_function_call(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
  p.bump(T!['(']);

  // Arbitrary newlines are allowed inside the function call parentheses.
  p.push_include_newlines(false);

  'token: loop {
    if p.eat(T![')']) {
      break 'token;
    }

    let expr = expression(p);
    if p.recovery() && expr.is_none() {
      // if there was a parse error in the argument then we've
      // probably been left in a weird place in the token stream,
      // so we'll bail out with a partial argument list.
      p.recover(T![')']);
      break 'token;
    }

    if p.eat(T![...]) {
      if !p.eat(T![')']) {
        if !p.recovery() {
          p.error("Missing closing parenthesis", "An expanded function argument (with ...) must be immediately followed by closing parentheses.");
        }

        p.recover(T![')']);
      }

      break 'token;
    }

    if p.eat(T![')']) {
      break 'token;
    }

    if !p.eat(T![,]) {
      p.error(
        "Missing argument separator",
        "A comma is required to separate each function argument from the next.",
      );
      p.recover(T![')']);
      break 'token;
    }
  }

  p.pop_include_newlines();
  Some(m.complete(p, EXPR_CALL))
}

fn tuple_cons(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
  p.bump(T!['[']);

  p.push_include_newlines(false);

  if p.at_contextual_kw("for") {
    let ret = finish_for_expr(p, m, ForExpression::Tuple);
    p.pop_include_newlines();
    return ret;
  }

  loop {
    if p.eat(T![']']) {
      break;
    }

    let e = expression(p);
    if p.recovery() && e.is_none() {
      // If expression parsing failed then we are probably in a strange
      // place in the token stream, so we'll bail out and try to reset
      // to after our closing bracket to allow parsing to continue.
      p.recover(T![']']);
      break;
    }

    if p.eat(T![']']) {
      break;
    }

    if !p.eat(T![,]) {
      p.error(
        "Missing argument separator",
        "A comma is required to separate each function argument from the next.",
      );
      p.recover(T![']']);
      break;
    }
  }

  p.pop_include_newlines();
  Some(m.complete(p, EXPR_TUPLE_LITERAL))
}

fn object_cons(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
  p.bump(T!['{']);

  // We must temporarily stop looking at newlines here while we check for
  // a "for" keyword, since for expressions are _not_ newline-sensitive,
  // even though object constructors are.
  p.push_include_newlines(false);
  let is_for = p.at_contextual_kw("for");
  p.pop_include_newlines();

  if is_for {
    return finish_for_expr(p, m, ForExpression::Object);
  }
}

enum ForExpression {
  Tuple,
  Object,
}
