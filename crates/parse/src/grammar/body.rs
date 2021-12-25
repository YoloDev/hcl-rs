use crate::{
  parser::{Marker, ParserScope},
  SyntaxKind::*,
};
use std::borrow::Cow;

pub(crate) fn body(p: &mut ParserScope, stop_on_r_curly: bool) {
  while !(stop_on_r_curly && p.at(T!['}']) || p.at(EOF)) {
    // let next = p.current();
    match p.current() {
      NEWLINE => {
        p.eat(NEWLINE);
      }
      IDENTIFIER => {
        body_item(p);
      }
      t => {
        if !p.recovery() {
          if t == SYM_O_QUOTE {
            p.error(
              "Invalid argument name",
              "Argument names must not be quoted.",
            );
          } else {
            p.error(
              "Argument or block definition required",
              "An argument or block definition is required here.",
            );
          }
        }

        if stop_on_r_curly {
          p.recover(T!['}']);
        } else {
          recover_after_body_item(p);
        }
      }
    }
  }
}

pub(crate) fn body_item(p: &mut ParserScope) {
  let m = p.start();
  if !p.eat(IDENTIFIER) {
    m.abandon(p);
    p.error(
      "Argument or block definition required",
      "An argument or block definition is required here.",
    );

    recover_after_body_item(p);
    return;
  }

  match p.current() {
    T![=] => finish_body_attribute(p, m, false),
    SYM_O_QUOTE | T!['{'] | IDENTIFIER => finish_body_block(p, m),
    _ => {
      m.abandon(p);
      p.error("Argument or block definition required", "An argument or block definition is required here. To set an argument, use the equals sign \"=\" to introduce the argument value.");

      recover_after_body_item(p);
    }
  }
}

// single_attr_body is a weird variant of body that deals with the
// body of a nested block containing only one attribute value all on a single
// line, like foo { bar = baz } . It expects to find a single attribute item
// immediately followed by the end token type with no intervening newlines.
pub(crate) fn single_attr_body(p: &mut ParserScope) {
  let m = p.start();
  if !p.eat(IDENTIFIER) {
    m.abandon(p);
    p.error(
      "Argument or block definition required",
      "An argument or block definition is required here.",
    );

    recover_after_body_item(p);
    return;
  }

  match p.current() {
    T![=] => finish_body_attribute(p, m, true),
    SYM_O_QUOTE | T!['{'] | IDENTIFIER => {
      m.abandon(p);
      p.error(
        "Argument definition required",
         "A single-line block definition can contain only a single argument. If you meant to define an argument, use an equals sign to assign it a value. To define a nested block, place it on a line of its own within its parent block.");
      recover_after_body_item(p);
    }
    _ => {
      p.error("Argument or block definition required", "An argument or block definition is required here. To set an argument, use the equals sign \"=\" to introduce the argument value.");
      recover_after_body_item(p);
    }
  }
}

fn finish_body_attribute(p: &mut ParserScope, m: Marker, single_line: bool) {
  if !p.eat(T![=]) {
    // should never happen if caller behaves
    unreachable!("finish_body_attribute called with next not equals");
  }

  let expr_result = expression(p);
  if p.recovery() && expr_result == ExpressionResult::Error {
    // recovery within expressions tends to be tricky, so we've probably
    // landed somewhere weird. We'll try to reset to the start of a body
    // item so parsing can continue.
    recover_after_body_item(p);
    return;
  } else if !single_line {
    let end = p.current();
    if end != NEWLINE && end != EOF {
      if !p.recovery() {
        let mut summary = Cow::Borrowed("Missing newline after argument");
        let mut detail = Cow::Borrowed("An argument definition must end with a newline.");

        if end == T![,] {
          summary = Cow::Borrowed("Unexpected comma after argument");
          detail = Cow::Owned({
            let prefix = "Argument definitions must be separated by newlines, not commas. ";
            let result = String::with_capacity(prefix.len() + detail.len());
            result.push_str(prefix);
            result.push_str(&*detail);
            result
          });
        }

        p.error(summary, detail);
      }

      m.abandon(p);
      recover_after_body_item(p);
    } else {
      m.complete(p, ITEM_ATTRIBUTE);
      p.eat(NEWLINE);
    }
  }
}

fn finish_body_block(p: &mut ParserScope, m: Marker) {
  let labels_marker = p.start();
  let body_marker = loop {
    match p.current() {
      T!['{'] => {
        labels_marker.complete(p, ITEM_BLOCK_LABELS);
        let body_marker = p.start();
        p.eat(T!['{']);
        break body_marker;
      }

      SYM_O_QUOTE => {
        quoted_string_literal(p);
        // quoted_string_literal recovers up to the closing quote
        // if it encounters problems, so we can continue looking for
        // more labels and eventually the block body even.
      }

      IDENTIFIER => {
        p.eat(IDENTIFIER);
      }

      _ => {
        match p.current() {
          T![=] => p.error("Invalid block definition", "The equals sign \"=\" indicates an argument definition, and must not be used when defining a block."),
          NEWLINE => p.error("Invalid block definition", "A block definition must have block content delimited by \"{\" and \"}\", starting on the same line as the block header."),
          _ if p.recovery() => p.error("Invalid block definition", "Either a quoted string block label or an opening brace (\"{\") is expected here."),
          _ => ()
        }

        labels_marker.abandon(p);
        m.abandon(p);
        recover_after_body_item(p);
        return;
      }
    }
  };

  // Once we fall out here, the peeker is pointed just after our opening
  // brace, so we can begin our nested body parsing.
  match p.current() {
    NEWLINE | EOF | T!['}'] => body(p, true),
    _ => {
      // Special one-line, single-attribute block parsing mode.
      single_attr_body(p);
      match p.current() {
        T!['}'] => {
          // the happy path - just consume the closing brace
          p.eat(T!['}']);
          body_marker.complete(p, ITEM_BLOCK_BODY);
        }

        T![,] => {
          // User seems to be trying to use the object-constructor
          // comma-separated style, which isn't permitted for blocks.
          p.error("Invalid single-argument block definition", "Single-line block syntax can include only one argument definition. To define multiple arguments, use the multi-line block syntax with one argument definition per line.");
          p.recover(T!['}']);
          body_marker.complete(p, ITEM_BLOCK_BODY);
        }

        NEWLINE => {
          // We don't allow weird mixtures of single and multi-line syntax.
          p.error("Invalid single-argument block definition", "An argument definition on the same line as its containing block creates a single-line block definition, which must also be closed on the same line. Place the block's closing brace immediately after the argument definition.");
          p.recover(T!['}']);
          body_marker.complete(p, ITEM_BLOCK_BODY);
        }

        _ => {
          // Some other weird thing is going on. Since we can't guess a likely
          // user intent for this one, we'll skip it if we're already in
          // recovery mode.
          if !p.recovery() {
            p.error("Invalid single-argument block definition", "A single-line block definition must end with a closing brace immediately after its single argument definition.");
          }
          p.recover(T!['}']);
          body_marker.complete(p, ITEM_BLOCK_BODY);
        }
      }
    }
  }

  if p.at(NEWLINE) || p.at(EOF) {
    p.eat(NEWLINE);
  } else {
    if !p.recovery() {
      p.error(
        "Missing newline after block definition",
        "A block definition must end with a newline.",
      );
    }
    recover_after_body_item(p);
  }

  m.complete(p, ITEM_BLOCK);
}

pub(crate) fn recover_after_body_item(p: &mut ParserScope) {
  todo!()
}
