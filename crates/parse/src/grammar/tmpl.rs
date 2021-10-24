use crate::{
  grammar::expr,
  parser::{CompletedMarker, Marker, Parser},
  token_set::TokenSet,
  SyntaxKind::{self, *},
};

pub(super) fn template_inner(p: &mut Parser, end_syntax: SyntaxKind) {
  template_parts(p, end_syntax);
}

fn template_parts(p: &mut Parser, end_syntax: SyntaxKind) {
  // let mut ltrim_next = false;
  // let mut next_can_trim_prev = false;

  loop {
    if p.eat(end_syntax) {
      // all done!
      break;
    }

    // let (ltrim, can_trim_prev) = (ltrim_next, next_can_trim_prev);
    // (ltrim_next, next_can_trim_prev) = (false, false);

    match p.current() {
      TMPL_LIT_STRING | TMPL_QUOT_STRING => {
        p.bump_any();
        // next_can_trim_prev = true;
      }

      TMPL_INTERPRET_START => {
        let m = p.start();
        p.bump(TMPL_INTERPRET_START);
        p.eat(TMPL_STRIP_MARKER);

        p.push_include_newlines(false);
        expr::expression(p);

        p.eat(TMPL_STRIP_MARKER);
        if !p.eat(TMPL_SEQUENCE_END) {
          if !p.recovery() {
            match p.current() {
              T![:] => {
                p.error("Extra characters after interpolation expression", "Template interpolation doesn't expect a colon at this location. Did you intend this to be a literal sequence to be processed as part of another language? If so, you can escape it by starting with \"$${\" instead of just \"${\".");
              }
              _ => {
                p.error("Extra characters after interpolation expression", "Expected a closing brace to end the interpolation expression, but found extra characters.\n\nThis can happen when you include interpolation syntax for another language, such as shell scripting, but forget to escape the interpolation start token. If this is an embedded sequence for another language, escape it by starting with \"$${\" instead of just \"${\".");
              }
            }
          }

          p.recover(TMPL_SEQUENCE_END);
        }

        m.complete(p, TMPL_INTERPRET);
        p.pop_include_newlines();
      }

      TMPL_CONTROL_START => {
        let m = p.start();
        p.bump(TMPL_CONTROL_START);
        p.eat(TMPL_STRIP_MARKER);

        p.push_include_newlines(false);
        if !p.at(IDENTIFIER) {
          if !p.recovery() {
            p.error("Invalid template directive", "A template directive keyword (\"if\", \"for\", etc) is expected at the beginning of a %{ sequence.")
          }

          p.recover(TMPL_SEQUENCE_END);
          p.pop_include_newlines();
          m.complete(p, TMPL_CONTROL);
          continue;
        }

        if p.at_contextual_kw("if") {
          p.bump_remap(KW_IF);
          expr::expression(p);
        } else if p.at_contextual_kw("else") {
          p.bump_remap(KW_ELSE);
        } else if p.at_contextual_kw("endif") {
          p.bump_remap(KW_ENDIF);
        } else if p.at_contextual_kw("for") {
          p.bump_remap(KW_FOR);

          if !p.eat(IDENTIFIER) {
            if !p.recovery() {
              p.error(
                "Invalid 'for' directive",
                "For directive requires variable name after 'for'.",
              );
            }

            p.recover(TMPL_SEQUENCE_END);
            p.pop_include_newlines();
            m.complete(p, TMPL_CONTROL);
            continue;
          }

          if p.eat(T![,]) {
            // What we just read was actually the key, then.

            if !p.eat(IDENTIFIER) {
              if !p.recovery() {
                p.error(
                  "Invalid 'for' directive",
                  "For directive requires value variable name after comma.",
                );
              }

              p.recover(TMPL_SEQUENCE_END);
              p.pop_include_newlines();
              m.complete(p, TMPL_CONTROL);
              continue;
            }
          }

          if !p.at_contextual_kw("in") {
            if !p.recovery() {
              p.error(
                "Invalid 'for' directive",
                "For directive requires 'in' keyword after names.",
              );
            }

            p.recover(TMPL_SEQUENCE_END);
            p.pop_include_newlines();
            m.complete(p, TMPL_CONTROL);
            continue;
          }

          p.bump_remap(KW_IN);
          expr::expression(p);
        } else if p.at_contextual_kw("endfor") {
          p.bump_remap(KW_ENDFOR);
        } else {
          if !p.recovery() {
            p.error(
              "Invalid template control keyword",
              "Valid control template keywords: if, for, else, endif, endfor",
            );
          }

          p.recover(TMPL_SEQUENCE_END);
          p.pop_include_newlines();
          m.complete(p, TMPL_CONTROL);
          continue;
        }

        p.eat(TMPL_STRIP_MARKER);
        if !p.eat(TMPL_SEQUENCE_END) {
          if !p.recovery() {
            match p.current() {
              T![:] => {
                p.error("Extra characters after interpolation expression", "Template interpolation doesn't expect a colon at this location. Did you intend this to be a literal sequence to be processed as part of another language? If so, you can escape it by starting with \"$${\" instead of just \"${\".");
              }
              _ => {
                p.error("Extra characters after interpolation expression", "Expected a closing brace to end the interpolation expression, but found extra characters.\n\nThis can happen when you include interpolation syntax for another language, such as shell scripting, but forget to escape the interpolation start token. If this is an embedded sequence for another language, escape it by starting with \"$${\" instead of just \"${\".");
              }
            }
          }

          p.recover(TMPL_SEQUENCE_END);
        }

        m.complete(p, TMPL_CONTROL);
        p.pop_include_newlines();
      }

      _ => {
        if !p.recovery() {
          p.error(
            "Unterminated template string",
            "No closing marker was found for the string.",
          );
        }

        p.recover(end_syntax);
        break;
      }
    }
  }
}
