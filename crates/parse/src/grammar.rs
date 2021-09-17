mod body;
mod expr;

use crate::{
  parser::{CompletedMarker, Parser},
  SyntaxKind::{self, *},
  TokenSet, TokenSource,
};

pub(crate) mod entry_points {
  use super::*;

  pub(crate) fn source_file(p: &mut Parser) {
    let m = p.start();
    // TODO: Eat BOM?
    body::body(p, false);
    m.complete(p, SOURCE_FILE);
  }
}
