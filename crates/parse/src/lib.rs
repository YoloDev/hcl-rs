//! The HCL parser.
//!
//! The parser doesn't know about concrete representation of tokens and syntax
//! trees. Abstract [`TokenSource`] and [`TreeSink`] traits are used instead.
//! As a consequence, this crate does not contain a lexer.
//!
//! The [`Parser`] struct from the [`parser`] module is a cursor into the
//! sequence of tokens. Parsing routines use [`Parser`] to inspect current
//! state and advance the parsing.
//!
//! The actual parsing happens in the [`grammar`] module.
//!
//! Tests for this crate live in the `syntax` crate.
//!
//! [`Parser`]: crate::parser::Parser

#[macro_use]
mod syntax_kind;
mod error;
mod event;
mod grammar;
mod parser;
mod token_set;

pub use error::ParseError;
pub use syntax_kind::SyntaxKind;
pub(crate) use token_set::TokenSet;

/// `TokenSource` abstracts the source of the tokens parser operates on.
///
/// Hopefully this will allow us to treat text and token trees in the same way!
pub trait TokenSource {
  fn current(&self) -> Token;

  /// Lookahead n token
  fn lookahead_nth(&self, n: usize) -> Token;

  /// bump cursor to next token
  fn bump(&mut self);

  /// Is the current token a specified keyword?
  fn is_keyword(&self, kw: &str) -> bool;
}

/// `Token` abstracts the cursor of `TokenSource` operates on.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Token {
  /// What is the current token?
  pub kind: SyntaxKind,
}

/// `TreeSink` abstracts details of a particular syntax tree implementation.
pub trait TreeSink {
  /// Adds new token to the current branch.
  fn token(&mut self, kind: SyntaxKind);

  /// Start new branch and make it current.
  fn start_node(&mut self, kind: SyntaxKind);

  /// Finish current branch and restore previous
  /// branch as current.
  fn finish_node(&mut self);

  fn error(&mut self, error: ParseError);
}

/// hcl parser allows you to choose one of the possible entry points.
pub enum ParserEntryPoint {
  SourceFile,
  Template,
  IdentOnly,
}

/// Parse given tokens into the given sink as a hcl file.
pub fn parse_source_file(token_source: &mut dyn TokenSource, tree_sink: &mut dyn TreeSink) {
  parse(token_source, tree_sink, ParserEntryPoint::SourceFile);
}

pub fn parse(
  token_source: &mut dyn TokenSource,
  tree_sink: &mut dyn TreeSink,
  entry_point: ParserEntryPoint,
) {
  let entry_point: fn(&'_ mut parser::Parser) = match entry_point {
    ParserEntryPoint::SourceFile => todo!(),
    ParserEntryPoint::Template => todo!(),
    ParserEntryPoint::IdentOnly => todo!(),
  };

  let mut p = parser::Parser::new(token_source);
  entry_point(&mut p);
  let events = p.finish();
  event::process(tree_sink, events);
}
