mod helpers;
mod scanner;
mod token;

#[cfg(feature = "bytes")]
use bytes::{Buf, BytesMut};
use helpers::VecDequeIterator;
pub use token::*;

pub struct Lexer(scanner::Lexer);

impl Lexer {
  pub fn new() -> Self {
    Self(scanner::Lexer::new())
  }

  pub fn new_template() -> Self {
    Self(scanner::Lexer::new_template())
  }

  pub fn new_ident_only() -> Self {
    Self(scanner::Lexer::new_ident_only())
  }

  pub fn lex_block<'a>(
    &'a mut self,
    data: &[u8],
    is_final_block: bool,
  ) -> (usize, impl Iterator<Item = Token> + 'a) {
    let consumed = self.0.scan_block(data, is_final_block);
    let tokens = VecDequeIterator::new(&mut self.0.tokens);
    (consumed, tokens)
  }

  pub fn lex_all(mut self, data: &[u8]) -> impl Iterator<Item = Token> {
    let consumed = self.0.scan_block(data, true);
    debug_assert_eq!(consumed, data.len());

    self.0.tokens.into_iter()
  }
}

#[cfg(feature = "bytes")]
impl Lexer {
  fn lex_bytes_inner<'a>(
    &'a mut self,
    buf: &mut BytesMut,
    is_final_block: bool,
  ) -> impl Iterator<Item = Token> + 'a {
    let consumed = self.0.scan_block(buf, is_final_block);
    buf.advance(consumed);
    VecDequeIterator::new(&mut self.0.tokens)
  }

  pub fn lex_bytes<'a>(&'a mut self, buf: &mut BytesMut) -> impl Iterator<Item = Token> + 'a {
    self.lex_bytes_inner(buf, false)
  }

  pub fn lex_bytes_eof<'a>(&'a mut self, buf: &mut BytesMut) -> impl Iterator<Item = Token> + 'a {
    self.lex_bytes_inner(buf, true)
  }
}

impl Default for Lexer {
  fn default() -> Self {
    Self::new()
  }
}
