use super::*;

fn assert_expected_tokens(actual: &[Token], expected: &[TokenKind]) {
  let mut actual = actual.iter();
  let mut expected = expected.iter();

  loop {
    match (actual.next(), expected.next()) {
      (None, None) => break,
      (Some(t), None) => panic!("Expected end of tokens, but received {:?}", t.kind()),
      (None, Some(t)) => panic!("Expected {:?}, but received end of tokens", t),
      (Some(l), Some(r)) => assert_eq!(l.kind(), *r),
    }
  }
}

fn test_scan_single_chunk(value: &'static [u8], expected: &[TokenKind]) -> Vec<Token> {
  let bytes = value;
  let mut lexer = Lexer::new();
  lexer.scan_block(bytes, true);
  let tokens = lexer.tokens.into_iter().collect::<Vec<_>>();

  assert_expected_tokens(&tokens, expected);
  tokens
}

fn test_scan_byte_by_byte(value: &'static [u8], expected: &[TokenKind]) -> Vec<Token> {
  let mut bytes = value;
  let mut lexer = Lexer::new();
  let mut tokens = Vec::new();
  let mut l = 0usize;
  while l < bytes.len() {
    let block = &bytes[..=l];
    let consumed = lexer.scan_block(block, false);
    if consumed > 0 {
      bytes = &bytes[consumed..];
      l = 0;
    } else {
      l += 1;
    }

    while let Some(tok) = lexer.tokens.pop_front() {
      tokens.push(tok);
    }
  }

  lexer.scan_block(&[], true);
  while let Some(tok) = lexer.tokens.pop_front() {
    tokens.push(tok);
  }

  assert_expected_tokens(&tokens, expected);
  tokens
}

fn test_scan(value: &'static [u8], expected: &[TokenKind]) {
  let single_chunk = test_scan_single_chunk(value, expected);
  let byte_by_byte = test_scan_byte_by_byte(value, expected);
  assert_eq!(single_chunk, byte_by_byte)
}

#[test]
fn empty_input() {
  test_scan(b"", &[TokenKind::TriviaEndOfFile])
}

#[test]
fn whitespace() {
  test_scan(
    b" ",
    &[TokenKind::TriviaWhitespace, TokenKind::TriviaEndOfFile],
  );
}

#[test]
fn newlines() {
  test_scan(
    b"\n\n",
    &[
      TokenKind::TriviaNewline,
      TokenKind::TriviaNewline,
      TokenKind::TriviaEndOfFile,
    ],
  )
}

#[test]
fn leading_byte_order_mark() {
  // Leading UTF-8 byte-order mark is ignored...
  test_scan(
    &[0xef, 0xbb, 0xbf],
    &[TokenKind::TriviaByteOrderMark, TokenKind::TriviaEndOfFile],
  )
}

#[test]
fn non_leading_byte_order_mark() {
  // Non-leading BOM is invalid
  test_scan(
    &[b' ', 0xef, 0xbb, 0xbf],
    &[
      TokenKind::TriviaWhitespace,
      TokenKind::ErrorInvalid,
      TokenKind::TriviaEndOfFile,
    ],
  )
}

#[test]
fn utf16_byte_order_mark() {
  // UTF-16 BOM is invalid
  test_scan(
    &[0xfe, 0xff],
    &[
      TokenKind::ErrorBadUtf8,
      TokenKind::ErrorBadUtf8,
      TokenKind::TriviaEndOfFile,
    ],
  )
}

mod number_lit {
  use super::*;

  #[test]
  fn single_digit() {
    test_scan(
      b"1",
      &[TokenKind::LiteralNumber, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn multiple_digits() {
    test_scan(
      b"12",
      &[TokenKind::LiteralNumber, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn decimals() {
    test_scan(
      b"12.3",
      &[TokenKind::LiteralNumber, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn exponent() {
    test_scan(
      b"1e2",
      &[TokenKind::LiteralNumber, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn signed_exponent() {
    test_scan(
      b"1e+2",
      &[TokenKind::LiteralNumber, TokenKind::TriviaEndOfFile],
    )
  }
}

mod idents {
  use super::*;

  #[test]
  fn simple() {
    test_scan(b"hello", &[TokenKind::Ident, TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn leading_underscore() {
    test_scan(b"_ello", &[TokenKind::Ident, TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn mid_underscore() {
    test_scan(b"hel_o", &[TokenKind::Ident, TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn hyphen() {
    test_scan(b"hel-o", &[TokenKind::Ident, TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn with_numbers() {
    test_scan(b"h3ll0", &[TokenKind::Ident, TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn acute_accent() {
    test_scan(
      "héllo".as_bytes(),
      &[TokenKind::Ident, TokenKind::TriviaEndOfFile],
    )
  }
}

mod literal_only_templates {
  use super::*;

  #[test]
  fn empty() {
    test_scan(
      b"\"\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn simple() {
    test_scan(
      b"\"hello\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_quotes() {
    test_scan(
      b"\"hello, \\\"world\\\"!\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn escaped_value() {
    // This one scans a little oddly because of how the scanner
    // handles the escaping of the dollar sign, but it's still
    // good enough for the parser since it'll just concatenate
    // these two string literals together anyway.
    test_scan(
      b"\"hello $$\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::LiteralQuoted,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}
