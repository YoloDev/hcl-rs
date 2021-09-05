use super::*;

fn assert_expected_tokens(actual: &[Token], expected: &[TokenKind]) {
  let mut actual = actual.iter();
  let mut expected = expected.iter();
  let mut index = 0;

  loop {
    match (actual.next(), expected.next()) {
      (None, None) => break,
      (Some(t), None) => panic!(
        "Expected end of tokens, but received {:?}. Index = {}",
        t.kind(),
        index
      ),
      (None, Some(t)) => panic!(
        "Expected {:?}, but received end of tokens. Index = {}",
        t, index
      ),
      (Some(l), Some(r)) => assert_eq!(l.kind(), *r, "Expected {:?}. Index = {}", r, index),
    }

    index += 1;
  }
}

fn test_scan_single_chunk(
  value: &'static [u8],
  expected: &[TokenKind],
  mut lexer: Lexer,
) -> Vec<Token> {
  let bytes = value;
  lexer.scan_block(bytes, true);
  let tokens = lexer.tokens.into_iter().collect::<Vec<_>>();

  assert_expected_tokens(&tokens, expected);
  tokens
}

fn test_scan_byte_by_byte(
  value: &'static [u8],
  expected: &[TokenKind],
  mut lexer: Lexer,
) -> Vec<Token> {
  let mut bytes = value;
  let mut tokens = Vec::new();
  let mut l = 0usize;
  while l < bytes.len() {
    let block = &bytes[..=l];
    let consumed = lexer.scan_block(block, false);
    if consumed > 0 {
      bytes = &bytes[consumed..];
      l -= consumed - 1;
    } else {
      l += 1;
    }

    while let Some(tok) = lexer.tokens.pop_front() {
      tokens.push(tok);
    }
  }

  lexer.scan_block(bytes, true);
  while let Some(tok) = lexer.tokens.pop_front() {
    tokens.push(tok);
  }

  assert_expected_tokens(&tokens, expected);
  tokens
}

fn test_scan_inner(
  value: &'static [u8],
  expected: &[TokenKind],
  lexer_factory: impl Fn() -> Lexer,
) {
  let single_chunk = test_scan_single_chunk(value, expected, lexer_factory());
  let byte_by_byte = test_scan_byte_by_byte(value, expected, lexer_factory());
  assert_eq!(single_chunk, byte_by_byte)
}

fn test_scan(value: &'static [u8], expected: &[TokenKind]) {
  test_scan_inner(value, expected, Lexer::new)
}

fn test_scan_template(value: &'static [u8], expected: &[TokenKind]) {
  test_scan_inner(value, expected, Lexer::new_template)
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
  fn dollar_dollar() {
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

  #[test]
  fn percent_percent() {
    // This one scans a little oddly because of how the scanner
    // handles the escaping of the dollar sign, but it's still
    // good enough for the parser since it'll just concatenate
    // these two string literals together anyway.
    test_scan(
      b"\"hello %%\"",
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

  #[test]
  fn ending_dollar() {
    test_scan(
      b"\"hello $\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn ending_percent() {
    test_scan(
      b"\"hello %\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn escaped_value() {
    test_scan(
      b"\"hello $${world}\"",
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

  #[test]
  fn escaped_control() {
    test_scan(
      b"\"hello %%{world}\"",
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

mod templates {
  use super::*;

  #[test]
  fn percent_dollar_value() {
    test_scan(
      b"\"hello %${world}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::LiteralQuoted,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn iterpret_num() {
    test_scan(
      b"\"${1}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::LiteralNumber,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn control_ident() {
    test_scan(
      b"\"%{a}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateControl,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn iterpret_brace() {
    test_scan(
      b"\"${{}}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::SymbolBraceOpen,
        TokenKind::SymbolBraceClose,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn iterpret_string() {
    test_scan(
      b"\"${\"\"}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::SymbolQuoteOpen,
        TokenKind::SymbolQuoteClose,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn template_in_template() {
    test_scan(
      b"\"${\"${a}\"}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn template_in_template_2() {
    test_scan(
      b"\"${\"${a} foo\"}\"",
      &[
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::SymbolQuoteOpen,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}

mod heredoc_templates {
  use super::*;

  #[test]
  fn hello_world() {
    test_scan(
      b"<<EOT\nhello world\nEOT\n",
      &[
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn windows_line_endings() {
    test_scan(
      b"<<EOT\r\nhello world\r\nEOT\r\n",
      &[
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_value() {
    test_scan(
      b"<<EOT\nhello ${name}\nEOT\n",
      &[
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::LiteralString,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_ending_after_value() {
    test_scan(
      b"<<EOT\n${name}EOT\nEOT\n",
      &[
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn nested() {
    test_scan(
      b"<<EOF\n${<<-EOF\nhello\nEOF\n}\nEOF\n",
      &[
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::TemplateInterpret,
        TokenKind::SymbolHeredocOpen,
        TokenKind::TriviaNewline,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::SymbolHeredocClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}

mod expressions {
  use super::*;

  #[test]
  fn math_with_parens() {
    test_scan(
      b" (1 + 2) * 3 ",
      &[
        TokenKind::TriviaWhitespace,
        TokenKind::SymbolParenOpen,
        TokenKind::LiteralNumber,
        TokenKind::TriviaWhitespace,
        TokenKind::OperatorPlus,
        TokenKind::TriviaWhitespace,
        TokenKind::LiteralNumber,
        TokenKind::SymbolParenClose,
        TokenKind::TriviaWhitespace,
        TokenKind::OperatorMul,
        TokenKind::TriviaWhitespace,
        TokenKind::LiteralNumber,
        TokenKind::TriviaWhitespace,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn modulo() {
    test_scan(
      b"9%8",
      &[
        TokenKind::LiteralNumber,
        TokenKind::OperatorModulo,
        TokenKind::LiteralNumber,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn assign() {
    test_scan(
      b"\na = 1\n",
      &[
        TokenKind::TriviaNewline,
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::OperatorAssign,
        TokenKind::TriviaWhitespace,
        TokenKind::LiteralNumber,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}

mod comments {
  use super::*;

  #[test]
  fn hash_comment() {
    test_scan(
      b"# hello\n",
      &[TokenKind::TriviaComment, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn slash_comment_no_newline() {
    test_scan(
      b"// hello\n// hello",
      &[
        TokenKind::TriviaComment,
        TokenKind::TriviaComment,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn slash_comment_with_text_between() {
    test_scan(
      b"// hello\nfoo\n// hello",
      &[
        TokenKind::TriviaComment,
        TokenKind::Ident,
        TokenKind::TriviaNewline,
        TokenKind::TriviaComment,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn hash_comment_no_newline() {
    test_scan(
      b"# hello\n# hello",
      &[
        TokenKind::TriviaComment,
        TokenKind::TriviaComment,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn hash_comment_with_text_between() {
    test_scan(
      b"# hello\nfoo\n# hello",
      &[
        TokenKind::TriviaComment,
        TokenKind::Ident,
        TokenKind::TriviaNewline,
        TokenKind::TriviaComment,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn multiline_comment() {
    test_scan(
      b"/* hello */",
      &[TokenKind::TriviaComment, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn multiline_comment_around_text() {
    test_scan(
      b"/* hello */ howdy /* hey */",
      &[
        TokenKind::TriviaComment,
        TokenKind::TriviaWhitespace,
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::TriviaComment,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}

mod invalid {
  use super::*;

  #[test]
  fn emoji() {
    test_scan(
      "🌻".as_bytes(),
      &[TokenKind::ErrorInvalid, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn bitwise_or() {
    test_scan(
      b"|",
      &[TokenKind::ErrorBitwiseOr, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn bad_utf8() {
    test_scan(
      &[0x80],
      &[TokenKind::ErrorBadUtf8, TokenKind::TriviaEndOfFile],
    );

    test_scan(
      &[0x80, 0x80],
      &[
        TokenKind::ErrorBadUtf8,
        TokenKind::ErrorBadUtf8,
        TokenKind::TriviaEndOfFile,
      ],
    );
  }

  #[test]
  fn tabs() {
    test_scan(
      b"\t\t",
      &[TokenKind::TriviaWhitespace, TokenKind::TriviaEndOfFile],
    )
  }
}

// Misc combinations that have come up in bug reports, etc.
mod misc {
  use super::*;

  #[test]
  fn misc_1() {
    test_scan(
      b"locals {\n  is_percent = percent_sign == \"%\" ? true : false\n}\n",
      &[
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::SymbolBraceOpen,
        TokenKind::TriviaNewline,
        TokenKind::TriviaWhitespace,
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::OperatorAssign,
        TokenKind::TriviaWhitespace,
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::OperatorEquals,
        TokenKind::TriviaWhitespace,
        TokenKind::SymbolQuoteOpen,
        TokenKind::LiteralQuoted,
        TokenKind::SymbolQuoteClose,
        TokenKind::TriviaWhitespace,
        TokenKind::SymbolQuestion,
        TokenKind::TriviaWhitespace,
        TokenKind::Ident,
        TokenKind::TriviaWhitespace,
        TokenKind::SymbolColon,
        TokenKind::TriviaWhitespace,
        TokenKind::Ident,
        TokenKind::TriviaNewline,
        TokenKind::SymbolBraceClose,
        TokenKind::TriviaNewline,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}

mod scan_template {
  use super::*;

  #[test]
  fn empty() {
    test_scan_template(b"", &[TokenKind::TriviaEndOfFile])
  }

  #[test]
  fn simple() {
    test_scan_template(
      b" hello ",
      &[TokenKind::LiteralString, TokenKind::TriviaEndOfFile],
    )
  }

  #[test]
  fn simple_with_newlines() {
    test_scan_template(
      b"\nhello\n",
      &[
        TokenKind::LiteralString,
        TokenKind::LiteralString,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_value() {
    test_scan_template(
      b"hello ${foo} hello",
      &[
        TokenKind::LiteralString,
        TokenKind::TemplateInterpret,
        TokenKind::Ident,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_value_strip() {
    test_scan_template(
      b"hello ${~foo~} hello",
      &[
        TokenKind::LiteralString,
        TokenKind::TemplateInterpret,
        TokenKind::TemplateStripMarker,
        TokenKind::Ident,
        TokenKind::TemplateStripMarker,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }

  #[test]
  fn with_control_strip() {
    test_scan_template(
      b"hello %{~foo~} hello",
      &[
        TokenKind::LiteralString,
        TokenKind::TemplateControl,
        TokenKind::TemplateStripMarker,
        TokenKind::Ident,
        TokenKind::TemplateStripMarker,
        TokenKind::TemplateSeqenceEnd,
        TokenKind::LiteralString,
        TokenKind::TriviaEndOfFile,
      ],
    )
  }
}
