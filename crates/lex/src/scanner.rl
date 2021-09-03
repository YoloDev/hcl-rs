#![allow(clippy::assign_op_pattern)]
#![allow(clippy::comparison_chain)]
#![allow(clippy::never_loop)]
#![allow(clippy::single_match)]
#![allow(dead_code)]
#![allow(non_upper_case_globals)]
#![allow(unused_parens)]
#![allow(while_true)]

// This file is generated from scan_tokens.rl. DO NOT EDIT.
%%{
  # (except when you are actually in scan_tokens.rl here, so edit away!)

  machine hcltok;
  access self.;
  variable p self.p;
  variable cs self.cs;
  variable top self.top;
  variable ts self.ts;
  variable te self.te;
  variable act self.act;
  # variable stack self.stack;
  write data;
}%%

%%{
  prepush {self.stack.push(0);}
  postpop {self.stack.pop();}
}%%

use crate::{Token, TokenKind};

struct Lexer {
  p: i32,
  cs: i32,
  top: u32,
  ts: i32,
  te: i32,
  act: u32,
  stack: ::smallvec::SmallVec<[i32; 4]>,
  heredocs: ::smallvec::SmallVec<[HereDocInProgress; 1]>,
  braces: u16,
  ret_braces: ::smallvec::SmallVec<[u16; 8]>,
  tokens: ::std::collections::VecDeque<Token>,
  tok_start: i32,
  #[cfg(debug_assertions)]
  len: u32,
  #[cfg(debug_assertions)]
  block_offset: u32,
}

struct HereDocInProgress {
  marker: ::smallvec::SmallVec<[u8; 16]>,
  start_of_line: bool,
}

impl Lexer {

  fn init(&mut self, cs: i32) {
    self.cs = cs;
    %%write init nocs;
  }

  fn token(&mut self, token_kind: TokenKind) {
    let token = Token::from_scanner(token_kind, self.ts, self.te);

    #[cfg(debug_assertions)]
    {
      self.len += token.len();
      assert_eq!(self.len, self.block_offset + (self.te as u32));
    }

    self.tokens.push_back(token);
  }

  fn token_from_consumed(&mut self, c: u8) {
    let kind = TokenKind::from_char(c).unwrap();
    self.token(kind)
  }

  fn scan_block(&mut self, data: &[u8], is_final_block: bool) -> usize {
    let pe = data.len() as i32;
    let eof = if is_final_block { pe } else { -1i32 };

    %%{
      include UnicodeDerived "unicode_derived.rl";

      UTF8Cont = 0x80 .. 0xBF;
      AnyUTF8 = (
        0x00..0x7F |
        0xC0..0xDF . UTF8Cont |
        0xE0..0xEF . UTF8Cont . UTF8Cont |
        0xF0..0xF7 . UTF8Cont . UTF8Cont . UTF8Cont
      );
      BrokenUTF8 = any - AnyUTF8;

      NumberLitContinue = (digit|'.'|('e'|'E') ('+'|'-')? digit);
      NumberLit = digit ("" | (NumberLitContinue - '.') | (NumberLitContinue* (NumberLitContinue - '.')));
      Ident = (ID_Start | '_') (ID_Continue | '-')*;

      # Symbols that just represent themselves are handled as a single rule.
      SelfToken = "[" | "]" | "(" | ")" | "." | "," | "*" | "/" | "%" | "+" | "-" | "=" | "<" | ">" | "!" | "?" | ":" | "\n" | "&" | "|" | "~" | "^" | ";" | "`" | "'";

      EqualOp = "==";
      NotEqual = "!=";
      GreaterThanEqual = ">=";
      LessThanEqual = "<=";
      LogicalAnd = "&&";
      LogicalOr = "||";

      Ellipsis = "...";
      FatArrow = "=>";

      Newline = '\r' ? '\n';
      EndOfLine = Newline;

      BeginStringTmpl = '"';
      BeginHeredocTmpl = '<<' ('-')? Ident Newline;

      Comment = (
        # The :>> operator in these is a "finish-guarded concatenation",
        # which terminates the sequence on its left when it completes
        # the sequence on its right.
        # In the single-line comment cases this is allowing us to make
        # the trailing EndOfLine optional while still having the overall
        # pattern terminate. In the multi-line case it ensures that
        # the first comment in the file ends at the first */, rather than
        # gobbling up all of the "any*" until the _final_ */ in the file.
        ("#" (any - EndOfLine)* :>> EndOfLine?) |
        ("//" (any - EndOfLine)* :>> EndOfLine?) |
        ("/*" any* :>> "*/")
      );

      # Note: hclwrite assumes that only ASCII spaces appear between tokens,
      # and uses this assumption to recreate the spaces between tokens by
      # looking at byte offset differences. This means it will produce
      # incorrect results in the presence of tabs, but that's acceptable
      # because the canonical style (which hclwrite itself can impose
      # automatically is to never use tabs).
      Spaces = (' ' | 0x09)+;

      action beginStringTemplate {
        self.token(TokenKind::SymbolQuoteOpen);
        fcall stringTemplate;
      }

      action endStringTemplate {
        self.token(TokenKind::SymbolQuoteClose);
        fret;
      }

      action beginHeredocTemplate {
        self.token(TokenKind::SymbolHeredocOpen);
        // the token is currently the whole heredoc introducer, like
        // <<EOT or <<-EOT, followed by a newline. We want to extract
        // just the "EOT" portion that we'll use as the closing marker.

        let mut marker = &data[(self.ts as usize + 2)..(self.te as usize - 1)];
        if marker[0] == b'-' {
          marker = &marker[1..];
        }

        if marker[marker.len() - 1] == b'\r' {
          marker = &marker[..(marker.len() - 1)];
        }

        self.heredocs.push(HereDocInProgress {
          marker: marker.into(),
          start_of_line: true,
        });

        fcall heredocTemplate;
      }

      action heredocLiteralEOL {
        // This action is called specificially when a heredoc literal
        // ends with a newline character.

        // This might actually be our end marker.
        let mut is_match = false;

        if let Some(topdoc) = self.heredocs.last_mut() {
          if topdoc.start_of_line {
            let maybe_marker = crate::helpers::trim_space(&data[(self.ts as usize)..(self.te as usize)]);
            is_match = maybe_marker == &*topdoc.marker;
          }

          if !is_match {
            topdoc.start_of_line = true;
            self.token(TokenKind::LiteralString);
          }
        }

        if is_match {
          // We actually emit two tokens here: the end-of-heredoc
          // marker first, and then separately the newline that
          // follows it. This then avoids issues with the closing
          // marker consuming a newline that would normally be used
          // to mark the end of an attribute definition.
          // We might have either a \n sequence or an \r\n sequence
          // here, so we must handle both.
          let mut nls = self.te - 1;
          let nle = self.te;
          self.te -= 1;

          if data[(self.te - 1) as usize] == b'\r' {
            // back up one more byte
            nls -= 1;
            self.te -= 1;
          }

          self.token(TokenKind::SymbolHeredocClose);
          self.ts = nls;
          self.te = nle;
          self.token(TokenKind::TriviaNewline);
          self.heredocs.pop();
          fret;
        }
      }

      action heredocLiteralMidline {
        // This action is called when a heredoc literal _doesn't_ end
        // with a newline character, e.g. because we're about to enter
        // an interpolation sequence.
        self.heredocs.last_mut().unwrap().start_of_line = false;
        self.token(TokenKind::LiteralString);
      }

      action bareTemplateLiteral {
        self.token(TokenKind::LiteralString);
      }

      action beginTemplateInterp {
        self.token(TokenKind::TemplateInterpret);
        self.braces += 1;
        self.ret_braces.push(self.braces);
        if let Some(heredoc) = self.heredocs.last_mut() {
          heredoc.start_of_line = false;
        }

        fcall main;
      }

      action beginTemplateControl {
        self.token(TokenKind::TemplateControl);
        self.braces += 1;
        self.ret_braces.push(self.braces);
        if let Some(heredoc) = self.heredocs.last_mut() {
          heredoc.start_of_line = false;
        }

        fcall main;
      }

      action openBrace {
        self.token(TokenKind::SymbolBraceOpen);
        self.braces += 1;
      }

      action closeBrace {
        match self.ret_braces.last() {
          Some(last_brace) if *last_brace == self.braces => {
            self.token(TokenKind::TemplateSeqenceEnd);
            self.braces -= 1;
            self.ret_braces.pop();
            fret;
          }
          _ => {
            self.token(TokenKind::SymbolBraceClose);
            self.braces -= 1;
          }
        }
      }

      action closeTemplateSeqEatWhitespace {
        // Only consume from the self.ret_braces stack and return if we are at
        // a suitable brace nesting level, otherwise things will get
        // confused. (Not entering this branch indicates a syntax error,
        // which we will catch in the parser.)
        match self.ret_braces.last() {
          Some(last_brace) if *last_brace == self.braces => {
            self.token(TokenKind::TemplateSeqenceEnd);
            self.braces -= 1;
            self.ret_braces.pop();
            fret;
          }
          _ => {
            // We intentionally generate a TokenTemplateSeqEnd here,
            // even though the user apparently wanted a brace, because
            // we want to allow the parser to catch the incorrect use
            // of a ~} to balance a generic opening brace, rather than
            // a template sequence.
            self.token(TokenKind::TemplateSeqenceEnd);
            self.braces -= 1;
          }
        }
      }

      action selfToken {
        let b = &data[(self.ts as usize)..(self.te as usize)];
        if b.len() != 1 {
          unreachable!();
        }

        self.token_from_consumed(b[0])
      }

      TemplateInterp = "${" ("~")?;
      TemplateControl = "%{" ("~")?;
      EndStringTmpl = '"';
      NewlineChars = ("\r"|"\n");
      NewlineCharsSeq = NewlineChars+;
      StringLiteralChars = (AnyUTF8 - NewlineChars);
      TemplateIgnoredNonBrace = (^'{' %{ fhold; });
      TemplateNotInterp = '$' (TemplateIgnoredNonBrace | TemplateInterp);
      TemplateNotControl = '%' (TemplateIgnoredNonBrace | TemplateControl);
      QuotedStringLiteralWithEsc = ('\\' StringLiteralChars) | (StringLiteralChars - ("$" | '%' | '"' | "\\"));
      TemplateStringLiteral = (
          (TemplateNotInterp) |
          (TemplateNotControl) |
          (QuotedStringLiteralWithEsc)+
      );
      HeredocStringLiteral = (
          (TemplateNotInterp) |
          (TemplateNotControl) |
          (StringLiteralChars - ("$" | '%'))*
      );
      BareStringLiteral = (
          (TemplateNotInterp) |
          (TemplateNotControl) |
          (StringLiteralChars - ("$" | '%'))*
      ) Newline?;

      stringTemplate := |*
          TemplateInterp        => beginTemplateInterp;
          TemplateControl       => beginTemplateControl;
          EndStringTmpl         => endStringTemplate;
          TemplateStringLiteral => { self.token(TokenKind::LiteralQuoted); };
          NewlineCharsSeq       => { self.token(TokenKind::ErrorQuotedNewline); };
          AnyUTF8               => { self.token(TokenKind::ErrorInvalid); };
          BrokenUTF8            => { self.token(TokenKind::ErrorBadUtf8); };
      *|;

      heredocTemplate := |*
          TemplateInterp        => beginTemplateInterp;
          TemplateControl       => beginTemplateControl;
          HeredocStringLiteral EndOfLine => heredocLiteralEOL;
          HeredocStringLiteral  => heredocLiteralMidline;
          BrokenUTF8            => { self.token(TokenKind::ErrorBadUtf8); };
      *|;

      bareTemplate := |*
          TemplateInterp        => beginTemplateInterp;
          TemplateControl       => beginTemplateControl;
          BareStringLiteral     => bareTemplateLiteral;
          BrokenUTF8            => { self.token(TokenKind::ErrorBadUtf8); };
      *|;

      identOnly := |*
          Ident            => { self.token(TokenKind::Ident) };
          BrokenUTF8       => { self.token(TokenKind::ErrorBadUtf8) };
          AnyUTF8          => { self.token(TokenKind::ErrorInvalid) };
      *|;

      main := |*
          Spaces           => { self.token(TokenKind::TriviaWhitespace) };
          NumberLit        => { self.token(TokenKind::LiteralNumber) };
          Ident            => { self.token(TokenKind::Ident) };

          Comment          => { self.token(TokenKind::TriviaComment) };
          Newline          => { self.token(TokenKind::TriviaNewline) };

          EqualOp          => { self.token(TokenKind::OperatorEquals); };
          NotEqual         => { self.token(TokenKind::OperatorNotEquals); };
          GreaterThanEqual => { self.token(TokenKind::OperatorGreaterThanOrEqual); };
          LessThanEqual    => { self.token(TokenKind::OperatorLessThanOrEqual); };
          LogicalAnd       => { self.token(TokenKind::OperatorAnd); };
          LogicalOr        => { self.token(TokenKind::OperatorOr); };
          Ellipsis         => { self.token(TokenKind::SymbolEllipsis); };
          FatArrow         => { self.token(TokenKind::SymbolFatArrow); };
          SelfToken        => selfToken;

          "{"              => openBrace;
          "}"              => closeBrace;

          "~}"             => closeTemplateSeqEatWhitespace;

          BeginStringTmpl  => beginStringTemplate;
          BeginHeredocTmpl => beginHeredocTemplate;

          BrokenUTF8       => { self.token(TokenKind::ErrorBadUtf8) };
          AnyUTF8          => { self.token(TokenKind::ErrorInvalid) };
      *|;

    }%%

    %%{
      write exec;
    }%%

    todo!()
  }
}
