#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
  kind: TokenKind,
  len: u32,
}

impl Token {
  pub(crate) fn from_scanner(kind: TokenKind, len: i32) -> Self {
    debug_assert!(len >= 0);
    Self {
      kind,
      len: len as u32,
    }
  }

  #[inline]
  pub fn kind(&self) -> TokenKind {
    self.kind
  }

  #[inline]
  pub const fn len(&self) -> u32 {
    self.len
  }

  #[inline]
  pub const fn is_empty(&self) -> bool {
    self.len == 0
  }
}

macro_rules! token_enum {
  (
    $(#[$($enum_m:tt)*])*
    pub enum $name:ident {
      $($case:ident$(($chr:literal))? $(#[$($m:tt)*])*)*
    }
  ) => {
    $(#[$($enum_m)*])*
    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    pub enum $name {
      $(
        $(#[$($m)*])*
        $case,
      )*
    }

    impl $name {
      pub(crate) const fn from_char(c: u8) -> Option<Self> {
        match c {
          $(
            $(
              $chr => Some(Self::$case),
            )?
          )*
          _ => None,
        }
      }
    }
  };
}

// SelfToken = "[" | "]" | "(" | ")" | "." | "," | "*" | "/" | "%" | "+" | "-" | "=" | "<" | ">" | "!" | "?" | ":" | "\n" | "&" | "|" | "~" | "^" | ";" | "`" | "'";

token_enum! {
  /// Token kind.
  pub enum TokenKind {
    // block symbols
    SymbolBraceOpen               /// `{`
    SymbolBraceClose              /// `}`
    SymbolBrackOpen(b'[')         /// `[`
    SymbolBrackClose(b']')        /// `]`
    SymbolParenOpen(b'(')         /// `(`
    SymbolParenClose(b')')        /// `)`
    SymbolQuoteOpen               /// `"` - when used to open a new quote
    SymbolQuoteClose              /// `"` - when used to close a quote
    SymbolHeredocOpen             /// `<<` or `<<-`
    SymbolHeredocClose            /// End delimititer for a heredoc (depends on the open sequence).

    // math operators
    OperatorMul(b'*')             /// `*`
    OperatorDiv(b'/')             /// `/`
    OperatorPlus(b'+')            /// `+`
    OperatorMinus(b'-')           /// `-`
    OperatorModulo(b'%')          /// `%`

    // comparison and assignment operators
    OperatorAssign(b'=')          /// `=`
    OperatorEquals                /// `==`
    OperatorNotEquals             /// `!=`
    OperatorLessThan(b'<')        /// `<`
    OperatorLessThanOrEqual       /// `<=`
    OperatorGreaterThan(b'>')     /// `>`
    OperatorGreaterThanOrEqual    /// `>=`

    // logic operators
    OperatorAnd                   /// `&&`
    OperatorOr                    /// `||`
    OperatorNot(b'!')             /// `!`

    // symbols
    SymbolDot(b'.')               /// `.`
    SymbolComma(b',')             /// `,`

    SymbolEllipsis                /// `...`
    SymbolFatArrow                /// `=>`

    SymbolQuestion(b'?')          /// `?`
    SymbolColon(b':')             /// `:`

    // template tokens
    TemplateInterpret             /// Start of `${` or `${~` block.
    TemplateControl               /// Start of `%{` or `%{~` block.
    TemplateSeqenceEnd            /// End of template interpret or control block.

    // literals
    LiteralQuoted                 /// Quoted string literal. Might contain backslash escapes.
    LiteralString                 /// String literal. Cannot contain backslash escapes.
    LiteralNumber                 /// Number literal.

    // ident
    Ident                         /// Identifier (or contextual keyword).


    // trivia
    TriviaWhitespace              /// Whitespace.
    TriviaComment                 /// Comment.
    TriviaNewline(b'\n')          /// `\n`
    TriviaEndOfFile               /// End of file token.
    TriviaByteOrderMark           /// Leading UTF-8 byte-order mark token.

    // The rest are not used in the language but recognized by the scanner so
    // we can generate good diagnostics in the parser when users try to write
    // things that might work in other languages they are familiar with, or
    // simply make incorrect assumptions about the HCL language.
    ErrorBitwiseAnd(b'&')         /// `&` token (invalid).
    ErrorBitwiseOr(b'|')          /// `|` token (invalid).
    ErrorBitwiseNot(b'~')         /// `~` token (invalid).
    ErrorBitwiseXor(b'^')         /// `^` token (invalid).
    ErrorPower                    /// `**` token (invalid).
    ErrorApostrophe(b'\'')        /// `'` token (invalid).
    ErrorBacktick(b'`')           /// `` ` `` token (invalid).
    ErrorSemicolon(b';')          /// `;` token (invalid).
    ErrorInvalid                  /// invalid token (invalid).
    ErrorBadUtf8                  /// bad utf-8 data (invalid).
    ErrorQuotedNewline            /// quoted newline (invalid).
  }
}
