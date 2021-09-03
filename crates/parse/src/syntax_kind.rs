macro_rules! define_syntax_kind {
  (@ ($name:ident) [$($acc:tt)*] [$($m_acc:tt)*]) => {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    #[repr(u16)]
    #[allow(non_camel_case_types, clippy::upper_case_acronyms, clippy::manual_non_exhaustive)]
    #[non_exhaustive]
    pub enum $name {
      $($acc)*
    }

    #[macro_export]
    macro_rules! T {
      $($m_acc)*
    }
  };

  (@ $ctx:tt [$($acc:tt)*] $m_acc:tt $name:ident #hidden $($rest:tt)*) => {
    define_syntax_kind! {
      @ $ctx [$($acc)* #[doc(hidden)] $name,] $m_acc $($rest)*
    }
  };

  (@ $ctx:tt [$($acc:tt)*] [$($m_acc:tt)*] $name:ident #[$($m:tt)*] $($rest:tt)*) => {
    define_syntax_kind! {
      @ $ctx [$($acc)* #[$($m)*] $name,] [$($m_acc)* [$name] => { $crate::SyntaxKind::$name };] $($rest)*
    }
  };

  (@ $ctx:tt [$($acc:tt)*] [$($m_acc:tt)*] $name:ident [$($tok:tt)*] #[$($m:tt)*] $($rest:tt)*) => {
    define_syntax_kind! {
      @ $ctx [$($acc)* #[$($m)*] $name,] [$($m_acc)* [$($tok)*] => { $crate::SyntaxKind::$name }; [$name] => { $crate::SyntaxKind::$name };] $($rest)*
    }
  };

  (pub enum $name:ident { $($body:tt)* }) => {
    define_syntax_kind! {
      @ ($name) [] [] $($body)*
    }
  };
}

define_syntax_kind! {
  pub enum SyntaxKind {
    // special
    TOMBSTONE                   #hidden
    EOF                         #hidden

    // operators
    OP_MUL                      [*] /// `*`
    OP_DIV                      [/] /// `/`
    OP_MOD                      [%] /// `%`
    OP_PLUS                     [+] /// `+`
    OP_MINUS                    [-] /// `-`

    OP_ASSIGN                   [=] /// `=`
    OP_EQUAL                    [==] /// `==`
    OP_NOT_EQUAL                [!=] /// `!=`
    OP_LESS_THAN                [<] /// `<`
    OP_GREATER_THAN             [>] /// `>`
    OP_LESS_THAN_OR_EQUAL       [<=] /// `<=`
    OP_GREATER_THAN_OR_EQUAL    [>=] /// `>=`

    OP_AND                      [&&] /// `&&`
    OP_OR                       [||] /// `||`
    OP_NOT                      [!] /// `!`

    // symbols
    SYM_DOT                     [.] /// `.`
    SYM_COMMA                   [,] /// `,`

    SYM_ELLIPSIS                [...] /// `...`
    SYM_FAT_ARROW               [=>] /// `=>`

    SYM_QMARK                   [?] /// `?`
    SYM_COLON                   [:] /// `:`

    // keywords
    KW_FOR                      [for]    /// `for`
    KW_IN                       [in]     /// `in`
    KW_IF                       [if]     /// `if`
    KW_ELSE                     [else]   /// `else`
    KW_ENDIF                    [endif]  /// `endif`
    KW_ENDFOR                   [endfor] /// `endfor`

    // symbols
    L_PAREN                     ['('] /// `(`
    R_PAREN                     [')'] /// `)`
    L_CURLY                     ['{'] /// `{`
    R_CURLY                     ['}'] /// `}`
    L_BRACK                     ['['] /// `[`
    R_BRACK                     [']'] /// `]`

    // The rest are not used in the language but recognized by the scanner so
    // we can generate good diagnostics in the parser when users try to write
    // things that might work in other languages they are familiar with, or
    // simply make incorrect assumptions about the HCL language.
    ERR_BITWISE_AND                   /// `&`
    ERR_BITWISE_OR                    /// `|`
    ERR_BITWISE_NOT                   /// `~`
    ERR_BITWISE_XOR                   /// `^`
    ERR_POW                           /// `**`
    ERR_APOSTROPHE                    /// `'`
    ERR_BACKTICK                      /// `` ` ``
    ERR_SEMICOLON                     /// `;`
    ERR_TABS                          /// `\t`
    ERR_INVALID                       /// `�`

    // special
    __LAST                      #hidden
  }
}

use SyntaxKind::*;

impl From<u16> for SyntaxKind {
  fn from(d: u16) -> SyntaxKind {
    assert!(d <= (__LAST as u16));
    unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
  }
}

impl From<SyntaxKind> for u16 {
  fn from(k: SyntaxKind) -> u16 {
    k as u16
  }
}
