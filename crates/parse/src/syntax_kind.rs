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

    // identifier
    IDENTIFIER                      /// identifier

    // literals
    LIT_NUMBER                      /// number literal
    LIT_TRUE                        /// `true` literal (contextual keyword)
    LIT_FALSE                       /// `false` literal (contextual keyword)
    LIT_NULL                        /// `null` literal (contextual keyword)

    // template tokens
    TMPL_LIT_STRING                 /// template string literal - cannot contain backslash escapes
    TMPL_QUOT_STRING                /// template quoted string literal - might contain backslash escapes
    TMPL_INTERPRET_START            /// template interpret start token
    TMPL_CONTROL_START              /// template control start token
    TMPL_SEQUENCE_END               /// template sequence end token
    TMPL_STRIP_MARKER               /// template strip marker

    // template productions
    TMPL_INTERPRET                  /// template interpret section
    TMPL_CONTROL                    /// template control section

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

    SYM_O_PAREN                 ['('] /// `(`
    SYM_C_PAREN                 [')'] /// `)`
    SYM_O_CURLY                 ['{'] /// `{`
    SYM_C_CURLY                 ['}'] /// `}`
    SYM_O_BRACK                 ['['] /// `[`
    SYM_C_BRACK                 [']'] /// `]`
    SYM_O_QUOTE                       /// Open quote
    SYM_C_QUOTE                       /// Close quote

    SYM_O_HEREDOC               [<<]  /// `<<WORD` where `WORD` can be any end delimiter.
    SYM_O_HEREDOC_INDENTED      [<<-] /// `<<-WORD` where `WORD` can be any end delimiter.
    SYM_C_HEREDOC                     /// Heredoc close token (the delimiter word used in the open earlier).

    // keywords
    KW_FOR                      [for]    /// `for`
    KW_IN                       [in]     /// `in`
    KW_IF                       [if]     /// `if`
    KW_ELSE                     [else]   /// `else`
    KW_ENDIF                    [endif]  /// `endif`
    KW_ENDFOR                   [endfor] /// `endfor`

    // newlines
    NEWLINE                              /// newlines

    // structures
    SOURCE_FILE                          /// a hcl source file

    // items
    ITEM_ATTRIBUTE                       /// hcl attribute item
    ITEM_BLOCK                           /// hcl block item
    ITEM_BLOCK_LABELS                    /// labels for a hcl block item
    ITEM_BLOCK_BODY                      /// hcl block item's body

    // expressions
    EXPR_TERNARY                         /// hcl ternary expression
    EXPR_BINARY_OP                       /// hcl binary operation expression
    EXPR_UNARY_OP                        /// hcl unary operation expression
    EXPR_RELATIVE_TRAVERSAL              /// hcl relative traversal expression
    EXPR_SPLAT                           /// hcl splat expression
    EXPR_PAREN                           /// hcl parenthesis expression
    EXPR_LITERAL                         /// hcl literal expression
    EXPR_VARIABLE                        /// hcl variable expression
    EXPR_TEMPLATE                        /// hcl template expression
    EXPR_CALL                            /// hcl call expression
    EXPR_TUPLE_LITERAL                   /// hcl tuple literal

    // error
    ERROR                                /// error syntax

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
