/// A trait for displaying the source text of a token
pub trait DisplaySource {
    fn display_source<'a>(&self, source: &'a str) -> &'a str;
}

impl From<usize> for Span {
    fn from(val: usize) -> Self {
        Span {
            start: val,
            end: val,
        }
    }
}

impl From<(usize, usize)> for Span {
    fn from(val: (usize, usize)) -> Self {
        let (start, end) = val;
        Span { start, end }
    }
}

/// A trait for converting a value into a token
pub trait IntoToken {
    fn into_token(self, span: impl Into<Span>) -> Token;
}

/// A span represents a range of characters in the source code
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(&self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// The bit size of a numerical value
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumericalBitSize {
    Bits8 = 8,
    Bits16 = 16,
    Bits32 = 32,
    Bits64 = 64,
    BitsPointer,
}

/// A numeric value parsed from the source code
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Number {
    Unsigned(u64),
    Signed(i64),
    Float(f64),
}

impl IntoToken for Number {
    fn into_token(self, span: impl Into<Span>) -> Token {
        TokenKind::Number(self).into_token(span)
    }
}

/// The kind of token
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum TokenKind {
    // Keywords
    If,
    Else,
    Type,
    Struct,
    Enum,
    Interface,
    Constant,
    Function,
    Variable,
    While,
    For,
    In,

    // Literals
    String,
    Bool(bool),
    Number(Number),
    Integer(NumericalBitSize),
    Unsigned(NumericalBitSize),
    Identifier,

    // End of file
    Eof,

    // Delimiters
    LBrace,    // {
    RBrace,    // }
    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    SemiColon, // ;
    Colon,     // :
    Comma,     // ,
    Dot,       // .

    // Comments
    Comment,
    DocComment,

    // Operators - Arithmetic
    Plus,  // +
    Minus, // -
    Star,  // *
    Div,   // /
    Mod,   // %

    // Operators - Increment/Decrement
    Increment, // ++
    Decrement, // --

    // Operators - Comparison
    Equal,        // ==
    NotEqual,     // !=
    Lesser,       // <
    Greater,      // >
    LesserEqual,  // <=
    GreaterEqual, // >=

    // Operators - Logical
    Not, // !
    Or,  // ||
    And, // &&

    // Operators - Bitwise
    BitNot, // ~
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^

    // Operators - Assignment
    Assign,       // =
    PlusAssign,   // +=
    MinusAssign,  // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    BitAndAssign, // &=
    BitOrAssign,  // |=
    BitXorAssign, // ^=
    LShiftAssign, // <<=
    RShiftAssign, // >>=
}

impl TokenKind {
    /// Converts an identifier string to a token kind
    pub fn from_identifier(identifier: &str) -> Self {
        match identifier {
            "if" => Self::If,
            "else" => Self::Else,
            "for" => Self::For,
            "in" => Self::In,
            "while" => Self::While,
            "type" => Self::Type,
            "struct" => Self::Struct,
            "enum" => Self::Enum,
            "interface" => Self::Interface,
            "fun" => Self::Function,
            "var" => Self::Variable,
            "const" => Self::Constant,
            "i8" => Self::Integer(NumericalBitSize::Bits8),
            "i16" => Self::Integer(NumericalBitSize::Bits16),
            "i32" => Self::Integer(NumericalBitSize::Bits32),
            "i64" => Self::Integer(NumericalBitSize::Bits64),
            "isize" => Self::Integer(NumericalBitSize::BitsPointer),
            "u8" => Self::Unsigned(NumericalBitSize::Bits8),
            "u16" => Self::Unsigned(NumericalBitSize::Bits16),
            "u32" => Self::Unsigned(NumericalBitSize::Bits32),
            "u64" => Self::Unsigned(NumericalBitSize::Bits64),
            "usize" => Self::Unsigned(NumericalBitSize::BitsPointer),
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            _ => Self::Identifier,
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            TokenKind::Bool(_) | TokenKind::Integer(_) | TokenKind::Unsigned(_)
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Div
                | TokenKind::Plus
                | TokenKind::Mod
                | TokenKind::Increment
                | TokenKind::Decrement
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Lesser
                | TokenKind::Greater
                | TokenKind::LesserEqual
                | TokenKind::GreaterEqual
                | TokenKind::Not
                | TokenKind::Or
                | TokenKind::And
                | TokenKind::BitNot
                | TokenKind::BitAnd
                | TokenKind::BitOr
                | TokenKind::BitXor
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::Dot
        )
    }
}

impl IntoToken for TokenKind {
    fn into_token(self, span: impl Into<Span>) -> Token {
        Token {
            kind: self,
            position: span.into(),
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Type => write!(f, "type"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::Enum => write!(f, "enum"),
            TokenKind::Interface => write!(f, "interface"),
            TokenKind::Constant => write!(f, "const"),
            TokenKind::Function => write!(f, "fun"),
            TokenKind::Variable => write!(f, "var"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Bool(value) => write!(f, "{value}"),
            TokenKind::Number(number) => match number {
                Number::Unsigned(value) => write!(f, "{value}"),
                Number::Signed(value) => write!(f, "{value}"),
                Number::Float(value) => write!(f, "{value}"),
            },
            TokenKind::Integer(bit_size) => match bit_size {
                NumericalBitSize::Bits8 => write!(f, "i8"),
                NumericalBitSize::Bits16 => write!(f, "i16"),
                NumericalBitSize::Bits32 => write!(f, "i32"),
                NumericalBitSize::Bits64 => write!(f, "i64"),
                NumericalBitSize::BitsPointer => write!(f, "isize"),
            },
            TokenKind::Unsigned(bit_size) => match bit_size {
                NumericalBitSize::Bits8 => write!(f, "u8"),
                NumericalBitSize::Bits16 => write!(f, "u16"),
                NumericalBitSize::Bits32 => write!(f, "u32"),
                NumericalBitSize::Bits64 => write!(f, "u64"),
                NumericalBitSize::BitsPointer => write!(f, "usize"),
            },
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Eof => write!(f, "eof"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBracket => write!(f, "]"),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comment => write!(f, "//"),
            TokenKind::DocComment => write!(f, "///"),
            TokenKind::Plus => write!(f, "-"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::Increment => write!(f, "++"),
            TokenKind::Decrement => write!(f, "--"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Lesser => write!(f, "<"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::LesserEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Not => write!(f, "!"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::And => write!(f, "&&"),
            TokenKind::BitNot => write!(f, "~"),
            TokenKind::BitAnd => write!(f, "&"),
            TokenKind::BitOr => write!(f, "|"),
            TokenKind::BitXor => write!(f, "^"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::PlusAssign => write!(f, "+="),
            TokenKind::MinusAssign => write!(f, "-="),
            TokenKind::MulAssign => write!(f, "*="),
            TokenKind::DivAssign => write!(f, "/="),
            TokenKind::ModAssign => write!(f, "%="),
            TokenKind::BitAndAssign => write!(f, "&="),
            TokenKind::BitOrAssign => write!(f, "|="),
            TokenKind::BitXorAssign => write!(f, "^="),
            TokenKind::LShiftAssign => write!(f, "<<="),
            TokenKind::RShiftAssign => write!(f, ">>="),
        }
    }
}

/// A token represents a lexical unit in the source code
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Span,
}

impl DisplaySource for Token {
    fn display_source<'a>(&self, source: &'a str) -> &'a str {
        &source[self.position.start..self.position.end]
    }
}

/// A stream of tokens
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TokenStream {
    inner: Vec<Token>,
    cursor: usize,
    eof: Span,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            eof: tokens
                .last()
                .map(|token| token.position)
                .expect("lexed tokens should have EOF at the end"),
            inner: tokens,
            cursor: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let cursor = self.cursor;
        self.cursor = usize::min(self.cursor + 1, self.inner.len() - 1);
        self.inner[cursor]
    }

    pub fn prev_token(&mut self) -> Token {
        self.cursor = self.cursor.saturating_sub(1);
        debug_assert!(!self.inner.is_empty());
        self.inner[self.cursor]
    }

    pub fn peek_token(&self) -> Token {
        self.inner[self.cursor]
    }

    pub fn peek_prev_token(&self) -> Token {
        let cursor = self.cursor.saturating_sub(1);
        debug_assert!(!self.inner.is_empty());
        self.inner[cursor]
    }

    pub fn peek_prev(&self) -> TokenKind {
        let cursor = self.cursor.saturating_sub(1);
        debug_assert!(!self.inner.is_empty());
        self.inner[cursor].kind
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> TokenKind {
        self.next_token().kind
    }

    pub fn prev(&mut self) -> TokenKind {
        self.prev_token().kind
    }

    pub fn peek(&self) -> TokenKind {
        self.peek_token().kind
    }

    pub fn consume(&mut self) {
        self.next();
    }
}

impl IntoIterator for TokenStream {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Token;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}