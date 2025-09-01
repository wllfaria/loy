/// A trait for displaying the source text of a token
pub trait DisplaySource {
    fn display_source<'a>(&self, source: &'a str) -> &'a str;
}

/// A trait for converting a value into a span
pub trait IntoSpan {
    fn into_span(self) -> Span;
}

impl IntoSpan for usize {
    fn into_span(self) -> Span {
        Span {
            start: self,
            end: self,
        }
    }
}

impl IntoSpan for (usize, usize) {
    fn into_span(self) -> Span {
        let (start, end) = self;
        Span { start, end }
    }
}

/// A trait for converting a value into a token
pub trait IntoToken {
    fn into_token(self, span: impl IntoSpan) -> Token;
}

/// A span represents a range of characters in the source code
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
    fn into_token(self, span: impl IntoSpan) -> Token {
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
    Constant,
    Function,
    Variable,

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
    LShift, // <<
    RShift, // >>

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

    // Other
    ThinArrow, // ->
}

impl TokenKind {
    /// Converts an identifier string to a token kind
    pub fn from_identifier(identifier: &str) -> Self {
        match identifier {
            "if" => Self::If,
            "else" => Self::Else,
            "type" => Self::Type,
            "struct" => Self::Struct,
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
}

impl IntoToken for TokenKind {
    fn into_token(self, span: impl IntoSpan) -> Token {
        Token {
            kind: self,
            position: span.into_span(),
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
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { inner: tokens }
    }
}

impl IntoIterator for TokenStream {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Token;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
