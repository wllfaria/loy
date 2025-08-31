mod result;

use std::{iter::Peekable, str::CharIndices};

use result::Result;

trait IntoSpan {
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

trait IntoToken {
    fn into_token(self, span: impl IntoSpan) -> Token;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Eof,
    Type,
}

impl IntoToken for TokenKind {
    fn into_token(self, span: impl IntoSpan) -> Token {
        Token {
            kind: self,
            position: span.into_span(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    kind: TokenKind,
    position: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenStream {
    inner: Vec<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { inner: tokens }
    }
}

#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn eof(&self) -> Token {
        TokenKind::Eof.into_token(self.source.len())
    }

    pub fn lex(&mut self) -> Result<TokenStream> {
        let mut tokens = vec![];
        if self.source.is_empty() {
            tokens.push(self.eof());
            return Ok(TokenStream::new(tokens));
        }

        while let Some((byte_pos, curr)) = self.next() {
            if curr.is_whitespace() {
                continue;
            }

            let next = self.peek();

            match (curr, next) {
                ('a'..='z' | 'A'..='Z' | '_', _) => self.take_identifier(byte_pos),
                _ => {}
            }
        }

        tokens.push(self.eof());

        Ok(TokenStream::new(tokens))
    }

    fn take_while<F>(&mut self, f: F, start: usize) -> usize
    where
        F: Fn(char) -> bool,
    {
        let mut last = (start, char::default());
        while let Some((byte_pos, ch)) = self.peek() {
            if !f(ch) {
                break;
            };

            last = (byte_pos, ch);
            self.next();
        }

        last.0 + last.1.len_utf8()
    }

    fn take_identifier(&mut self, start: usize) {
        let end = self.take_while(|c| !is_space(c), start);
        let identifier = &self.source[start..end];
        println!("{identifier}");
    }
}

#[inline]
fn is_space(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}
