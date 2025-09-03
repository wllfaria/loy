use std::str::CharIndices;

use crate::cursor::{Cursor, is_valid_identifier_char};
use crate::result::{Error, Result};
use crate::token::{IntoToken, Number, Token, TokenKind, TokenStream};

#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: Cursor<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: Cursor::new(source.char_indices()),
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek(0).copied()
    }

    fn peek2(&mut self) -> Option<(usize, char)> {
        self.chars.peek(1).copied()
    }

    fn eof(&self) -> Token {
        TokenKind::Eof.into_token(self.source.len())
    }

    fn double_token(&mut self, kind: TokenKind, start: usize) -> Token {
        self.next();
        kind.into_token((start, start + 2))
    }

    fn triple_token(&mut self, kind: TokenKind, start: usize) -> Token {
        self.next();
        self.next();
        kind.into_token((start, start + 3))
    }

    pub fn lex(&mut self) -> Result<TokenStream> {
        let mut tokens = vec![];
        while let Some((byte_pos, curr)) = self.next() {
            if curr.is_whitespace() {
                continue;
            }

            let next = self.peek().map(|(_, ch)| ch);
            let next2 = self.peek2().map(|(_, ch)| ch);
            let token = match (curr, next, next2) {
                // triple character tokens
                ('<', Some('<'), Some('=')) => self.triple_token(TokenKind::LShiftAssign, byte_pos),
                ('>', Some('>'), Some('=')) => self.triple_token(TokenKind::RShiftAssign, byte_pos),
                ('/', Some('/'), Some('/')) => self.take_doc_comment(byte_pos),

                // double characters assignment operators
                ('+', Some('='), _) => self.double_token(TokenKind::PlusAssign, byte_pos),
                ('-', Some('='), _) => self.double_token(TokenKind::MinusAssign, byte_pos),
                ('*', Some('='), _) => self.double_token(TokenKind::MulAssign, byte_pos),
                ('/', Some('='), _) => self.double_token(TokenKind::DivAssign, byte_pos),
                ('%', Some('='), _) => self.double_token(TokenKind::ModAssign, byte_pos),
                ('&', Some('='), _) => self.double_token(TokenKind::BitAndAssign, byte_pos),
                ('|', Some('='), _) => self.double_token(TokenKind::BitOrAssign, byte_pos),
                ('^', Some('='), _) => self.double_token(TokenKind::BitXorAssign, byte_pos),

                // double character arithmetic operators
                ('+', Some('+'), _) => self.double_token(TokenKind::Increment, byte_pos),
                ('-', Some('-'), _) => self.double_token(TokenKind::Decrement, byte_pos),

                // double character logical operators
                ('|', Some('|'), _) => self.double_token(TokenKind::Or, byte_pos),
                ('&', Some('&'), _) => self.double_token(TokenKind::And, byte_pos),

                // double character comparison operators
                ('=', Some('='), _) => self.double_token(TokenKind::Equal, byte_pos),
                ('!', Some('='), _) => self.double_token(TokenKind::NotEqual, byte_pos),
                ('<', Some('='), _) => self.double_token(TokenKind::LesserEqual, byte_pos),
                ('>', Some('='), _) => self.double_token(TokenKind::GreaterEqual, byte_pos),

                ('/', Some('*'), _) => self.take_multiline_comment(byte_pos)?,
                ('/', Some('/'), _) => self.take_single_line_comment(byte_pos),
                ('-', Some('>'), _) => self.double_token(TokenKind::ThinArrow, byte_pos),
                ('-', Some('0'..='9'), _) => self.take_number(byte_pos)?,

                // arithmetic operators
                ('+', _, _) => TokenKind::Plus.into_token(byte_pos),
                ('-', _, _) => TokenKind::Minus.into_token(byte_pos),
                ('*', _, _) => TokenKind::Star.into_token(byte_pos),
                ('/', _, _) => TokenKind::Div.into_token(byte_pos),
                ('%', _, _) => TokenKind::Mod.into_token(byte_pos),

                // bitwise operators
                ('~', _, _) => TokenKind::BitNot.into_token(byte_pos),
                ('&', _, _) => TokenKind::BitAnd.into_token(byte_pos),
                ('|', _, _) => TokenKind::BitOr.into_token(byte_pos),
                ('^', _, _) => TokenKind::BitXor.into_token(byte_pos),

                // logical operators
                ('!', _, _) => TokenKind::Not.into_token(byte_pos),

                // comparison operators
                ('<', _, _) => TokenKind::Lesser.into_token(byte_pos),
                ('>', _, _) => TokenKind::Greater.into_token(byte_pos),

                ('=', _, _) => TokenKind::Assign.into_token(byte_pos),
                ('{', _, _) => TokenKind::LBrace.into_token(byte_pos),
                ('}', _, _) => TokenKind::RBrace.into_token(byte_pos),
                ('(', _, _) => TokenKind::LParen.into_token(byte_pos),
                (')', _, _) => TokenKind::RParen.into_token(byte_pos),
                (':', _, _) => TokenKind::Colon.into_token(byte_pos),
                (';', _, _) => TokenKind::SemiColon.into_token(byte_pos),
                (',', _, _) => TokenKind::Comma.into_token(byte_pos),
                ('.', _, _) => TokenKind::Dot.into_token(byte_pos),

                ('"', _, _) => self.take_string(byte_pos)?,
                ('a'..='z' | 'A'..='Z' | '_', _, _) => self.take_identifier(byte_pos),
                ('0'..='9', _, _) => self.take_number(byte_pos)?,
                t => todo!("{t:?}"),
            };

            tokens.push(token);
        }

        tokens.push(self.eof());
        Ok(TokenStream::new(tokens))
    }

    /// Parses a string literal
    fn take_string(&mut self, start: usize) -> Result<Token> {
        loop {
            let Some((byte_pos, curr)) = self.next() else {
                return Err(Error::UnexpectedEof);
            };

            match curr {
                // consume the next character regardless of what it is since its being escaped
                '\\' => _ = self.next(),
                '\n' => return Err(Error::UnterminatedString),
                '"' => break Ok(TokenKind::String.into_token((start + 1, byte_pos))),
                _ => {}
            }
        }
    }

    /// Parses an identifier
    fn take_identifier(&mut self, start: usize) -> Token {
        let end = self.take_while(|c| !is_valid_identifier_char(c), start);
        let identifier = &self.source[start..end];
        TokenKind::from_identifier(identifier).into_token((start, end))
    }

    /// Parses a number literal
    fn take_number(&mut self, start: usize) -> Result<Token> {
        let end = self.take_while(|c| matches!(c, '_' | '.' | '0'..='9'), start);
        let literal = &self.source[start..end];

        let number_of_dots = literal
            .chars()
            .fold(0, |acc, ch| if ch == '.' { acc + 1 } else { acc });

        if number_of_dots > 1 {
            let message = format!("number literal '{literal}' has too many decimal separators");
            return Err(Error::InvalidToken(message));
        }

        let literal = literal.replace('_', "");
        let is_signed = literal.contains('-');
        let is_float = literal.contains('.');

        match (is_signed, is_float) {
            (false, false) => Ok(Number::Unsigned(literal.parse()?).into_token((start, end))),
            (true, false) => Ok(Number::Signed(literal.parse()?).into_token((start, end))),
            (_, true) => Ok(Number::Float(literal.parse()?).into_token((start, end))),
        }
    }

    /// Parses a single-line comment
    fn take_single_line_comment(&mut self, start: usize) -> Token {
        let end = self.take_while(|ch| !matches!(ch, '\n'), start);
        TokenKind::Comment.into_token((start, end + 1))
    }

    /// Parses a multi-line comment
    fn take_multiline_comment(&mut self, start: usize) -> Result<Token> {
        self.next(); // consuming the * from /*
        let mut open_comments = 1;

        loop {
            let Some((byte_pos, curr)) = self.next() else {
                return Err(Error::UnterminatedMultilineComment);
            };

            let next = self.peek().map(|(_, ch)| ch);
            match (curr, next) {
                ('/', Some('*')) => {
                    self.next();
                    open_comments += 1;
                }
                ('*', Some('/')) => {
                    self.next();
                    open_comments -= 1;
                }
                _ => {}
            }

            if open_comments == 0 {
                break Ok(TokenKind::Comment.into_token((start, byte_pos + 1)));
            }
        }
    }

    /// Parses a documentation comment
    fn take_doc_comment(&mut self, start: usize) -> Token {
        let end = self.take_while(|ch| !matches!(ch, '\n'), start);
        TokenKind::DocComment.into_token((start, end + 1))
    }

    /// Continues parsing while the predicate returns true
    fn take_while<F>(&mut self, f: F, start: usize) -> usize
    where
        F: Fn(char) -> bool,
    {
        let mut last = (start, char::default());
        while let Some((byte_pos, ch)) = self.peek() {
            if !f(ch) {
                break;
            }
            last = (byte_pos, ch);
            self.next();
        }

        last.0 + last.1.len_utf8()
    }
}