use std::collections::VecDeque;

/// An iterator wrapper that allows peeking multiple items ahead.
/// This is used by the lexer to perform lookahead for multi-character tokens.
#[derive(Debug)]
pub struct Cursor<I: Iterator> {
    iter: I,
    buffer: VecDeque<I::Item>,
}

impl<I: Iterator> Cursor<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            buffer: VecDeque::new(),
        }
    }

    pub fn next(&mut self) -> Option<I::Item> {
        if let Some(item) = self.buffer.pop_front() { Some(item) } else { self.iter.next() }
    }

    pub fn peek(&mut self, n: usize) -> Option<&I::Item> {
        while self.buffer.len() <= n {
            match self.iter.next() {
                Some(item) => self.buffer.push_back(item),
                None => break,
            }
        }
        self.buffer.get(n)
    }
}

/// Determines if a character is valid in an identifier.
/// Valid identifier characters are letters, digits, and underscore.
// NOTE: identifiers cannot start on a digit, so this is used to verify internal characters
#[inline]
pub fn is_valid_identifier_char(ch: char) -> bool {
    !matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}
