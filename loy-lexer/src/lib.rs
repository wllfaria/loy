mod cursor;
mod lexer;
mod result;

#[cfg(test)]
mod tests;

pub use lexer::Lexer;
pub use result::{Error, Result};