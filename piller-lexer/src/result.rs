pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    InvalidToken(String),
    #[error("{0}")]
    InvalidInt(#[from] std::num::ParseIntError),
    #[error("{0}")]
    InvalidFloat(#[from] std::num::ParseFloatError),
    #[error("reached nexpected end of file (EOF)")]
    UnexpectedEof,
    #[error("unterminated string literal")]
    UnterminatedString,
}