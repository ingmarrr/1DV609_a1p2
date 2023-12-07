use std::num::ParseIntError;

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum EvalError {}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum SemanticError {
    #[error("Unsupported operator: {0}")]
    UnsupportedOperator(String),
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(crate::tokenizer::Token),

    #[error("Unexpected end of input.")]
    UnexpectedEof,

    #[error("Expected: {0} , found: {1}")]
    Expected(String, String),

    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),

    #[error(transparent)]
    IntParseError(#[from] ParseIntError),
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenizerError {
    #[error("Floating point number cannot contain multiple dots")]
    MultipleDots,

    #[error("Unterminated String.")]
    UnterminatedString,
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExecError {
    #[error("No arguments provided")]
    NoArgs,

    #[error("Invalid argument: {0}")]
    InvalidArg(String),

    #[error("Missing arguments.")]
    MissingArgs,

    #[error("Too many arguments: {0}")]
    TooManyArgs(String),
}
