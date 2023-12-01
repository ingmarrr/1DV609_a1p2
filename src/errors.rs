#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(crate::tokenizer::Token),
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
