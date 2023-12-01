#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenizerError {
    #[error("Floating point number cannot contain multiple dots")]
    MultipleDots,
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
