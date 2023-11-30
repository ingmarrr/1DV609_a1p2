#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExecError {
    #[error("No arguments provided")]
    NoArgs,

    #[error("Invalid argument: {0}")]
    InvalidArg(String),
}

pub fn exec(args: &[String]) -> Result<(), ExecError> {
    match args.len() {
        0 => Err(ExecError::NoArgs),
        _ => Err(ExecError::InvalidArg(args[0].clone())),
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn exec_should_err_on_empty() {
        let args = vec![];
        let result = exec(&args);
        assert!(result.is_err());
    }

    #[test]
    fn exec_should_err_on_invalid_args() {
        let args = vec!["foo".to_string(), "bar".to_string()];
        let result = exec(&args);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            ExecError::InvalidArg("foo".to_string())
        );
    }
}
