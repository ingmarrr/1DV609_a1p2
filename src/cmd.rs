#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExecError {
    #[error("No arguments provided")]
    NoArgs,
}

pub fn exec(args: &[String]) -> Result<(), ExecError> {
    Err(ExecError::NoArgs)
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
        assert_eq!(result.unwrap_err(), ExecError::InvaliArgument("foo".into()));
    }
}
