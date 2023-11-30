pub type Func = fn(&[String]) -> Result<(), ExecError>;

pub struct Cmd {
    pub name: &'static str,
    pub description: &'static str,
    pub func: Func,
}

const CMDS: &[Cmd] = &[Cmd {
    name: "help",
    description: "Prints this help message",
    func: help,
}];

pub fn help(args: &[String]) -> Result<(), ExecError> {
    Ok(())
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExecError {
    #[error("No arguments provided")]
    NoArgs,

    #[error("Invalid argument: {0}")]
    InvalidArg(String),
}

pub fn exec(args: &[String]) -> Result<(), ExecError> {
    let (cmd, args) = args.split_first().ok_or(ExecError::NoArgs)?;
    let cmd = CMDS
        .iter()
        .find(|c| c.name == cmd)
        .ok_or_else(|| ExecError::InvalidArg(cmd.to_string()))?;

    (cmd.func)(args)
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
        let args = vec!["foo".into(), "bar".into()];
        let result = exec(&args);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ExecError::InvalidArg("foo".into()));
    }

    #[test]
    fn exec_fn_should_ok_on_valid_arg() {
        let args = vec!["help".into()];
        let result = exec(&args);
        assert!(result.is_ok());
    }

    #[test]
    fn exec_should_write_to_out_writer() {
        let args = vec!["help".into()];
        let writer = StdoutWriter::new();
        let result = exec(&args, &writer);
        assert!(result.is_ok());
        assert_eq!(writer.get(), usage());
    }
}
