pub type Func = fn(&[String], &mut dyn Writer) -> Result<(), ExecError>;

pub trait Writer {
    fn write(&mut self, s: &str);
}

pub struct NoopWriter {}

impl NoopWriter {
    pub fn new() -> Self {
        Self {}
    }
}

impl Writer for NoopWriter {
    fn write(&mut self, _: &str) {}
}

pub struct BufWriter {
    buf: String,
}

impl BufWriter {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }
}

impl Writer for BufWriter {
    fn write(&mut self, s: &str) {
        self.buf.push_str(s);
    }
}

pub struct Cmd {
    pub name: &'static str,
    pub description: &'static str,
    pub func: Func,
}

const CMDS: &[Cmd] = &[Cmd {
    name: "help",
    description: "Prints the list of commands available.",
    func: help,
}];

pub fn help(args: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    match args.len() {
        0 => writer.write(&usage()),
        _ => return Err(ExecError::InvalidArg(args[0].clone())),
    }
    Ok(())
}

pub fn usage() -> String {
    let mut buf = String::from("Usage:\n");
    for cmd in CMDS {
        buf.push_str(&format!("  {:<10} - {}\n", cmd.name, cmd.description));
    }
    buf
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExecError {
    #[error("No arguments provided")]
    NoArgs,

    #[error("Invalid argument: {0}")]
    InvalidArg(String),
}

pub fn exec(args: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    let (cmd, args) = args.split_first().ok_or(ExecError::NoArgs)?;
    let cmd = CMDS
        .iter()
        .find(|c| c.name == cmd)
        .ok_or_else(|| ExecError::InvalidArg(cmd.to_string()))?;

    (cmd.func)(args, writer)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn exec_should_err_on_empty() {
        let args = vec![];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_err());
    }

    #[test]
    fn exec_should_err_on_invalid_args() {
        let args = vec!["foo".into(), "bar".into()];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ExecError::InvalidArg("foo".into()));
    }

    #[test]
    fn exec_fn_should_ok_on_valid_arg() {
        let args = vec!["help".into()];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_ok());
    }

    #[test]
    fn exec_should_write_to_out_writer() {
        let args = vec!["help".into()];
        let mut writer = BufWriter::new();
        let result = exec(&args, &mut writer);
        assert!(result.is_ok());
        assert_eq!(writer.buf, usage());
    }
}
