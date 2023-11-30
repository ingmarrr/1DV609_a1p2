pub type Func = fn(&[String], &mut dyn Writer) -> Result<(), ExecError>;

pub fn exec(args: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    let (cmd, args) = args.split_first().ok_or(ExecError::NoArgs)?;
    let cmd = find_or(cmd, ExecError::InvalidArg(cmd.clone()))?;

    (cmd.func)(args, writer)
}

pub fn help(args: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    match args.len() {
        0 => writer.write(&usage()),
        1 => {
            let cmd = find_or(&args[0], ExecError::InvalidArg(args[0].clone()))?;
            writer.writeln(&format!("Usage:\n  {}", cmd));
        }
        _ => return Err(ExecError::InvalidArg(args[0].clone())),
    }
    Ok(())
}

pub fn repl(_: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    Ok(())
}

pub fn sim(_: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    Err(ExecError::MissingArgs)
}

fn usage() -> String {
    let mut buf = String::from("Usage:\n");
    for cmd in CMDS {
        buf.push_str(&format!("  {}\n", cmd));
    }
    buf
}

fn find_or(cmd: &str, err: ExecError) -> Result<&Cmd, ExecError> {
    CMDS.iter().find(|c| c.name == cmd).ok_or(err)
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

pub struct Cmd {
    pub name: &'static str,
    pub args: &'static [&'static str],
    pub description: &'static str,
    pub func: Func,
}

impl std::fmt::Display for Cmd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:<10} {:<10} - {}",
            self.name,
            self.args.join(" "),
            self.description
        )
    }
}

const CMDS: &[Cmd] = &[
    Cmd {
        name: "help",
        args: &["<command>"],
        description: "Prints the list of commands available.",
        func: help,
    },
    Cmd {
        name: "repl",
        args: &[],
        description: "Starts the REPL.",
        func: repl,
    },
    Cmd {
        name: "sim",
        args: &["<file>"],
        description: "Simulates a program.",
        func: sim,
    },
];

pub trait Writer {
    fn write(&mut self, s: &str);
    fn writeln(&mut self, s: &str) {
        self.write(s);
        self.write("\n");
    }
}

pub struct NoopWriter {}

impl NoopWriter {
    pub fn new() -> Self {
        Self {}
    }
}

impl Writer for NoopWriter {
    fn write(&mut self, _: &str) {}
    fn writeln(&mut self, s: &str) {}
}

pub struct StdoutWriter {}

impl StdoutWriter {
    pub fn new() -> Self {
        Self {}
    }
}

impl Writer for StdoutWriter {
    fn write(&mut self, s: &str) {
        print!("{}", s);
    }

    fn writeln(&mut self, s: &str) {
        println!("{}", s);
    }
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

    fn writeln(&mut self, s: &str) {
        self.buf.push_str(s);
        self.buf.push_str("\n");
    }
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

    #[test]
    fn exec_should_err_too_many_args_for_any_cmd() {
        let args = vec!["help".into(), "foo".into(), "bar".into()];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ExecError::TooManyArgs("bar".into()));
    }

    #[test]
    fn help_should_print_usage_for_command() {
        let args = vec!["help".into(), "repl".into()];
        let mut writer = BufWriter::new();
        let result = exec(&args, &mut writer);
        println!("Buf: {}", writer.buf);
        assert!(result.is_ok());
        let replcmd = find_or("repl", ExecError::InvalidArg("repl".into())).unwrap();
        let expected = format!("Usage:\n  {}\n", replcmd);
        assert_eq!(writer.buf, expected);
    }

    #[test]
    fn sim_should_err_on_missing_file() {
        let args = vec!["sim".into()];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ExecError::MissingArgs);
    }
}
