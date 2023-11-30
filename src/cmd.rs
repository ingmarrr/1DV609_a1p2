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
            writer.writeln(&format!("Usage:\n  {:<10} - {}", cmd.name, cmd.description));
        }
        _ => return Err(ExecError::InvalidArg(args[0].clone())),
    }
    Ok(())
}

pub fn repl(_: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    Ok(())
}

pub fn sim(_: &[String], writer: &mut dyn Writer) -> Result<(), ExecError> {
    Ok(())
}

fn usage() -> String {
    let mut buf = String::from("Usage:\n");
    for cmd in CMDS {
        buf.push_str(&format!("  {:<10} - {}\n", cmd.name, cmd.description));
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
}

pub struct Cmd {
    pub name: &'static str,
    pub description: &'static str,
    pub func: Func,
}

const CMDS: &[Cmd] = &[
    Cmd {
        name: "help",
        description: "Prints the list of commands available.",
        func: help,
    },
    Cmd {
        name: "repl",
        description: "Starts the REPL.",
        func: repl,
    },
    Cmd {
        name: "sim",
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
    fn help_should_print_usage_for_command() {
        let args = vec!["help".into(), "repl".into()];
        let mut writer = BufWriter::new();
        let result = exec(&args, &mut writer);
        println!("Buf: {}", writer.buf);
        assert!(result.is_ok());
        let replcmd = find_or("repl", ExecError::InvalidArg("repl".into())).unwrap();
        let expected = format!("Usage:\n  {:<10} - {}\n", replcmd.name, replcmd.description);
        assert_eq!(writer.buf, expected);
    }

    #[test]
    fn sim_should_err_on_missing_file() {
        let args = vec!["sim".into()];
        let result = exec(&args, &mut NoopWriter::new());
        assert!(result.is_err());
    }
}
