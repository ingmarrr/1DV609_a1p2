use std::io::Write;

use crate::errors::ExecError;

pub type Func = fn(&[String], &mut dyn Write) -> Result<(), ExecError>;

pub fn exec(args: &[String], writer: &mut dyn Write) -> Result<(), ExecError> {
    let (cmd, args) = args.split_first().ok_or(ExecError::NoArgs)?;
    let cmd = find_or(cmd, ExecError::InvalidArg(cmd.clone()))?;
    if args.len() > cmd.args.len() {
        return Err(ExecError::TooManyArgs(args[cmd.args.len()].clone()));
    }

    (cmd.func)(args, writer)
}

pub fn help(args: &[String], writer: &mut dyn Write) -> Result<(), ExecError> {
    match args.len() {
        0 => {
            writer.write(&usage().as_bytes());
        }
        // Can only be one argument, since we check in the exec function if
        // the user provided too many arguments.
        _ => {
            let cmd = find_or(&args[0], ExecError::InvalidArg(args[0].clone()))?;
            writer.write(&format!("Usage:\n  {}\n", cmd).as_bytes());
        }
    };
    writer.flush();
    Ok(())
}

pub fn repl(_: &[String], writer: &mut dyn Write) -> Result<(), ExecError> {
    Ok(())
}

pub fn sim(_: &[String], writer: &mut dyn Write) -> Result<(), ExecError> {
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

pub struct NoopWriter {}

impl NoopWriter {
    pub fn new() -> Self {
        Self {}
    }
}

impl Write for NoopWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
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

impl Write for BufWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buf.push_str(std::str::from_utf8(buf).unwrap());
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
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
