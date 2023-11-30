pub fn exec(args: &[String]) -> Result<(), std::io::Error> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        "No arguments provided.",
    ))
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
}
