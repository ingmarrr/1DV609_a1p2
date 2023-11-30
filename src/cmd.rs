#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn exec_should_err_on_empty() {
        let args = vec![];
        let result = exec(args);
        assert!(result.is_err());
    }
}
