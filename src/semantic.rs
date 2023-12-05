#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn semantics_check_should_fail_for_unsupported_operators() {
        let mut ast = parser::parse("1 + \"hello\"");
        let result = semantic::check(&mut ast);
        assert!(result.is_err());
    }
}
