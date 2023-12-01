#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_should_ok_on_empty() {
        let mut parse = Parser::new("");
        let result = parse.parse();
        assert_eq!(result, Ok(vec![]));
    }
}

